;; network-connection-ss.lisp -- Secure Network with Self-Sync Stream Encoding
;; --------------------------------------------------------------------------------------
;; Butterfly -- a system for easy distributed computing, going beyond what is available
;; in Erlang with the full power of Common Lisp.
;;
;; Copyright (C) 2008,2009 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08, 06-12/09
;; --------------------------------------------------------------------------------------

(in-package #:com.ral.actors.network)

;; -----------------------------------------------------------------------

(defvar *default-port*            65001.)
(defvar *socket-timeout-period*   20.)
(defvar *ws-collection*           nil)
(defvar *aio-accepting-handle*    nil)

(defconstant +MAX-FRAGMENT-SIZE+ 65536.)

;; -------------------------------------------------------------

(defstruct intf-state
  title
  ip-addr
  io-state
  local-services
  kill-timer
  io-running
  writer
  shutdown)

;; -------------------------------------------------------------------------
;; IO-Running controller - ensures that we don't write when socket has
;; been closed, and closes socket on input error conditions.
;;
;; By placing all non-idempotent actions in leaf Actors, we avoid
;; problems caused by parallel restarts when BECOME fails, since we
;; are transactional. These Actors will only be sent messages when
;; there was no restart. This avoids the need to place such Actors
;; behind a Serializer.
;;
;; By having Actors send a result to a customer, they become sequenceable.

(deflex socket-closer
  (create
   (lambda (cust io-state)
     (comm:close-async-io-state io-state)
     (send cust :ok))))

(deflex forcible-socket-closer
  (create
   (lambda (cust io-state)
     (comm:async-io-state-abort-and-close io-state)
     (send cust :ok))))

;; -----------------------------------------------

(defun io-base-beh (io-state)
  (alambda
   ((cust :terminate)
    (become (io-closed-beh))
    (send forcible-socket-closer cust io-state))

   ((cust :try-writing _ if-nok)
    (send println "Can't happen - attempt to write while busy writing.")
    (funcall if-nok)
    (send cust :err))

   ((cust . _)
    (send cust :err))
   ))

(defun io-closed-beh ()
  ;; Fully closed state
  (alambda
   ((cust :try-writing _ if-nok)
    (funcall if-nok)
    (send cust :err))
   
   ((cust . _)
    (send cust :err))
   ))

(defun io-running-beh (io-state base)
  ;; In this state we are open to async reads, no writing under way,
  (alambda
   ((cust :try-writing if-ok _)
    (become (io-running-write-beh io-state base))
    (send executor cust if-ok))

   ((cust :done-reading)
    (become (io-closed-beh))
    (send socket-closer cust io-state))

   (_
    (repeat-send base))
   ))

(defun io-running-write-beh (io-state base)
  ;; Async reads are active, writing is under way.
  (alambda
   ((cust :finish-wr-ok)
    (become (io-running-beh io-state base))
    (send cust :ok))

   ((cust :finish-wr-fail)
    (become (io-closed-beh))
    (send forcible-socket-closer cust io-state))

   ((cust :done-reading)
    (become (io-running-not-reading-beh io-state base))
    (send cust :ok))

   (_
    (repeat-send base))
   ))

(defun io-running-not-reading-beh (io-state base)
  ;; writing is under way, but async reads are closed down
  (alambda
   ((cust msg) / (member msg '(:finish-wr-ok :finish-wr-fail))
    (become (io-closed-beh))
    (send socket-closer cust io-state))

   (_
    (repeat-send base))
   ))

(defun make-io-running-monitor (io-state)
  (let ((base (create (io-base-beh io-state)) ))
    (create (io-running-beh io-state base))
    ))
    
;; -------------------------------------------------------------------------
;; Socket writer
;;
;;  byte-vec  +-----------+   +------------+    +-------------+
;;     ------>|    Gate   |-->| Serializer |<-->| Phys Writer |
;;            +-----------+   +------------+    +-------------+
;;
;; PhysWriter is the connection to the async output socket port.  The
;; Serializer is to prevent parallel access to the physical socket
;; output port.  Serializers need a customer, even if SINK. Gate
;; provides that SINK.
;;
;; If anything goes wrong, the Phys Writer sends a shutdown signal.
;;

(defun physical-writer-beh (state)
  (lambda (cust byte-vec)
    (with-accessors ((state-io-state intf-state-io-state)
                     (io-running     intf-state-io-running)
                     (kill-timer     intf-state-kill-timer)
                     (shutdown       intf-state-shutdown)) state
      (labels
          ;; these functions execute in the thread of the async socket handler
          ((not-writing ()
             (send cust :err)) ;; clear the Serializer
           
           (write-done (io-state &rest _)
             ;; this is a callback routine, executed in the thread of
             ;; the async collection
             (declare (ignore _))
             (cond ((comm:async-io-state-write-status io-state)
                    (β _
                        (send io-running β :finish-wr-fail)
                      (send shutdown)
                      (send cust :err)))
                   (t
                    (send io-running cust :finish-wr-ok))
                   ))
           
           (begin-write ()
                (comm:async-io-state-write-buffer state-io-state
                                                  byte-vec
                                                  #'write-done)
                (send kill-timer :resched)) )
        
        (send io-running sink :try-writing #'begin-write #'not-writing)
        ))))

(defun make-writer (state)
  (serializer-sink (create (physical-writer-beh state))))

;; -------------------------------------------------------------------------
;; Watchdog Timer - shuts down interface after prologned inactivity

(defun watchdog-timer-beh (killer tag)
  (alambda
   ((:resched)
    (let ((new-tag (tag self)))
      (become (watchdog-timer-beh killer new-tag))
      (send-after *socket-timeout-period* new-tag :timed-out)))
   ((atag :timed-out) / (eql atag tag)
    (send killer))
   ((:discard)
    (become-sink))
   ))

(defun make-kill-timer (timer-fn)
  (actors ((tag    (tag-beh timer))
           (killer timer-fn)
           (timer  (watchdog-timer-beh killer tag)))
    timer))

;; -------------------------------------------------------------
;; List of currently active sockets
;;
;; The socket is denoted externally by the encoder Actor used for
;; sending data through the socket. It is recorded here along with the
;; socket state, and an Actor to be used for its "Channel". Initially
;; both the socket and its channel are the same.
;;
;; Sockets marshal encode, whatever is being sent, into streams of
;; unsigned bytes, possibly compressed for network efficiency, then
;; chopped into packets of length no larger than 64KB. The original
;; data encoding is reassembled at the receiving end, then
;; decompressed and unmarshalled back into Lisp objects.
;;
;; Every socket also has an associated list of local-services, which
;; records session duration services, along with ephemeral reply
;; proxies on the client. An initial service is provided to enable a
;; negotiation for a secure encrypted channel. Encryption is a layer
;; above this socket layer. We have nothing to do with it here, except
;; to offer the initial per-socket local negotiation service.
;;
;; In this model, clients send commands and requests to a server.
;; Servers may or may not reply. Initially, only globally advertised
;; services are available at the server. But once a connection has
;; been established, parties are free to invent their own
;; sub-protocols.
;;
;; Received messages are dispatched to local-services.
;;
;; Message structure for clients:
;;      (SEND chan srv-id :SEND cust-id . message)
;;                |------------------------------|
;;                         = message sent to server local services list.
;;
;; Message structure for server replies:
;;      (SEND chan cust-id :SEND . message)
;;                |-----------------------|
;;                         = message sent to client local services list
;;
;; chan represents either the socket Actor or another (typ. encrypted)
;; channel Actor on the socket.
;;
;; srv-id and cust-id are locally generated UUID's denoting proxy
;; Actors registered in the local services list for the socket on
;; either side.  srv-id would be registered at the receiving server,
;; and cust-id will be registered at the sending client end.
;;

(defstruct connection-rec
  ip-addr ip-port state sender chan
  report-ip-addr handshake custs)

(defun find-connection-from-ip (cnx-lst ip-addr ip-port)
  (find-if (lambda (rec)
             (and (eql ip-addr (connection-rec-ip-addr rec))
                  (eql ip-port (connection-rec-ip-port rec))))
           cnx-lst))

(defun find-connection-from-state (cnx-lst state)
  (find state cnx-lst :key #'connection-rec-state))

(defun find-connection-from-sender (cnx-lst sender)
  (find sender cnx-lst :key #'connection-rec-sender))

(defun counter-beh (n)
  (lambda (cust)
    (let ((new-n (1+ n)))
      (become (counter-beh new-n))
      (send cust n))))

(deflex get-server-count
  (create (counter-beh 0)))

(def-beh connections-list-beh (&optional cnx-lst)
   
  ((:add-server peer-ip peer-port io-state)
   (let ((rec (find-connection-from-ip cnx-lst peer-ip peer-port)))
     (cond (rec
            ;; already exists or is pending
            (send forcible-socket-closer sink io-state))
           
           (peer-ip
            ;; reserve our place while we create a channel
            (let ((rec (make-connection-rec
                        :ip-addr         peer-ip
                        :ip-port         peer-port
                        :state           :pending-server)))
              (become (connections-list-beh (cons rec cnx-lst)))
              (β (ct)
                  (send get-server-count β)
                (let ((server-name (format nil "~A#~D" (machine-instance) ct)))
                  (send fmt-println "Server Socket (~S->~A:~D) starting up"
                        server-name (comm:ip-address-string peer-ip) peer-port)
                  (send (create-socket-intf :kind             :server
                                            :ip-addr          peer-ip
                                            :ip-port          peer-port
                                            :report-ip-addr   server-name
                                            :io-state         io-state)
                        nil))
                )))
            )))
        
  ((cust :add-socket ip-addr ip-port state sender)
   (send cust :ok)
   (let ((rec (find-connection-from-ip cnx-lst ip-addr ip-port)))
     (cond ((and rec
                 (eql :pending-server (connection-rec-state rec)))
            (let ((new-rec (make-connection-rec
                            :ip-addr  ip-addr
                            :ip-port  ip-port
                            :state    state
                            :sender   sender)))
              (become (connections-list-beh (cons new-rec (remove rec cnx-lst))))
              ))
           
           (rec
            (let* ((old-state (connection-rec-state rec))
                   (new-rec   (copy-with rec
                                         :state  state
                                         :sender sender))
                   (new-lst (cons new-rec (remove rec cnx-lst))))
              (unless (or (null old-state)
                          (eql old-state state))
                (send (intf-state-shutdown old-state)))
              (become (connections-list-beh new-lst))
              ))
           
           (t
            (let ((new-rec (make-connection-rec
                            :ip-addr  ip-addr
                            :ip-port  ip-port
                            :state    state
                            :sender   sender)))
              (become (connections-list-beh (cons new-rec cnx-lst)))
              ))
           )))
  
  ((cust :find-socket ip-addr ip-port report-ip-addr handshake)
   (let ((rec (find-connection-from-ip cnx-lst ip-addr ip-port)))
     (if rec
         (let ((custs (connection-rec-custs rec)))
           (if custs
               ;; waiting custs so join the crowd
               (let* ((new-rec (copy-with rec
                                          :custs (cons cust custs)))
                      (new-lst (cons new-rec (remove rec cnx-lst))))
                 (become (connections-list-beh new-lst)))
             ;; else - no waiting list, just send our chan to requestor
             (send cust (connection-rec-chan rec))
             ))
       ;; else - no record yet, so create and wait on the channel
       (let* ((new-rec (make-connection-rec
                        :ip-addr        ip-addr
                        :ip-port        ip-port
                        :report-ip-addr report-ip-addr
                        :handshake      handshake
                        :custs          (list cust)))
              (new-lst (cons new-rec cnx-lst)))
         (become (connections-list-beh new-lst))
         (send (make-socket-connection ip-addr ip-port report-ip-addr))
         ))
     ))
  
  ((:abort ip-addr ip-port)
   (let ((rec (find-connection-from-ip cnx-lst ip-addr ip-port)))
     (when rec
       ;; leaves all waiting custs hanging...
       (become (connections-list-beh (remove rec cnx-lst)))
       (restartable
         (error "Can't connect to: ~S"
                (connection-rec-report-ip-addr rec)))
       )))
  
  ((cust :negotiate state socket)
   (let ((rec (find-connection-from-sender cnx-lst socket)))
     (if rec
         (send (connection-rec-handshake rec) cust socket (intf-state-local-services state))
       ;; else
       (send cust :ok))
     ))
  
  ((cust :set-channel sender chan)
   (send cust :ok)
   (let ((rec (find-connection-from-sender cnx-lst sender)))
     (when rec
       (let* ((custs    (connection-rec-custs rec))
              (state    (connection-rec-state rec))
              (new-rec  (copy-with rec
                                   :chan  chan
                                   :custs nil))
              (new-cnxs (cons new-rec (remove rec cnx-lst))))
         (become (connections-list-beh new-cnxs))
         (when custs
           (restartable
             (multiple-value-bind (peer-ip peer-port)
                 #+:LISPWORKS8
               (comm:socket-connection-peer-address (intf-state-io-state state))
               #+:LISPWORKS7
               (comm:get-socket-peer-address (slot-value (intf-state-io-state state) 'comm::object))
               (send fmt-println "Client Socket (~S->~A:~D) starting up"
                     (connection-rec-report-ip-addr rec)
                     (comm:ip-address-string peer-ip) peer-port))
             (send-to-all custs chan))))
       )))
    
  
  ((cust :remove state)
   (send cust :ok)
   (let ((rec (find-connection-from-state cnx-lst state)))
     (when rec
       (become (connections-list-beh (remove rec cnx-lst))))
     )))

(def-actor connections
  (create (connections-list-beh)))

;; -------------------------------------------------------------

(defun make-socket-shutdown (state)
  (actor ()
    (with-accessors ((kill-timer       intf-state-kill-timer)
                     (io-running       intf-state-io-running)
                     (title            intf-state-title)
                     (ip-addr          intf-state-ip-addr)) state
      (become-sink)
      (send fmt-println "~A Socket (~S) shutting down"
            title ip-addr)
      (β _
          (send io-running β :terminate)
        (send kill-timer :discard)
        (send connections sink :remove state)
        ))))

;; ------------------------------------------------------------------------
;; For both clients and servers, we assemble the socket state, make
;; initial encoder/decoder which marshals and compresses and fragments
;; into 64kB packets. Set up the initial negotiation service in a per
;; socket local-services, start the socket reader going, and return
;; the state, enccoder, and local-services to the customer.

(defun create-socket-intf (&key kind ip-addr ip-port io-state report-ip-addr)
  (create-service
   (lambda (cust)
     (let* ((title (if (eq kind :client) "Client" "Server"))
            (local-services (make-local-services))
            (state (make-intf-state
                    :title            title
                    :ip-addr          report-ip-addr
                    :io-state         io-state
                    :local-services   local-services))
            ;; Use self-sync encoding on the wire. No more prefix
            ;; counts on packets.
            ;;
            ;; Data are chunked to avoid over-large packets. There is a
            ;; limit to allowable size in the async interface.
            ;;
            ;; Initial connection is unencrypted. Once the ECDH dance
            ;; is over, an encrypting preprocessor is used ahead of the
            ;; encoder, and a decrypting postprocessor is used at the
            ;; tail of the decoder.
            (writer  (make-writer state)) ;; async output is sent here
            (encoder (sink-pipe  (marshal-encoder)
                                 (self-sync-encoder)
                                 writer))
            (accum   (self-synca:stream-decoder
                      ;; async arrivals are sent here
                      (sink-pipe (fail-silent-marshal-decoder)
                                 local-services)))
            (packet-ctr 0)  ;; a counter of input packet fragments 
            (shutdown (once (make-socket-shutdown state))))
       (β  _
           ;; provide a service to establish an encrypted channel
           (send local-services β :add-service-with-id +server-connect-id+
                 (server-crypto-gateway encoder local-services))
         
         (β  _
             (send connections β :add-socket ip-addr ip-port state encoder)
           
           (with-accessors ((title            intf-state-title)
                            (io-state         intf-state-io-state)
                            (kill-timer       intf-state-kill-timer)
                            (io-running       intf-state-io-running)
                            (state-writer     intf-state-writer)
                            (state-shutdown   intf-state-shutdown)) state
             
             (setf kill-timer (make-kill-timer
                               #'(lambda ()
                                   (send println "Inactivity shutdown request")
                                   (send shutdown)))
                   state-writer   writer
                   state-shutdown shutdown
                   io-running     (make-io-running-monitor io-state))
             
             (labels
                 ((rd-callback-fn (state buffer end)
                    ;; callback for I/O thread - on continuous async read
                    #|
                    (send fmt-println "Socket Reader Callback (STATUS = ~A, END = ~A)"
                          (comm:async-io-state-read-status state)
                          end)
                    |#
                    (let ((status (or (comm:async-io-state-read-status state)
                                      (when (> end +max-fragment-size+)
                                        "Incoming packet too large"))
                                  ))
                      (cond (status
                             ;; terminate on any error
                             (comm:async-io-state-finish state)
                             (send fmt-println "~A Incoming error state: ~A" title status)
                             (β _
                                 (send io-running β :done-reading)
                               (send shutdown)))
                           
                            ((plusp end)
                             ;; Every input packet is numbered here,
                             ;; starting from 1. This enables us to
                             ;; deal properly with possible
                             ;; out-of-order delivery to the self-sync
                             ;; decoder. They arrive on the wire and
                             ;; are delivered here in temporal order.
                             ;; But the Actor subsystem might jumble
                             ;; some deliveries to the decoder. (I have
                             ;; witnessed this happening.)
                             (send accum :deliver (incf packet-ctr) (subseq buffer 0 end))
                             (send kill-timer :resched)
                             (comm:async-io-state-discard state end))
                            )))
                  (send-shutdown ()
                    (send shutdown)))
              
               (send kill-timer :resched)
               (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                       :element-type '(unsigned-byte 8))
               (send cust state encoder)
               ))))))))
  
;; -------------------------------------------------------------

(defun make-socket-connection (ip-addr ip-port report-ip-addr)
  (let ((handler (α (io-state args)
                   (cond
                    (args
                     (send println
                           (format nil "CONNECTION-ERROR: ~S" report-ip-addr)
                           (apply #'format nil args))
                     (send connections :abort ip-addr ip-port))
                    
                    (t
                     (β (state socket)
                         (send (create-socket-intf :kind           :client
                                                   :ip-addr        ip-addr
                                                   :ip-port        ip-port
                                                   :report-ip-addr report-ip-addr
                                                   :io-state       io-state)
                               β)
                       (send connections nil :negotiate state socket)))
                    ))))

    (flet ((callback (io-state args)
             ;; performed in the process of collection, so keep it short.
             (send handler io-state args)))
      
      (α ()
        (apply 'mp:funcall-async 'comm:create-async-io-state-and-connected-tcp-socket
               *ws-collection*
               ip-addr ip-port #'callback
               #-:WINDOWS
               `(:connect-timeout 5 :ipv6 nil)
               #+:WINDOWS
               `(:connect-timeout 5)
               ))
      )))

(defun canon-ip-addr (ip-addr)
  (comm:get-host-entry ip-addr :fields '(:address)))

(defgeneric parse-ip-addr (addr)
  (:method ((addr integer))
   addr)
  (:method ((addr string))
   ;; handle addresses like: "arroyo.local:65001"
   ;; return ip addr and port separately
   (let ((cpos (position #\: addr)))
     (if cpos
         (values (subseq addr 0 cpos)
                 (read-from-string (subseq addr (1+ cpos))))
       addr))))

(def-actor client-connector
  ;; Called from client side wishing to connect to a server.
  (α (cust handshake ip-addr &optional (ip-port *default-port*))
    (multiple-value-bind (addr port)
        (parse-ip-addr ip-addr)
      (let ((clean-ip-addr (canon-ip-addr addr))
            (port          (or port ip-port *default-port*)))
        (unless clean-ip-addr
          (error "Unknown host: ~S" ip-addr))
        (send connections cust :find-socket clean-ip-addr port ip-addr handshake)
        ))))

;; -------------------------------------------------------------

(defun start-server-messenger (accepting-handle io-state)
  "Internal routine to start a network interface from the server side.
The interface is identical to that on the client side, but the
connection handshake differs from this side.

See the discussion under START-CLIENT-MESSENGER for details."

  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (declare (ignore accepting-handle))
  (multiple-value-bind (peer-ip peer-port)
      #+:LISPWORKS8 (comm:socket-connection-peer-address io-state)
      #+:LISPWORKS7 (comm:get-socket-peer-address (slot-value io-state 'comm::object))
      (send connections :add-server peer-ip peer-port io-state)
      ))

;; --------------------------------------------------------------

(defun terminate-server (reply-to)
  (if *aio-accepting-handle*
      (progn
        (comm:close-accepting-handle *aio-accepting-handle*
                                     (lambda (coll)
                                       ;; we are operating in the collection process
                                       (comm:close-wait-state-collection coll)
                                       (setf *aio-accepting-handle* nil
                                             *ws-collection*        nil)
                                       (unwind-protect
                                           (mp:process-terminate (mp:get-current-process))
                                         (send reply-to :ok)))))
    ;; else
    (send reply-to :ok)))

(defun start-tcp-server (&optional (tcp-port-number *default-port*))
  "An internal routine to start up a server listener socket on the
indicated port number."
  (let ((starter (actor _
                   (setq *ws-collection*
                         (comm:create-and-run-wait-state-collection "Actor Server"))
                   (setq *aio-accepting-handle* 
                         (comm:accept-tcp-connections-creating-async-io-states
                          *ws-collection*
                          tcp-port-number
                          #'start-server-messenger
                          :ipv6    nil
                          ))
                   (send fmt-println "Actor Server started on port ~A" tcp-port-number))))
    (terminate-server starter)))

;; --------------------------------------------------
;;

(defun reset-global-state ()
  (setf *ws-collection*        nil
        *aio-accepting-handle* nil))

(defun* lw-start-actor-server _
  ;; called by Action list with junk args
  ;;
  ;; We need to delay the construction of the system logger till this
  ;; time so that we get a proper background-error-stream.  Cannot be
  ;; performed on initial load of the LFM.
  (unless *ws-collection*
    (start-tcp-server)))

(def-actor terminator
  (create-service
   (lambda (cust)
     (terminate-server cust))))

(defun* lw-reset-actor-server _
  (ask terminator)
  (reset-global-state)
  (print "Actor Server has been shut down."))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor Server"
                    'lw-start-actor-server
                    :after "Start up Actor System" ;; "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor Server"
                    'lw-reset-actor-server
                    :before "Stop Actor System")

  (lw:define-action "Save Session After"
                    "Restart Actor Server"
                    'lw-start-actor-server
                    :after "Restart Actor System")
  )

(defun com.ral.actors:start ()
  (lw-start-actor-server))

#| ;; for manual loading mode
(unless *ws-collection*
  (if (mp:get-current-process)
      (lw-start-tcp-server)
    (pushnew '("Start Actor Server" () lw-start-tcp-server) mp:*initial-processes*
             :key #'third)))
|#
