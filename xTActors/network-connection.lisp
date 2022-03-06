;; bfly-socket.lisp
;; --------------------------------------------------------------------------------------
;; Butterfly -- a system for easy distributed computing, going beyond what is available
;; in Erlang with the full power of Common Lisp.
;;
;; Copyright (C) 2008,2009 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08, 06-12/09
;; --------------------------------------------------------------------------------------

(in-package #:com.ral.actors.network)

(um:eval-always
  (hcl:add-package-local-nickname :sec-comm :com.ral.actors.secure-comm)
  (hcl:add-package-local-nickname :act-base :com.ral.actors.base))

(um:eval-always
  (import '(um:when-let
            um:wr
            sec-comm:make-local-services
            sec-comm:server-crypto-gateway
            sec-comm:+server-connect-id+
            ;; act-base::dbg-println
            act-base::make-ubv

            ;; act-base::with-printer
            )))

#|
(defun dbg-println (fmtstr &rest args)
  (with-printer (s *standard-output*)
    (apply #'format s fmtstr args)
    (terpri s)
    (finish-output s)))
|#

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
  (io-running (list 1))
  decr-io-count-fn
  writer
  shutdown)

;; -------------------------------------------------------------------------
;; Socket writer
;;
;;  byte-vec  +-----------+   +------------+    +---------------+    +------------+    +-------------+
;;     ------>| Discarder |-->| Serializer |<-->| Prefix Writer |<-->| Write Gate |<-->| Phys Writer |
;;            +-----------+   +------------+    +---------------+    +------------+    +-------------+
;;
(defun physical-writer-beh (state)
  (with-accessors ((decr-io-count  intf-state-decr-io-count-fn)
                   (state-io-state intf-state-io-state)
                   (io-running     intf-state-io-running)
                   (kill-timer     intf-state-kill-timer)) state
    (alambda
     ((:discard)
      (become (sink-beh)))
     
     ((cust byte-vec)
      (let ((me  self))
        (labels
            ((finish-fail (io-state)
               (funcall decr-io-count io-state)
               (send cust me :fail))
             (finish-ok (io-state)
               (send cust me
                     (if (zerop (funcall decr-io-count io-state))
                         :fail
                       :ok)))
             (write-done (io-state &rest ignored)
               ;; this is a callback routine, executed in the thread of
               ;; the async collection
               (declare (ignore ignored))
               (cond ((comm:async-io-state-write-status io-state)
                      (finish-fail io-state))
                     (t
                      (finish-ok io-state))
                     )))
          (cond
           ((sys:compare-and-swap (car io-running) 1 2) ;; still running recieve?
            (comm:async-io-state-write-buffer state-io-state
                                              byte-vec #'write-done)
            (send kill-timer :resched))
           
           (t
            (send cust self :fail))
           )))))))

(defun write-gate-beh (state ser-gate phys-writer)
  (alambda
   ((a-tag :fail) / (eql a-tag phys-writer)
    ;; no reply to serializer, keeps it locked up
    (send (intf-state-shutdown state)))

   ((a-tag :ok) / (eql a-tag phys-writer)
    ;; reply to serializer to prod next message
    (send ser-gate :ok))

   ((a-tag byte-vec)
    ;; next message from serializer, its tag is injected as customer.
    ;; Forward byte-vec to phys-writer, injecting ourself as customer.
    ;; (send dbg-println "-- send across network ~D --" (length byte-vec))
    (become (write-gate-beh state a-tag phys-writer))
    (send phys-writer self byte-vec))
   ))

(defun prefixing-write-beh (writer)
  ;; writes a 4-byte Little-Endian prefix count
  ;; and stages the write of the byte-vec afterward
  (λ (a-tag byte-vec)
    (let ((pref (make-ubv 4))
          (size (length byte-vec)))
      (dotimes (ix 4)
        (setf (aref pref ix) (ldb (byte 8 (ash ix 3)) size)))
      (send writer self pref)
      (become (pend-prefixing-write-beh a-tag byte-vec writer))
      )))

(defun pend-prefixing-write-beh (tag byte-vec writer)
  ;; writes the actual byte vector
  (λ _
    (send writer tag byte-vec)
    (become (prefixing-write-beh writer))))

(defun discarder-beh (ser-gate phys-writer)
  ;; pass thru byte-vecs until we get a :DISCARD signal
  (alambda
   ((:discard)
    (send phys-writer :discard)
    (become (sink-beh)))
   (msg
    ;; provide SINK pseudo-customer for SERIALIZER
    (send* ser-gate sink msg))
   ))

(defun make-writer (state)
  ;; serializer needs a customer on every message, so it can interpose
  ;; between senders and the serialized Actor. We inject SINK as the
  ;; customer on write-messages, which are simply byte vectors and no
  ;; customer.
  (actors ((phys-writer (physical-writer-beh state))
           (writer      (write-gate-beh state nil phys-writer))
           (prefixer    (serializer-beh
                         (create
                          (prefixing-write-beh writer))))
           (discarder   (discarder-beh prefixer phys-writer)))
    discarder))

;; -------------------------------------------------------------------------
;; Incoming Packet Aggregation - maintain a queue of incoming packet fragments
;; and peel off as requested by reader.

(defun holding-accum-beh (deliv pkt-ix scrap start2 end2)
  (alambda
   ((cust :req buf nb)
    (let ((nel (min (- end2 start2) nb)))
      (replace buf scrap
               :start1 0 :end1 nel
               :start2 start2 :end2 end2)
      (cond ((= nb nel)
             (send cust buf)
             (become (holding-accum-beh deliv pkt-ix scrap (+ start2 nel) end2)))
            
            (t
             (let ((new-pkt-ix (1+ pkt-ix)))
               (send deliv self :req new-pkt-ix)
               (become (pend-packet-accum-beh deliv new-pkt-ix cust buf nel nb))))
            )))
   ))

(defun pend-packet-accum-beh (deliv pkt-ix cust buf start1 end1)
  (λ (byte-vec)
    (let* ((nel   (length byte-vec))
           (nwant (- end1 start1))
           (nb    (min nwant nel)))
      (replace buf byte-vec
               :start1 start1 :end1 end1
               :start2 0      :end2 nel)
      (cond ((>= nel nwant)
             (send cust buf)
             (become (holding-accum-beh deliv pkt-ix byte-vec nb nel)))

            (t
             (let ((new-pkt-ix  (1+ pkt-ix)))
               (send deliv self :req new-pkt-ix)
               (become (pend-packet-accum-beh deliv new-pkt-ix cust buf (+ start1 nb) end1))
               ))
            ))))

;; ----------------------------------------------
;; In-order packet delivery - ensure that packets are delivered in
;; arrival order

(defun base-ordered-delivery-beh (packets)
  (alambda
   ((cust :req ix)
    (let ((pair (assoc ix packets)))
      (cond (pair
             (let ((new-packets (remove pair packets)))
               (send cust (cdr pair))
               (become (base-ordered-delivery-beh new-packets))))
            (t
             (become (waiting-ordered-delivery-beh cust ix packets)))
            )))

   ((:deliver ix packet)
    (become (base-ordered-delivery-beh (acons ix packet packets))))
   ))

(defun waiting-ordered-delivery-beh (cust ix packets)
  (alambda
   ((:deliver an-ix packet)
    (cond ((eql ix an-ix)
           (send cust packet)
           (become (base-ordered-delivery-beh packets)))

          (t
           (become (waiting-ordered-delivery-beh cust ix (acons an-ix packet packets))))
          ))))

;; ------------------------------------------------------------
;; Socket Reader - an autonomous socket reader loop

(defun socket-reader-beh (reader decoder accum)
  (λ _
    (let ((buf (make-ubv 4)))
      (β  _
          (send accum β :req buf 4)
        (let ((len 0))
          (dotimes (ix 4)
            (setf len (dpb (aref buf ix) (byte 8 (ash ix 3)) len)))
          (let ((buf (make-ubv len)))
            (β _
                (send accum β :req buf len)
              (send decoder buf)
              (send reader))
            )))
      )))

(defun make-reader (decoder)
  (α _
    (actors ((accum  (holding-accum-beh self 0 nil 0 0))
             (reader (socket-reader-beh reader decoder accum)))
      (become (base-ordered-delivery-beh nil))
      (send reader)
      (repeat-send self))
    ))

;; -------------------------------------------------------------------------
;; Watchdog Timer - shuts down interface after prologned inactivity

(defun make-kill-timer (timer-fn)
  (let ((timer (mp:make-timer #'mp:funcall-async timer-fn)))
    (create
     (alambda
      ((:resched)
       (mp:schedule-timer-relative timer *socket-timeout-period*))
      ((:discard)
       (mp:unschedule-timer timer)
       (become (sink-beh)))
      ))))

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

(defun connections-list-beh (&optional cnx-lst)
  (alambda
   ((cust :add-socket ip-addr ip-port state sender)
    (send cust :ok)
    (let ((rec (find-connection-from-ip cnx-lst ip-addr ip-port)))
      (if rec
          (let* ((old-state (connection-rec-state rec))
                 (new-rec   (um:copy-with rec
                                          :state  state
                                          :sender sender))
                 (new-lst (cons new-rec (remove rec cnx-lst))))
            (unless (or (null old-state)
                        (eql old-state state))
              (send (intf-state-shutdown old-state)))
            (become (connections-list-beh new-lst)))
        ;; else
        (let ((new-rec (make-connection-rec
                        :ip-addr  ip-addr
                        :ip-port  ip-port
                        :state    state
                        :sender   sender)))
          (become (connections-list-beh (cons new-rec cnx-lst)))
          ))
      ))

   ((cust :find-socket ip-addr ip-port report-ip-addr handshake)
    (let ((rec (find-connection-from-ip cnx-lst ip-addr ip-port)))
      (if rec
          (let ((custs (connection-rec-custs rec)))
            (if custs
                (let* ((new-rec (um:copy-with rec
                                              :custs (cons cust custs)))
                       (new-lst (cons new-rec (remove rec cnx-lst))))
                  (become (connections-list-beh new-lst)))
              ;; else
              (send cust (connection-rec-chan rec))
              ))
        ;; else
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
        (become (connections-list-beh (remove rec cnx-lst)))
        (send (α ()
                (error "Can't connect to: ~S"
                       (connection-rec-report-ip-addr rec)))
              ))
      ))

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
               (new-rec  (um:copy-with rec
                                       :chan  chan
                                       :custs nil))
               (new-cnxs (cons new-rec (remove rec cnx-lst))))
          (become (connections-list-beh new-cnxs))
          (when custs
            (multiple-value-bind (peer-ip peer-port)
                #+:LISPWORKS8
              (comm:socket-connection-peer-address (intf-state-io-state state))
              #+:LISPWORKS7
              (comm:get-socket-peer-address (slot-value (intf-state-io-state state) 'comm::object))
              (send fmt-println "Client Socket (~S->~A:~D) starting up"
                    (connection-rec-report-ip-addr rec)
                    (comm:ip-address-string peer-ip) peer-port))
            (send-to-all custs chan))
          ))
      ))
            
   ((cust :remove state)
    (send cust :ok)
    (let ((rec (find-connection-from-state cnx-lst state)))
      (when rec
        (become (connections-list-beh (remove rec cnx-lst))))
      ))
   ))

(defactor connections
  (connections-list-beh))

;; -------------------------------------------------------------

(defun make-socket-shutdown (state)
  (actor ()
    (with-accessors ((kill-timer       intf-state-kill-timer)
                     (io-running       intf-state-io-running)
                     (io-state         intf-state-io-state)
                     (writer           intf-state-writer)
                     (title            intf-state-title)
                     (ip-addr          intf-state-ip-addr)) state
      (send fmt-println "~A Socket (~S) shutting down"
            title ip-addr)
      (send kill-timer :discard)
      (send writer :discard)
      (send connections sink :remove state)
      ;; ---------------------
      (wr (car io-running) 0)
      (comm:async-io-state-abort-and-close io-state)
      )))

;; ------------------------------------------------------------------------
;; For both clients and servers, we assemble the socket state, make
;; initial encoder/decoder which marshals and compresses and fragments
;; into 64kB packets. Set up the initial negotiation service in a per
;; socket local-services, start the socket reader going, and return
;; the state, enccoder, and local-services to the customer.

(defun create-socket-intf (&key kind ip-addr ip-port io-state report-ip-addr)
  (α (cust)
    (let* ((title (if (eq kind :client) "Client" "Server"))
           (local-services (make-local-services))
           (state (make-intf-state
                   :title            title
                   :ip-addr          report-ip-addr
                   :io-state         io-state
                   :local-services   local-services))
           (writer  (make-writer state))
           (encoder (sink-pipe (marshal-encoder)
                               (chunker :max-size (- +max-fragment-size+ 500))
                               (marshal-encoder)
                               writer))
           (decoder (sink-pipe (marshal-decoder)
                               (dechunker)
                               (marshal-decoder)
                               local-services))
           (accum    (make-reader decoder))
           (packet-ctr 0)
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
                           (decr-io-count-fn intf-state-decr-io-count-fn)) state
            
            (setf kill-timer (make-kill-timer
                              #'(lambda ()
                                  (send println "Inactivity shutdown request")
                                  (send shutdown)))
                  (intf-state-writer   state) writer
                  (intf-state-shutdown state) shutdown)
            
            (labels
                ((rd-callback-fn (state buffer end)
                   ;; callback for I/O thread - on continuous async read
                   #|
                   (send fmt-println "Socket Reader Callback (STATUS = ~A, END = ~A)"
                         (comm:async-io-state-read-status state)
                         end)
                   |#
                   (let (err-too-large)
                     (when (plusp end)
                       ;; (send fmt-println "~A Incoming bytes: ~A" title buffer)
                       (if (> end +max-fragment-size+)
                           (setf err-too-large "Incoming packet too large")
                         (progn
                           ;; (send dbg-println "-- recv from network ~D --" end)
                           (if nil
                               (send accum #|:deliver (incf packet-ctr)|# (subseq buffer 0 end))
                             ;; else
                             (send accum :deliver (incf packet-ctr) (subseq buffer 0 end)))
                           (send kill-timer :resched)))
                       (comm:async-io-state-discard state end))
                     (um:when-let (status (or (comm:async-io-state-read-status state)
                                              err-too-large))
                       ;; terminate on any error
                       (comm:async-io-state-finish state)
                       (send fmt-println "~A Incoming error state: ~A" title status)
                       (decr-io-count state))
                     ))
                 
                 (decr-io-count (io-state)
                   (let ((ct (sys:atomic-fixnum-decf (car io-running))))
                     (when (zerop ct) ;; >0 is running
                       (comm:close-async-io-state io-state)
                       (send println "Connection Shutdown")
                       (send shutdown))
                     ct)))
              
              (setf decr-io-count-fn #'decr-io-count)
              
              (send kill-timer :resched)
              (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                      :element-type '(unsigned-byte 8))
              (send cust state encoder)
              )))))))
  
;; -------------------------------------------------------------

(defun canon-ip-addr (ip-addr)
  (comm:get-host-entry ip-addr :fields '(:address)))

(defun make-socket-connection (ip-addr ip-port report-ip-addr)
  (actor () 
    (β (io-state)
        (mp:funcall-async
         (lambda ()
           (comm:create-async-io-state-and-connected-tcp-socket
            *ws-collection*
            ip-addr ip-port
            (lambda (state args)
              (cond (args
                     (send println
                           (format nil "CONNECTION-ERROR: ~S" report-ip-addr)
                           (apply #'format nil args))
                     (send connections :abort ip-addr ip-port))
                    (t
                     (send β state))
                    ))
            :connect-timeout 5
            #-:WINDOWS :ipv6    #-:WINDOWS nil)))
      (β (state socket)
          (send (create-socket-intf :kind           :client
                                    :ip-addr        ip-addr
                                    :ip-port        ip-port
                                    :report-ip-addr report-ip-addr
                                    :io-state       io-state)
                β)
        (send connections nil :negotiate state socket)
        ))))

(defactor client-connector
  ;; Called from client side wishing to connect to a server.
  (λ (cust handshake ip-addr &optional (ip-port *default-port*))
    (let ((clean-ip-addr (canon-ip-addr ip-addr)))
      (unless clean-ip-addr
        (error "Unknown host: ~S" ip-addr))
      (send connections cust :find-socket clean-ip-addr ip-port ip-addr handshake)
      )))

;; -------------------------------------------------------------

(defvar *server-count* 0)

(defun start-server-messenger (accepting-handle io-state)
  "Internal routine to start a network interface from the server side.
The interface is identical to that on the client side, but the
connection handshake differs from this side.

See the discussion under START-CLIENT-MESSENGER for details."

  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (declare (ignore accepting-handle))
  (let ((server-name (format nil "~A#~D" (machine-instance) (incf *server-count*))))
    (multiple-value-bind (peer-ip peer-port)
        #+:LISPWORKS8
      (comm:socket-connection-peer-address io-state)
      #+:LISPWORKS7
      (comm:get-socket-peer-address (slot-value io-state 'comm::object))
      (send fmt-println "Server Socket (~S->~A:~D) starting up" server-name
            (comm:ip-address-string peer-ip) peer-port)
      (send (create-socket-intf :kind             :server
                                :ip-addr          peer-ip
                                :ip-port          peer-port
                                :report-ip-addr   server-name
                                :io-state         io-state)
            nil)
      )))

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
        *aio-accepting-handle* nil)
  ;; (reset-singleton-actors)
  )

(defun* lw-start-tcp-server _
  ;; called by Action list with junk args
  ;;
  ;; We need to delay the construction of the system logger till this
  ;; time so that we get a proper background-error-stream.  Cannot be
  ;; performed on initial load of the LFM.
  (unless *ws-collection*
    (start-tcp-server)))

(defun* lw-reset-actor-system _
  (terminate-server sink)
  (reset-global-state)
  (print "Actor Server has been shut down."))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor Server"
                    'lw-start-tcp-server
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor Server"
                    'lw-reset-actor-system)

  (lw:define-action "Save Session After"
                    "Restart Actor Server"
                    'lw-start-tcp-server)
  )

(defun com.ral.actors:start ()
  (lw-start-tcp-server))

#| ;; for manual loading mode
(unless *ws-collection*
  (if (mp:get-current-process)
      (lw-start-tcp-server)
    (pushnew '("Start Actor Server" () lw-start-tcp-server) mp:*initial-processes*
             :key #'third)))
|#
