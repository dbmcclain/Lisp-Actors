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
  (import '(um:when-let
            um:wr
            com.ral.actors.secure-comm:make-local-services
            com.ral.actors.secure-comm:server-crypto-gateway
            com.ral.actors.secure-comm:+server-connect-id+
            com.ral.actors.secure-comm:server-skey
            ;; com.ral.actors.base::dbg-println
            com.ral.actors.base::make-ubv
            )))

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
  accepting-handle
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
                         (make-actor
                          (prefixing-write-beh writer))))
           (discarder   (discarder-beh prefixer phys-writer)))
    discarder))

;; -------------------------------------------------------------------------
;; Incoming Packet Aggregation - maintain a queue of incoming packet fragments
;; and peel off as requested by reader.

(defun empty-packet-accum-beh ()
  (alambda
   ((cust :req buf nb)
    (become (pend-packet-accum-beh cust buf nb 0 +emptyq+)))
   
   ((sender byte-vec)
    (send sender :ok)
    (let ((nel (length byte-vec)))
      (become (holding-accum-beh nel (addq +emptyq+ (list byte-vec 0 nel)) ))
      ))
   ))

(defun holding-accum-beh (tbytes queue)
  (alambda
   ((cust :req buf nb)
    (cond ((> tbytes nb)
           (let ((new-queue (fill-req buf nb queue)))
             (send cust buf)
             (become (holding-accum-beh (- tbytes nb) new-queue))
             ))

          ((= tbytes nb)
           (fill-req buf nb queue)
           (send cust buf)
           (become (empty-packet-accum-beh)))

          (t
           (become (pend-packet-accum-beh cust buf nb tbytes queue)))
          ))

   ((sender byte-vec)
    (send sender :ok)
    (let* ((nel       (length byte-vec))
           (new-queue (addq queue (list byte-vec 0 nel))))
      (become (holding-accum-beh (+ tbytes nel) new-queue))
      ))
   ))

(defun pend-packet-accum-beh (cust buf nbreq tbytes queue)
  (λ (sender byte-vec)
    (send sender :ok)
    (let* ((nel        (length byte-vec))
           (new-queue  (addq queue (list byte-vec 0 nel)))
           (new-tbytes (+ tbytes nel)))
      (cond ((> new-tbytes nbreq)
             (let ((new-queue (fill-req buf nbreq new-queue)))
               (send cust buf)
               (become (holding-accum-beh (- new-tbytes nbreq) new-queue))
               ))

            ((= new-tbytes nbreq)
             (fill-req buf nbreq new-queue)
             (send cust buf)
             (become (empty-packet-accum-beh)))

            (t
             (become (pend-packet-accum-beh cust buf nbreq new-tbytes new-queue)))
            ))))

(defun fill-req (buf nb queue)
  (um:nlet iter ((offs  0)
                 (queue queue))
    (if (>= offs nb)
        queue
      (multiple-value-bind (triple new-queue) (popq queue)
        (destructuring-bind (byte-vec bv-off bv-nel) triple
          (let ((nwant (- nb offs))
                (nhave (- bv-nel bv-off)))
            (cond ((> nhave nwant)
                   (replace buf byte-vec
                            :start1 offs :end1 nb
                            :start2 bv-off :end2 bv-nel)
                   (pushq new-queue (list byte-vec (+ bv-off nwant) bv-nel)))

                ((= nhave nwant)
                 (replace buf byte-vec
                          :start1 offs :end1 nb
                          :start2 bv-off :end2 bv-nel)
                 new-queue)

                (t
                 (replace buf byte-vec
                          :start1 offs :end1 nb
                          :start2 bv-off :end2 bv-nel)
                 (go-iter (+ offs nhave) new-queue))
                ))
          ))
      )))

(defun make-accum ()
  (make-actor (empty-packet-accum-beh)))

;; ----------------------------------------------
;; In-order packet delivery - ensure that packets are delivered in
;; arrival order

(defun empty-ordered-delivery-beh (deliv acceptor)
  (prunable-alambda
   ((:req ix)
    (let ((next (make-actor self-beh)))
      (become (waiting-ordered-delivery-beh deliv acceptor ix next))))
   
   ((:deliver ix packet)
    (let ((next (make-actor self-beh)))
      (become (packet-delivery-beh deliv acceptor ix packet next))))
   ))

(defun waiting-ordered-delivery-beh (deliv acceptor ix next)
  (prunable-alambda
   ((:deliver an-ix packet) / (eql an-ix ix)
    (prune-self next)
    (β _
        (send acceptor β packet)
      (send deliv :req (1+ ix))))

   (_
    (repeat-send next))
   ))

(defun packet-delivery-beh (deliv acceptor ix packet next)
  (prunable-alambda
   ((:req an-ix) / (eql an-ix ix)
    (prune-self next)
    (β _
        (send acceptor β packet)
      (send deliv :req (1+ ix))))

   (_
    (repeat-send next))
   ))

(defun make-in-order-delivery (client)
  (actors ((deliv (empty-ordered-delivery-beh deliv client)))
    (send deliv :req 1)
    deliv))

;; ------------------------------------------------------------
;; Socket Reader - an autonomous socket reader loop

(defun socket-reader (decoder accum)
  (α _
    (let ((again self)
          (buf   (make-ubv 4)))
      (β  _
          (send accum β :req buf 4)
        (let ((len 0))
          (dotimes (ix 4)
            (setf len (dpb (aref buf ix) (byte 8 (ash ix 3)) len)))
          (let ((buf (make-ubv len)))
            (β _
                (send accum β :req buf len)
              (send decoder buf)
              (send again))
            )))
      )))

(defun make-reader (decoder)
  (let* ((accum (make-accum))
         (deliv (make-in-order-delivery accum)))
    (send (socket-reader decoder accum))
    deliv))

;; -------------------------------------------------------------------------
;; Watchdog Timer - shuts down interface after prologned inactivity

(defun make-kill-timer (timer-fn)
  (let ((timer (mp:make-timer #'mp:funcall-async timer-fn)))
    (make-actor
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

(defun empty-connections-list-beh ()
  (prunable-alambda

   ((cust :add-socket ip-addr ip-port state sender)
    (let ((next (make-actor self-beh)))
      (become (connection-node-beh ip-addr ip-port state sender sender next))
      (send cust :ok)))

   ((:on-find-sender _ _ _ if-not-found)
    (send if-not-found))

   ((cust :remove . _)
    (send cust :ok))
   ))

(defun connection-node-beh (ip-addr ip-port state sender chan next)
  (prunable-alambda

   ((cust :add-socket an-ip-addr an-ip-port new-state new-sender) when (and (eql an-ip-addr ip-addr)
                                                                            (eql an-ip-port ip-port))
    ;; replacing the sender and state - kills the old chan and has to be renegotiated
    (become (connection-node-beh ip-addr ip-port new-state new-sender new-sender next))
    (send cust :ok))

   ((:on-find-sender an-ip-addr an-ip-port if-found . _) when (and (eql an-ip-addr ip-addr)
                                                                   (eql an-ip-port ip-port))
    (send if-found sender chan (intf-state-local-services state)))

   ((cust :set-channel a-sender new-chan) when (eq a-sender sender)
    (become (connection-node-beh ip-addr ip-port state sender new-chan next))
    (send cust :ok))

   ((cust :remove a-state) when (eq a-state state)
    (prune-self next)
    (send cust :ok))
   
   (_
    (repeat-send next))
   ))

(deflex connections
  (make-actor (empty-connections-list-beh)))

;; -------------------------------------------------------------

(defun make-socket-shutdown (state)
  (actor ()
    (with-accessors ((kill-timer       intf-state-kill-timer)
                     (io-running       intf-state-io-running)
                     (io-state         intf-state-io-state)
                     (accepting-handle intf-state-accepting-handle)
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
      (when accepting-handle
        (um:deletef (comm:accepting-handle-user-info accepting-handle) state))
      )))

;; ------------------------------------------------------------------------
;; For both clients and servers, we assemble the socket state, make
;; initial encoder/decoder which marshals and compresses and fragments
;; into 64kB packets. Set up the initial negotiation service in a per
;; socket local-services, start the socket reader going, and return
;; the state, enccoder, and local-services to the customer.

(defun create-socket-intf (&key kind ip-addr ip-port io-state accepting-handle report-ip-addr)
  (actor (cust)
    (let* ((title (if (eq kind :client) "Client" "Server"))
           (local-services (make-local-services))
           (state (make-intf-state
                   :title            title
                   :ip-addr          report-ip-addr
                   :io-state         io-state
                   :accepting-handle accepting-handle
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
      (beta _
          ;; provide a service to establish an encrypted channel
          (send local-services beta :add-service-with-id +server-connect-id+
                (server-crypto-gateway (server-skey) encoder local-services))

        (beta _
            (send connections beta :add-socket ip-addr ip-port state encoder)
          
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
                           (send accum :deliver (incf packet-ctr) (subseq buffer 0 end))
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
              (send cust state encoder local-services)
              )))))))
  
;; -------------------------------------------------------------

(defun canon-ip-addr (ip-addr)
  (comm:get-host-entry ip-addr :fields '(:address)))

(defun empty-pending-connections-beh (pends)
  (prunable-alambda

   ((cx-cust :connect ip-addr ip-port report-ip-addr)
    (let ((next (make-actor self-beh)))
      (become (pending-connections-beh ip-addr ip-port report-ip-addr (list cx-cust) next))
      (send (make-socket-connection ip-addr ip-port report-ip-addr) pends)
      ))
   ))

(defun pending-connections-beh (ip-addr ip-port report-ip-addr custs next)
  (prunable-alambda

   ((cx-cust :connect an-ip-addr an-ip-port . _) when (and (eql an-ip-addr ip-addr)
                                                           (eql an-ip-port ip-port))
    (become (pending-connections-beh ip-addr ip-port report-ip-addr (cons cx-cust custs) next)))
   
   ((:ready an-ip-addr an-ip-port intf-state sender local-services) when (and (eql an-ip-addr ip-addr)
                                                                              (eql an-ip-port ip-port))
    (prune-self next)
    
    (multiple-value-bind (peer-ip peer-port)
        #+:LISPWORKS8
      (comm:socket-connection-peer-address (intf-state-io-state intf-state))
      #+:LISPWORKS7
      (comm:get-socket-peer-address (slot-value (intf-state-io-state intf-state) 'comm::object))
      (send fmt-println "Client Socket (~S->~A:~D) starting up" report-ip-addr
            (comm:ip-address-string peer-ip) peer-port)
      (send-to-all custs sender sender local-services)
      ))

   ((:abort an-ip-addr an-ip-port) when (and (eql an-ip-addr ip-addr)
                                             (eql an-ip-port ip-port))
    (prune-self next)
    (send (α ()
            (error "Can't connect to: ~S" report-ip-addr))))

   (_
    (repeat-send next))
   ))

(defun make-socket-connection (ip-addr ip-port report-ip-addr)
  (actor (pends) 
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
                     (send pends :abort ip-addr ip-port))
                    (t
                     (send β state))
                    ))
            :connect-timeout 5
            #-:WINDOWS :ipv6    #-:WINDOWS nil)))
      (β (intf-state sender local-services)
          (send (create-socket-intf :kind           :client
                                    :ip-addr        ip-addr
                                    :ip-port        ip-port
                                    :report-ip-addr report-ip-addr
                                    :io-state       io-state)
                β)
        (send pends :ready ip-addr ip-port intf-state sender local-services)
        ))))

(defun client-connector-beh (pending-connections)
  (alambda
   ((cust ip-addr)
    (send self cust ip-addr *default-port*))

   ((cust ip-addr ip-port)
    (let ((clean-ip-addr (canon-ip-addr ip-addr)))
      (cond ((null clean-ip-addr)
             (error "Unknown host: ~S" ip-addr))
            (t
             (β _
                 (send connections :on-find-sender clean-ip-addr ip-port cust β)
               (send pending-connections cust :connect clean-ip-addr ip-port ip-addr)))
            )))
   ))

(defun init-connector-beh ()
  (lambda* _
    (actors ((pends (empty-pending-connections-beh pends)))
      (become (client-connector-beh pends))
      (repeat-send self))))
    
(deflex client-connector
  ;; Called from client side wishing to connect to a server.
  (make-actor (init-connector-beh)))

;; -------------------------------------------------------------

(defvar *server-count* 0)

(defun start-server-messenger (accepting-handle io-state)
  "Internal routine to start a network interface from the server side.
The interface is identical to that on the client side, but the
connection handshake differs from this side.

See the discussion under START-CLIENT-MESSENGER for details."

  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (let ((server-name (format nil "~A#~D" (machine-instance) (incf *server-count*))))
    (beta (state sender local-services)
        (send (create-socket-intf :kind             :server
                                  :report-ip-addr   server-name
                                  :io-state         io-state
                                  :accepting-handle accepting-handle)
              beta)
      ;; for server side, this user-info is the only reference to intf
      ;; until we get registered into the ip-mapping table.
      (declare (ignore sender local-services))
      (push state (comm:accepting-handle-user-info accepting-handle))
      (multiple-value-bind (peer-ip peer-port)
          #+:LISPWORKS8
          (comm:socket-connection-peer-address io-state)
          #+:LISPWORKS7
          (comm:get-socket-peer-address (slot-value io-state 'comm::object))
        (send fmt-println "Server Socket (~S->~A:~D) starting up" server-name
              (comm:ip-address-string peer-ip) peer-port)
        ))))

;; --------------------------------------------------------------

(defun terminate-server (reply-to)
  (if *aio-accepting-handle*
      (progn
        (setf (comm:accepting-handle-user-info *aio-accepting-handle*) nil)
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
