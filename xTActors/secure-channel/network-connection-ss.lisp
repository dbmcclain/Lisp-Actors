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

(defparameter *default-port*            65001.)
(defparameter *socket-timeout-period*   20.)

(defconstant +MAX-FRAGMENT-SIZE+ 65536.)

;; -----------------------------------------------
;; The main async manager

(defun async-socket-system-beh (&optional ws-collection aio-accepting-handle)
  (αλ
   ;; --------------------------------------
   ((cust :terminate-server)
    (flet ((ws-handler (coll)
             ;; we are operating in the collection process
             (comm:close-wait-state-collection coll)
             (>> cust :ok)
             (mpc:process-terminate (mpc:get-current-process))
             ))
      (cond
       (aio-accepting-handle
        (β! (async-socket-system-beh))
        (on-commit
          (comm:close-accepting-handle aio-accepting-handle #'ws-handler) ))
       
       (ws-collection
        (β! (async-socket-system-beh))
        (on-commit
          (comm:apply-in-wait-state-collection-process ws-collection #'ws-handler)))
       
       (t
        (>> cust :ok))
       )) )
       
   ;; --------------------------------------
   ((cust :start-tcp-server . options)
    (cond ((or ws-collection aio-accepting-handle)
           (let+ ((me   self)
                  (msg  self-msg)
                  (:β _ (racurry self :terminate-server)))
             (>>* me msg)))

          (t
           (let+ ((tcp-port-number (or (car options)
                                       *default-port*))
                  (me              self)
                  (ws-coll         (comm:create-and-run-wait-state-collection
                                    "Actor Server"
                                    :handler (lambda* _
                                               (>> me sink :terminate-server))))
                  (handle          (comm:accept-tcp-connections-creating-async-io-states
                                    ws-coll
                                    tcp-port-number
                                    #'start-server-listener
                                    :ipv6    nil
                                    ) ))
             (β! (async-socket-system-beh ws-coll handle))
             (>> fmt-println "-- Actor Server started on port ~A --" tcp-port-number)
             (>> cust :ok)) )
          ))
   
   ;; --------------------------------------
   ((cust :connect ip-addr ip-port)
    ;; Message sent from clients wanting to connect to a server.
    ;; Cust is the CONNECTIONS manager.
    (cond (ws-collection
           (flet ((callback (io-state args)
                    ;; Performed in the process of collection, so keep it short.
                    (>>* cust io-state args)))
             (<<* #'comm:create-async-io-state-and-connected-tcp-socket
                  ws-collection
                  ip-addr ip-port #'callback
                  #-:WINDOWS
                  `(:connect-timeout 5 :ipv6 nil)
                  #+:WINDOWS
                  `(:connect-timeout 5)
                  )))

          (t
           (let* ((me       self)
                  (ws-coll  (comm:create-and-run-wait-state-collection
                            "Actor Clients"
                            :handler (lambda* _
                                       (>> me sink :terminate-server))
                            )))
             (β! (async-socket-system-beh ws-coll))
             (repeat-send self)))
          ))
   ))

(deflex* async-socket-system
  (create (async-socket-system-beh)))

;; -------------------------------------------------------------
;; Socket connection state

(defstruct intf-state
  title
  ip-addr
  io-state
  local-services
  kill-timer
  io-running
  shutdown)

;; --------------------------------------------------
;; Gentle and Forcible Socket Closing

(deflex socket-closer
  ;; Use only when it is known that no async streams are operating
  (α (cust io-state)
    (comm:close-async-io-state io-state)
    (>> cust :ok)))

(deflex forcible-socket-closer
  ;; Used when an async reader or writer may currently be running
  (α (cust io-state)
    (comm:async-io-state-abort-and-close io-state)
    (>> cust :ok)))

;; -------------------------------------------------------------------------
;; IO-Running controller - Coordinates the activity and closure of
;; socket streams.
;;
;; By placing all non-idempotent actions in leaf Actors, we avoid
;; problems caused by parallel restarts when BECOME fails, since we
;; are transactional. These Actors will only be sent messages when
;; there was no restart. This avoids the need to place such Actors
;; behind a Serializer.
;;
;; By always having Actors send a result to a customer, they become
;; sequenceable.
;; -----------------------------------------------

(defun io-base-beh (io-state)
  (αλ
   ((cust :terminate)
    (β! (io-closed-beh))
    (>> forcible-socket-closer cust io-state))

   ((cust :try-writing _ if-nok)
    (>> println "Can't happen - attempt to write while busy writing.")
    (>> if-nok)
    (>> cust :err))

   ((cust :is-open)
    (>> cust t))
   ))

(defun io-closed-beh ()
  ;; Fully closed state
  (αλ
   ((cust :try-writing _ if-nok)
    (>> if-nok)
    (>> cust :err))

   ((cust :is-open)
    (>> cust nil))
   ))

(defun io-running-beh (io-state base)
  ;; In this state we are open to async reads, no writing under way,
  (αλ
   ((cust :try-writing if-ok _)
    (β! (io-running-write-beh io-state base))
    (>> if-ok)
    (>> cust :ok))

   (_
    (repeat-send base))
   ))

(defun io-running-write-beh (io-state base)
  ;; Async reads are active, writing is under way.
  (αλ
   ((cust :finish-wr-ok)
    (β! (io-running-beh io-state base))
    (>> cust :ok))

   ((cust :finish-wr-fail)
    (β! (io-closed-beh))
    (>> forcible-socket-closer cust io-state))

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
;;  byte-vec  +-------+   +------------+    +-------------+
;;     ------>| LABEL |-->| SERIALIZER |<-->| Phys Writer |
;;            +-------+   +------------+    +-------------+
;;
;; PhysWriter is the connection to the async output socket port.  The
;; Serializer is to prevent parallel access to the physical socket
;; output port.  Serializers need a customer, even if SINK. LABEL
;; provides that SINK.
;;
;; If anything goes wrong, the Phys Writer sends a shutdown signal.
;;

(defun physical-writer-beh (state)
  (lambda (cust byte-vec)
    (let+ ((:slots (io-state
                    io-running
                    kill-timer
                    shutdown) state))
      (labels
          ;; these functions execute in the thread of the async socket handler
          ((not-writing ()
             (>> cust :err)) ;; clear the Serializer
           
           (write-done (io-state &rest _)
             ;; this is a callback routine, executed in the thread of
             ;; the async collection
             (declare (ignore _))
             (cond ((comm:async-io-state-write-status io-state)
                    (let+ ((:β _  (racurry io-running :finish-wr-fail)))
                      (>> shutdown)
                      (>> cust :err)))
                   (t
                    ;; io-running will reply to our cust for us
                    (>> io-running cust :finish-wr-ok))
                   ))
           
           (begin-write ()
             (comm:async-io-state-write-buffer io-state
                                               byte-vec
                                               #'write-done)
             (>> kill-timer :resched)) )
        
        (send io-running sink :try-writing
              (create #'begin-write)
              (create #'not-writing))
        ))))

(defun make-writer (state)
  (serializer-sink (create (physical-writer-beh state))
                   :timeout nil)) ;; muffle warning, 'cause phys-writer always responds (eh?!)

;; -------------------------------------------------------------------------
;; Watchdog Timer - shuts down interface after prologned inactivity

(defun watchdog-timer-beh (killer tag)
  (αλ
   ((:resched)
    (let ((new-tag (tag self)))
      (β! (watchdog-timer-beh killer new-tag))
      (send-after *socket-timeout-period* new-tag :timed-out)))
   
   ((atag :timed-out) / (eql atag tag)
    (become-sink)
    (>> killer))

   ((:discard)
    (become-sink))
   ))

(defun make-kill-timer (killer)
  (um:letrec ((tag    (tag timer))
              (timer  (create
                       (watchdog-timer-beh killer tag))))
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
;;      (SEND chan srv-id cust-id . message)
;;                |------------------------|
;;                         = message sent to server local services list.
;;
;; Message structure for server replies:
;;      (SEND chan cust-id . message)
;;                |-----------------|
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

(defun same-ip-test (ip-addr ip-port)
  (lambda (rec)
    (let+ ((:slots ((rec-ip-addr ip-addr)
                    (rec-ip-port ip-port)) rec))
      (and (eql ip-addr rec-ip-addr)
           (eql ip-port rec-ip-port))
      )))
  
(defun find-connection-from-ip (cnx-lst ip-addr ip-port)
  (find-if (same-ip-test ip-addr ip-port) cnx-lst))

(defun find-connection-from-state (cnx-lst state)
  (find state cnx-lst :key #'connection-rec-state))

(defun find-connection-from-sender (cnx-lst sender)
  (find sender cnx-lst :key #'connection-rec-sender))

;; ---------------------
;; A global counter to label instances of server connections from this
;; host

(defun counter-beh (n)
  (lambda (cust)
    (let ((new-n (1+ n)))
      (β! (counter-beh new-n))
      (>> cust n))))

(deflex* get-server-count
  (create (counter-beh 0)))

;; ------------------------
;; The list of currently active socket connections

(defun connections-list-beh (&optional cnx-lst)
  (αλ
   
   ((cust :show)
    (>> cust cnx-lst))

   #| --------------------------------------------
    A Server sends :ADD-SERVER when a new client connection is
    established. The peer-ip addr is that of the client.
   
    If a record already exists for this peer-ip, shut down our new
    attempt here. No point having a duplicate path.
   
    Else, construct a new record for our list of connections,
    marked as a :PENDING-SERVER connection. The STATE and SENDER
    Actor will be filled in during the construction of a socket
    interface, which we initiate here.
   |#
   ((cust :add-server peer-ip peer-port io-state)
    (>> cust :ok)
    (let ((rec (find-connection-from-ip cnx-lst peer-ip peer-port)))
      (cond

       (rec
        ;; already exists or is pending - so shut down our attempt
        (>> forcible-socket-closer sink io-state))
       
       (peer-ip
        ;; reserve our place while we create a channel
        (let ((rec (make-connection-rec
                    :ip-addr         peer-ip
                    :ip-port         peer-port
                    :state           :pending-server)))
          (β! (connections-list-beh (cons rec cnx-lst)))
          (let+ ((:β ct  get-server-count)
                 (server-name (format nil "~A#~D" (machine-instance) ct)))
            (>> fmt-println "Server Socket (~S->~A:~D) starting up"
                  server-name
                  (comm:ip-address-string peer-ip)
                  peer-port)
            (>> (create-socket-intf :kind             :server
                                      :ip-addr          peer-ip
                                      :ip-port          peer-port
                                      :report-ip-addr   server-name
                                      :io-state         io-state)
                  sink))
          ))
       )))
        
   #| --------------------------------------------
    Message :ADD-SOCKET is sent during construction of the socket
    interface graph.
   
    Add an initial socket connection to our list of available
    connections. At this point the connection is not-encrypted.
   
    Look for an existing record for this same ip-addr:
   
      1. If we find one and it is a :PENDING-SERVER record, then
      record the STATE and SENDER (socket writer Actor) in the
      record.  This makes it no longer pending, and available for
      unsecured comms.
   
      2. Otherwise, if we found a record, and the STATE is not us,
      then shut the previous connection down. Record the new SENDER
      and STATE in the record.
   
      3. Else, no record was found, so construct a new one and add
      it to our list. That happens when a client wants a connection
      to a new server.
   |#
   ((cust :add-socket ip-addr ip-port new-state sender)
    (>> cust :ok)
    (let ((rec (find-connection-from-ip cnx-lst ip-addr ip-port)))
      (cond (rec
             (let+ ((:slots ((old-state state)) rec))
               (cond ((eql :pending-server old-state)
                      (let ((new-rec (make-connection-rec
                                      :ip-addr  ip-addr
                                      :ip-port  ip-port
                                      :state    new-state
                                      :sender   sender)))
                        (β! (connections-list-beh (cons new-rec (remove rec cnx-lst))))
                        ))
                     
                     (t
                      (let+ ((:slots (shutdown) old-state)
                             (new-rec   (copy-with rec
                                                   :state  new-state
                                                   :sender sender))
                             (new-lst (cons new-rec (remove rec cnx-lst))))
                        (unless (or (null old-state)
                                    (eql old-state new-state))
                          (>> shutdown))
                        (β! (connections-list-beh new-lst))
                        ))
                     )))
           
            (t
             (let ((new-rec (make-connection-rec
                             :ip-addr  ip-addr
                             :ip-port  ip-port
                             :state    new-state
                             :sender   sender)))
               (β! (connections-list-beh (cons new-rec cnx-lst)))
               ))
            )))
  
   #| --------------------------------------------
    Message :FIND-SOCKET is sent by client Actors on this machine,
    looking for a socket connection to a remote host.
   
    Look for an available connection already in place.
   
    If we find one, and it has no waiting list, then send its
    writer Actor to the customer as an available connection to the
    server.
   
    If we find one, but it has a waiting list, then join the wait -
    they are waiting on a successful handshake with the server. All
    will be notified if it completes.
   
    If no records are found for the server ip address, then
    construct one with us as the first waiting member, add the new
    record to the list, and then try to form a network TCP
    connection with the server.
   |#
   ((cust :find-socket ip-addr ip-port report-ip-addr handshake)
    (let ((rec  (find-connection-from-ip cnx-lst ip-addr ip-port)))
      (cond
       (rec
        (let+ ((:slots (custs
                        chan) rec))
          (cond (custs
                 ;; waiting custs so join the crowd
                 (let* ((new-rec (copy-with rec
                                            :custs (cons cust custs)))
                        (new-lst (cons new-rec (remove rec cnx-lst))))
                   (β! (connections-list-beh new-lst))))

                (t
                 ;; no waiting list - just go
                 (>> cust chan))
                )))

       (t
        ;; else - no record yet, so create and wait on the channel
        ;; No response will be issued from us here to the customer.
        ;; If a connection succeeds, and the handshake completes,
        ;; then the customer will be notified at that time.
        (let* ((new-rec (make-connection-rec
                         :ip-addr        ip-addr
                         :ip-port        ip-port
                         :report-ip-addr report-ip-addr
                         :handshake      handshake
                         :custs          (list cust)))
               (new-lst (cons new-rec cnx-lst))
               (me      self))
          (β! (connections-list-beh new-lst))
          (let+ ((:β (io-state . args)  (racurry async-socket-system
                                                 :connect ip-addr ip-port) ))
            (cond
             (args
              (>> fmt-println "CONNECTION-ERROR: ~S~%~S"
                  report-ip-addr
                  (<<* #'format nil args))
              (>> me sink :abort ip-addr ip-port))

             ((typep io-state 'comm:async-io-state)
              (let+ ((:β (state socket)  (create-socket-intf :kind           :client
                                                             :ip-addr        ip-addr
                                                             :ip-port        ip-port
                                                             :report-ip-addr report-ip-addr
                                                             :io-state       io-state)))
                (>> me sink :negotiate state socket)
                ))

             ;; else - some error happened, but has probably already been reported to user
             ))
          ))
       )))
       
   #| --------------------------------------------
    Message :ABORT might be sent during an initial attempt to form a
    TCP connection with a server. If that connection fails, we get
    this message.
   
    Look for a records mentioning the ip adddr and purge it from our
    list of available connections. Future requests will begin anew.
    Any waiting clients will just be left hanging.
   |#
   ((cust :abort ip-addr ip-port)
    (>> cust :ok)
    (let* ((same-ip (same-ip-test ip-addr ip-port))
           (rec     (find-if same-ip cnx-lst)))
      (when rec
        ;; leaves all waiting custs hanging...
        (let+ ((:slots (report-ip-addr) rec)
               (new-lst (remove-if same-ip cnx-lst)))
          (β! (connections-list-beh new-lst))
          (on-commit
            ;; ...would also prevent us from updating the list...
            (error "Can't connect to: ~S" report-ip-addr))
          ))))

   #| --------------------------------------------
    Mesasge :NEGOTIATE is sent after successfully forming a TCP
    connection with a server.
   
    The server is now waiting for us to start the DH handshake
    crypto-dance.
   
    Client Actors must await a secure channel. The only Actors that
    are permitted to communicate over an insecure channeol are
    those involved in the DH crypto-dance of the initial handshake
    with the server. And even that is encrypted in a different way.
   
    When an initial TCP connection is esablished with a server on
    the network, that server just sits there silently waiting for
    the handshake dance to begin from the client side. The server
    only responds if it recognizes the client and its encrypted
    handshae message.
   
    If we didn't find any record of this TCP connection in our
    list, then just play dumb and say :OK.
   
    Here STATE is the state devloped by the socket interface, and
    SOCKET is the unencrypted output Actor.
   
    All the SOCKET Actor does is to serialize whatever is being
    sent, then encode it into a self-sync byte streaam, before
    sending it out the physical async socket port. It doesn't even
    chop it into manageable blocks of data. So there will be some
    maximum size that can be transferred successfully across the TCP
    port.
   
    The server side has the same situation at this time. So whatever
    is transferred in either direction is done so in the clear,
    except for these simple encodings. Anyone listening with
    decoders for these serial protocols will be able to read what is
    sent.
    ------------------------------------------------------------
    ;; The initial DH handshake sends encrypted random numbers across
    ;; the cleartext link, along with small AES encrypted packets
    ;; containing connection info for each party.
    ;;
    ;; For both Lattice Crypto and ECC we are immune to attack from
    ;; Quantum Computing during the initial DH handshake dance. In the
    ;; case of Lattice Crypto, the algorithms employed give no
    ;; advantage to QC.
    ;;
    ;; But even in the case of ECC vulnerabilities against Schorr's
    ;; Algorithm in QC, busting open our crypto keying provides no
    ;; advantage because the messages passed are merely random numbers
    ;; and have no overt relation to the AES keying in the data
    ;; packets. No public keys are sent except under AES encryption.
    ;;
    ;; With SHA3 hashes and AES encryption, our data packets are
    ;; immune to QC attack.
   |#
   ((cust :negotiate state socket)
    (let ((rec (find-connection-from-sender cnx-lst socket)))
      (if rec
          (let+ ((:slots (handshake)      rec)
                 (:slots (local-services) state))
            ;; Let the dance begin...
            (>> handshake cust socket local-services))
        ;; else
        (>> cust :ok))
      ))
  
   #| --------------------------------------------
    Message :SET-CHANNEL is sent during handshake after a secure
    channel writer (Actor) has been constructed for this socket
    connection.
   
    The writeable endpoint Actor for that encrypted channel is
    CHAN.
   
    Look up the record containing the non-secure writer, SENDER,
    and add the secure endpoint, CHAN, to the record.
   
    Then notify all waiting clients about it. Clear the waiting
    list and update our records.
   |#
   ((cust :set-channel sender chan)
    (>> cust :ok)
    (let ((rec (find-connection-from-sender cnx-lst sender)))
      (when rec
        (let+ ((:slots (custs
                        state
                        report-ip-addr) rec)
               (:slots (io-state) state)
               (new-rec  (copy-with rec
                                    :chan  chan
                                    :custs nil))
               (new-cnxs (cons new-rec (remove rec cnx-lst))))
          (β! (connections-list-beh new-cnxs))
          (when custs
            (let+ ((:mvb (peer-ip peer-port)
                    #+:LISPWORKS8
                    (comm:socket-connection-peer-address io-state)
                    #+:LISPWORKS7
                    (comm:get-socket-peer-address (slot-value io-state 'comm::object)) ))
              (>> fmt-println "Client Socket (~S->~A:~D) starting up"
                    report-ip-addr
                    (comm:ip-address-string peer-ip)
                    peer-port))
            (send-to-all custs chan)))
        )))
    
   #| --------------------------------------------
    Message :REMOVE is sent when a socket connection is closed down.
    Just remove any records referencing STATE.
   |#
   ((cust :remove state)
    (>> cust :ok)
    (let ((rec (find-connection-from-state cnx-lst state)))
      (when rec
        (let ((new-lst (remove state cnx-lst ;; should only be one...
                               :key #'connection-rec-state)))
          (β! (connections-list-beh new-lst))
          ))))
   ))

(deflex connections
  (create (connections-list-beh)))

#|
(>> connections (create #'inspect) :show)
|#

;; -------------------------------------------------------------
;; Socket Shutdown needs the state, but itself is part of that state.
;; So we implement a 2-stage construction here.

(defun socket-shutdown-beh (state)
  (let+ ((:slots (kill-timer
                  io-running
                  title
                  ip-addr) state))
    (lambda ()
      (become-sink)
      (let+ ((:β _  (racurry io-running :terminate)))
        (>> fmt-println "~A Socket (~S) shutting down"
            title ip-addr)
        (>> kill-timer :discard)
        (>> connections sink :remove state)
        ))))

(defun initial-socket-shutdown-beh ()
  (αλ
   ((cust :init state)
    (β! (socket-shutdown-beh state))
    (>> cust :ok))
   ))

(defun make-socket-shutdown ()
  (create (initial-socket-shutdown-beh)))

;; ------------------------------------------------------------------------
;; For both clients and servers, we assemble the socket state, make
;; initial encoder/decoder which marshals and compresses and fragments
;; into 64kB packets. Set up the initial negotiation service in a per
;; socket local-services, start the socket reader going, and send the
;; state and encoder to the customer.

(defun create-socket-intf (&key kind ip-addr ip-port io-state report-ip-addr)
  (create
   (lambda (cust)
     (let+ ((title          (if (eq kind :client) "Client" "Server"))
            (local-services (make-local-services))
            (io-running     (make-io-running-monitor io-state))
            (shutdown       (make-socket-shutdown))
            (kill-timer     (make-kill-timer
                             (create
                              (lambda ()
                                (>> println "Inactivity shutdown request")
                                (>> shutdown))
                              )))
            (state   (make-intf-state
                      :title            title
                      :ip-addr          report-ip-addr
                      :io-state         io-state
                      :local-services   local-services
                      :io-running       io-running
                      :shutdown         shutdown
                      :kill-timer       kill-timer
                      ))
            (encoder (sink-pipe  (marshal-encoder) ;; async output is sent here
                                 (self-sync-encoder)
                                 (make-writer state)))
            (accum   (self-synca:stream-decoder    ;; async arrivals are sent here
                        (sink-pipe (fail-silent-marshal-decoder)
                                   local-services)))
            (:β _    (racurry shutdown :init state))
            (:β _    (if (eq kind :server)
                         (racurry local-services :add-single-use-service
                                  +server-connect-id+
                                  (server-crypto-gateway encoder local-services))
                       ;; else
                       true))
            (:β _    (racurry connections
                              :add-socket ip-addr ip-port state encoder) )
            (fragment-ctr  0)
            (rd-callback-fn (lambda (state buffer end)
                              ;; callback for I/O thread - on continuous async read
                              #|
                              (>> fmt-println "Socket Reader Callback (STATUS = ~A, END = ~A)"
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
                                       (>> fmt-println "~A Incoming error state: ~A" title status)
                                       (>> shutdown))
                                      
                                      ((plusp end)
                                       #|
                                       ;; TCP ensures that messages arrive on the
                                       ;; wire and are delivered here in whole, or
                                       ;; not at all. But the async input system
                                       ;; delivers them as they arrive in piecemeal
                                       ;; fashion.
                                       ;;
                                       ;; Every input fragment is numbered here,
                                       ;; starting from 1. But the Actor system
                                       ;; might jumble the order of fragment-ctr
                                       ;; delivery.
                                       ;; 
                                       ;; (I have witnessed this happening.)
                                       ;; 
                                       ;; Numbering them enables us to deal
                                       ;; properly with possible out-of-order
                                       ;; delivery to the self-sync decoder.
                                       |#
                                       (>> accum :deliver (incf fragment-ctr) (subseq buffer 0 end))
                                       (>> kill-timer :resched)
                                       (comm:async-io-state-discard state end))
                                      )))) )
       
       ;; Start things running...
       (comm:async-io-state-read-with-checking io-state rd-callback-fn
                                               :element-type '(unsigned-byte 8))
       (>> kill-timer :resched)
       
       ;; And now we can tell our customer that our graph is complete and running
       (>> cust state encoder)
       ))
   ))

;; -------------------------------------------------------------

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

(deflex client-connector
  ;; Called from client side wishing to connect to a server.
  (create
   (lambda (cust handshake ip-addr &optional (ip-port *default-port*))
     (let+ ((:mvb (addr port)  (parse-ip-addr ip-addr))
            (clean-ip-addr (canon-ip-addr addr))
            (port          (or port ip-port *default-port*)))
       (unless clean-ip-addr
         (error "Unknown host: ~S" ip-addr))
       (>> connections cust :find-socket clean-ip-addr port ip-addr handshake)
       ))))

;; -------------------------------------------------------------

(defun start-server-listener (accepting-handle io-state)
  "Called, from the server, when a client connects to the server port."
  ;; this is a callback function from the socket event loop manager
  ;; so we can't dilly dally...
  (declare (ignore accepting-handle))
  (let+ ((:mvb (peer-ip peer-port)
          #+:LISPWORKS8 (comm:socket-connection-peer-address io-state)
          #+:LISPWORKS7 (comm:get-socket-peer-address (slot-value io-state 'comm::object))
          ))
    (>> connections sink :add-server peer-ip peer-port io-state)
    ))

;; --------------------------------------------------------------
;;

(defun* lw-start-actors-server _
  ;; called by Action list with junk args
  ;;
  ;; We need to delay the construction of the system logger till this
  ;; time so that we get a proper background-error-stream.  Cannot be
  ;; performed on initial load of the LFM.
  ;;
  (ask async-socket-system :start-tcp-server)
  ;; flush pending messages...
  (ask true))

(defun* lw-reset-actors-server _
  (ask async-socket-system :terminate-server)
  (princ "Actor Server has been shut down."))


(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor Server"
                    'lw-start-actors-server
                    :after "Start up Actor System" ;; "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor Server"
                    'lw-reset-actors-server
                    :before "Stop Actor System")

  (lw:define-action "Save Session After"
                    "Restart Actor Server"
                    'lw-start-actors-server
                    :after "Restart Actor System")
  )

(defun com.ral.actors:start ()
  (lw-start-actors-server))

#| ;; for manual loading mode
(if (mpc:get-current-process)
    (lw-start-tcp-server)
  (pushnew '("Start Actor Server" () lw-start-actors-server) mpc:*initial-processes*
           :key #'third))
|#
