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
  shutdown)

;; --------------------------------------------------
;; Gentle and Forcible Socket Closing

(deflex socket-closer
  ;; Use only when it is known that no async streams are operating
  (create
   (lambda (cust io-state)
     (comm:close-async-io-state io-state)
     (send cust :ok))))

(deflex forcible-socket-closer
  ;; Used when an async reader or writer may currently be running
  (create
   (lambda (cust io-state)
     (comm:async-io-state-abort-and-close io-state)
     (send cust :ok))))

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
  (alambda
   ((cust :terminate)
    (become (io-closed-beh))
    (send forcible-socket-closer cust io-state))

   ((cust :try-writing _ if-nok)
    (send println "Can't happen - attempt to write while busy writing.")
    (send if-nok)
    (send cust :err))
   ))

(defun io-closed-beh ()
  ;; Fully closed state
  (alambda
   ((cust :try-writing _ if-nok)
    (send if-nok)
    (send cust :err))
   ))

(defun io-running-beh (io-state base)
  ;; In this state we are open to async reads, no writing under way,
  (alambda
   ((cust :try-writing if-ok _)
    (become (io-running-write-beh io-state base))
    (send if-ok)
    (send cust :ok))

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
                    ;; io-running will reply to our cust for us
                    (send io-running cust :finish-wr-ok))
                   ))
           
           (begin-write ()
                (comm:async-io-state-write-buffer state-io-state
                                                  byte-vec
                                                  #'write-done)
                (send kill-timer :resched)) )
        
        (send io-running sink :try-writing
              (create #'begin-write)
              (create #'not-writing))
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
    (become-sink)
    (send killer))

   ((:discard)
    (become-sink))
   ))

(defun make-kill-timer (timer-beh)
  (actors ((tag    (tag-beh timer))
           (killer timer-beh)
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

(defun same-ip-test (ip-addr ip-port)
  (lambda (rec)
    (and (eql ip-addr (connection-rec-ip-addr rec))
         (eql ip-port (connection-rec-ip-port rec)))))
  
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
      (become (counter-beh new-n))
      (send cust n))))

(deflex get-server-count
  (create (counter-beh 0)))

;; ------------------------
;; The list of currently active socket connections

(defun connections-list-beh (&optional cnx-lst)
  (alambda   
   ;; --------------------------------------------
   ;; A Server sends :ADD-SERVER when a new client connection is
   ;; established. The peer-ip addr is that of the client.
   ;;
   ;; If a record already exists for this peer-ip, shut down our new
   ;; attempt here. No point having a duplicate path.
   ;;
   ;; Else, construct a new record for our list of connections,
   ;; marked as a :PENDING-SERVER connection. The STATE and SENDER
   ;; Actor will be filled in during the construction of a socket
   ;; interface, which we initiate here.
   ;;
   ((:add-server peer-ip peer-port io-state)
    (let ((rec (find-connection-from-ip cnx-lst peer-ip peer-port)))
      (cond

       (rec
        ;; already exists or is pending - so shut down our attempt
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
                    sink))
            )))
      )))
        
   ;; --------------------------------------------
   ;; Message :ADD-SOCKET is sent during construction of the socket
   ;; interface graph.
   ;;
   ;; Add an initial socket connection to our list of available
   ;; connections. At this point the connection is not-encrypted.
   ;;
   ;; Look for an existing record for this same ip-addr:
   ;;
   ;;   1. If we find one and it is a :PENDING-SERVER record, then
   ;;   record the STATE and SENDER (socket writer Actor) in the
   ;;   record.  This makes it no longer pending, and available for
   ;;   unsecured comms.
   ;;
   ;;   2. Otherwise, if we found a record, and the STATE is not us,
   ;;   then shut the previous connection down. Record the new SENDER
   ;;   and STATE in the record.
   ;;
   ;;   3. Else, no record was found, so construct a new one and add
   ;;   it to our list. That happens when a client wants a connection
   ;;   to a new server.
   ;;
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
  
   ;; --------------------------------------------
   ;; Message :FIND-SOCKET is sent by client Actors on this machine,
   ;; looking for a socket connection to a remote host.
   ;;
   ;; Look for an available connection already in place.
   ;;
   ;; If we find one, and it has no waiting list, then send its
   ;; writer Actor to the customer as an available connection to the
   ;; server.
   ;;
   ;; If we find one, but it has a waiting list, then join the wait -
   ;; they are waiting on a successful handshake with the server. All
   ;; will be notified if it completes.
   ;;
   ;; If no records are found for the server ip address, then
   ;; construct one with us as the first waiting member, add the new
   ;; record to the list, and then try to form a network TCP
   ;; connection with the server.
   ;;
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
          (non-idempotent
            (try-to-connect-socket ip-addr ip-port report-ip-addr))
          ))
      ))
  
   ;; --------------------------------------------
   ;; Message :ABORT might be sent during an initial attempt to form a
   ;; TCP connection with a server. If that connection fails, we get
   ;; this message.
   ;;
   ;; Look for a records mentioning the ip adddr and purge it from our
   ;; list of available connections. Future requests will begin anew.
   ;; Any waiting clients will just be left hanging.
   ;;
   ((cust :abort ip-addr ip-port)
    (send cust :ok)
    (let* ((same-ip (same-ip-test ip-addr ip-port))
           (rec     (find-if same-ip cnx-lst)))
      (when rec
        ;; leaves all waiting custs hanging...
        (let ((new-lst (remove-if same-ip cnx-lst)))
          (become (connections-list-beh new-lst))
          (non-idempotent
            ;; ...would also prevent us from updating the list...
            (error "Can't connect to: ~S"
                   (connection-rec-report-ip-addr rec)))
          ))))

   ;; --------------------------------------------
   ;; Mesasge :NEGOTIATE is sent after successfully forming a TCP
   ;; connection with a server.
   ;;
   ;; The server is now waiting for us to start the handshake
   ;; crypto-dance.
   ;;
   ;; Client Actors must await a secure channel. The only Actors that
   ;; are permitted to communicate over an insecure channeol are
   ;; those involved in the crypto-dance of the initial handshake
   ;; with the server. And even that is encrypted in a different way.
   ;;
   ;; When an initial TCP connection is esablished with a server on
   ;; the network, that server just sits there silently waiting for
   ;; the handshake dance to begin from the client side. The server
   ;; only responds if it recognizes the client and its encrypted
   ;; handshae message.
   ;;
   ;; If we didn't find any record of this TCP connection in our
   ;; list, then just play dumb and say :OK.
   ;;
   ;; Here STATE is the state devloped by the socket interface, and
   ;; SOCKET is the unencrypted output Actor.
   ;;
   ;; All the SOCKET Actor does is to serialize whatever is being
   ;; sent, then encode it into a self-sync byte streaam, before
   ;; sending it out the physical async socket port. It doesn't even
   ;; chop it into manageable blocks of data. So there will be some
   ;; maximum size that can be transferred successfully across the TCP
   ;; port.
   ;;
   ;; The server side has the same situation at this time. So whatever
   ;; is transferred in either direction is done so in the clear,
   ;; except for these simple encodings. Anyone listening with
   ;; decoders for these serial protocols will be able to read what is
   ;; sent.
   ;; ------------------------------------------------------------
   ;; With the Lattice crypto in place, the client first sends an
   ;; ephemeral UUID that refers to himself as a recipient Actor on
   ;; his side, and a list containing two encrypted packets. The first
   ;; packet contains a public key encryption of an AES-256 key to
   ;; unlock the second packet. The second packet contains the
   ;; client's public key id (a key into the database that identifies
   ;; his public key), and a 32-byte random vector, A
   ;;
   ;; On receipt of that transmission, the server checks that a UUID
   ;; was received followed by the list of two encypted packets. He
   ;; decrypts the first, using his private Lattice key. Then with the
   ;; revealed AES key he decrypts the second packet.
   ;;
   ;; If the server recognizes the public key id of the client, he
   ;; accepts random vector A, and sends back two items - an ephemeral
   ;; UUID that refers to himself as a recipient Actor on his end, and
   ;; a public key encrypted 32-byte random vector B.  Thereafter the
   ;; session key will be the hash SHA3/256(B | A).
   ;;
   ;; Those are the only two transmissions performed "in the clear".
   ;; All that the world can see are just two ephemeral UUID's, used
   ;; to refer to the Actor sending the message. They can't even see
   ;; the identity of the node, nor any public keys, from which the
   ;; messages originated. TCP furnishes the IP address of the sending
   ;; node, but that may be a forwarding node, not the originator of
   ;; the message.
   ;;
   ;; Thereafter all transmissions are 3-element messages containing a
   ;; message sequence vector (16 bytes from a hash), an AES-256
   ;; encrypted vector of the actual message, and an authentication
   ;; vector (32 bytes from a hash) for the transmitted message. Each
   ;; new message triple, sent from either side, uses a unique
   ;; evloving encryption key based on the secret session key
   ;; developed during the initial handshake.
   ;;
   ;; Every incoming message triple is first checked to see that it
   ;; isn't a replay message, already seen before. Then the
   ;; authentication is verified. If that checks out then the AES
   ;; cryptotext is decrypted to reveal an actual message.
   ;;
   ;; Actors referred to in messages, as for customers, and other
   ;; args, are translated before sending, into ephemeral UUID's. On
   ;; message receipt those UUID's are either logged for message
   ;; reply, or looked up in a local directory to find the actual
   ;; Actor behind them. Ephemeral UUID's expire after 10 sec.
   ;;
   ;; The entire socket connection is disolved after 20 sec of
   ;; inactivity on either side. Fresh connections are reestablished
   ;; on demand. At the user level, Actors don't even know if they are
   ;; sending messages to other local Actors, or to Actors at the end
   ;; of a remote connection. There is no difference, for them, in
   ;; message sending.
   ;;
   ((cust :negotiate state socket)
    (let ((rec (find-connection-from-sender cnx-lst socket)))
      (if rec
          ;; Let the dance begin...
          (send (connection-rec-handshake rec) cust socket (intf-state-local-services state))
        ;; else
        (send cust :ok))
      ))
  
   ;; --------------------------------------------
   ;; Message :SET-CHANNEL is sent during handshake after a secure
   ;; channel writer (Actor) has been constructed for this socket
   ;; connection.
   ;;
   ;; The writeable endpoint Actor for that encrypted channel is
   ;; CHAN.
   ;;
   ;; Look up the record containing the non-secure writer, SENDER,
   ;; and add the secure endpoint, CHAN, to the record.
   ;;
   ;; Then notify all waiting clients about it. Clear the waiting
   ;; list and update our records.
   ;;
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
            (multiple-value-bind (peer-ip peer-port)
              #+:LISPWORKS8
              (comm:socket-connection-peer-address (intf-state-io-state state))
              #+:LISPWORKS7
              (comm:get-socket-peer-address (slot-value (intf-state-io-state state) 'comm::object))
              (send fmt-println "Client Socket (~S->~A:~D) starting up"
                    (connection-rec-report-ip-addr rec)
                    (comm:ip-address-string peer-ip) peer-port))
            (send-to-all custs chan)))
        )))
    
   ;; --------------------------------------------
   ;; Message :REMOVE is sent when a socket connection is closed down.
   ;; Just remove any records referencing STATE.
   ;;
   ((cust :remove state)
    (send cust :ok)
    (let ((rec (find-connection-from-state cnx-lst state)))
      (when rec
        (let ((new-lst (remove state cnx-lst ;; should only be one...
                               :key #'connection-rec-state)))
          (become (connections-list-beh new-lst))
          ))))
   ))
  
(def-actor connections
  (create (connections-list-beh)))

;; -------------------------------------------------------------
;; Socket Shutdown needs the state, but itself is part of that state.
;; So we implement a 2-stage construction here.

(defun socket-shutdown-beh (state)
  (with-accessors ((kill-timer       intf-state-kill-timer)
                   (io-running       intf-state-io-running)
                   (title            intf-state-title)
                   (ip-addr          intf-state-ip-addr)) state
    (lambda ()
      (become-sink)
      (β _
          (send io-running β :terminate)
        (send fmt-println "~A Socket (~S) shutting down"
              title ip-addr)
        (send kill-timer :discard)
        (send connections sink :remove state)
        ))))

(defun initial-socket-shutdown-beh ()
  (alambda
   ((cust :init state)
    (become (socket-shutdown-beh state))
    (send cust :ok))
   ))

(defun make-socket-shutdown ()
  (create (initial-socket-shutdown-beh)))

;; ------------------------------------------------------------------------
;; For both clients and servers, we assemble the socket state, make
;; initial encoder/decoder which marshals and compresses and fragments
;; into 64kB packets. Set up the initial negotiation service in a per
;; socket local-services, start the socket reader going, and send the
;; state and encoder to the customer.

(defun socket-intf-beh (kind ip-addr ip-port io-state report-ip-addr)
  (lambda (cust)
    (let* ((title          (if (eq kind :client) "Client" "Server"))
           (local-services (make-local-services))
           (io-running     (make-io-running-monitor io-state))
           (shutdown       (make-socket-shutdown))
           (kill-timer     (make-kill-timer
                            (lambda ()
                              (send println "Inactivity shutdown request")
                              (send shutdown))
                            ))
           (state (make-intf-state
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
           (accum    (self-synca:stream-decoder   ;; async arrivals are sent here
                      (sink-pipe (fail-silent-marshal-decoder)
                                 local-services))))
      
      (sequential-β
        (;; complete the STATE initialization
         (send shutdown β :init state)
         
         ;; Inform LOCAL-SERVICES that we can form secure connections with clients
         (send local-services β :add-service-with-id +server-connect-id+
                   (server-crypto-gateway encoder local-services))

         ;; Inform the system that we are an available socket interface
         (send connections β :add-socket ip-addr ip-port state encoder))

        ;; set up the async reader and start it running
        
        (let ((fragment-ctr 0))  ;; a counter of async input fragments 
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
                          (β _
                              (send io-running β :done-reading)
                            (send fmt-println "~A Incoming error state: ~A" title status)
                            (send shutdown)))
                         
                         ((plusp end)
                          ;; TCP assures that messages arrive on the
                          ;; wire and are delivered here in whole, or
                          ;; not at all. But the async input system
                          ;; delivers them as they arrive in piecemeal
                          ;; fashion.
                          ;;
                          ;; Every input fragment is numbered here,
                          ;; starting from 1. But the Actor system
                          ;; might jumble the order of fragment
                          ;; delivery.
                          ;;
                          ;; (I have witnessed this happening.)
                          ;;
                          ;; Numbering them enables us to deal
                          ;; properly with possible out-of-order
                          ;; delivery to the self-sync decoder.
                          ;;
                          (send accum :deliver (incf fragment-ctr) (subseq buffer 0 end))
                          (send kill-timer :resched)
                          (comm:async-io-state-discard state end))
                         ))))
            
            ;; Start things running...
            (comm:async-io-state-read-with-checking io-state #'rd-callback-fn
                                                    :element-type '(unsigned-byte 8))
            (send kill-timer :resched)
            
            ;; And now we can tell our customer that our graph is complete and running
            (send cust state encoder)
            ))
        ))))

(defun create-socket-intf (&key kind ip-addr ip-port io-state report-ip-addr)
  (create (socket-intf-beh kind ip-addr ip-port io-state report-ip-addr)))

;; -------------------------------------------------------------

(defun try-to-connect-socket (ip-addr ip-port report-ip-addr)
  (let ((handler (α (io-state args)
                   (cond
                    (args
                     (send println
                           (format nil "CONNECTION-ERROR: ~S" report-ip-addr)
                           (apply #'format nil args))
                     (send connections sink :abort ip-addr ip-port))
                    
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
      
      (apply #'comm:create-async-io-state-and-connected-tcp-socket
             *ws-collection*
             ip-addr ip-port #'callback
             #-:WINDOWS
             `(:connect-timeout 5 :ipv6 nil)
             #+:WINDOWS
             `(:connect-timeout 5)
             ))
    ))

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
