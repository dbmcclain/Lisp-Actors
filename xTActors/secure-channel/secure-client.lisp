;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :com.ral.actors.secure-comm)

;; --------------------------------------------------------------------
;; Client side

;; ------------------------------------------------------------------
;; X3DH Shared Key Development for Repudiable Communications
;;
;;     -- Initial Keying Exchanges --
;;
;;  Given: Client identified by Client-PKey-ID (a UUID)
;;         Server identified by Server-PKey-ID (a UUID)
;;
;;  Encryption is AES/256 with Key and IV, using CTR mode:
;;     E(Key, IV) XOR msg  => ctxt = enceypted message
;;     E(Key, IV) XOR ctxt => msg  = decrypted message
;;
;;  Hashing, H, is SHA3/256 of concatenated args
;;
;;  Comm channels are logically labeled with unique UUIDs, called
;;  Channel ID's. Messages are directed toward these channels.
;;
;;  Actors on a sending machine are denoted by unique UUID Channel IDs
;;  for the other machines on the network. Local Actors embedded in a
;;  network transmitted message get converted to local Comm Channel
;;  UUIDs for transmission, and ephemeral channel handlers are
;;  installed on the local machine. These handlers will forward
;;  incoming network messages to their local Actors.
;;
;;  On receipt of a message, any UUID encoded Actor channels cause the
;;  construction of, and replacement by, local proxy Actors on the
;;  recipient machine. The only job of a proxy Actor is to forward
;;  messages across the network to their Comm Channels on the
;;  originating machine.
;;
;;     
;;  Client                       Server
;;  ------                       ------
;;  APt  = a*G, a random
;;  Server-PKey = Lookup(Server-PKey-ID)
;;  Validate Server-PKey pt
;;  EKey = H(a*Server-PKey)
;;  IV   = H(EKey, new UUID)
;;  Ephem-ID = new UUID for comm channel
;;  ctxt = E(EKey, IV) XOR (+SERVER-CONNECT-ID+, Ephem-ID, Client-PKey-ID)
;;  chk  = H(EKey, IV, ctxt)
;;  (APt, (IV, ctxt, chk)) -->
;;                               Validate APt pt
;;                               EKey = H(server-skey * APt)
;;                               Validate chk == H(EKey, IV, ctxt)
;;                               (chan, Ephem-ID, Client-PKey-ID) = H(EKey, IV) XOR ctxt
;;
;;                               -- IF chan == +SERVER-CONNECT-ID+ -------------------
;;                               Client-PKey = Lookup(Client-PKey-ID)
;;                               Validate Client-PKey pt
;;                               BPt   = b*G, b random
;;                               EKey' = H(b*Client-PKey)
;;                               IV'   = H(EKey', new UUID)
;;                               CnxID = new UUID for comm channel
;;                               ctxt' = E(EKey', IV') XOR (Ephem-ID, CnxID)
;;                               chk'  = H(EKey', IV', ctxt')
;;                           <-- (BPt, (IV', ctxt', chk'))
;;                               Derive shared EKey'' for use on channel CnxID
;;
;;  Validate BPt pt
;;  EKey' = H(client-skey * BPt)
;;  Validate chk' == H(EKey', IV', ctxt')
;;  (chan, CnxID) = E(EKey', IV') XOR ctxt'
;;
;;  -- If chan == Ephem-ID -------------------
;;  Derive shared EKey'' for use on channel CnxID
;;  
;;
;;    => EKey'' = H(a*BPt | client-skey*BPt | a*Server-PKey)    ;; at client side
;;              = H(b*APt | b*Client-PKey   | server-skey*APt)  ;; at server side
;;              = H(a*b*G | b*c*G | a*s*G)
;;
;; No signatures employed. There are no visible ties of a public key
;; to any encryption. All it takes is knowledge of public keys and
;; random points.  Anyone can do, even if totally faked. But only the
;; two sides participating will understand the resulting shared secret
;; EKey''.
;;
;; During a connection, the actual key employed is uniquely generated
;; from EKey'' for each transmission. Keying is advanced using a
;; Double-Ratchet mechanism.
;;
;; The mere abilty to perform a successful keying exchange is proof to
;; both sides that the other controls the secret key behind the public
;; key ID used by each side in the initial keying exchange.
;;
;; Encryption and authentication have perfect forward secrecy, even
;; after a breach which discovers the secret keys for both client and
;; server. The dynamic EKey'' lasts only as long as the client-server
;; connection remains open. After that EKey'' is forgotten by both
;; parties. So even the client and server will not be able to decrypt
;; a log of encrypted transmissions from prior sessions.
;;
;; Anyone can forge a transcript by making up random (a, b) values for
;; the attacker and ther victim, and using their public key along with
;; the public key of the victim. There is no way to prove that any
;; participant actually held a conversation. No signatures means
;; nothing to refute.
;;
;;       ...for all subsequent messages...
;;       Client                        Server
;;       ------                        ------
;;       IV = H(:NONCE, fresh-UUID, EKey'')
;;       Key = H(:ENCR, EKey'', IV)
;;       ctxt = E(Key, IV) XOR (CnxID, msg)
;;       authKey = H(:AUTH, Key'', IV)
;;       auth = H(authKey, IV, ctxt)
;;       (IV, ctxt, auth) -->
;;                                     Discard duplicate IVs to prevent replay attacks
;;                                     authKey = H(:AUTH, Key'', IV)
;;                                     Check auth == H(authKey, IV, ctxt)
;;                                     Broadcast (:AUTH-KEY, IV, authKey) for refutability
;;                                     Key = H(:ENCR, EKey'', IV)
;;                                     (chan, msg) = D(Key, IV, ctxt)
;;                                     -- on chan CnxID perform local message send ----
;;                                     
;;
;; If a response is expected, the client message will contain a
;; customer ID. That takes the place of CnxID in server side messages
;; using the same protocol.
;;
;; The only things visible are the IV, cryptotext, and authentication.
;; After validating the authentication, the authentication key is
;; broadcast to the world in the clear, for our refutability. The
;; channel ID and message are kept hidden by encryption with a roving
;; key.
;;
;; Connections are transparently established for users, and then are
;; shut down after some period of inactivity (currently 20s). All the
;; client needs to know is the IP Address of the server and the name
;; of the service. Both parties are completely unaware of EKey and IV
;; being used in layers beneath them.
;;
;; Any computer running an Actors system can behave as both client and
;; server. The distinction is merely that clients send requests, and
;; servers might respond with replies. On initial connection, the
;; client side initiates the keying handshake.
;;
;;   G      = Generator Pt for Curve1174
;;   H      = SHA3/256
;;   E      = AES/256 in CTR mode
;;
;; Encryption uses AES/256 in CTR mode. Decryption is the same as
;; Encryption.  All IV are generated from monotonically increasing
;; (time based to nearest 10 ns) type-1 UUID, and these IV label each
;; transmission. Message IV are checked for uniqueness, and messages
;; with duplicate IV are discarded to prevent replay attacks.
;;
;; Prior to encryption and wire transmission, the arbitrary Lisp
;; objects of a message are serialized, compressed, and possibly
;; chunked into fragments smaller than some maximum limit.
;;
;; Each fragment is then separately transmitted through the encryption
;; scheme and onto the wire using a self-sync encoding.  Transmission
;; order of message fragments is usually scrambled due to parallel
;; concurrent activity and can be arbitrary.
;;
;; Every fragment is assigned a fresh IV, and double-ratchet produces
;; roving keying.
;;
;; Since IV is also a component of the encryption and authentication
;; keying, every fragment is uniquely encrypted and authenticated. No
;; two encryptions of the same fragment will appear the same. This,
;; again, relies on SHA3/256 never having seen a hash collision.
;;
;; Received fragments are decoded, then decrypted and reassembled
;; (unchunked), then decompressed, and then deserialized back to Lisp
;; objects. Arrival order of message fragments can be arbitrary
;; despite in-order TCP packet reassembly.
;;
;; ---------------------------------------------------------------
;;
;;                  -- Post-QC Security --
;;
;; I believe that our use of ECC X3DH is QC resistant. We never expose
;; any keying, neither Public nor Private, to the network.
;;
;; When we need to convey a public key, it must already be known to
;; the recipient, and is identified by PKey-ID. That ID is looked up
;; in a local database to find the actual PKey. PKey-ID are never
;; exposed to the network, but always conveyed in an AES/256 encrypted
;; wrapper.
;;
;; To crack the initial key exchange AES packets, you have to know the
;; Public key of the recipient and the DLP of the random ECC point
;; sent, or equivalently, know the random ECC point and the
;; recipient's Secret Key.
;;
;; To derive the shared session keying, you have to see both random
;; ECC Points, obtain the DLP for one of those points, AND know the
;; Secret Key of the originator, AND the Public Key of the recipient.
;;
;; Once the parties have a shared session key, all further
;; communication occurs via AES/256 encrypted packets, using
;; double-ratchet keying based off of the initial shared session key,
;; advancing the ratchet for every message. Shared session keys are
;; forgotten after the connecton is closed.
;;
;; We use repudiable communications, where the two parties can be
;; certain of each other's message origination, yet authenticated
;; messages can be easily forged by 3rd parties after the message has
;; been received.
;;
;; The recipient broadcasts the authentication keying used in the
;; received message. This allows anyone to construct a fake transcript
;; of the session by posing as one of the parties involved. Hence,
;; we have repudiable communications.
;;
;; We are protected against replay attacks by ensuring that every
;; message received carries a fresh (IV) sequence number.
;;
;; At the first sign of tampering, the connection is silently closed.
;; Invalid messages are ignored. Failures of decryption,
;; decompression, or unmarshaling are taken as signs of tampering.
;;
;; Although we refer to Public Keys, they are never openly published
;; anywhere. They are as guarded as Secret Keys, except for making
;; them available, indirectly via PKey-ID, to others via secure
;; transport.
;;
;; New public keys are introduced, along with a PKey-ID and a proof of
;; validity, either through side channels, or else by way of
;; introduction from a recognized sponsor using encrypted messaging.
;;
;; ----------------------------------------------------------------
;;
;; The bit of repudiable cleverness is derived from ideas presented by
;; Trevor Perrin and Moxie Marlinspike of Signal Foundation.
;;
;; ----------------------------------------------------------------
;;
;;           === Double-Ratchet Encryption Advancing ===
;;
;; We now use a double-ratchet mechanism to advance the encryption
;; keying on every message exchange. The sender of a message chooses a
;; random DH point and transmits that along with the message. The pair
;; are encrypted and authenticated, usng the shared session key, for
;; transmission over the network.
;;
;; The random (DHpt = krand*G) implies a new shared DH key, (DHKey =
;; krand*PKey = skey*DHpt), between both parties, where PKey is the
;; recipient's Public Key, and skey is our own Secret Key.
;;
;; On the sender side, the new DH Key will be (krand*PKey), using the
;; Public Key of the recipient. A new session next encryption key is
;; formed by hashing together the existing session encryption key and
;; this new DHKey.
;;
;; On reception of a message, the current session key is used to
;; authenticate and decrypt the incoming message. The random DH point
;; is peeled off and the rest of the message is forwarded to the
;; customer.
;;
;; The random DH point is multiplied by our Secret Key to find the new
;; shared DH session key, and the next shared encryption key will be
;; the hash of the current session key and this new DH key.
;;
;; There is no need to remember message ID's for protection against
;; replay attacks. There can be no valid replay messsages.  Every new
;; messsage is sent using a fresh key, and all older keys will have
;; been forgotten. So any replayed messages simply won't authenticate
;; under the advancing session keying.
;;
;; Just knowing a session key is not enough to enable breaking into
;; future messages, nor any past messages. And getting the next
;; encryption key is more than merely hashing the existing key.  You
;; also have to know how to compute the next shared DHKey for the
;; session. This is the double ratchet.
;;
;; ---------------------------------------------------------------

#-:lattice-crypto
(deflex negotiator
  (create
   (behav (cust socket local-services)
     (let+ ((client-id       (uuid:make-v1-uuid))
            (:β (srv-pkey)   eccke:srv-pkey)
            (:β (my-pkeyid)  eccke:my-pkeyid)
            (:β (arand apt aescrypt) (racurry eccke:ecc-cnx-encrypt
                                              srv-pkey +server-connect-id+ client-id my-pkeyid))
            (responder  (create
                         (alambda
                          ((bpt server-id) /  (and (typep bpt         'ecc-pt)
                                                   (typep server-id   'uuid:uuid))
                           (let+ ((:β (my-skey)  eccke:ecc-skey)
                                  (ekey  (with-ed-curve +ECC-CURVE+
                                           (hash/256 (ed-mul bpt arand)           ;; B*a
                                                     (ed-mul bpt my-skey)         ;; B*c
                                                     (ed-mul srv-pkey arand))))   ;; S*a
                                  (chan  (create
                                          (behav (&rest msg)
                                            (>>* local-services :ssend server-id msg))
                                          ))
                                  (ratchet   (client-ratchet-manager ekey my-skey srv-pkey))
                                  (encryptor (ratchet-encryptor ratchet))
                                  (decryptor (ratchet-decryptor socket ratchet))
                                  (:β _  (racurry local-services :set-crypto
                                                  socket encryptor decryptor)))
                             (>> connections cust :set-channel socket chan)
                             ))
                          
                          ( _
                            (error "Server not following connection protocol"))
                          )))
            (:β _  (racurry local-services :add-single-use-service client-id responder)) )
       (>> socket apt aescrypt)
       ))
   ))
  
;; -----------------------------------------------------------------------------------

#+:lattice-crypto
(deflex negotiator
  (create
   (behav (cust socket local-services)
     (let+ ((client-id         (uuid:make-v1-uuid))
            (:β (srv-pkeyid)   lattice-ke:srv-pkeyid)
            (:β (my-pkeyid)    lattice-ke:my-pkeyid)
            (:β (akey latcrypt aescrypt) (racurry lattice-ke:cnx-packet-encoder
                                                  srv-pkeyid +server-connect-id+
                                                  client-id my-pkeyid))
            (responder
             (create
              (alambda
               ((bkey server-id) /  (and (typep bkey      'ub8-vector)
                                         (typep server-id 'uuid:uuid))
                (let+ ((ekey  (hash/256 bkey akey))
                       (chan  (create
                               (behav (&rest msg)
                                 (>>* local-services :ssend server-id msg))
                               ))
                       (:β _  (racurry local-services :set-crypto ekey socket)))
                  (>> connections cust :set-channel socket chan)
                  ))
               
               ( _
                 (error "Server not following connection protocol"))
               )))
            (:β _ (racurry local-services :add-single-use-service client-id responder)) )
       (>> socket latcrypt (butlast aescrypt)) ;; elide the auth check
       ))
   ))

;; -----------------------------------------------------------------------------------

(deflex client-gateway
    ;; This is the main local client service used to initiate
    ;; connections with foreign servers.
    ;; Go lookup the encrypted channel for this IP, constructing it on
    ;; demand if not already present.
    (create
     (behav (cust host-ip-addr)
       (>> client-connector cust negotiator host-ip-addr) )
     ))

;; ---------------------------------------------------
;; User side of Client Interface

(defun parse-remote-actor (id)
  ;; parse id in form "eval@arroyo.local:65001"
  (let* ((sid  (string id))
         (apos (position #\@ sid))
         (svc  (um:kwsymbol (subseq sid 0 apos))))
    (if apos
        (values svc (subseq sid (1+ apos)))
      svc)
    ))

(defun remote-service (name &optional host-ip-addr)
  ;; An Actor and send target. Connection to remote service
  ;; established on demand.
  (let+ ((:mvb (svc host) (parse-remote-actor name))
         (host (or host host-ip-addr)))
    (unless host
      (error "No server host specified: ~S" name))
    (create
     (behav (cust &rest msg)
       (β (chan)
           (>> client-gateway β host)
         (>>* chan cust svc msg))
       ))))

;; ------------------------------------------------------------
#|
(defun tst (host)
  (let ((recho (remote-service :echo host))
        (msg   :hello))
    (β (ans)
        (>> recho β msg)
      (>> fmt-println "(>> recho println ~S) sez: ~S" msg ans))))
(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "rambo.local")
(install-atrace)
(atrace)
(atrace nil)

(defun tst (host)
  (let ((recho (remote-service :echo host))
        (txt   (hcl:file-string "./xTActors/secure-channel/encoding.lisp")))
    (β (ans)
        (>> recho β txt)
      (>> fmt-println "echo comparison: ~A" (string= txt ans)))))

(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (ans)
        (>> reval β '(list (um:zulu-date-string) (machine-instance)))
      #|
        (>> reval β '(um:capture-ans-or-exn
                            (error "test-error")))
        |#
      ;; (error "Waht!?")
      (trace-me)
      (>> fmt-println "reval sez: ~S" (um:recover-ans-or-exn ans)))
    ))
(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "rambo.local")
(tst "dachshund.local")
(tst "honeypot.local")
(tst "david-pc.local")
(tst "umbra.local")
(tst "zircon.local")
(tst "fornax.local")
(tst "sextans.local")

(defun tst (host n)
  (let* ((recho (remote-service :echo host))
         (ac1   (α (n)
                  (when (plusp n)
                    (let ((me    self)
                          (start (usec:get-time-usec)))
                      (β _
                          (>> recho β "")
                        (let ((stop (usec:get-time-usec)))
                          (>> println (- stop start))
                          (>> me (1- n))))
                      )))))
    (>> ac1 n)))

(tst "localhost" 10)
(tst "zircon.local" 10)
(tst "rincon.local" 10)
(tst "david-pc.local" 10)

(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (ans)
        (>> (timed-service reval 10) β `(um:capture-ans-or-exn kvdb:kvdb))
      (let ((rkvdb (um:recover-ans-or-exn ans)))
        (β (proxy)
            (>> (timed-service rkvdb 10) β :req-proxy)
          (β (uuid)
              (>> (timed-service proxy 10) β :find 'kvdb::version)
            (>> println (uuid:when-created uuid))
            ))))))

(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "zircon.local")
(tst "fornax.local")

(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (proxy)
        (>> (timed-service reval 10) nil `(>> kvdb:kvdb ,β :req-proxy))
      (β (uuid)
          (>> (timed-service proxy 10) β :find 'kvdb::version)
        (>> println (uuid:when-created uuid))
        ))))

;; ---------------------------------------------------------
;; Demonstrate Hewett's Unbounded Nondeterminism
;;
(defun tst (host)
  ;; Make a counter Actor that increments its state by 1, on message
  ;; :COUNT, and then sends itself another :COUNT message, until
  ;; receiving a :STOP message.
  ;;
  ;; At :START it fires off a kind of timer to send itself a :STOP
  ;; message ASAP and sends itself the first :COUNT message.
  ;;
  ;; The counter will stop and print out its count on receipt of the
  ;; :STOP message.
  ;;
  ;; The timer here is the network delay in asking a remote server to
  ;; send that :STOP message back to the counter Actor. It has unknown
  ;; and variable delay.
  ;;
  ;; Lacking a response from the network, there is a secondary
  ;; :TIMEOUT message sent by a parallel timer, to cause the Actor to
  ;; cease counting. This second timer is reliable, unlike a TCP
  ;; network.
  ;;
  ;; The final count is nondeterministic and unbounded, and the code
  ;; is Halting. This program is something that a pure Lambda Calculus
  ;; cannot perform. Hence, proving that Actors are more general than
  ;; Lambda Calculus.
  ;;
  ;; Lambda Calculus performs comptation by substitution of bindings.
  ;; Lambda Calculus is provably equivalent to a Turing Machine. Hence
  ;; both are subsumed by Actors, and the notion of Universal
  ;; Computation - being anything that a Turing Complete computing
  ;; system can perform, is a proper subset of reality.
  ;;
  ;; Lisp is Lambda Calculus PLUS an Environment into which program
  ;; state is recorded. Since I'm doing this in Lisp, it is obvious
  ;; that Lisp transcends Lambda Calculus too. To the extent that I
  ;; have captured all of Actors potential, it proves that Lisp and
  ;; Actors Universality are the same.
  ;;
  ;; There is no mutation in this code, except for the controlled
  ;; mutation provided by BECOME.
  ;;
  (labels
      ((counter-beh (&optional (count 0))
         (alambda
          ((:start)
           (>> (create (timer-beh)) self)
           (>> self :count)
           (send-after 20 self :timeout)
           (>> println :tst-starting))
          ((:timeout)
           (>> fmt-println "tst: emergency stop: count = ~D" count)
           (become-sink))
          ((:stop)
           (>> fmt-println "tst: final count = ~D" count)
           (become-sink))
          ((:count)
           (become (counter-beh (1+ count)))
           (>> self :count))
          (msg
           (send fmt-println "tst: unexpected message: ~W" msg)
           (become-sink))) )
       (timer-beh ()
         (behav (cust)
           (let ((recho (remote-service :echo host)))
             (>> recho cust :stop) ))))
    (>> (create (counter-beh)) :start)
    ))

(tst "rincon.local")
(tst "rambo.local")
(tst "sextans.local")
|#
