;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :com.ral.actors.secure-comm)

;; --------------------------------------------------------------------
;; Client side

;; ------------------------------------------------------------------
;; ECDH Shared Key Development for Repudiable Communications
;;
;;     -- Initial Keying Exchanges --
;;  Client                       Server
;;  ------                       ------
;;  APt = a*G, a random
;;  Ephem-ID APt Client-PKey --> +SERVER-CONNECT-ID+
;;                               BPt = b*G, b random
;;                  Ephem-ID <-- CnxID BPt Server-PKey
;;
;;    => EKey = H(a*BPt | client-skey*BPt | a*Server-PKey)    ;; at client side
;;            = H(b*APt | b*Client-PKey   | server-skey*APt)  ;; at server side
;;            = H(a*b*G | b*c*G | a*s*G)
;;
;; No signatures employed. There are no visible ties of a public key
;; to any encryption. All it takes is knowledge of public keys and
;; random points.  Anyone can do, even if totally faked. But only the
;; two sides participating will understand the resulting shared secret
;; EKey.
;;
;; During a connection, the abilty to perform a request and receive a
;; response is proof to both sides that the other controls the secret
;; key behind the presented public key of the initial keying exchange.
;;
;; Encryption and authentication have perfect forward secrecy, even
;; after a breach which discovers the secret keys for both client and
;; server. EKey lasts only as long as the client-server connection
;; remains open. After that EKey is forgotten by both parties. So even
;; the client and server will not be able to decrypt a log of
;; encrypted transmissions from prior sessions.
;;
;; Anyone can forge a transcript by making up random (a, b) values for
;; the attacker and ther victim, and using their public key along with
;; the public key of the victim. There is no way to prove that any
;; participant actually held a conversation. No signatures means
;; nothing to refute.
;;
;;     ...for all subsequent messages...
;;       Client                        Server
;;       ------                        ------
;;       Ephem-ID' Seq E(msg) Auth --> CnxID                 
;;                       Ephem-ID' <-- Seq' E(response) Auth'  ;; if we generate a response
;;
;; Connection ID's are always sent in the clear (not encrypted, but
;; encoded for serialization) so that receivers can dispatch. But
;; these are randomly generated, ephemeral, UUID's.
;;
;; Connections are transparently established for users, and then are
;; shut down after some period of inactivity (currently 20s). All the
;; user needs to know is the IP Address of the server and the name of
;; the service. Both parties are completely unaware of EKey and Seq.
;;
;; Any computer running an Actors system can behave as both client and
;; server. The distinction is merely that clients send requests, and
;; servers might respond with replies.
;;
;;   G      = Generator Pt for Curve1174
;;   H      = SHA3/256
;;   Nonce[0] = Int( H(Fresh-UUID/v1) ) < 2^256, init at start of Lisp session
;;   Nonce[n] = Nonce[n-1] + 2^256, Noncer is global resource for Lisp session.
;;
;;   Seq    = Nonce[++n]
;;   Auth   = H( H(:AUTH | EKey | Seq) | Seq | E(msg))
;;   E(msg) = SHAKE256(:ENC | EKey | Seq) XOR msg, effectively a one-time-pad
;;
;; Decryption is the same as Encryption.  All Seq are selected from
;; sequential nonces and label each transmission. Generated
;; independently on both sides. Allows for avoiding replay attacks.
;;
;; For all practical purposes, Nonces are true nonces. They start as
;; the SHA3/256 of a fresh v1-UUID at the start of a Lisp session, and
;; are provided to client code on demand. v1-UUID are 60-bit
;; timestamped and increment every 100ns. Every Noncer request
;; increments the global nonce by 2^256. Since SHA3/256 < 2^256, and
;; since nobody has ever seen a hash collision in SHA3/256, these are
;; essentially unique numbers.
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
;; Every fragment is assigned a fresh Seq, obtained from the Noncer.
;; They may not be strictly sequential above 2^256 because other
;; connections may be requesting Nonces too. But every fresh Seq will
;; be some increment above the previous one.
;;
;; Since Seq is also a component of the encryption and authentication
;; keying, every fragment is uniquely encrypted and authenticated. No
;; two encryptions of the same fragment will appear the same. This,
;; again, relies on SHA3/256 never having seen a hash collision.
;;
;; Received fragments are decoded, then decrypted and reassembled
;; (unchunked), then decompressed, and then deserialized back to Lisp
;; objects. Arrival order of message fragments can be arbitrary
;; despite in-order TCP packet reassembly.
;;
;; The bit of repudiable cleverness is derived from ideas presented by
;; Trevor Perrin and Moxie Marlinspike of Signal Foundation.
;;

#-:lattice-crypto
(deflex negotiator
  (create
   (lambda (cust socket local-services)
     (let+ ((client-id       (uuid:make-v1-uuid))
            (:β (srv-pkey)   eccke:srv-pkey)
            (:β (my-pkeyid)  eccke:my-pkeyid)
            (:β (arand apt aescrypt) (racurry eccke:ecc-cnx-encrypt
                                              srv-pkey +server-connect-id+ client-id my-pkeyid))
            (responder  (create
                         (alambda
                          ((bpt server-id) /  (and (typep bpt         'ecc-pt)
                                                   (ed-validate-point bpt)
                                                   (typep server-id   'uuid:uuid))
                           (let+ ((:β (my-skey)  eccke:ecc-skey)
                                  (ekey  (hash/256 (ed-mul bpt arand)           ;; B*a
                                                   (ed-mul bpt my-skey)         ;; B*c
                                                   (ed-mul srv-pkey arand)))    ;; S*a
                                  (chan  (create
                                          (lambda (&rest msg)
                                            (>>* local-services :ssend server-id msg))
                                          ))
                                  (:β _  (racurry local-services :set-crypto ekey socket)))
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
   (lambda (cust socket local-services)
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
                               (lambda (&rest msg)
                                 (>>* local-services :ssend server-id msg))
                               ))
                       (:β _  (racurry local-services :set-crypto ekey socket)))
                  (>> connections cust :set-channel socket chan)
                  ))
               
               ( _
                 (error "Server not following connection protocol"))
               )))
            (:β _ (racurry local-services :add-single-use-service client-id responder)) )
       (>> socket latcrypt aescrypt)
       ))
   ))

;; -----------------------------------------------------------------------------------

(deflex client-gateway
    ;; This is the main local client service used to initiate
    ;; connections with foreign servers.
    ;; Go lookup the encrypted channel for this IP, constructing it on
    ;; demand if not already present.
    (create
     (lambda (cust host-ip-addr)
       (>> client-connector cust negotiator host-ip-addr) )
     ))

;; ---------------------------------------------------
;; User side of Client Interface

(defun parse-remote-actor (id)
  ;; parse id in form "eval@arroyo.local:65001"
  (let* ((sid  (string id))
         (apos (position #\@ sid)))
    (if apos
        (values (um:kwsymbol (subseq sid 0 apos))
                (subseq sid (1+ apos)))
      (values (um:kwsymbol sid)))
    ))

(defun remote-service (name &optional host-ip-addr)
  ;; An Actor and send target. Connection to remote service
  ;; established on demand.
  (let+ ((:mvb (svc host) (parse-remote-actor name))
         (host (or host host-ip-addr)))
    (unless host
      (error "No server host specified: ~S" name))
    (create
     (lambda (cust &rest msg)
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
           (become (counter-beh 0))
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
         (lambda (cust)
           (let ((recho (remote-service :echo host)))
             (>> recho cust :stop) ))))
    (>> (create (counter-beh)) :start)
    ))

(tst "rincon.local")
(tst "rambo.local")
|#
