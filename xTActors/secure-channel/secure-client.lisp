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
(progn
  (deflex negotiator
    (create
     (lambda (cust socket local-services)
       (let ((client-id  (uuid:make-v1-uuid)))
         (β (srv-pkey)
             (send eccke:srv-pkey β)
           (β (my-pkeyid)
               (send eccke:my-pkeyid β)
             (β (arand apt aescrypt)
                 (send eccke:ecc-cnx-encrypt β srv-pkey
                       +server-connect-id+ client-id my-pkeyid)
               (let ((responder
                      (create
                       (alambda
                        ((bpt server-id) /  (and (typep bpt       'ecc-pt)
                                                 (typep server-id 'uuid:uuid))
                         (β (my-skey)
                             (send eccke:ecc-skey β)
                           (let ((ekey  (hash/256 (ed-mul bpt arand)           ;; B*a
                                                  (ed-mul bpt my-skey)         ;; B*c
                                                  (ed-mul srv-pkey arand)))    ;; S*a
                                 (chan  (create
                                         (lambda (&rest msg)
                                           (send* local-services :ssend server-id msg))
                                         )))
                             (β _
                                 (send local-services β :set-crypto ekey socket)
                               (send connections cust :set-channel socket chan))
                             )))
                          
                        ( _
                          (error "Server not following connection protocol"))
                        ))))
                   (β _
                       (send local-services β :add-single-use-service client-id responder)
                     (send socket apt aescrypt))
                   )))
           )))
     ))
    
  (deflex client-gateway
    ;; This is the main local client service used to initiate
    ;; connections with foreign servers.
    ;; Go lookup the encrypted channel for this IP, constructing it on
    ;; demand if not already present.
    (create
     (lambda (cust host-ip-addr)
       (send client-connector cust negotiator host-ip-addr) )
     )) )

;; -----------------------------------------------------------------------------------

#+:lattice-crypto
(progn
  (deflex negotiator
    (create
     (lambda (cust socket local-services)
       (let ((client-id  (uuid:make-v1-uuid)))
         (β (srv-pkeyid)
             (send lattice-ke:srv-pkeyid β)
           (β (my-pkeyid)
               (send lattice-ke:my-pkeyid β)
             (β (akey latcrypt aescrypt)
                 (send lattice-ke:cnx-packet-encoder β srv-pkeyid
                       +server-connect-id+ client-id my-pkeyid)
               (let ((responder
                      (create
                       (alambda
                        ((bkey server-id) /  (and (typep bkey      'ub8-vector)
                                                  (typep server-id 'uuid:uuid))
                         (let ((ekey  (hash/256 bkey akey))
                               (chan  (create
                                       (lambda (&rest msg)
                                         (send* local-services :ssend server-id msg))
                                       )))
                           (β _
                               (send local-services β :set-crypto ekey socket)
                             (send connections cust :set-channel socket chan))
                           ))
                        
                        ( _
                          (error "Server not following connection protocol"))
                        ))))
                 (β _
                     (send local-services β :add-single-use-service client-id responder)
                   (send socket latcrypt aescrypt))
                )))
           )))))
    
  (deflex client-gateway
    ;; This is the main local client service used to initiate
    ;; connections with foreign servers.
    ;; Go lookup the encrypted channel for this IP, constructing it on
    ;; demand if not already present.
    (create
     (lambda (cust host-ip-addr)
       (send client-connector cust negotiator host-ip-addr) )
     )) )

;; ---------------------------------------------------
;; User side of Client Interface

(defun parse-remote-actor (id)
  ;; parse id in form "eval@arroyo.local:65001"
  (let ((apos (position #\@ id)))
    (if apos
        (values (um:kwsymbol (subseq id 0 apos))
                (subseq id (1+ apos)))
      (error "No service specified: ~S" id)
      )))

(defun remote-service-by-id (id)
  ;; id string: "svc@ipaddr:port"
  (multiple-value-bind (name host-ip-addr)
      (parse-remote-actor id)
    (remote-service name host-ip-addr)))

(defun remote-service (name host-ip-addr)
  ;; An Actor and send target. Connection to remote service
  ;; established on demand.
  (create
   (lambda (cust &rest msg)
     (β (chan)
         (send client-gateway β host-ip-addr)
       (send* chan cust name msg))
     )))

;; ------------------------------------------------------------
#|
(defun tst (host)
  (let ((recho (remote-service :echo host))
        (msg   :hello))
    (β (ans)
        (send recho β msg)
      (send fmt-println "(send recho println ~S) sez: ~S" msg ans))))
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
        (send recho β txt)
      (send fmt-println "echo comparison: ~A" (string= txt ans)))))

(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (ans)
        (send reval β '(list (get-universal-time) (machine-instance)))
      #|
        (send reval β '(um:capture-ans-or-exn
                            (error "test-error")))
        |#
      (trace-me)
      (send fmt-println "reval sez: ~S" (um:recover-ans-or-exn ans)))
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
                          (send recho β "")
                        (let ((stop (usec:get-time-usec)))
                          (send println (- stop start))
                          (send me (1- n))))
                      )))))
    (send ac1 n)))

(tst "localhost" 10)
(tst "zircon.local" 10)
(tst "rincon.local" 10)
(tst "david-pc.local" 10)

(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (ans)
        (send (timed-service reval 10) β `(um:capture-ans-or-exn kvdb:kvdb))
      (let ((rkvdb (um:recover-ans-or-exn ans)))
        (β (proxy)
            (send (timed-service rkvdb 10) β :req-proxy)
          (β (uuid)
              (send (timed-service proxy 10) β :find 'kvdb::version)
            (send println (uuid:when-created uuid))
            ))))))

(tst "localhost")
(tst "arroyo.local")
(tst "rincon.local")
(tst "zircon.local")
(tst "fornax.local")

(defun tst (host)
  (let ((reval (remote-service :eval host)))
    (β (proxy)
        (send (timed-service reval 10) nil `(send kvdb:kvdb ,β :req-proxy))
      (β (uuid)
          (send (timed-service proxy 10) β :find 'kvdb::version)
        (send println (uuid:when-created uuid))
        ))))
|#
