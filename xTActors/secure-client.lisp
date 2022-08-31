;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :com.ral.actors.secure-comm)

;; --------------------------------------------------------------------
;; Client side

(defun client-channel (&key
                       local-services
                       encryptor
                       decryptor)
  ;; One of these serves as a client-local private portal with an
  ;; established connection to a server. It encrypts and signs a
  ;; request message for a server service. It also instantiates a
  ;; private decryptor for any returning replies being sent back to
  ;; the customer on this client.
  ;;
  ;; Every request to the server sent from here carries an
  ;; incrementing sequence number to ensure a change of encryption
  ;; keying for every new message.
  (actor msglst
    (β (xmsglst)
        (send (client-translation local-services decryptor msglst) β)
      (send* encryptor xmsglst))))

(defstruct (client-proxy
            (:constructor make-client-proxy (id)))
  id)

(defun client-translation-beh (local-services decryptor msglst)
  ;; translate top level Actors into client proxy objects
  (lambda (cust &rest xlst)
    (if msglst
        (let ((obj (car msglst))
              (me  self))
          (become (client-translation-beh local-services decryptor (cdr msglst)))
          (if (actor-p obj)
              (if (is-pure-sink? obj) ;; short circuit any sinks
                  (send* me cust nil xlst)
                ;; else
                (β (id)
                    (create-ephemeral-client-proxy β local-services (sink-pipe decryptor obj))
                  (send* me cust (make-client-proxy id) xlst)))
            ;; else
            (send* me cust obj xlst)))
      ;; else
      (send cust (reverse xlst)))
    ))

(defun client-translation (local-services decryptor msglst)
  (create (client-translation-beh local-services decryptor msglst)))
          
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
;; Trevor Perrin and Moxie Marlinspike of Signal.
;;

(defactor negotiate-secure-channel
  ;; EC Diffie-Hellman key exchange
  (λ (cust socket local-services)
    (let* ((arand  (int (ctr-drbg 256)))
           (apt    (ed-nth-pt arand))
           ;; (socket      (show-client-outbound socket)) ;;
           (responder
            (αα
             ((server-id bpt server-pkey) / (and (typep server-id 'uuid:uuid)
                                                 (integerp bpt)
                                                 (integerp server-pkey)
                                                 (sets:mem *allowed-members* server-pkey))
              (let* ((ekey  (hash/256 (ed-mul (ed-decompress-pt bpt) arand)           ;; B*a
                                      (ed-mul (ed-decompress-pt bpt) (actors-skey))   ;; B*c
                                      (ed-mul (ed-decompress-pt server-pkey) arand))) ;; S*a
                     (chan  (client-channel
                             :local-services  local-services
                             :encryptor       (sink-pipe
                                               (secure-sender ekey)
                                               (remote-actor-proxy server-id socket))
                             :decryptor       (secure-reader ekey)
                             )))
                (send connections cust :set-channel socket chan)
                ))
             (_
              (error "Server not following connection protocol"))
             )))
      (β (client-id)
          (create-ephemeral-client-proxy β local-services responder)
        (send (remote-actor-proxy +server-connect-id+ socket)
              client-id (int apt) (int (actors-pkey))))
      )))

(defactor client-gateway
  ;; This is the main local client service used to initiate
  ;; connections with foreign servers.
  ;; Go lookup the encrypted channel for this IP, constructing it on
  ;; demand if not already present.
  (λ (cust host-ip-addr)
    (send client-connector cust negotiate-secure-channel host-ip-addr)))

;; ---------------------------------------------------
;; User side of Client Interface

(defun remote-service (name host-ip-addr)
  ;; An Actor and send target. Connection to remote service
  ;; established on demand.
  (α (cust &rest msg)
    (β (chan)
        (send client-gateway β host-ip-addr)
      (send* chan cust name msg))))

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
        (txt   (hcl:file-string "./xTActors/encoding.lisp")))
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
|#
