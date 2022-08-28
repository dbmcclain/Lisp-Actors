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
  (α (cust verb &rest msg)
    ;; (send println (format nil "trying to send: ~S" self-msg))
    (if (is-pure-sink? cust)
        (send* encryptor nil verb msg)
      (β (cust-id)
          (create-ephemeral-client-proxy β local-services (sink-pipe decryptor cust))
        (send* encryptor cust-id verb msg))
      )))

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
;;    => EKey = H(a*BPt | Client-SKey*BPt | a*Server-PKey)    ;; at server side
;;            = H(b*APt | b*Client-PKey   | Server-SKey*APt)  ;; at client side
;;
;; No signatures employed. All it takes is knowledge of public keys
;; and random points.  Anyone can do, even if totally faked. But only
;; the two sides participating will understand the resulting shared
;; secret EKey.
;;
;;     ...for all subsequent messages...
;;          Seq E(msg) Auth --> CnxID                 
;;                Ephem-ID' <-- Seq' E(response) Auth'  ;; if we generate a response
;;
;; Connection ID's are always sent in the clear (not encrypted, but
;; encoded for serialization) so that receivers can dispatch. But
;; these are randomly generated, ephemeral, UUID's.
;;
;; Connections are transparently established for users, and then are
;; shut down after some period of inactivity (currently 20s). All the
;; user needs to know is the IP Address of the server and the name of
;; the service. Any computer running an Actors system can behave as
;; both client and server. The distinction is merely that clients
;; request, and servers might respond.
;;
;;   G        = Generator Pt for Curve1174
;;   H        = SHA3/256
;;   Seq[n+1] = Seq[n]+2^256, Seq[0] = Int( H(UUID/v1) ) < 2^256
;;   Auth     = H( H(:AUTH | EKey | Seq) | Seq | E(msg))
;;   E(msg)   = SHAKE256(:ENC | EKey | Seq) XOR msg, effetively a one-time-pad
;;
;; Decryption is the same as Encryption.  All Seq are sequential
;; nonces and label each transmission. Generated independently on both
;; sides. Allows for avoiding replay attacks.
;;
;; Prior to encryption and wire transmission, the arbitrary Lisp
;; objects of a message are serialized, compressed, and possibly
;; chunked into fragments smaller than some maximum limit.
;;
;; Each fragment is then separately transmitted through the encryption
;; scheme and onto the wire using a self-sync encoding.
;;
;; Received fragments are decoded on the other side, then decrypted
;; and reassembled (unchunked), then decompressed, and then
;; deserialized back to Lisp objects on the other side.
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
            (create
             (alambda
              ((server-id bpt server-pkey) / (and (typep server-id 'uuid:uuid)
                                                  (integerp bpt)
                                                  (integerp server-pkey))
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
                (error "Server can't be authenticated"))
              ))))
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
