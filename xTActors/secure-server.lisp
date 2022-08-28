;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :com.ral.actors.secure-comm)

;; ------------------------------------------------------------------
;; Server side

#| ;; for debugging
(defun show-server-outbound (socket)
  (actor (&rest msg)
    (send println (format nil "s/out: ~S" msg))
    (send* socket msg)))

(defun show-server-inbound ()
  (actor (cust &rest msg)
    (send println (format nil "s/in: ~S" msg))
    (send* cust msg)))
|#

(defun server-crypto-gateway (socket local-services)
  ;; Foreign clients first make contact with us here. They send us
  ;; their client-id for this exchange, a random ECC point, and a
  ;; valid signature on these items using our shared keying.
  ;;
  ;; We develop a unique ECDH encryption key shared secretly between
  ;; us and furnish a private handler for encrypted requests along
  ;; with our own random ECC point.
  (create
   (alambda ;; silently ignore other attempts
    ((client-id apt client-pkey sig)
     (when (and (typep client-id 'uuid:uuid)
                (integerp apt)
                (integerp client-pkey)
                (consp sig))
       (let ((pld (vector apt client-pkey)))
         (when (check-signature client-id pld sig (ed-decompress-pt client-pkey))
           (let* ((brand     (int (ctr-drbg 256)))
                  (bpt       (ed-nth-pt brand))
                  (pkey      (server-pkey))
                  (ekey      (hash/256 (ed-mul (ed-decompress-pt apt) brand)           ;; A*b
                                       (ed-mul (ed-decompress-pt client-pkey) brand)   ;; C*b
                                       (ed-mul (ed-decompress-pt apt) (server-skey)))) ;; A*s
                  ;; (socket    (show-server-outbound socket))  ;; ***
                  (encryptor (secure-sender ekey))
                  (chan      (server-channel
                              :socket      socket
                              :encryptor   encryptor))
                  (decryptor (sink-pipe
                              (secure-reader ekey)
                              ;; (show-server-inbound) ;; ***
                              chan)))
             (β (cnx-id)
                 (create-service-proxy β local-services decryptor)
               (let* ((ibpt  (int bpt))
                      (ipkey (int pkey))
                      (pld   (vector ibpt ipkey))
                      (sig   (make-signature cnx-id pld (server-skey)))) 
                 (send (remote-actor-proxy client-id socket)  ;; remote client cust
                       cnx-id ibpt ipkey sig)))
             ))
         )))
    )))

(defun server-channel (&key
                       socket
                       encryptor)
  ;; This is a private portal for exchanges with a foreign client.
  ;; One of these exist for each connection established through the
  ;; main crypto gate.
  ;;
  ;; We authenticate requests as coming from the client public key,
  ;; decrypt the requests, and pass along to a local service. For each
  ;; request we make an encrypting forwarder back to the client
  ;; customer, and pass that along as the local customer for the
  ;; request to the local service.
  (create
   (alambda
    ;; A significant difference between LAMBDA and ALAMBDA - if an
    ;; incoming arg list does not match what LAMBDA expects, it
    ;; produces an error. ALAMBDA uses pattern matching, and anything
    ;; arriving that does not match is simply ignored.
    ((cust-id verb . msg) ;; remote client cust
     ;; (send println (format nil "server rec'd req: ~S" self-msg))
     (let ((proxy (when cust-id
                    (sink-pipe encryptor (remote-actor-proxy cust-id socket)))))
       (send* global-services proxy :send verb msg)))
    )))

;; ---------------------------------------------------------------
;; For generating key-pairs...
#|
(multiple-value-bind (skey pkey)
    (make-deterministic-keys (uuid:make-v1-uuid)) ;; +server-id+)
  (with-standard-io-syntax
    (format t "~%skey: #x~x" skey)
    (format t "~%pkey: #x~x" (int pkey))))
|#

;; ------------------------------------------------------------
#|
(let* ((msg :diddly)
       (seq 1)
       (ekey (ctr-drbg 256)))
  (multiple-value-bind (skey pkey) (ed-random-pair)
    (let* ((emsg (encrypt ekey seq msg))
           (sig  (make-signature emsg skey)))
       (values emsg
               sig
               (check-signature emsg sig pkey)
               (decrypt ekey seq emsg))
       )))

(defun tst-beh (&rest args &key a b c)
  ;; show the need to trim away prior garbage
  (alambda
   ((:show)
    (send writeln args)
    (when (eql a 1)
      (become (apply #'tst-beh
                     :a 2
                     args))
      (send self :show)))
   ))

(send (create (tst-beh :a 1 :b 2 :c 3)) :show)

 |#

