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

#-:lattice-crypto
(defun server-crypto-gateway (socket local-services)
  ;; Foreign clients first make contact with us here. They send us
  ;; their client-id for this exchange, a random ECC point, and their
  ;; public key (ECC point).
  ;;
  ;; We develop a unique ECDH encryption key shared secretly between
  ;; us and furnish a private handler ID for encrypted requests along
  ;; with our own random ECC point and our public key.
  (create
   (alambda
    ((apt client-id client-pkeyid) / (and (typep apt       'edec:ecc-pt)
                                          (typep client-id 'uuid:uuid))
     ;; silently ignore other kinds of requests
     (β (client-pkey)
         (send eccke:ecc-pkey β client-pkeyid)
       (when client-pkey
         (ignore-errors
           (multiple-value-bind (apt client-pkey)
               (values (ed-validate-point apt)
                       (ed-validate-point client-pkey))
             (β (cnx-id)
                 (send local-services β :add-service global-services)
               (β (brand bpt aescrypt)
                   (send eccke:ecc-cnx-encrypt β client-pkey
                         client-id cnx-id)
                 (β (my-skey)
                     (send eccke:ecc-skey β)
                   (let ((ekey (hash/256 (ed-mul apt brand)            ;; A*b
                                         (ed-mul client-pkey brand)    ;; C*b
                                         (ed-mul apt my-skey))         ;; A*s
                               ))
                     (β _
                         (send local-services β :set-crypto ekey socket)
                       (send socket bpt aescrypt)))
                   ))))))
       )))
   ))

;; ----------------------------------------------------------------

#+:lattice-crypto
(defun server-crypto-gateway (socket local-services)
  ;; Foreign clients first make contact with us here. They send us
  ;; their client-id for this exchange, a random ECC point, and their
  ;; public key (ECC point).
  ;;
  ;; We develop a unique ECDH encryption key shared secretly between
  ;; us and furnish a private handler ID for encrypted requests along
  ;; with our own random ECC point and our public key.
  (create
   (alambda
    ((akey client-id client-pkeyid) / (and (typep akey 'ub8-vector)
                                           (typep client-id 'uuid:uuid))
     (β (cnx-id)
         (send local-services β :add-service global-services)
       (β (bkey latcrypt aescrypt)
           (send lattice-ke:cnx-packet-encoder β client-pkeyid
                client-id cnx-id)
         (let ((ekey  (hash/256 bkey akey)))
           (β _
               (send local-services β :set-crypto ekey socket)
             (send socket latcrypt aescrypt))
           ))))
    ;; silently ignore other kinds of requests
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

