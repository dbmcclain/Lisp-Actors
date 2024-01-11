;; secure-connection.lisp -- communication between client and server
;; via secure channel
;;

(in-package :com.ral.actors.secure-comm)

;; ------------------------------------------------------------------
;; Server side

#| ;; for debugging
(defun show-server-outbound (socket)
  (actor 
      (lambda (&rest msg)
        (>> println (format nil "s/out: ~S" msg))
        (>>* socket msg))))

(defun show-server-inbound ()
  (actor 
      (lambda (cust &rest msg)
        (>> println (format nil "s/in: ~S" msg))
        (>>* cust msg))))
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
    ((apt client-id client-pkeyid) / (and (typep apt       'ecc-pt)
                                          (typep client-id 'uuid:uuid))
     ;; silently ignore other kinds of requests
     (let+ ((:β (client-pkey)  (racurry eccke:ecc-pkey client-pkeyid)) )
       (when client-pkey
         (ignore-errors
           (ed-validate-point client-pkey)
           (let+ ((:β (cnx-id)             (racurry local-services :add-service global-services))
                  (:β (brand bpt aescrypt) (racurry eccke:ecc-cnx-encrypt
                                                    client-pkey client-id cnx-id))
                  (:β (my-skey)            eccke:ecc-skey)
                  (ekey (hash/256 (ed-mul apt brand)            ;; A*b
                                  (ed-mul client-pkey brand)    ;; C*b
                                  (ed-mul apt my-skey)) )       ;; A*s
                  (:β _  (>> local-services β :set-crypto ekey socket)))
             (>> socket bpt aescrypt))
           ))))
    ;; silently ignore other requests
    )))

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
     (let+ ((:β (cnx-id)  (racurry local-services :add-service global-services))
            (:β (bkey latcrypt aescrypt) (racurry lattice-ke:cnx-packet-encoder
                                                  client-pkeyid client-id cnx-id))
            (ekey  (hash/256 bkey akey))
            (:β _  (>> local-services β :set-crypto ekey socket)))
       (>> socket latcrypt aescrypt)
       ))
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
    (>> writeln args)
    (when (eql a 1)
      (β! (apply #'tst-beh
                     :a 2
                     args))
      (>> self :show)))
   ))

(>> (create (tst-beh :a 1 :b 2 :c 3)) :show)

 |#

