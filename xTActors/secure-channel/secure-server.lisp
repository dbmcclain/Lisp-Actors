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
  ;; their client-id for this exchange, a random ECC point, and their
  ;; public key (ECC point).
  ;;
  ;; We develop a unique ECDH encryption key shared secretly between
  ;; us and furnish a private handler ID for encrypted requests along
  ;; with our own random ECC point and our public key.
  (αα
   ((client-id apt client-pkey) / (and (typep client-id 'uuid:uuid)
                                       (integerp apt)
                                       (integerp client-pkey)
                                       (sets:mem *allowed-members* client-pkey))
    ;; silently ignore other kinds of requests
    (ignore-errors
      (multiple-value-bind (apt client-pkey)
          (values (ed-validate-point apt)
                  (ed-validate-point client-pkey))
        (multiple-value-bind (brand bpt)
            (ed-random-pair)
          (let ((ekey (hash/256 (ed-mul apt brand)            ;; A*b
                                (ed-mul client-pkey brand)    ;; C*b
                                (ed-mul apt (actors-skey))))) ;; A*s
            (β _
                (send local-services β :set-crypto ekey socket)
              (β (cnx-id)
                  (create-service-proxy β local-services global-services)
                (send socket client-id cnx-id (int bpt) (int (actors-pkey)) ))
              )))
        )))
   ))

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

