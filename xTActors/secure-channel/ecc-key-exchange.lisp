;; ecc-key-exchange.lisp
;;
;; DM/RAL  2023/09/15 08:56:51
;; ----------------------------------

(defpackage #:com.ral.crypto.ecc-key-exchange
  (:use #:common-lisp #:vec-repr #:hash #:edec #:ac)
  (:export
   #:ecc-cnx-encrypt
   #:ecc-cnx-decrypt
   #:ecc-skey
   #:ecc-pkey
   #:my-pkeyid
   #:srv-pkey
   ))

(in-package #:com.ral.crypto.ecc-key-exchange)

;; ----------------------------------

(deflex ecc-skey
  (create
   (lambda (cust)
     (flet (
            #+:MACOSX
            (get-seed ()
              (let ((txt (with-output-to-string (s)
                           ;; Using Mac KeyChain to hold secret keying
                           (sys:call-system-showing-output
                            "security find-generic-password -a $USER -s 'LispActorsSystem' -w"
                            :output-stream s))))
                (subseq (cadr (um:split-string txt :delims '(#\newline))) 2)))
            #+:WINDOWS
            (get-seed ()
              (with-open-file (f "~/.my-syzygy")
                (read f))) )
       (let ((skey (make-deterministic-keys :skey (get-seed))))
         (send cust skey)
         (become (const-beh skey)))
       ))
   ))
    
(deflex ecc-pkey
  (create
   (lambda (cust pkey-id)
     (send kvdb:kvdb cust :find pkey-id))
   ))

;; ------------------------------------------------------
;; AES-256/CTR Encryption/Decryption

(defun random-key ()
  (vec (hash/256 :random-key (uuid:make-v1-uuid)
                 (prng:ctr-drbg 256))))

(defun aes-enc/dec (key iv vsrc)
  (let ((cipher (ironclad:make-cipher :aes
                                      :mode :ctr
                                      :key  key
                                      :initialization-vector iv)))
    (ironclad:encrypt-in-place cipher vsrc)
    vsrc))

(defun make-auth-chk (key iv cdata)
  (vec (hash/256 :chk key iv cdata)))

(defun make-iv (key)
  (subseq
   (vec (hash/256 :iv key (uuid:make-v1-uuid)))
   0 16))

(defun make-aes-packet (key &rest data)
  (let* ((vdata  (loenc:encode (coerce data 'vector)))
         (iv     (make-iv key))
         (cdata  (aes-enc/dec key iv vdata))
         (chk    (make-auth-chk key iv cdata)))
    (list iv cdata chk)
    ))

(defun* decode-aes-packet (key (iv cdata chk))
  (let ((chkx (make-auth-chk key iv cdata)))
    (when (equalp chkx chk) ;; just drop on the floor if not valid
      (let ((vdata  (aes-enc/dec key iv cdata)))
        (coerce (loenc:decode vdata) 'list))
      )))

;; ----------------------------------------------------

(deflex ecc-cnx-encrypt
  (create
   (lambda (cust pubkey &rest info)
     (let+ ((:mvb (rand rand-pt) (ed-random-pair)))
       (ignore-errors
         (let* ((enc-pt     (ed-mul pubkey rand))
                (aes-key    (vec (hash/256 :ecc-cnx-key enc-pt)))
                (aes-packet (apply #'make-aes-packet aes-key :canary info)))
           (send cust rand (ed-affine rand-pt) aes-packet)
           ))))
   ))

(deflex ecc-cnx-decrypt
  (create
   (lambda (cust rand-pt aes-packet)
     (let-β ((skey ecc-skey))
       (ignore-errors
         (let* ((enc-pt   (ed-mul rand-pt skey))
                (aes-key  (vec (hash/256 :ecc-cnx-key enc-pt)))
                (info     (decode-aes-packet aes-key aes-packet)))
           (when (and (consp info)
                      (eq (car info) :canary))
             (send cust (cdr info)) )))
       ))))

;; ----------------------------------------------------
;; For Actors-based code, using ECC encryption

(deflex my-pkeyid
  (create (lambda (cust)
            (send kvdb:kvdb cust :find :my-ecc-pkeyid))
          ))

(deflex srv-pkey
  (create (lambda (cust)
            (send kvdb:kvdb cust :find "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}"))
          ))

#|
(β (skey)
    (send ecc-skey β)
  (let ((pkey  (ed-nth-pt skey)))
    (send kvdb:kvdb println :add :my-ecc-pkeyid "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}")
    (send kvdb:kvdb println :add "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}" pkey)))

(β (pkey-id)
    (send my-pkeyid β)
  (send ecc-pkey println pkey-id))

 |#



