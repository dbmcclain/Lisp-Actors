;; lattice-key-exchange.lisp
;;
;; DM/RAL  2023/03/24 03:26:29
;; ----------------------------------

(defpackage #:com.ral.crypto.lattice-key-exchange
  (:use #:common-lisp #:lattice #:vec-repr #:hash #:ac)
  (:export
   #:lattice-skey
   #:lattice-pkey
   #:random-key
   #:make-aes-packet
   #:decode-aes-packet
   #:make-connection-to-server-packet
   #:make-connection-to-client-packet
   #:decode-server-connection-packet
   #:decode-client-connection-packet

   #:cnx-packet-encoder
   #:cnx-packet-decoder

   #:my-pkeyid
   #:srv-pkeyid
   ))

(in-package #:com.ral.crypto.lattice-key-exchange)

;; ----------------------------------

(deflex lattice-skey
  (create
   (alambda
    ((cust)
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
       (let ((me  self))
         (β (sys)
             (send lattice-system β)
           (let* ((seed (get-seed))
                  (skey (lat2-gen-deterministic-skey sys seed)))
             (send me cust :update skey) ))
         )))
    
    ((cust :update skey)
     (become (const-beh skey))
     (send cust skey))
    )))

(deflex lattice-pkey
  (create
   (lambda (cust pkey-id)
     (β (pkey)
         (send kvdb:kvdb β :find pkey-id)
       (when pkey ;; if not found, just drop things on the floor...
         (send cust pkey)))
     )))

(deflex lat2-encoder
  (create
   (lambda (cust pkey v)
     (β (sys)
         (send lattice-system β)
       (let ((enc (ignore-errors
                    (lat2-encode pkey v sys))))
         (when enc
           (send cust enc)))
       ))
   ))

(deflex lat2-decoder
  (create
   (lambda (cust cs)
     (β (sys)
         (send lattice-system β)
       (β (skey)
           (send lattice-skey β)
         (let ((dec (ignore-errors
                      (lat2-decode skey cs sys))))
           (when dec
             (send cust dec)))
         )))
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

(defun decode-aes-packet (key packet)
  (destructuring-bind (iv cdata chk) packet
    (let ((chkx (make-auth-chk key iv cdata)))
      (when (equalp chkx chk) ;; just drop on the floor if not valid
        (let ((vdata  (aes-enc/dec key iv cdata)))
          (coerce (loenc:decode vdata) 'list))
        ))
    ))

#|
   -----------------------------------------------------
   Secure Key Exchange
  
      client                                server
      --------------------------            ---------------------------
      Random Kc
      Ps = Lookup(SrvID)
      LatEnc(Ps, Kc), AES(Kc,CliID)  -->    Check CliID for membership
                                            Pc = Lookup(CliID)
                                            Random Ks
                                            Kses = H(Ks, Kc)
                                     <--    LatEnc(Pc, Ks)
      Kses = H(Ks, Kc)
  
      AES(Kses, Data)                -->
                                     <--    AES(Kses, Reply)
      ...
  
|#

(deflex cnx-packet-encoder
  (create
   (lambda (cust dest-pkeyid &rest info)
     (β (pkey) ;; if not found just drop things on the floor
         (send lattice-pkey β dest-pkeyid)
       (let* ((rkey       (random-key))
              (aes-packet (ignore-errors
                            (apply #'make-aes-packet rkey :canary info))))
         (when aes-packet
           (β (lat-enc)
               (send lat2-encoder β pkey rkey)
             (send cust rkey lat-enc aes-packet)
             )))))
   ))

(deflex cnx-packet-decoder
  (create
   (lambda (cust latcrypt aescrypt)
     ;; During handshake we exchange pairs of packets. The first is a
     ;; Lattice encrypted random key, the second is an AES encrypted
     ;; packet using that key.
     ;;
     ;; Return both the random key and the decrypted info.
     ;; Client/Server differ only in the contents of that info.
     (β (rkey)
         (send lat2-decoder β latcrypt)
       (let ((info (ignore-errors
                     (decode-aes-packet rkey aescrypt))))
         (when (and (consp info)
                    (eq (car info) :canary))
           (send cust rkey (cdr info))))
       ))
   ))

;; ----------------------------------------------------
;; For Actors-based code, using parallel Lattice encryption

(deflex my-pkeyid
  (create (lambda (cust)
            (send kvdb:kvdb cust :find :my-pkeyid))
          ))

(deflex srv-pkeyid
  (const "{d73be812-5309-11ee-9c10-f643f5d48a65}"))

