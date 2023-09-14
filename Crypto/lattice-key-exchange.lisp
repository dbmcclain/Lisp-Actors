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

   #:cnx-to-server-packet-maker
   #:cnx-to-client-packet-maker
   #:client-connection-packet-decoder
   #:server-connection-packet-decoder
   ))

(in-package #:com.ral.crypto.lattice-key-exchange)

;; ----------------------------------

#|
(deflex lattice-skey
  (create
   (lambda (cust)
     (send kvdb:kvdb cust :find :lat2-syzygy))
   ))
|#
(deflex lattice-skey
  (create
   (lambda (cust)
     (send kvdb:kvdb cust :find :my-syzygy))
   ))

#|
(defun node-to-kw (node-name &key (prefix "") (suffix ""))
  (intern (string-upcase (concatenate 'string prefix node-name suffix))
          (find-package :keyword)))

(deflex lattice-pkey
  (create
   (lambda (cust node)
     (β (pkey)
         (send kvdb:kvdb β :find (node-to-kw node :prefix "lat2-pkey-"))
       (unless pkey
         (error "No PKEY for node: ~A" node))
       (send cust pkey)))
   ))
|#
(deflex lattice-pkey
  (create
   (lambda (cust pkey-id)
     (β (pkey)
         (send kvdb:kvdb β :find pkey-id)
       (unless pkey
         (error "No PKEY for id: ~A" pkey-id))
       (send cust pkey)))
   ))

(deflex lat2-encoder
  (create
   (lambda (cust pkey v)
     (β (sys)
         (send lattice-system β)
       (send cust (lat2-encode pkey v sys))
       ))
   ))

(deflex lat2-decoder
  (create
   (lambda (cust cs)
     (β (sys)
         (send lattice-system β)
       (β (skey)
           (send lattice-skey β)
         (send cust (lat2-decode skey cs sys))
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
      (unless (equalp chkx chk)
        (error "Invalid packet")))
    (let* ((vdata  (aes-enc/dec key iv cdata)))
      (values-list (coerce
                    (loenc:decode vdata)
                    'list))
      )))

;; -----------------------------------------------------
;; Secure Key Exchange
;;
;;    client                                server
;;    --------------------------            ---------------------------
;;    Random Kc
;;    Ps = Lookup(SrvID)
;;    LatEnc(Ps, Kc), AES(Kc,CliID)  -->    Check CliID for membership
;;                                          Pc = Lookup(CliID)
;;                                          Random Ks
;;                                          Kses = H(Ks, Kc)
;;                                   <--    LatEnc(Pc, Ks)
;;    Kses = H(Ks, Kc)
;;
;;    AES(Kses, Data)                -->
;;                                   <--    AES(Kses, Reply)
;;    ...
;;

(deflex client-connection-packet-decoder
  (create
   (lambda (cust latcrypt)
     ;; At the client, we decode the random server key.
     (send lat2-decoder cust latcrypt))
   ))

(deflex server-connection-packet-decoder
  (create
   (lambda (cust packet)
     ;; At the server, we decode the random client key and their Lattice
     ;; ID.
     (destructuring-bind (latcrypt aescrypt) packet
       (β (rkey)
           (send client-connection-packet-decoder β latcrypt)
         (let ((id (decode-aes-packet rkey aescrypt)))
           (send cust rkey id)
           ))))
   ))

;; ----------------------------------------------------
;; For Actors-based code, using parallel Lattice encryption

(deflex my-pkeyid
  (create (lambda (cust)
            (send kvdb:kvdb cust :find :my-pkeyid))
          ))

(deflex srv-pkey
  (create (lambda (cust)
            (send kvdb:kvdb cust :find :PKEY-09508427274D7A861BD983E3992A09AB5B176790CB482F1120E2AD8A427E97E7))
          ))

(deflex cnx-to-server-packet-maker
  (create
   (lambda (cust)
     (β (pkey)
         (send srv-pkey β)
       (β (my-id)
           (send my-pkeyid β)
         (let ((rkey  (random-key)))
           (β (lat-enc)
               (send lat2-encoder β pkey rkey)
             (send cust rkey (list lat-enc (make-aes-packet rkey my-id))))
           ))))
   ))

#|
(deflex cnx-to-server-packet-maker
  (create
   (lambda (cust srv-node)
     (β (pkey)
         (send lattice-pkey β srv-node)
       (let ((rkey  (random-key))
             (my-id (machine-instance)))
         (β (lat-enc)
             (send lat2-encoder β pkey rkey)
           (send cust rkey (list lat-enc (make-aes-packet rkey my-id))))
         )))
   ))
|#

(deflex cnx-to-client-packet-maker
  (create
   (lambda (cust client-id)
     (β (pkey)
         (send lattice-pkey β client-id)
       (let ((rkey (random-key)))
         (β (lat-enc)
             (send lat2-encoder β pkey rkey)
           (send cust rkey lat-enc))
         )))
   ))

#|
(with-open-file (f "~/.my-pkeyid"
                   :direction :input)
  (read f))
                   
(multiple-value-bind (skey pkey)
    (lattice::lat2-gen-keys (lattice::get-lattice-system))
  (let ((pkey-id (um:kwsymb "Pkey-" (hex-str (hash/256 pkey)))))
    (with-standard-io-syntax
      (with-open-file (f "~/.my-pkeyid"
                         :direction :output
                         :if-exists :supersede)
        (write pkey-id :stream f))
      (with-open-file (f "~/.my-pkey"
                         :direction :output
                         :if-exists :supersede)
        (write pkey :stream f))
      (with-open-file (f "~/.my-syzygy"
                         :direction :output
                         :if-exists :supersede)
        (write skey :stream f))
      )))

(let ((pkey-id  (with-open-file (f "~/.my-pkeyid")
                  (read f))))
  (send kvdb:kvdb println :add :my-pkeyid pkey-id)
  (let ((pkey     (with-open-file (f "~/.my-pkey")
                    (read f))))
    (send kvdb:kvdb println :add pkey-id pkey))
  (let ((skey     (with-open-file (f "~/.my-syzygy")
                    (read f))))
    (send kvdb:kvdb println :add :my-syzygy skey)))
 
(defun lat2-gen-all (&optional (sys (lattice::get-lattice-system)))
  (with-open-file (f "~/.syzygy"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (loop for node in '("fornax.local"
                        "arroyo.local"
                        "zircon.local"
                        "rincon.local"
                        "umbra.local"
                        "david-pc.local")
        collect
          (let ((sym  (node-to-kw node :prefix "lat2-pkey-")))
            (multiple-value-bind (skey pkey)
                (lattice::lat2-gen-keys sys)
              (with-standard-io-syntax
                (print (list node skey pkey) f))
              (list sym skey pkey)))
          )))

(setf *all-keys* (lat2-gen-all))
(setf *my-lat2-skey* (second (fifth *all-keys*)))

(dolist (pars *all-keys*)
  (send kvdb:kvdb println :add (car pars) (third pars)))

(send kvdb:kvdb println :add :lat2-syzygy *my-lat2-skey*)

(let ((sys (ask kvdb:kvdb :find :lat2-system)))
  (with-open-file (f "~/.lat2-system"
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :direction :output)
    (with-standard-io-syntax
      (print sys f))))

(with-open-file (f "~/.lat2-system"
                   :direction :input)
  (let ((sys (read f)))
    (send kvdb:kvdb println :add :lat2-system sys)))

(defun inhale ()
  (with-open-file (f "~/.syzygy"
                     :direction :input)
    (let* ((machid  (machine-instance))
           (machlen (length machid)))
      (um:nlet iter ()
        (let ((rec (read f t f)))
          (unless (eql rec f)
            (destructuring-bind (name skey pkey) rec
              (send kvdb:kvdb println :add (node-to-kw name :prefix "lat2-pkey-") pkey)
              (let ((len (length name)))
                (when (and (>= len machlen)
                           (string-equal (subseq name 0 machlen) machid))
                  (send kvdb:kvdb println :add :lat2-syzygy skey))
                (go-iter)))
            ))))))
(inhale)
|#
