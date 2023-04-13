;; lattice-key-exchange.lisp
;;
;; DM/RAL  2023/03/24 03:26:29
;; ----------------------------------

(defpackage #:com.ral.crypto.lattice-key-exchange
  (:use #:common-lisp #:lattice #:vec-repr #:hash #:ac)
  (:export
   #:lattice-skey
   #:lattice-pkey
   #|
   #:lattice-local-id
   #:lattice-id-for-node
   #:lattice-pkey-for-id
   #:lattice-pkey-for-node
   |#
   #:random-key
   #:make-aes-packet
   #:decode-aes-packet
   #:make-connection-to-server-packet
   #:make-connection-to-client-packet
   #:decode-server-connection-packet
   #:decode-client-connection-packet

   #:cnx-to-server-packet-maker
   #:cnx-to-client-packet-maker
   ))

(in-package #:com.ral.crypto.lattice-key-exchange)

;; ----------------------------------
#|
(defvar *dict*
  (loop for node in '("fornax.local"
                      "arroyo.local"
                      "zircon.local"
                      "rincon.local"
                      "umbra.local"
                      "david-pc.local")
        collect
        (let* ((skey (lat-gen-skey))
               (pkey (lat-gen-pkey skey)))
          (list node skey pkey))
          ))

(defvar *nodes*
  (loop for entry in *dict* collect
        (destructuring-bind (name (ms skey) (id mp pkey)) entry
          (assert (eql ms mp)) ;; sanity check
          (assert (string= (str (hex (hash/256 (list ms skey)))) id))
          (list name id))))

(with-standard-io-syntax
  (dolist (node *nodes*)
    (print node))
  (values))

(defvar *pkeys*
  (loop for entry in *dict* collect
        (destructuring-bind (name skey pkey) entry
          pkey)))

(with-open-file (f "~/.lattice-pkeys"
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (with-standard-io-syntax
    (print *pkeys* f))
  (values))

(defvar *skeys*
  (loop for entry in *dict* collect
        (destructuring-bind (name skey (id mp pkey)) entry
          (list name id skey))))
          
(with-standard-io-syntax
  (print *skeys*)
  (values))
|#

#|
(defvar *nodes-dict*
  (let* ((dict '(("fornax.local"   "F64C29AC38A5D42D66FEB97CE72FE75178A62216DBF018D4AA8BCCF6723DAE8C") 
                 ("arroyo.local"   "799EAE208DC03EE713D9B93259A36BF7306A1ADF501DE4C0FDB5A5AF6D002C44") 
                 ("zircon.local"   "A7AFB5BB0F0BAB70108142382BF47282AD304E2239A88D992CD2BC2A2E8207AC") 
                 ("rincon.local"   "522CDF4F3EFF13C960B120EAD7B6D94CE90F755C0B0B0C619F09652D07759F05") 
                 ("umbra.local"    "97E927F2A215632279DC629869D3B5A32172D080ED2402324896BA1781891050") 
                 ("david-pc.local" "CB81A6A56C5420F9E0B0C20090176009112D368131DBFE0129FAF4DBB1FCDD46")
                 ))
         (mine (find (machine-instance) dict
                     :key  #'car
                     :test #'string-equal)))
    (if mine
        (cons (list "localhost" (cadr mine)) dict)
      dict)))

(defvar *pkeys-dict*
  (with-open-file (f "~/.lattice-pkeys"
                                 :direction :input)
                (read f)))

(defun lattice-skey ()
  (with-open-file (f "~/.lattice-skey"
                     :direction :input)
    (third (read f))))

(defun lattice-local-id ()
  (str (hex (hash/256 (lattice-skey)))))

(defun lattice-id-for-node (node)
  (cadr (find node *nodes-dict*
              :key #'car
              :test #'string-equal)))

(defun lattice-pkey-for-id (id)
  (find id *pkeys-dict*
        :key #'car
        :test #'string-equal))
  
(defun lattice-pkey-for-node (node)
  (let ((id (lattice-id-for-node node)))
    (lattice-pkey-for-id id)))
|#

(defun lattice-skey ()
  (ask kvdb:kvdb :find :lat2-syzygy))

(defun lattice-pkey (node)
  (ask kvdb:kvdb :find (node-to-kw node :prefix "lat2-pkey-")))

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

#|
(defun make-connection-to-server-packet (srv-node)
  ;; Clients send client-side random key to server using Lattice
  ;; encryption, along with their lattice-id in an AES encrypted
  ;; packet.
  (let* (;; (pkey   (lattice-pkey-for-node srv-node))
         (pkey   (lattice-pkey srv-node))
         (rkey   (random-key))
         ;; (my-id  (lattice-local-id))
         (my-id  (machine-instance)))
    (values rkey
            (list
             (progn ;; with-pkey (pkey pkey)
               (lat-encode pkey rkey))
             (make-aes-packet rkey my-id)))
    ))

(defun make-connection-to-client-packet (client-id)
  ;; Server replies to client with server-side random key
  ;; using Lattice encrypted packet.
  (let* (;; (pkey (lattice-pkey-for-id client-id))
         (pkey (lattice-pkey client-id))
         (rkey (random-key)))
    (values rkey
            (progn ;; with-pkey (pkey pkey)
              (lat-encode pkey rkey)))
    ))
|#

(defun decode-server-connection-packet (packet)
  ;; At the server, we decode the random client key and their Lattice
  ;; ID.
  (allow-recursive-ask
    (destructuring-bind (latcrypt aescrypt) packet
      (let* ((skey (lattice-skey))
             (rkey (progn ;; with-skey (skey skey)
                     (lat2-decode skey latcrypt)))
             (id   (decode-aes-packet rkey aescrypt)))
        (values rkey
                id)))))

(defun decode-client-connection-packet (latcrypt)
  ;; At the client, we decode the random server key.
  (allow-recursive-ask
    (let* ((skey (lattice-skey))
           (rkey (progn ;; with-skey (skey skey)
                   (lat2-decode skey latcrypt))))
      rkey)))

;; ----------------------------------------------------
;; For Actors-based code, using parallel Lattice encryption

#|
(deflex cnx-to-server-packet-maker
  (create
   (lambda (cust srv-node)
     (let (;; (pkey  (lattice-pkey-for-node srv-node))
           (pkey  (lattice-pkey srv-node))
           (rkey  (random-key))
           ;; (my-id (lattice-local-id))
           (my-id (machine-instance)))
       (β (lat-enc)
           (send plat-encoder β pkey rkey)
         (send cust rkey (list lat-enc (make-aes-packet rkey my-id))))
       ))))
           
(deflex cnx-to-client-packet-maker
  (create
   (lambda (cust client-id)
     (let (;; (pkey (lattice-pkey-for-id client-id))
           (pkey (lattice-pkey client-id))
           (rkey (random-key)))
       (β (lat-enc)
           (send plat-encoder β pkey rkey)
         (send cust rkey lat-enc))
       ))))
|#

(deflex cnx-to-server-packet-maker
  (create
   (lambda (cust srv-node)
     (allow-recursive-ask
       (let* (;; (pkey  (lattice-pkey-for-node srv-node))
              (pkey  (lattice-pkey srv-node))
              (rkey  (random-key))
              ;; (my-id (lattice-local-id))
              (my-id (machine-instance))
              (lat-enc (lat2-encode pkey rkey)))
         (send cust rkey (list lat-enc (make-aes-packet rkey my-id))))
       ))))
           
(deflex cnx-to-client-packet-maker
  (create
   (lambda (cust client-id)
     (allow-recursive-ask
       (let* (;; (pkey (lattice-pkey-for-id client-id))
              (pkey (lattice-pkey client-id))
              (rkey (random-key))
              (lat-enc (lat2-encode pkey rkey)))
         (send cust rkey lat-enc))
       ))))



