;; ecc-key-exchange.lisp
;;
;; DM/RAL  2023/09/15 08:56:51
;; ----------------------------------

(defpackage #:com.ral.crypto.ecc-key-exchange
  (:use #:common-lisp #:vec-repr #:hash #:edec #:ac #:modmath)
  (:export
   #:ecc-cnx-encrypt
   #:ecc-cnx-decrypt
   #:ecc-skey
   #:ecc-pkey
   #:my-pkeyid
   #:srv-pkey
   ))

(in-package #:com.ral.crypto.ecc-key-exchange)

(um:eval-always
  (import '(com.ral.actors.encoding:make-auth
            com.ral.actors.encoding:check-auth)))

;; ----------------------------------
;;
;; Store skey seed in system keychain with:
;;  security add-generic-password -a $USER -s 'LispActorsSystem' -w <key>
;;
;; If you need to delete before adding:
;;  security delete-generic-password -a $USER -s 'LispActorsSystem'

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
         (>> cust skey)
         (β! (const-beh skey)))
       ))
   ))

;; -----------------------------------------------------------

(defstruct (signed-item
            (:constructor %signed-item))
  item u hk)

(defun signed-item (item skey)
  ;; provide a package containing item and a verifiable Schnorr
  ;; signature from skey
  (let* ((pkey  (ed-nth-pt skey))
         (nonce (edec::ssig-nonce))
         (krand (int (hash/256 nonce item skey pkey)))
         (kpt   (ed-nth-pt krand))
         (hk    (hash/256 item kpt))
         (u     (with-mod *ed-r*
                  (m- krand (m* (int hk) skey))
                  )))
    (%signed-item
     :item item
     :u    u
     :hk   hk)
    ))

(defmethod verify-item ((obj signed-item) pkey)
  ;; Verify that item was signed by pkey.
  ;;
  ;; Signature (u, Hk) provides number, u, and random hash of item, 
  ;; Hk = H(item, krand), such that:
  ;;
  ;;   Hk = H(item, u*G + Hk*P)
  ;;
  ;;     -- IOW, Hk has a pre-image based on itself. Could only
  ;;        happen if pkey knows skey.
  ;;
  ;; This version of test seems easier to comprehend.
  ;; Kind of like an Ouroboros...
  (ignore-errors
    (ed-validate-point pkey)
    (with-slots (item u hk) obj
      (let ((hpt (ed-add (ed-nth-pt u)
                         (ed-mul pkey (int hk) )
                         )))
        (values (hash= hk
                       (hash/256 item hpt))
                item)
        ))))

;; ----------------------------------------------------
;; Secure Storage in Database
;;
;; We protect PKeys from general browsing. PKeys are always stored as
;; encrypted items with authentication. The PKeys are themselves
;; accompanied by a proof of validity.

(defstruct encrypted-entry
  iv enc auth)

(deflex database-key
  (create
   (lambda (cust)
     (β (skey)
         (send ecc-skey β)
       (let ((ekey (hash/256 :database skey)))
         (become (const-beh ekey))
         (send cust ekey))
       ))
   ))

(defun check-db-authentication (ekey)
  (create
   (lambda (cust seq emsg auth)
     (when (check-auth ekey seq emsg auth)
       (send cust seq (copy-seq emsg)))) ;; decryption is mutating
   ))

(deflex format-encoder
  (create
   (lambda (cust iv enc auth)
     ;; args should all be ub8v
     (send cust
           (make-encrypted-entry
            :iv   iv
            :enc  enc
            :auth auth)))
   ))

(deflex format-decoder
  (create
   (lambda (cust pkt)
     (when (encrypted-entry-p pkt)
       (with-slots (iv enc auth) pkt
         (send cust iv enc auth))))
   ))

(defun db-encryptor (ekey)
  (pipe (marshal-encoder)
        (marshal-compressor)
        (encryptor ekey)
        (authentication ekey)
        format-encoder
        ))

(defun db-decryptor (ekey)
  (pipe format-decoder
        (check-db-authentication ekey)
        (decryptor ekey)
        (fail-silent-marshal-decompressor)
        (fail-silent-marshal-decoder)))

(deflex encrypt-for-database
  (create
   (lambda (cust item)
     (β (ekey)
         (send database-key β)
       (send (db-encryptor ekey) cust item)))
   ))

(deflex decrypt-from-database
  (create
   (lambda (cust data)
     (β (ekey)
         (send database-key β)
       (send (db-decryptor ekey) cust data)))
   ))

;; ----------------------------------------------

(defun pkey-validation-gate (cust)
  (create
   (lambda (info)
     ;; info should be a verifiable pkey, with proof that whoever
     ;; provided pkey also has knowledge of skey, and that pkey is a
     ;; valid ECC point.
     (become-sink)
     (when (and info
                (encrypted-entry-p info))
       (β (pkey-pkg)
           (send decrypt-from-database β info)
         (multiple-value-bind (ok pkey)
             (verify-item pkey-pkg (signed-item-item pkey-pkg))
           (when ok
             (send cust pkey))
           ))
       ))
   ))

;; -----------------------------------------------------------

(deflex ecc-pkey
  (create
   (lambda (cust pkey-id)
     (>> kvdb:kvdb (pkey-validation-gate cust) :find pkey-id))
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
  (let* ((vdata  (loenc:encode (loenc:unshared-list data)
                               :max-portability t))
         (iv     (make-iv key))
         (cdata  (aes-enc/dec key iv vdata))
         (chk    (make-auth-chk key iv cdata)))
    (list iv cdata chk)
    ))

(defun* decode-aes-packet (key (iv cdata chk))
  (let ((chkx (make-auth-chk key iv cdata)))
    (when (equalp chkx chk) ;; just drop on the floor if not valid
      (let ((vdata  (aes-enc/dec key iv cdata)))
        (loenc:decode vdata))
      )))

;; ----------------------------------------------------

(defun aes-packet-key (pt)
  (vec (hash/256 :ecc-cnx-key pt)))

(deflex ecc-cnx-encrypt
  (create
   (lambda (cust pubkey &rest info)
     (let+ ((:mvb (rand rand-pt) (ed-random-pair)))
       (ignore-errors
         (let+ ((enc-pt     (ed-mul pubkey rand))
                (aes-key    (aes-packet-key enc-pt))
                (aes-packet (<<* #'make-aes-packet aes-key info)))
           (>> cust rand (ed-affine rand-pt) aes-packet)
           ))))
   ))

(deflex ecc-cnx-decrypt
  (create
   (lambda (cust rand-pt aes-packet)
     (let+ ((:β (skey) ecc-skey))
       (ignore-errors
         (ed-validate-point rand-pt)
         (let+ ((enc-pt   (ed-mul rand-pt skey))
                (aes-key  (aes-packet-key enc-pt))
                (info     (decode-aes-packet aes-key aes-packet)))
           (when (consp info)
             (>> cust info))
           ))
       ))
   ))

;; ----------------------------------------------------
;; For Actors-based code, using ECC encryption

(deflex my-pkeyid
  (create
   (lambda (cust)
     (let ((gate (create
                  (lambda (id)
                    (when id
                      (>> cust id))
                    (become-sink))
                  )))
       (>> kvdb:kvdb gate :find :my-ecc-pkeyid)))
   ))

(deflex srv-pkey
  (create
   (lambda (cust)
     (>> kvdb:kvdb (pkey-validation-gate cust)
         :find "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}"))
   ))

(defmethod store-pkey (pkey-id (pkey-pkg signed-item))
  (when (verify-item pkey-pkg (signed-item-item pkey-pkg))
    (β (enc)
        (send encrypt-for-database β pkey-pkg)
      (send kvdb:kvdb println :add pkey-id enc))
    ))

#|
(β (skey)
    (>> ecc-skey β)
  (let ((pkey  (ed-nth-pt skey)))
    (>> kvdb:kvdb println :add :my-ecc-pkeyid "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}")
    (store-pkey "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}" (signed-item pkey skey))
    ))

(β (pkey-id)
    (>> my-pkeyid β)
  (>> ecc-pkey println pkey-id))

 |#
