;; ecc-key-exchange.lisp
;;
;; DM/RAL  2023/09/15 08:56:51
;; ----------------------------------

(in-package #:com.ral.crypto.ecc-key-exchange)

;; ----------------------------------
;;
;; Store skey seed in system keychain with:
;;  security add-generic-password -a $USER -s 'LispActorsSystem' -w <key>
;;
;; If you need to delete before adding:
;;  security delete-generic-password -a $USER -s 'LispActorsSystem'

(defun ecc-skey-beh ()
  (behav (cust)
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
                (read-line f))) )
       (let ((skey (with-ed-curve +ECC-CURVE+
                     (compute-deterministic-elligator-skey :skey (get-seed))
                     ;; (make-deterministic-keys :skey (get-seed))
                     )))
         (>> cust skey)
         (β! (const-beh skey)))
       )))

(deflex ecc-skey
  (create (ecc-skey-beh)))

;; --------------------------------------------

(defun* lw-start-ke _
  (setf ecc-skey (create (ecc-skey-beh))))

(defun* lw-kill-ke _
  (setf ecc-skey (create (ecc-skey-beh))))

#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up ECC Key Exchange"
                    'lw-start-ke
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop ECC Key Exchange"
                    'lw-kill-ke)

  (lw:define-action "Save Session After"
                    "Restart ECC Key Exchange"
                    'lw-start-ke)
  )

;; -----------------------------------------------------------
;; Signed Items - data + PKey + Schnorr signature.
;;
;; An item is signed by claimant, PKey, attesting that they vouch for
;; the item. They identify themselves with their Public Key, and
;; provide proof that the signature was not forged.
;;
;; Signature = (u, Hk) where Hk = H(u*G + Hk*P, P, item)

(defstruct (signed-item
            (:constructor %signed-item))
  item pkey u hk)

(defun signed-item (item skey)
  ;; provide a package containing item and a verifiable Schnorr
  ;; signature from skey
  (with-ed-curve +ECC-CURVE+
    (let* ((pkey  (ed-compress-pt (ed-nth-pt skey)))
           (hash  (hash/256 pkey item))
           (krand (int hash))
           (kpt   (ed-nth-pt krand))
           (hk    (hash/256 kpt pkey item))
           (u     (with-mod *ed-r*
                    (m- krand (m* (int hk) skey))
                    )))
      (%signed-item
       :item item
       :pkey pkey
       :u    u
       :hk   hk)
      )))

(defmethod verify-signature ((obj signed-item))
  ;; Verify that item was signed by pkey.
  ;;
  ;; Signature (u, Hk) provides number, u, and random hash of item, 
  ;; Hk = H(krand*G, P, item), such that:
  ;;
  ;;   Hk = H(u*G + Hk*P, P, item)
  ;;
  ;;     -- IOW, Hk has a pre-image based on itself. Could only
  ;;        happen if pkey knows skey.
  ;;
  ;; This version of test seems easier to comprehend.
  ;; Kind of like an Ouroboros...
  ;;
  (with-slots (item pkey u hk) obj
    (with-ed-curve +ECC-CURVE+
      (ignore-errors
        (let ((pkey (ed-decompress-pt pkey)))
          (ed-validate-point pkey)
          (let ((hpt (ed-add (ed-nth-pt u)
                             (ed-mul pkey (int hk) )
                             )))
            (values (hash= hk
                           (hash/256 hpt pkey item))
                    item)
            ))))))

(defmethod verify-signature (obj)
  ;; no signature to verify, so NO
  nil)

;; ----------------------------------------------------
;; Secure Storage in Database
;;
;; We protect PKeys from general browsing. PKeys are always stored as
;; encrypted items with authentication. The PKeys are themselves
;; accompanied by a Schnorr signature as proof of their own validity -
;; meaning that the originator of the Public Key knows the accompanying
;; Secret Key.
;;
;; We encapsulate encrypted items in a formal struct so that you can
;; at least visually identify such items in the database.
;;
;; Encryptions include an IV vector for AES-256 encryption, the
;; encrypted data vector, and an authentication vector. Since AES-256
;; is malleable, you need the authentication to conclude that it
;; represents a valid database entry, and has not been altered.

(defstruct encrypted-entry
  iv enc auth)

(deflex database-key
  (create
   (behav (cust)
     (β (skey)
         (send ecc-skey β)
       (let ((ekey (hash/256 :database skey)))
         (become (const-beh ekey))
         (send cust ekey))
       ))
   ))

(defun check-db-authentication (ekey)
  (create
   (behav (cust seq emsg auth)
     (when (check-auth ekey seq emsg auth)
       (send cust seq emsg)))
   ))

(deflex format-encoder
  (create
   (behav (cust iv enc auth)
     ;; args should all be ub8v
     (send cust
           (make-encrypted-entry
            :iv   iv
            :enc  enc
            :auth auth)))
   ))

(deflex format-decoder
  (create
   (behav (cust pkt)
     (when (encrypted-entry-p pkt)
       (with-slots (iv enc auth) pkt
         (send cust iv enc auth))))
   ))

(defun db-encryptor (ekey)
  (pipe marshal-encoder
        smart-compressor
        (encryptor ekey)
        (authentication ekey)
        format-encoder
        ))

(defun db-decryptor (ekey)
  (pipe format-decoder
        (check-db-authentication ekey)
        (non-destructive-decryptor ekey)
        fail-silent-smart-decompressor
        fail-silent-marshal-decoder))

(deflex encrypt-for-database
  (create
   (behav (cust item)
     (β (ekey)
         (send database-key β)
       (send (db-encryptor ekey) cust item)))
   ))

(deflex decrypt-from-database
  (create
   (behav (cust data)
     (β (ekey)
         (send database-key β)
       (send (db-decryptor ekey) cust data)))
   ))

(deflex secure-kvdb
  (create
   (alambda
    ((cust :find key)
     (β (item)
         (send kvdb β :find key)
       (if (encrypted-entry-p item)
           (send decrypt-from-database cust item)
         (send cust item))
       ))

    ((cust :secure-add key item)
     (β (enc)
         (send encrypt-for-database β item)
       (send kvdb cust :add key enc)))
    
    (msg
     (send* kvdb msg))
    )))

;; ----------------------------------------------
;; A validation gate serves as an interposing customer for database
;; queries about Public Keys.
;;
;; It verfies that the database entry exists, that it is an
;; Encrypted-Entry, that it decrypts to a Signed-Item, and that the
;; Schnorr signature is valid. The Signed-Item carries a copy of the
;; PKey.
;;
;; That verification includes a test of PKey being a legitimate ECC
;; curve point, and not a member of a small group.
;;
;; IFF the PKey passes these tests, it is forwarded to the original
;; customer.

(defun pkey-validation-gate (cust)
  (create
   (behav (item)
     ;; info should be a verifiable pkey, with proof that whoever
     ;; provided pkey also has knowledge of skey, and that pkey is a
     ;; valid ECC point.
     (become-sink)
     (when (verify-signature item)
       (send cust (with-ed-curve +ECC-CURVE+
                    (ed-decompress-pt (signed-item-pkey item))))
       ))
   ))

;; -----------------------------------------------------------

(deflex ecc-pkey
  ;; All PKey queries to the database go through a validation handler
  ;; before being sent to the customer.
  (create
   (behav (cust pkey-id)
     (>> secure-kvdb (pkey-validation-gate cust) :find pkey-id))
   ))

;; ------------------------------------------------------
;; AES-256/CTR Encryption/Decryption

(defun random-key ()
  (vec (hash/256 :random-key (uuid:make-v1-uuid)
                 (prng:ctr-drbg 256))))

#|
(defun aes-enc/dec (key iv vsrc)
  (let ((cipher (ironclad:make-cipher :aes
                                      :mode :ctr
                                      :key  key
                                      :initialization-vector iv)))
    (ironclad:encrypt-in-place cipher vsrc)
    vsrc))

(defun make-iv (key)
  (subseq
   (vec (hash/256 :iv key (uuid:make-v1-uuid)))
   0 16))
|#
#||#
(defun aes-enc/dec (key iv vsrc)
  (let ((cipher (ironclad:make-cipher :xchacha
                                      :mode :stream
                                      :key  key
                                      :initialization-vector iv)))
    (ironclad:encrypt-in-place cipher vsrc)
    vsrc))

(defun make-iv (key)
  (subseq
   (vec (hash/256 :iv key (uuid:make-v1-uuid)))
   4 28))
#||#

(defun make-auth-chk (key iv cdata)
  (vec (hash/256 :chk key iv cdata)))

(defun make-aes-packet (key &rest data)
  (let* ((vdata  (padder
                  (loenc:encode (loenc:unshared-list data)
                                :max-portability t)))
         (iv     (make-iv key))
         (cdata  (aes-enc/dec key iv vdata))
         (chk    (make-auth-chk key iv cdata)))
    (list iv cdata chk)
    ))

(defun* decode-aes-packet (key (iv cdata chk))
  (let ((chkx (make-auth-chk key iv cdata)))
    (when (equalp chkx chk) ;; just drop on the floor if not valid
      (let ((vdata  (aes-enc/dec key iv cdata)))
        (loenc:decode (unpadder vdata)))
      )))

;; ----------------------------------------------------
;; We have Elligators for just this purpose. Let's use them here.

(defun aes-packet-key (pt)
  (vec (hash/256 :ecc-cnx-key pt)))

(deflex ecc-cnx-encrypt
  (create
   (behav (cust pubkey &rest info)
     (ignore-errors
       (with-ed-curve +ECC-CURVE+
         (let+ ((:mvb (rand rand-tau)
                 (compute-deterministic-elligator-skey
                  :CONNECT (uuid:make-v1-uuid)))
                (enc-pt     (ed-mul pubkey rand))
                (aes-key    (aes-packet-key enc-pt))
                (aes-packet (apply #'make-aes-packet aes-key info)))
           (>> cust rand rand-tau aes-packet)
           ))
       ))
   ))

(deflex ecc-cnx-decrypt
  (create
   (behav (cust rand-tau aes-packet)
     (let+ ((:β (skey) ecc-skey))
       (ignore-errors
         (with-ed-curve +ECC-CURVE+
           (let+ ((rand-pt  (elli2-decode rand-tau))
                  (_        (ed-validate-point rand-pt))
                  (enc-pt   (ed-mul rand-pt skey))
                  (aes-key  (aes-packet-key enc-pt))
                  (info     (decode-aes-packet aes-key aes-packet)))
             (>> cust (ed-affine rand-pt) info)
             ))
         )))
   ))

;; ----------------------------------------------------
;; For Actors-based code, using ECC encryption

(deflex my-pkeyid
  (create
   (behav (cust)
     (let ((gate (create
                  (behav (id)
                    (when id
                      (>> cust id))
                    (become-sink))
                  )))
       (>> secure-kvdb gate :find :my-ecc-pkeyid)))
   ))

(deflex srv-pkey
  (create
   (behav (cust)
     (>> ecc-pkey cust "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}"))
   ))

(defmethod store-pkey (pkey-id (pkey-pkg signed-item))
  (when (verify-signature pkey-pkg)
    (send secure-kvdb println :secure-add pkey-id pkey-pkg)))

#|
(β (skey)
    (>> ecc-skey β)
  (>> secure-kvdb println :add :my-ecc-pkeyid "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}")
  (store-pkey "{a6f4ce88-53e2-11ee-9ce9-f643f5d48a65}" (signed-item :PKEY skey)))

(β (pkey-id)
    (>> my-pkeyid β)
  (>> ecc-pkey println pkey-id))

 |#
;; --------------------------------------------
