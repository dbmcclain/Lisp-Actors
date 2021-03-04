#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :ecc-crypto-b571)

;; -----------------------------------------------------------------------------
;; All block ciphers in CBC mode with random IV's

(defun make-cbc-cipher (kind key iv)
  (ironclad:make-cipher kind
                        :mode :cbc
                        :key  key
                        :initialization-vector iv))

;; -------------------------------------------

(defstruct multiple-cbc-cipher
  ciphers)

(defmethod safe-encrypt-in-place ((cipher multiple-cbc-cipher) &rest blks)
  (dolist (cipher (reverse (multiple-cbc-cipher-ciphers cipher)))
    (apply #'safe-encrypt-in-place cipher blks)))

(defmethod safe-decrypt-in-place ((cipher multiple-cbc-cipher) &rest blks)
  (dolist (cipher (multiple-cbc-cipher-ciphers cipher))
    (apply #'safe-decrypt-in-place cipher blks)))

(defun make-twofish-aes-cbc-cipher (key-2fish iv-2fish key-aes iv-aes &optional (aes-mode :aesx))
  (make-multiple-cbc-cipher
   :ciphers (list
             (make-cbc-cipher :twofish key-2fish iv-2fish)
             (make-cbc-cipher aes-mode key-aes   iv-aes)) ))
                          
;; -----------------------------------------------------------------------------
;; This version of AES-Encrypt is fully compatible with C version of
;; AESCrypt -- uses increased rounds versions of AES

(defun start-aes (iv key)
  (let ((digbytes (make-ub-array 32
                                 :initial-element 0))
        (key  (ensure-8bitv key)))
    (replace digbytes iv
             :start1 0 :end1 16)
    (loop repeat 8192 do
          (let ((dig (ironclad:make-digest :sha256)))
            (safe-update-digest dig digbytes key)
            (ironclad:produce-digest dig :digest digbytes)))
    digbytes))

(defmethod aes-encrypt-file (fname-in fname-out (key string))
  (aes-encrypt-file fname-in fname-out
                    (convert-text-to-int8-array key)))

(defmethod aes-encrypt-file (fname-in fname-out (key vector))
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      
      (let* ((flen        (file-length fin))
             (lastn       (logand flen 15))
             (buf         (subseq
                           (let ((flen-bytes  (ensure-8bitv (convert-int-to-nbytes flen 8)))
                                 (fname-bytes (convert-text-to-int8-array (namestring fname-in)))
                                 (dig         (ironclad:make-digest :sha256)))
                             (safe-update-digest dig flen-bytes fname-bytes)
                             (ironclad:produce-digest dig))
                           0 16)))
          (setf (aref buf 15) (dpb lastn (byte 4 0) (aref buf 15)))
          (write-sequence buf fout)
          
          (let* ((digbytes (start-aes buf key))
                 (enc (ironclad:make-cipher :aesx
                                            :key  digbytes
                                            :mode :cbc
                                            :initialization-vector buf))
                 (hmac (ironclad:make-hmac digbytes :sha256)))
            
            (loop for offset from 0 below flen by 16 do
                  (let ((nb (min 16 (- flen offset))))
                    (read-sequence buf fin :end nb)
                    (safe-update-hmac hmac buf)
                    (safe-encrypt-in-place enc buf)
                    (write-sequence buf fout)))
            
            (write-sequence (ironclad:hmac-digest hmac) fout)) ))))

(defmethod aes-decrypt-file (fname-in fname-out (key string))
  (aes-decrypt-file fname-in fname-out
                    (convert-text-to-int8-array key)))

(defmethod aes-decrypt-file (fname-in fname-out (key vector))
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (let* ((flen (file-length fin)))
        (when (< flen #.(+ 16 32))
          (error "File too short"))
        (when (plusp (rem flen 16))
          (error "File size not a multiple of 16"))
        (decf flen #.(+ 16 32))
        (let* ((buf (make-ub-array 16)))
          (read-sequence buf fin)
          (let* ((lastn    (ldb (byte 4 0) (aref buf 15)))
                 (digbytes (start-aes buf key))
                 (enc      (ironclad:make-cipher :aesx
                                                 :key  digbytes
                                                 :mode :cbc
                                                 :initialization-vector buf))
                 (hmac     (ironclad:make-hmac digbytes :sha256)))
            (unless (zerop lastn)
              (decf flen (- 16 lastn)))
            (loop for offset from 0 below flen by 16 do
                  (read-sequence buf fin)
                  (safe-decrypt-in-place enc buf)
                  (safe-update-hmac hmac buf)
                  (let ((nb (min 16 (- flen offset))))
                    (write-sequence buf fout :end nb)))
            (let ((buf (make-ub-array 32))
                  (hmac (ironclad:hmac-digest hmac)))
              (read-sequence buf fin)
              (unless (equalp buf hmac)
                (error "HMAC check failed")) ))))) ))

;; -----------------------------------------------------------------------------
;;

(defun sha_d-256 (msg)
  (let ((dig (ironclad:make-digest :sha256))
        (m   (ensure-8bitv msg))
        (pre #.(make-ub-array 64
                              :initial-element 0)))
    (safe-update-digest dig pre m)
    (let ((h  (ironclad:produce-digest dig)))
      (reinitialize-instance dig)
      (safe-update-digest dig h)
      (ironclad:produce-digest dig))))

(defun sha_dbl-256 (msg)
  (let ((dig (ironclad:make-digest :sha256))
        (m   (ensure-8bitv msg)))
    (safe-update-digest dig m)
    (let ((hash (ironclad:produce-digest dig)))
      (reinitialize-instance dig)
      (safe-update-digest dig hash m)
      (ironclad:produce-digest dig))) )

;; -----------------------------------------------------------------------------
;; ECDSA

(defvar *ecdsa-signature-hdr*
  #.(uuid:uuid "{F34476BA-2B3D-11E1-84AB-C82A14446EA7}"))

(defun sign-with-hmac (hmac id)
  (let* ((d     (get-private-key id))
         (kpub  (get-public-key id))
         (uuid  (uuid:uuid-to-byte-array (uuid:make-v1-uuid)))
         (xhmac (sha_d-256 (loenc:encode (list hmac uuid kpub))))
         (e     (convert-bytes-to-int xhmac)))
    (with-mod *ecc-r*
      (um:nlet iter ()
        (let ((k (convert-bytes-to-int (ctr-drbg 571))))
          (if (not (< 0 k *ecc-r*))
            (iter)
            (let* ((kp  (ecc-mul *ecc-gen* k))
                   (r   (mmod (ecc-pt-x kp))))
              (if (zerop r)
                  (go-iter)
                (let ((s (m/ (m+ e (m* d r)) k)))
                  (if (zerop s)
                      (go-iter)
                    (list *ecdsa-signature-hdr*
                          uuid kpub r s)) )))) )))))

(defun check-signature-with-hmac (hmac sig)
  (handler-case
      (destructuring-bind (hdr uuid kpub r s) sig
        (assert (uuid:uuid= hdr *ecdsa-signature-hdr*))
        (assert (< 0 r *ecc-r*))
        (assert (< 0 s *ecc-r*))
        (ecc-validate-public-key kpub)
        (with-mod *ecc-r*
          (let* ((xhmac (sha_d-256 (loenc:encode (list hmac uuid kpub))))
                 (e     (convert-bytes-to-int xhmac))
                 (w     (m/ s))
                 (u1    (m* e w))
                 (u2    (m* r w))
                 (x     (ecc-add (ecc-mul *ecc-gen* u1)
                                 (ecc-mul kpub u2))))
            (assert (not (ecc-infinite-p x)))
            (let* ((x1   (ecc-pt-x x))
                   (v    (mod x1 *ecc-r*))
                   (uuid (uuid:byte-array-to-uuid uuid)))
              (assert (= v r))
              ;; (format t "Signed:~%~A~%" (list (uuid:when-created uuid) kpub))
              (values uuid kpub)) )))
    (error (err)
           (declare (ignore err))
           (error "Invalid signature")) ))
      
;; -----------------------------------------------------------------------------
;;

(defun sign (msg id)
  (cons msg (sign-with-hmac (sha_d-256 msg) id)))

(defun check-signature (signed-msg)
  (handler-case
      (destructuring-bind (msg &rest sig) signed-msg
        (multiple-value-bind (uuid kpub)
            (check-signature-with-hmac (sha_d-256 msg) sig)
          (values msg uuid kpub)))
    (error (err)
      (declare (ignore err))
      (error "Invalid signature"))))
      
;; -----------------------------------------------------------------------------
;;

(defun hmac-of-cmsg (cmsg)
  (destructuring-bind (ctype ctxt hmac) cmsg
    (declare (ignore ctype ctxt))
    hmac))

(defun sign-with-cmsg (cmsg my-id)
  (sign-with-hmac (hmac-of-cmsg cmsg) my-id))

(defun check-signature-with-cmsg (cmsg sig)
  (handler-case
      (check-signature-with-hmac (hmac-of-cmsg cmsg) sig)
    (error (err)
      (declare (ignore err))
      (error "Invalid signature")) ))

;; -----------------------------------------------------------------------------
;;

(defun encrypt-with-engines (cipher hmac msg)
  (with-sensitive-objects (cipher hmac)
    (let ((buf (loenc:encode msg :align 16)))
      (safe-update-hmac hmac buf)
      (safe-encrypt-in-place cipher buf)
      (let ((dig (ironclad:hmac-digest hmac)))
        (safe-encrypt-in-place cipher dig)
        (values buf dig)))))
  
(defun decrypt-with-engines (cipher hmac c m)
  (with-sensitive-objects (cipher hmac)
    (safe-decrypt-in-place cipher c m)
    (safe-update-hmac hmac c)
    (let ((dig (ironclad:hmac-digest hmac)))
      (unless (equalp m dig)
        (error "Invalid HMAC"))
      (loenc:decode c)) ))

;; -------------------------------------------

(defvar *aes-256-single-level-encryption*
  #.(uuid:uuid "{937F2BA2-2B3E-11E1-84AB-C82A14446EA7}"))

(defvar *aesx-256-single-level-encryption*
  #.(uuid:uuid "{8876C3CC-5C22-11E1-A65D-C82A14446EA7}"))

(defun derive-engines-for-encryption (keys &optional (aes-mode :aesx))
  (with-kdf-fields (((ke 256)
                     (km 256)
                     (iv 128))
                    keys)
    (let* ((cipher (make-cbc-cipher aes-mode ke iv))
           (hmac   (ironclad:make-hmac km :sha256)))
      (values
       (ecase aes-mode
         (:aesx *aesx-256-single-level-encryption*)
         (:aes  *aes-256-single-level-encryption* ))
       cipher hmac))))

;; -----------------------------------------------------------------------------
;;

(defvar *twofish-aes-256-dual-level-encryption*
  #.(uuid:uuid "{A8A231B4-2B3E-11E1-84AB-C82A14446EA7}"))

(defvar *twofish-aesx-256-dual-level-encryption*
  #.(uuid:uuid "{6CC8F500-5C22-11E1-A65D-C82A14446EA7}"))

(defun derive-engines-for-two-level-encryption (keys &optional (aes-mode :aesx))
  (with-kdf-fields (((ke1 256)
                     (ke2 256)
                     (km  256)
                     (iv1 128)
                     (iv2 128))
                    keys)
    (let* ((cipher (make-twofish-aes-cbc-cipher
                    ke1 iv1 ke2 iv2 aes-mode))
           (hmac  (ironclad:make-hmac km :sha256)))
      (values (ecase aes-mode
                (:aesx *twofish-aesx-256-dual-level-encryption*)
                (:aes  *twofish-aes-256-dual-level-encryption*))
              cipher hmac))))
  
;; -----------------------------------------------------------------------------
;;

(defun encrypt-msg (msg &key
                        (type :twofish-aes)
                        keys)
  (multiple-value-bind (ctype cipher hmac)
      (ecase type
        (:twofish-aes (derive-engines-for-two-level-encryption keys))
        (:aes         (derive-engines-for-encryption keys)))
    (multiple-value-bind (ctxt dig)
        (encrypt-with-engines cipher hmac msg)
      (list ctype ctxt dig))))

(defun decrypt-msg (cmsg keys)
  (destructuring-bind (ctype c m) cmsg
    (multiple-value-bind (etype cipher hmac)
        (cond ((uuid:uuid= ctype *twofish-aesx-256-dual-level-encryption*)
               (derive-engines-for-two-level-encryption keys :aesx))
              ((uuid:uuid= ctype *twofish-aes-256-dual-level-encryption*)
               (derive-engines-for-two-level-encryption keys :aes))
              ((uuid:uuid= ctype *aes-256-single-level-encryption*)
               (derive-engines-for-encryption keys))
              (t (error "Unrecognized encryption mode")))
      (declare (ignore etype))
      (decrypt-with-engines cipher hmac c m))))
  
;; -----------------------------------------------------------------------------
;;

(defvar *dh-b571-encryption*
  #.(uuid:uuid "{DD07E610-2B3E-11E1-84AB-C82A14446EA7}"))

(defun dh-encrypt (msg kpub)
  ;; (validate-public-key kpub)
  (let* ((r    (ctr-drbg-int *nbits*))
         (rpt  (ecc-mul *ecc-gen* r))
         (zpt  (ecc-mul kpub r))
         (k    (ctr-drbg-int *nbits*))
         (mpt  (ecc-mul *ecc-gen* k))
         (xpt  (ecc-add mpt zpt))
         (cmsg (encrypt-msg msg :keys mpt)))
    (with-sensitive-objects (zpt mpt)
      (list *dh-b571-encryption* rpt xpt cmsg))))

(defun dh-decrypt (psec kpriv)
  (destructuring-bind (ctype rpt xpt cmsg) psec
    ;; (validate-public-key rpt)
    ;; (validate-public-key xpt)
    (assert (uuid:uuid= ctype *dh-b571-encryption*))
    (let* ((zpt (ecc-mul rpt kpriv))
           (mpt (ecc-sub xpt zpt)))
      (with-sensitive-objects (zpt mpt)
        (decrypt-msg cmsg mpt)))))

;; -----------------------------------------------------------------------------
;;

(defvar *psec-encryption-header*
  #.(uuid:uuid "{706DBB06-2B4D-11E1-84AB-C82A14446EA7}"))

(defun psec-encrypt (msg kpub)
  ;; (validate-public-key kpub)
  (let* ((r   (ctr-drbg-int *nbits*))
         (k   (mod r *ecc-r*))
         (rpt (ecc-mul *ecc-gen* k))
         (zpt (ecc-mul kpub k))
         (s   (logxor r (convert-bytes-to-int
                         (kdf *nbits*
                              (ecc-pt-x rpt)
                              (ecc-pt-y rpt)
                              (ecc-pt-x zpt)
                              (ecc-pt-y zpt)))))
         (cmsg (encrypt-msg msg :keys zpt)))
    (wipe zpt)
    (list *psec-encryption-header* rpt s cmsg)))
  
(defun psec-decrypt (psec kpriv)
  (destructuring-bind (hdr rpt s cmsg) psec
    (assert (uuid:uuid= hdr *psec-encryption-header*))
    ;; (validate-public-key rpt)
    (let* ((zpt   (ecc-mul rpt kpriv))
           (r     (logxor s (convert-bytes-to-int
                             (kdf *nbits*
                                  (ecc-pt-x rpt)
                                  (ecc-pt-y rpt)
                                  (ecc-pt-x zpt)
                                  (ecc-pt-y zpt)))))
           (k     (mod r *ecc-r*))
           (rpt2  (ecc-mul *ecc-gen* k)))
      (with-sensitive-objects (zpt)
        (unless (equalp rpt rpt2)
          (error "Invalid PSEC"))
        (decrypt-msg cmsg zpt)) )))

;; -----------------------------------------------------------------------------
;;

(defun encrypt (msg from-id to-id)
  (encode-object-to-base64 (dh-encrypt (sign msg from-id) (get-public-key to-id))))

(defun decrypt (psec my-id)
  ;; psec is an encoded message string in Base64
  ;; id is your email address
  (let* ((challenge (ctr-drbg-int *nbits*))
         (response  (get-private-key my-id nil challenge))
         (kpub      (get-public-key my-id)))
    (when response
      (multiple-value-bind (msg uuid kpub)
          (check-signature
           (dh-decrypt (decode-object-from-base64 psec)
                       (solve-gf-lagrange challenge
                                          (make-crypto-share
                                           :x (ecc-pt-x kpub)
                                           :y (ecc-pt-y kpub))
                                          (make-crypto-share
                                           :x 0
                                           :y response))) )
        (if msg
            (values msg
                    (uuid:when-created uuid)
                    kpub)
            (error "invalid message")))) ))

;; -----------------------------------------------------------------------------
;;

(defvar *dh-multiple-recipients*
  #.(uuid:uuid "{33954394-2B47-11E1-84AB-C82A14446EA7}"))

(defun encrypt-for-multiple-recipients (msg from-id to-ids)
  (with-progress-bar ()
    (let* ((enc-priv (ctr-drbg-int *nbits*))
           (enc-pub  (ecc-mul *ecc-gen* enc-priv)))
      (destructuring-bind (dtype rpt xpt cmsg)
          (dh-encrypt (sign msg from-id) enc-pub)
        (declare (ignore dtype))
        (let* ((kpt    (ecc-sub xpt (ecc-mul rpt enc-priv)))
               (recips (mapcar (lambda (id)
                                 (let ((kpub (get-public-key id)))
                                   (cons kpub (ecc-add kpt (ecc-mul kpub enc-priv)))))
                               (adjoin from-id to-ids :test #'string-equal))))
          (encode-object-to-base64 (list *dh-multiple-recipients* enc-pub recips cmsg)) )))))

(defun decrypt-for-multiple-recipients (psec my-id)
  ;; psec is an encoded message string in Base64
  ;; id is your email address
  (let ((kpub  (get-public-key my-id)))
    (destructuring-bind (dtype rpt recips cmsg) (decode-object-from-base64 psec)
      (assert (uuid:uuid= dtype *dh-multiple-recipients*))
      (let ((key (sys:cdr-assoc kpub recips :test #'ecc-pt=)))
        (unless key
          (error "You are not an intended recipient"))
        (let* ((challenge (ctr-drbg-int *nbits*))
               (response  (get-private-key my-id nil challenge)))
          (when response
            (with-progress-bar ()
              (let ((kpriv (solve-gf-lagrange challenge
                                              (make-crypto-share
                                               :x (ecc-pt-x kpub)
                                               :y (ecc-pt-y kpub))
                                              (make-crypto-share
                                               :x 0
                                               :y response))))
                (multiple-value-bind (msg uuid kpub)
                    (check-signature (dh-decrypt (list *dh-b571-encryption* rpt key cmsg) kpriv))
                  (if msg
                      (values msg
                              (uuid:when-created uuid)
                              kpub)
                    (error "invalid message"))) ))) )))))

;; -----------------------------------------------------------------------------
;;
#|
(defun c-encode-dh (enc)
  (destructuring-bind (rpt xpt c m) enc
    (ubstream:with-output-to-ubyte-stream (s)
      (write-point rpt s)
      (write-point xpt s)
      (write-sequence m s)
      (write-vector c s))))
  
(defun c-encrypt (msg to-id)
  (;; format-bytes
   encode-bytes-to-base64
   (c-encode-dh (dh-encrypt msg (get-public-key to-id)))))

(defun c-decrypt (cstr64 my-id)
  (ubstream:with-input-from-ubyte-stream (s (decode-bytes-from-base64 cstr64))
    (let ((rpt (read-point s))
          (xpt (read-point s))
          (m   (read-nvector 32 s))
          (c   (read-vector s))
          (k   (get-private-key my-id)))
      (when k
        (dh-decrypt (list rpt xpt c m) k)) )))
|#
;; ------------------------------------------------------------------
;; IBE with B-571 ECC
;;
;; To send a message to someone with ID, first encrypt message M with
;; random symmetric key K producing cryptotext C = E{K,M}. Then also
;; send along ECC{ KPub_server, K || ID }.
;;
;; User ID gets the encrpted message C = E{K,M} and requests
;; decryption key K from server by sending second part of message to
;; server, ECC{ KPub_server, K || ID }.
;;
;; After authenticating requestor via password key exchange with
;; server, server decrypts ECC portion with its private key and
;; returns the symmetric encryption key K for ciphertext decryption.
;;
;; This seems a whole lot simpler than using Tate Pairings on
;; Supersingular EC's. And does not yield known security of
;; non-supersingular curves. Tate Pairings were used to attack
;; supersingular EC's in the first place, and so the only security is
;; provided by using excessively large groups, e.g., P = 6780 bits
;; with subfield Q = 512 bits. Whereas, our B-571 ECC uses only
;; 571-bit keys throughout on non-supersingular EC's.
;; ----------------------------------------------------------------

(defvar *ibe-encryption*
  #.(uuid:uuid "{5A8D6AC4-2B44-11E1-84AB-C82A14446EA7}"))

(defun encrypt-ibe (msg from-id to-id)
  ;; use twofish-aes encryption
  (let* ((ks        (list (ctr-drbg #.(+ 256 256 256 128 128))))
         (cmsg      (encrypt-msg msg :keys ks))
         (signature (sign-with-cmsg cmsg from-id))
         (keys      (dh-encrypt (list ks (um:paste-strings #\space from-id to-id))
                                *ecc-acudora-public-key*)))
    (with-sensitive-objects (ks)
      (encode-object-to-base64 (list *ibe-encryption* signature keys cmsg)))))

;; -----------------------------------------------------------------------------
;; Twofish-AES double encryption with attached ECDSA signature
;;
;; 16 bytes = file signature = *twofish-aes-file-encryption*
;; 16 bytes = IV seed
;; <file contents> padded to 16 bytes multiple
;; 80 bytes = kpub_x --+
;; 80 bytes = kpub_y   |
;; 80 bytes = R        +-- ECDSA Signature
;; 80 bytes = S      --+
      

(defvar *twofish-aes-file-encryption*
  #.(uuid:uuid "{C2F02FF2-2B44-11E1-84AB-C82A14446EA7}"))

(defvar *twofish-aesx-file-encryption*
  #.(uuid:uuid "{2C174C28-5C22-11E1-A65D-C82A14446EA7}"))

(defun aes-encrypt-file-with-signature (fname-in fname-out key id)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      
      (write-sequence (uuid:uuid-to-byte-array
                       *twofish-aesx-file-encryption*)
                      fout)
      
      (let* ((flen   (file-length fin))
             (lastn  (logand flen 15))
             (salt   (ctr-drbg 256))
             (buf    (kdf 128 flen fname-in salt)))
        (wipe salt)
        (setf (aref buf 15) (dpb lastn (byte 4 0) (aref buf 15)))
        (write-sequence buf fout)
        
        (multiple-value-bind (ctype cipher hmac)
            (derive-engines-for-two-level-encryption (list key buf))
          (declare (ignore ctype))
          
          (with-sensitive-objects (cipher hmac)
            (loop for offset from 0 below flen by 16 do
                  (let ((nb (min 16 (- flen offset))))
                    (read-sequence buf fin :end nb)
                    (safe-update-hmac hmac buf)
                    (safe-encrypt-in-place cipher buf)
                    (write-sequence buf fout)))
            
            (let ((dig (ironclad:hmac-digest hmac)))
              (with-sensitive-objects (dig)
                (destructuring-bind (stype uuid kpub r s) (sign-with-hmac dig id)
                  (declare (ignore stype))
                  
                  (let ((kx (convert-int571-to-80bytes (ecc-pt-x kpub)))
                        (ky (convert-int571-to-80bytes (ecc-pt-y kpub)))
                        (rr (convert-int571-to-80bytes r))
                        (ss (convert-int571-to-80bytes s)))
                    
                    (with-sensitive-objects (uuid kx ky rr ss)
                      (dolist (item (list uuid kx ky rr ss))
                        (safe-encrypt-in-place cipher item)
                        (write-sequence item fout))) )))) ))) )))

(defun aes-decrypt-file-with-signature (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      
      (let* ((flen (let ((flen     (file-length fin))
                         (min-flen #.(+ 16 16 16 80 80 80 80)))
                     (when (< flen min-flen)
                       (error "File too short"))
                     (when (plusp (rem flen 16))
                       (error "File size not a multiple of 16"))
                     (- flen min-flen)))

             (aes-mode
              (let ((ftype (uuid:byte-array-to-uuid (read-nvector 16 fin))))
                (cond
                 ((uuid:uuid= *twofish-aesx-file-encryption* ftype) :aesx)
                 ((uuid:uuid= *twofish-aes-file-encryption* ftype)  :aes)
                 (t (error "Unrecognized encryption mode")) )))
             
             (buf   (read-nvector 16 fin))
             (lastn (ldb (byte 4 0) (aref buf 15))))
        (unless (zerop lastn)
          (decf flen (- 16 lastn)))
        
        (multiple-value-bind (ctype cipher hmac)
            (derive-engines-for-two-level-encryption (list key buf) aes-mode)
          (declare (ignore ctype))
          
          (with-sensitive-objects (cipher hmac)
            (loop for offset from 0 below flen by 16 do
                  (read-sequence buf fin)
                  (safe-decrypt-in-place cipher buf)
                  (safe-update-hmac hmac buf)
                  (let ((nb (min 16 (- flen offset))))
                    (write-sequence buf fout :end nb)))
            
            (let* ((uuid (read-nvector 16 fin))
                   (kx   (read-nvector 80 fin))
                   (ky   (read-nvector 80 fin))
                   (rr   (read-nvector 80 fin))
                   (ss   (read-nvector 80 fin))
                   (dig  (ironclad:hmac-digest hmac)))
              
              (with-sensitive-objects (dig uuid kx ky rr ss)
                (dolist (item (list uuid kx ky rr ss))
                  (safe-decrypt-in-place cipher item))
                
                (check-signature-with-hmac dig
                                           (list
                                            *ecdsa-signature-hdr*
                                            uuid
                                            (make-ecc-pt
                                             :x (convert-bytes-to-int kx)
                                             :y (convert-bytes-to-int ky))
                                            (convert-bytes-to-int rr)
                                            (convert-bytes-to-int ss)))) )) )) )))
        
;; ---------------------------------------------------------------
;; Challenge/Response routines
;; Based on GF(2^571) arithmetic
;;
;; Challenger presents a unique random number every time, treated as
;; an X value. Responder must return a Y value, such that line extends
;; through Key at y-intercept (X = 0), zero at challenger's X, and Y
;; at (X * P)_x, where P is the B-571 generator point.
;;
;; NOTE: Instead of using P, the generator of B-571, we really ought
;; to choose a shared secret point, some random multiple of P.

(defun generate-response-to-challenge (key challenge)
  (let* ((pt (ecc-mul *ecc-gen* challenge)))
    (solve-gf-lagrange (ecc-pt-x pt)
                       (make-crypto-share
                        :x 0 :y key)
                       (make-crypto-share
                        :x challenge :y 0))))

(defun solve-challenge-response (challenge response)
  (let* ((pt (ecc-mul *ecc-gen* challenge)))
    (solve-gf-lagrange 0
                       (make-crypto-share
                        :x challenge :y 0)
                       (make-crypto-share
                        :x (ecc-pt-x pt) :y response))))

;; ---------------------------------------------------
