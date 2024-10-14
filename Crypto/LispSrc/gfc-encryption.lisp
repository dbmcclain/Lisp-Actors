(in-package :ecc-crypto-b571)
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

;; ---------------------------------------------------------------------------
;; GFC Mode 
;;

(defvar *gfc-cipher-file-encryption*
  #.(uuid:uuid "{DCBAEF2E-5A0D-11E1-A22D-C82A14446EA7}"))

(defun gfc-signature ()
  (uuid:uuid-to-byte-array *gfc-cipher-file-encryption*))

(defun make-ecb-cipher (kind key)
  (ironclad:make-cipher kind
                        :mode :ecb
                        :key  key))

(defclass gfc-cipher ()
  ((cipher  :accessor gfc-cipher-cipher :initarg :cipher) ;; list of ECB ciphers
   (salt    :accessor gfc-cipher-salt   :initarg :salt)   ;; ubyte vector 16
   (nonce   :accessor gfc-cipher-nonce  :initarg :nonce)  ;; ubyte vector 16
   (ctr     :accessor gfc-cipher-ctr    :initarg :ctr)    ;; 32-bit integer
   (kmul    :accessor gfc-cipher-kmul   :initarg :kmul)   ;; GF(2^128) integer
   (c0      :accessor gfc-cipher-c0     :initarg :c0)     ;; GF(2^128) integer
   (tag     :accessor gfc-cipher-tag    :initarg :tag)    ;; GF(2^128) integer
   (alen    :accessor gfc-cipher-alen   :initarg :alen)   ;; 64-bit integer length of authentication data
   (clen    :accessor gfc-cipher-clen   :initarg :clen)   ;; 64-bit integer length of cryptotext
   ))

(defmethod wipe-obj ((cipher gfc-cipher))
  (apply #'wipe
         (gfc-cipher-salt cipher)
         (gfc-cipher-nonce cipher)
         (um:mklist (gfc-cipher-cipher cipher)))
  (setf (gfc-cipher-ctr cipher)  0
        (gfc-cipher-kmul cipher) 0
        (gfc-cipher-c0 cipher)   0
        (gfc-cipher-tag cipher)  0
        (gfc-cipher-alen cipher) 0
        (gfc-cipher-clen cipher) 0))

(defun fold-gfc-authentication (tag kmul vec)
  (with-gf2^128
    (gf* kmul (logxor tag (convert-bytes-to-int vec)))))

(defmethod safe-encrypt-in-place ((ciphers cons) &rest blks)
  (dolist (cipher ciphers)
    (apply #'safe-encrypt-in-place cipher blks)))

(defmethod safe-decrypt-in-place ((ciphers cons) &rest blks)
  (dolist (cipher (reverse ciphers))
    (apply #'safe-decrypt-in-place cipher blks)))

(defun make-cipher-block (&optional (nel 16))
  (make-ub-array nel :initial-element 0))

(defun make-displaced-cipher-block (arr offset &optional (nel 16))
  (make-ub-array nel
                 :displaced-to arr
                 :displaced-index-offset offset))

(defun make-gfc-cipher (key &key nonce salt)
  (let* ((nonce  (if nonce
                     (copy-seq nonce)
                   (make-nonce)))
         (salt   (if salt
                     (copy-seq salt)
                   (ctr-drbg 128)))

         (cipher (list (make-ecb-cipher :aesx    (kdf 256 :aesx    key nonce salt))
                       (make-ecb-cipher :twofish (kdf 256 :twofish key nonce salt))))

         (kmul   (let ((vec (make-cipher-block)))
                   ;; derive authentication multiplier = E(K,0^128)
                   (safe-encrypt-in-place cipher vec)
                   (convert-bytes-to-int vec)))
         
         (c0     (let ((vec (copy-seq nonce)))
                   ;; get final tag xor Y0 = E(IV | 0^31 | 1)
                   (replace vec (convert-int-to-nbytes 1 4) :start1 12)
                   (safe-encrypt-in-place cipher vec)
                   (convert-bytes-to-int vec)))
                   
         ;; get initial authentication data = (cipher UUID || nonce)
         (tag    (fold-gfc-authentication
                  (fold-gfc-authentication
                   (fold-gfc-authentication 0 kmul (gfc-signature))
                   kmul salt)
                  kmul nonce)))
    
    (make-instance 'gfc-cipher
                   :cipher cipher
                   :nonce  nonce
                   :salt   salt
                   :ctr    1
                   :kmul   kmul
                   :c0     c0
                   :tag    tag
                   :alen   48
                   :clen   0)
    ))

(defvar *gfc-digester-salt*  #(38 94 18 19 218 150 110 155 30 252 213 91 228 195 122 215))
(defvar *gfc-digester-nonce* (uuid:uuid-to-byte-array #.(uuid:uuid "{726070FA-5B15-11E1-879E-C82A14446EA7}")))

(defun make-gfc-digester (key &key (salt *gfc-digester-salt*) (nonce *gfc-digester-nonce*))
  (make-gfc-cipher key
                   :salt  salt
                   :nonce nonce))

(defmethod update-gfc-digest ((cipher gfc-cipher) &rest blks)
  (let* ((nel   (reduce #'+ (mapcar #'length blks)))
         (nblks (ceiling nel 16))
         (arr   (make-cipher-block (* 16 nblks))))

    ;; move plaintext into working buffers
    (copy-data-to-working-buffers arr blks)

    ;; encrypt plaintext and accumulate authentication tag
    (with-accessors ((kmul  gfc-cipher-kmul)
                     (tag   gfc-cipher-tag)
                     (alen  gfc-cipher-alen)) cipher
      (incf alen nel)
      (loop repeat nblks
            for offs from 0 by 16
            do
            (let ((dvec (make-displaced-cipher-block arr offs)))
              (setf tag (fold-gfc-authentication tag kmul dvec)))
            ))))

(defmethod gfc-produce-digest ((cipher gfc-cipher))
  (finish-gfc-encryption cipher))

(defun copy-data-loop (arr blks fn)
  (let ((offs  0))
    (dolist (blk blks)
      (let* ((nel (length blk))
             (vec (make-displaced-cipher-block arr offs nel)))
        (funcall fn vec blk)
        (incf offs nel)))))

(defun copy-data-to-working-buffers (arr blks)
  (copy-data-loop arr blks #'replace))

(defun copy-working-buffers-to-data (arr blks)
  (copy-data-loop arr blks (lambda (vec blk)
                             (replace blk vec))))

(defun encrypt-decrypt-gfc (cipher blks encdec)
  (let* ((nel   (reduce #'+ (mapcar #'length blks)))
         (nblks (ceiling nel 16))
         (arr   (make-cipher-block (* 16 nblks)))
         (cvec  (make-cipher-block)))

    ;; move plaintext into working buffers
    (copy-data-to-working-buffers arr blks)

    ;; encrypt plaintext and accumulate authentication tag
    (with-accessors ((kmul  gfc-cipher-kmul)
                     (ctr   gfc-cipher-ctr)
                     (nonce gfc-cipher-nonce)
                     (tag   gfc-cipher-tag)
                     (clen  gfc-cipher-clen)
                     (ecb   gfc-cipher-cipher)) cipher
      (incf clen nel)
      (loop repeat nblks
            for offs from 0 by 16
            do
            (let ((dvec (make-displaced-cipher-block arr offs)))
              
              (when (eq encdec :decrypt)
                (setf tag (fold-gfc-authentication tag kmul dvec)))
              
              (incf ctr)
              (replace cvec nonce :end1 12)
              (replace cvec (convert-int-to-nbytes ctr 4) :start1 12)
              (safe-encrypt-in-place ecb cvec)
              
              (map-into dvec #'logxor cvec dvec)
              
              (when (eq encdec :encrypt)
                (let ((nb (- nel offs)))
                  (when (< nb 16)
                    (fill dvec 0 :start nb)))
                (setf tag (fold-gfc-authentication tag kmul dvec)))
              )))

    ;; move ciphertext back into caller's buffers
    (copy-working-buffers-to-data arr blks)
    ))
  
(defmethod safe-encrypt-in-place ((cipher gfc-cipher) &rest blks)
  (encrypt-decrypt-gfc cipher blks :encrypt))

(defmethod safe-decrypt-in-place ((cipher gfc-cipher) &rest blks)
  (encrypt-decrypt-gfc cipher blks :decrypt))

(defmethod finish-gfc-encryption ((cipher gfc-cipher))
  ;; compute final authentication tag
  (let ((vec (make-cipher-block)))
    (with-accessors ((alen  gfc-cipher-alen)
                     (clen  gfc-cipher-clen)
                     (kmul  gfc-cipher-kmul)
                     (tag   gfc-cipher-tag)
                     (c0    gfc-cipher-c0)) cipher
      (replace vec (convert-int-to-nbytes alen 8))
      (replace vec (convert-int-to-nbytes clen 8) :start1 8)
      (logxor c0
              (fold-gfc-authentication tag kmul vec)) )))

(defun gfc-digest-file (fname-in key &key (nonce *gfc-digester-nonce*) (salt *gfc-digester-salt*))
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (let ((cipher (make-gfc-digester key
                                     :salt salt
                                     :nonce nonce))
          (flen   (file-length fin))
          (buf    (make-cipher-block)))
        
        (with-sensitive-objects (cipher)
          (loop for offset from 0 below flen by 16 do
                (let* ((nb  (min 16 (- flen offset)))
                       (buf (if (< nb 16)
                                (make-cipher-block nb)
                              buf)))
                  (read-sequence buf fin)
                  (update-gfc-digest cipher buf)) )

          ;; get authentication tag
          (gfc-produce-digest cipher)) )))

(defun gfc-encrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)

      ;; Encrypted file format is:
      ;;   encryption-UUID  16 bytes
      ;;   salt             16 bytes
      ;;   nonce            16 bytes
      ;;   encrypted-data    N bytes
      ;;   auth-tag         16 bytes
      
      (let ((cipher (make-gfc-cipher key))
            (flen   (file-length fin))
            (buf    (make-cipher-block)))
        
        (with-sensitive-objects (cipher)
          ;; write header info (encryption UUID, salt, nonce)
          (write-sequence (gfc-signature) fout)
          (write-sequence (gfc-cipher-salt cipher) fout)
          (write-sequence (gfc-cipher-nonce cipher) fout)

          ;; write encrypted contents
          (loop for offset from 0 below flen by 16 do
                (let* ((nb  (min 16 (- flen offset)))
                       (buf (if (< nb 16)
                                (make-cipher-block nb)
                              buf)))
                  (read-sequence buf fin)
                  (safe-encrypt-in-place cipher buf)
                  (write-sequence buf fout)))

          ;; write authentication tag
          (write-sequence (convert-int-to-nbytes
                           (finish-gfc-encryption cipher)
                           16)
                          fout)
          )))))


(defun gfc-decrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      
      ;; Encrypted file format is:
      ;;   encryption-UUID  16 bytes
      ;;   salt             16 bytes
      ;;   nonce            16 bytes
      ;;   encrypted-data    N bytes
      ;;   auth-tag         16 bytes
      
      (let ((flen (let ((len (file-length fin)))
                    (when (< len 64)
                      (error "File too short"))
                    (- len 64)))
            (buf  (make-cipher-block)))
        
        (read-sequence buf fin)
        (assert (equalp buf (gfc-signature)))

        (let ((cipher (progn
                        (read-sequence buf fin) ;; read salt
                        (let ((salt (copy-seq buf)))
                          (read-sequence buf fin) ;; read nonce
                          (let ((nonce (copy-seq buf)))
                            (make-gfc-cipher key :salt salt :nonce nonce)) )) ))

          ;; decrypt file contents
          (with-sensitive-objects (cipher)
            (loop for offset from 0 below flen by 16 do
                  (let* ((nb  (min 16 (- flen offset)))
                         (buf (if (< nb 16)
                                  (make-cipher-block nb)
                                buf)))
                    (read-sequence buf fin)
                    (safe-decrypt-in-place cipher buf)
                    (write-sequence buf fout)))
            
            ;; check authentication tag
            (read-sequence buf fin)
            (unless (= (finish-gfc-encryption cipher) (convert-bytes-to-int buf))
              (error "GFC authentication failure"))
            )))) ))

;; ---------------------------------------------------------------

(defun delivered-gfc-encrypt ()
  (let ((status 0))
    (handler-case
        (let ((progname (first sys:*line-arguments-list*))
              (encdec   (second sys:*line-arguments-list*))
              (fromfile (third sys:*line-arguments-list*))
              (tofile   (fourth sys:*line-arguments-list*))
              (key      (fifth sys:*line-arguments-list*)))
          (unless (and encdec
                       (or (string-equal encdec "ENC" :end1 3)
                           (string-equal encdec "DEC" :end1 3))
                       fromfile
                       tofile
                       key)
            (error "Usage: ~A [ENC|DEC] <from-file> <to-file> <key>"
                   progname))
          #|
           (print sys:*line-arguments-list*)
           (format t "~%Progname: ~A" progname)
           (format t "~%EncDec:   ~A" encdec)
           (format t "~%From:     ~A" fromfile)
           (format t "~%To:       ~A" tofile)
           (format t "~%Key:      ~A" key)
           (terpri)
           |#
          (setf sys:*stack-overflow-behaviour* nil)
          (if (string-equal encdec "enc" :end1 3)
              (gfc-encrypt-file fromfile tofile key)
            (gfc-decrypt-file fromfile tofile key))
          )
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))

;; ---------------------------------------------------------------
#|
(defvar *nonce* nil)
(defvar *salt*  (ctr-drbg 128))
(defvar *buf*   (make-array 16 :element-type 'ubyte :initial-element 0))
(defvar *msg*   (ensure-8bitv
                 #>.end
(defun make-twofish-aes-gfc-cipher (key-2fish key-aes &optional nonce)
  (let* ((twofish (make-gfc-cipher :twofish key-2fish nonce))
         (nonce   (or nonce (gfc-cipher-nonce twofish)))
         (aesx    (make-gfc-cipher :aesx key-aes nonce)))
    (make-multiple-gfc-cipher
     :nonce   nonce
     :ciphers (list twofish aesx))))
.end
))

(setf *print-length* nil)

;; encryption
(let* ((key    "Howdy!")
       (cipher (make-gfc-cipher key :nonce *nonce* :salt *salt*)))
  (setf *nonce* (gfc-cipher-nonce cipher))
  (safe-encrypt-in-place cipher *msg*)
  (list (gfc-cipher-nonce cipher)
        *msg*
        (finish-gfc-encryption cipher)))

;; decryption
(let* ((key    "Howdy!")
       (cipher (make-gfc-cipher key :nonce *nonce* :salt *salt*)))
  (setf *nonce* (gfc-cipher-nonce cipher))
  (safe-decrypt-in-place cipher *msg*)
  (list (gfc-cipher-nonce cipher)
        *msg*
        (finish-gfc-encryption cipher)))
       
(let* ((key    "Howdy!")
       (cipher (make-gfc-cipher :aesx key *nonce*)))
  (setf *nonce* (gfc-cipher-nonce cipher))
  (safe-encrypt-in-place cipher *buf*)
  (prog1
      (hex *buf*)
    (terpri)))
       
(let* ((keyTwofish    "Howdy!")
       (keyAES        "HowdyAgain!")
       (cipher (make-twofish-aes-gfc-cipher keyTwofish keyAES *nonce*)))
  (setf *nonce* (multiple-gfc-cipher-nonce cipher))
  (safe-encrypt-in-place cipher *buf*)
  (prog1
      (hex *buf*)
    (terpri)))
       
(let* ((key    (kdf 256 "Howdy!"))
       (cipher (make-ecb-cipher :aesx key))
       (buf    (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
       ;; (loenc:encode #(0 0 0 0 0 0 0 0 0 0) :align 16)
       (orig   (copy-seq buf)))
  (safe-encrypt-in-place cipher buf)
  (hex (list orig buf)))

|#

