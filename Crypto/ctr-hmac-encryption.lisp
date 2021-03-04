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

;; ---------------------------------------------------------------

(defvar *ctr-hmac-cipher-file-encryption*
  #.(uuid:uuid "{2C8631A4-5C9C-11E1-83D2-C82A14446EA7}"))

(defun ctr-hmac-signature ()
  (uuid:uuid-to-byte-array *ctr-hmac-cipher-file-encryption*))

;; ---------------------------------------------------------------

(defclass generic-ctr-cipher ()
  ((key     :accessor ctr-cipher-key    :initarg :key)
   (salt    :accessor ctr-cipher-salt   :initarg :salt) ;; ub vec 16
   (nonce   :accessor ctr-cipher-nonce  :initarg :nonce)  ;; ub vec 16

   (ctr     :accessor ctr-cipher-ctr    :initform 0)    ;; 32-bit integer
   (cvec    :accessor ctr-cipher-cvec   :initform (make-cipher-block))
   
   (sig     :accessor ctr-cipher-sig    :initarg :sig)  ;; UUID of crypto kind
   (ecb     :accessor ctr-cipher-ecb)    ;; list of ECB ciphers
   (mac     :accessor ctr-cipher-mac)  ;; HMAC SHA-256
   ))

(defmethod shared-initialize :around ((cipher generic-ctr-cipher) slot-names &rest args &key &allow-other-keys)
  (let ((ans (call-next-method))) ;; allow all :BEFORE, specific, and :AFTER methods to run first.
    (with-accessors ((salt  ctr-cipher-salt)
                     (nonce ctr-cipher-nonce)
                     (sig   ctr-cipher-sig)) cipher
      (when (slot-boundp cipher 'mac)
        (update-mac cipher sig)
        (update-mac cipher salt)
        (update-mac cipher nonce))
      ans)))

(defmethod wipe-obj ((cipher generic-ctr-cipher))
  (apply #'wipe
         (ctr-cipher-salt cipher)
         (ctr-cipher-nonce cipher)
         (ctr-cipher-mac cipher)
         (ctr-cipher-cvec cipher)
         (um:mklist (ctr-cipher-ecb cipher)))
  (setf (ctr-cipher-ctr cipher)  0))

;; ---------------------------------------------------------

(defclass generic-ctr-hmac-cipher (generic-ctr-cipher)
  ())

(defmethod update-mac ((cipher generic-ctr-hmac-cipher) buf)
  (ironclad:update-hmac (ctr-cipher-mac cipher) buf))

(defmethod get-mac ((cipher generic-ctr-hmac-cipher))
  (ironclad:hmac-digest (ctr-cipher-mac cipher)))

(defmethod mac-length ((cipher generic-ctr-hmac-cipher))
  32)

(defmethod cipher-overhead ((cipher generic-ctr-hmac-cipher))
  80)

;; ---------------------------------------------------------------

(defclass ctr-hmac-cipher (generic-ctr-hmac-cipher)
  ()
  (:default-initargs
   :sig (ctr-hmac-signature)))    ;; UUID of crypto kind

(defmethod shared-initialize :after ((cipher ctr-hmac-cipher) slot-names &rest args &key &allow-other-keys)
  (when (and (slot-boundp cipher 'key)
             (slot-boundp cipher 'salt)
             (slot-boundp cipher 'nonce))

    (with-accessors ((ecb   ctr-cipher-ecb)
                     (mac   ctr-cipher-mac)
                     (key   ctr-cipher-key)
                     (salt  ctr-cipher-salt)
                     (nonce ctr-cipher-nonce)) cipher
      
      (setf ecb  (list (make-ecb-cipher :aesx    (kdf 256 :aesx    key nonce salt))
                       (make-ecb-cipher :twofish (kdf 256 :twofish key nonce salt)))
            
            mac  (ironclad:make-hmac (kdf 256 :hmac-sha256 key nonce salt) :sha256))
      )))

;; ---------------------------------------------------------------

(defun make-ecb-cipher (kind key)
  (ironclad:make-cipher kind
                        :mode :ecb
                        :key  key))

(defun make-cipher-block (&optional (nel 16))
  (make-ub-array nel :initial-element 0))

(defun make-displaced-cipher-block (arr offset &optional (nel 16))
  (make-ub-array nel
                 :displaced-to arr
                 :displaced-index-offset offset))

;; ---------------------------------------------------------------

(defun make-ctr-hmac-cipher (key &key nonce salt)
  (let* ((nonce  (if nonce
                     (copy-seq nonce)
                   (make-nonce)))
         (salt   (if salt
                     (copy-seq salt)
                   (ctr-drbg 128))))
    
    (make-instance 'ctr-hmac-cipher
                   :key   key
                   :nonce nonce
                   :salt  salt) ))

;; ---------------------------------------------------------------

#|
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
|#

(defmethod encrypt-decrypt-ctr ((cipher generic-ctr-cipher) blk)
  ;; encrypt/decrypt one 16-byte (or shorter) block
  (with-accessors ((ctr   ctr-cipher-ctr)
                   (nonce ctr-cipher-nonce)
                   (cvec  ctr-cipher-cvec)
                   (ecb   ctr-cipher-ecb)) cipher

    (let ((nblk (length blk)))
      (loop for off from 0 below nblk by 16 do
            (let* ((nel  (min 16 (- nblk off)))
                   (dvec (make-displaced-cipher-block blk off nel)))
              (incf ctr)
              (replace cvec nonce :end1 12)
              (replace cvec (convert-int-to-nbytes ctr 4) :start1 12)
              (safe-encrypt-in-place ecb cvec)
              (map-into dvec #'logxor cvec dvec) )) )))

;; ---------------------------------------------------------------

(defmethod safe-encrypt-in-place ((cipher cons) &rest blks)
  (dolist (ciph cipher)
    (apply #'safe-encrypt-in-place ciph blks)))

(defmethod safe-decrypt-in-place ((cipher cons) &rest blks)
  (dolist (ciph (reverse cipher))
    (apply #'safe-decrypt-in-place ciph blks)))

;; ---------------------------------------------------------------

(defun ctr-encrypt-stream-with-cipher (fin flen fout cipher)
  
      ;; Encrypted file format is:
      ;;   encryption-UUID  16 bytes
      ;;   salt             16 bytes
      ;;   nonce            16 bytes
      ;;   encrypted-data    N bytes
      ;;   hmac(cyphertext) 32 bytes
      
      (let ((buf (make-cipher-block)))
        
        (with-sensitive-objects (cipher)
          ;; write header info (encryption UUID, salt, nonce)
          (write-sequence (ctr-cipher-sig cipher) fout)
          (write-sequence (ctr-cipher-salt cipher) fout)
          (write-sequence (ctr-cipher-nonce cipher) fout)

          ;; write encrypted contents
          (loop for offset from 0 below flen by 16 do
                (let* ((nb  (min 16 (- flen offset)))
                       (buf (if (< nb 16)
                                (adjust-array buf nb)
                              buf)))
                  (read-sequence buf fin)
                  (encrypt-decrypt-ctr cipher buf)
                  (update-mac cipher buf)
                  (write-sequence buf fout)))

          ;; write HMAC
          (let ((digest (get-mac cipher)))
            ;; (encrypt-decrypt-ctr cipher digest)
            (write-sequence digest fout))
          )))

(defun ctr-hmac-encrypt-stream (fin flen fout key)
  (ctr-encrypt-stream-with-cipher fin flen fout
                                  (make-ctr-hmac-cipher key)))

(defun ctr-hmac-encrypt-sequence (seq key)
  (ubstream:with-output-to-ubyte-stream (sout)
    (ubstream:with-input-from-ubyte-stream (sin (ensure-8bitv seq))
      (ctr-hmac-encrypt-stream sin (length seq) sout key))))
  
(defun ctr-hmac-encrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (ctr-hmac-encrypt-stream fin (file-length fin) fout key) )))

;; ---------------------------------------------------------------

(defun ctr-decrypt-stream-with-cipher (fin flen fout cipher)
  
      ;; Encrypted file format is:
      ;;   encryption-UUID  16 bytes
      ;;   salt             16 bytes
      ;;   nonce            16 bytes
      ;;   encrypted-data    N bytes
      ;;   hmac(cyphertext) 32 bytes
      
      (let* ((noh  (cipher-overhead cipher))
             (flen (let ((len flen))
                     (when (< len noh)
                       (error "File too short"))
                     (- len noh)))
             (buf  (make-cipher-block)))
        
        (read-sequence buf fin)
        (assert (equalp buf (ctr-cipher-sig cipher)))

        (read-sequence buf fin) ;; read salt
        (let ((salt (copy-seq buf)))
          (read-sequence buf fin) ;; read nonce
          (let ((nonce (copy-seq buf)))
            (reinitialize-instance cipher
                                   :salt  salt
                                   :nonce nonce) ))

        ;; decrypt file contents
        (with-sensitive-objects (cipher)
          (loop for offset from 0 below flen by 16 do
                (let* ((nb  (min 16 (- flen offset)))
                       (buf (if (< nb 16)
                                (make-cipher-block nb)
                              buf)))
                  (read-sequence buf fin)
                  (update-mac cipher buf)
                  (encrypt-decrypt-ctr cipher buf)
                  (write-sequence buf fout)))
          
          ;; check authentication tag
          (let ((digest (make-cipher-block (mac-length cipher)))
                (mac    (get-mac cipher)))
            (read-sequence digest fin)
            ;; (encrypt-decrypt-ctr cipher digest)
            (unless (equalp digest mac)
              (error "CTR-HMAC digest failure")) )
          )))

(defun ctr-hmac-decrypt-stream (fin flen fout key)
  (ctr-decrypt-stream-with-cipher fin flen fout
                                  (make-instance 'ctr-hmac-cipher
                                                 :key key)))
;
(defun ctr-hmac-decrypt-sequence (seq key)
  (ubstream:with-output-to-ubyte-stream (sout)
    (ubstream:with-input-from-ubyte-stream (sin seq)
      (ctr-hmac-decrypt-stream sin (length seq) sout key))))
  
(defun ctr-hmac-decrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (ctr-hmac-decrypt-stream fin (file-length fin) fout key))))

;; ---------------------------------------------------------------

(defun delivered-ctr-hmac-encrypt ()
  (let ((status 0))
    (handler-case
        (destructuring-bind (progname encdec fromfile tofile key &rest _)
            sys:*line-arguments-list*
          (declare (ignore _))
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
          (if (string-equal encdec "ENC" :end1 3)
              (ctr-hmac-encrypt-file fromfile tofile key)
            (ctr-hmac-decrypt-file fromfile tofile key))
          )
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))

;; ---------------------------------------------------------------
