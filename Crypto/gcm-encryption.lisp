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

(defvar *gcm-cipher-file-encryption*
  #.(uuid:uuid "{9E6BCEDE-B91A-11E1-8A87-C82A14446EA7}"))

(defun gcm-signature ()
  (uuid:uuid-to-byte-array *gcm-cipher-file-encryption*))

;; ---------------------------------------------------------------

(defclass gcm-cipher (generic-ctr-cipher)
  ((nauth   :accessor gcm-cipher-nauth  :initform 0)
   (ndata   :accessor gcm-cipher-ndata  :initform 0)
   (poly    :accessor gcm-cipher-poly) ;; mac GF128 polynomial
   )
  (:default-initargs
   :sig (gcm-signature)) )

(defmethod shared-initialize :after ((cipher gcm-cipher) slot-names &rest args &key &allow-other-keys)
  (when (and (slot-boundp cipher 'key)
             (slot-boundp cipher 'salt)
             (slot-boundp cipher 'nonce))

    (with-accessors ((ecb   ctr-cipher-ecb)
                     (key   ctr-cipher-key)
                     (salt  ctr-cipher-salt)
                     (nonce ctr-cipher-nonce)
                     (poly  gcm-cipher-poly)
                     (mac   ctr-cipher-mac)
                     (nauth gcm-cipher-nauth)
                     (cvec  ctr-cipher-cvec)) cipher

      (let* ((key3  (kdf 768 key nonce salt)))
        (setf ecb (list (make-ecb-cipher :twofish (subseq key3 0   32))
                        (make-ecb-cipher :aesx    (subseq key3 32  64))
                        (make-ecb-cipher :twofish (subseq key3 64  96)) )
              poly (progn
                     (safe-encrypt-in-place ecb cvec)
                     (convert-bytes-to-int cvec))

              mac   0
              nauth (* 3 16) )) )))

(defmethod update-mac ((cipher gcm-cipher) buf)
  (with-accessors ((x     ctr-cipher-mac)
                   (h     gcm-cipher-poly)) cipher
    (with-gf2^128
      (setf x (gf* h
                   (gf+ x
                        (convert-bytes-to-int buf))) ))))

(defmethod get-mac ((cipher gcm-cipher))
  (with-accessors ((ndata  gcm-cipher-ndata)
                   (nauth  gcm-cipher-nauth)
                   (poly   gcm-cipher-poly)
                   (mac    ctr-cipher-mac)
                   (cvec   ctr-cipher-cvec)) cipher
    (replace cvec (convert-int-to-nbytes (* 8 nauth) 8))
    (replace cvec (convert-int-to-nbytes (* 8 ndata) 8) :start1 8)
    (update-mac cipher cvec)
    (with-gf2^128
      (setf mac (gf+ mac poly)))
    (convert-int-to-nbytesv mac 16) ))

(defmethod cipher-overhead ((cipher gcm-cipher))
  64)

(defmethod mac-length ((cipher gcm-cipher))
  16)

;; ---------------------------------------------------------------

(defmethod wipe-obj :after ((cipher gcm-cipher))
  (setf (gcm-cipher-poly cipher) 0
        (ctr-cipher-mac cipher)  0))

;; ---------------------------------------------------------------

(defun make-gcm-cipher (key &key nonce salt)
  (let* ((nonce  (if nonce
                     (copy-seq nonce)
                   (make-nonce)))
         (salt   (if salt
                     (copy-seq salt)
                   (ctr-drbg 128))))
    
    (make-instance 'gcm-cipher
                   :key   key
                   :nonce nonce
                   :salt  salt) ))

;; ---------------------------------------------------------------

(defmethod encrypt-decrypt-ctr :after ((cipher gcm-cipher) blk)
  ;; encrypt/decrypt one 16-byte (or shorter) block
  (with-accessors ((ndata gcm-cipher-ndata)) cipher
    (incf ndata (length blk)) ))

;; ---------------------------------------------------------------

(defun gcm-encrypt-stream (fin flen fout key)
  (ctr-encrypt-stream-with-cipher fin flen fout
                                  (make-gcm-cipher key)))

(defun gcm-encrypt-sequence (seq key)
  (ubstream:with-output-to-ubyte-stream (sout)
    (ubstream:with-input-from-ubyte-stream (sin (ensure-8bitv seq))
      (gcm-encrypt-stream sin (length seq) sout key))))
  
(defun gcm-encrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (gcm-encrypt-stream fin (file-length fin) fout key) )))

;; ---------------------------------------------------------------

(defun gcm-decrypt-stream (fin flen fout key)
  (ctr-decrypt-stream-with-cipher fin flen fout
                                  (make-instance 'gcm-cipher
                                                 :key key)))

(defun gcm-decrypt-sequence (seq key)
  (ubstream:with-output-to-ubyte-stream (sout)
    (ubstream:with-input-from-ubyte-stream (sin seq)
      (gcm-decrypt-stream sin (length seq) sout key))))
  
(defun gcm-decrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (gcm-decrypt-stream fin (file-length fin) fout key))))

;; ---------------------------------------------------------------

(defun delivered-gcm-encrypt ()
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
          (if (string-equal encdec "ENC" :end1 3)
              (gcm-encrypt-file fromfile tofile key)
            (gcm-decrypt-file fromfile tofile key))
          )
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))

;; ---------------------------------------------------------------
