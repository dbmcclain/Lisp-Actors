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

(defvar *3ctr-hmac-cipher-file-encryption*
  #.(uuid:uuid "{82272E0A-B0C1-11E1-9C55-C82A14446EA7}"))

(defun 3ctr-hmac-signature ()
  (uuid:uuid-to-byte-array *3ctr-hmac-cipher-file-encryption*))

;; ---------------------------------------------------------------

(defclass 3ctr-hmac-cipher (generic-ctr-hmac-cipher)
  ()
  (:default-initargs
   :sig (3ctr-hmac-signature)))    ;; UUID of crypto kind

(defmethod shared-initialize :after ((cipher 3ctr-hmac-cipher) slot-names &rest args &key &allow-other-keys)
  (when (and (slot-boundp cipher 'key)
             (slot-boundp cipher 'salt)
             (slot-boundp cipher 'nonce))
    (with-accessors ((ecb   ctr-cipher-ecb)
                     (mac   ctr-cipher-mac)
                     (key   ctr-cipher-key)
                     (salt  ctr-cipher-salt)
                     (nonce ctr-cipher-nonce)) cipher
      
      (let ((key4  (kdf 1024 key nonce salt)))
        
        (setf ecb  (list (make-ecb-cipher :twofish (subseq key4 0   32))
                         (make-ecb-cipher :aesx    (subseq key4 32  64))
                         (make-ecb-cipher :twofish (subseq key4 64  96)) )
              
              mac  (ironclad:make-hmac (subseq key4 96) :sha256))
        ))))

;; ---------------------------------------------------------------

(defun make-3ctr-hmac-cipher (key &key nonce salt)
  (let* ((nonce  (if nonce
                     (copy-seq nonce)
                   (make-nonce)))
         (salt   (if salt
                     (copy-seq salt)
                   (ctr-drbg 128))))

    (make-instance '3ctr-hmac-cipher
                   :key   key
                   :nonce nonce
                   :salt  salt) ))

;; ---------------------------------------------------------------

(defun 3ctr-hmac-encrypt-stream (fin flen fout key)
  (ctr-encrypt-stream-with-cipher fin flen fout
                                  (make-3ctr-hmac-cipher key)))

(defun 3ctr-hmac-encrypt-sequence (seq key)
  (ubstream:with-output-to-ubyte-stream (sout)
    (ubstream:with-input-from-ubyte-stream (sin (ensure-8bitv seq))
      (3ctr-hmac-encrypt-stream sin (length seq) sout key))))
  
(defun 3ctr-hmac-encrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (3ctr-hmac-encrypt-stream fin (file-length fin) fout key) )))

;; ---------------------------------------------------------------

(defun 3ctr-hmac-decrypt-stream (fin flen fout key)
  (ctr-decrypt-stream-with-cipher fin flen fout
                                  (make-instance '3ctr-hmac-cipher
                                                 :key key)))

(defun 3ctr-hmac-decrypt-sequence (seq key)
  (ubstream:with-output-to-ubyte-stream (sout)
    (ubstream:with-input-from-ubyte-stream (sin seq)
      (3ctr-hmac-decrypt-stream sin (length seq) sout key))))
  
(defun 3ctr-hmac-decrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type 'ubyte)
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'ubyte)
      (3ctr-hmac-decrypt-stream fin (file-length fin) fout key))))

;; ---------------------------------------------------------------

;; -----------------------------------------------------
#+:DECRYPT-DELIVERY
(progn
  (fli:disconnect-module :cryptolib)
  (fli:get-embedded-module :cryptolib
                           (translate-logical-pathname 
                            "PROJECTS:LIB;libLispCrypto.dylib")))

(defun live-connect-to-cryptolib ()
  (fli:install-embedded-module :cryptolib))

;; -----------------------------------------------------

(defun delivered-3ctr-hmac-encrypt ()
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
          (live-connect-to-cryptolib)
          (cond ((string= key "file:" :end1 5)
                 (let ((str (make-string 512)))
                   (with-open-file (f (subseq key 5)
                                      :direction :input)
                     (let ((flen (min (length str)
                                      (file-length f))))
                       (read-sequence str f :start 0 :end flen)
                       (setf key (subseq str 0 flen))))))
                
                ((string= key "hex:" :end1 4)
                 (let ((*read-base* 16))
                   (setf key (read-from-string (subseq key 4)))))
                )
          (if (string-equal encdec "ENC" :end1 3)
              (3ctr-hmac-encrypt-file fromfile tofile key)
            (3ctr-hmac-decrypt-file fromfile tofile key))
          )
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))

;; ---------------------------------------------------------------
