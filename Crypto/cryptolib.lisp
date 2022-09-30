;; crypto-le.lisp -- interface to C++ crypto dylib
;; adjusted for Little-Endian storage of bignums
;;
;; DM/Acudora 06/12
;; ----------------------------------------------------------------
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

(in-package :cryptolib)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *dylib-version* "")
  (defun c-name (str)
    (concatenate 'string str *dylib-version*)))

(fli:disconnect-module :cryptolib :remove t)
(fli:register-module :cryptolib
                     ;; :connection-style :immediate
                     :dlopen-flags t ;; non-nil needed for Mac to unload dylib on disconnect-module
                     :real-name
                     (merge-pathnames
                      (concatenate 'string "libLispCrypto" *dylib-version*
                                   #+:MAC   ".dylib"
                                   #+:WIN32 ".dll")
                      (translate-logical-pathname "PROJECTS:LIB;xxx")))

(fli:define-c-struct sha2_context
  (total  (:foreign-array :uint32  (2)))
  (state  (:foreign-array :uint32  (8)))
  (buffer (:foreign-array :uint8  (64)))
  (ipad   (:foreign-array :uint8  (64)))
  (opad   (:foreign-array :uint8  (64)))
  (is224  :int))

(fli:define-foreign-function (sha2_starts #.(c-name "sha2_starts") :source)
    ((ctx  (:pointer sha2_context))
     (:constant 0 :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2_update #.(c-name "sha2_update") :source)
    ((ctx   (:pointer sha2_context))
     (input (:pointer :uint8))
     (ilen  :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2_finish #.(c-name "sha2_finish") :source)
    ((ctx    (:pointer sha2_context))
     (output (:pointer :uint8))) ;; should point to 32 bytes
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2 #.(c-name "sha2") :source)
    ((input  (:pointer :uint8))
     (ilen   :int)
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2_file #.(c-name "sha2_file") :source)
    ((path   (:reference-pass (:ef-mb-string :limit 256)))
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :int
  :language :ansi-c
  :module   :cryptolib)

(fli:define-foreign-function (shad2_file #.(c-name "shad2_file") :source)
    ((path   (:reference-pass (:ef-mb-string :limit 256)))
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :int
  :language :ansi-c
  :module   :cryptolib)

(fli:define-foreign-function (sha2_self_test #.(c-name "sha2_self_test") :source)
    ((verbose :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (sha2-hmac #.(c-name "sha2_hmac") :source)
    ((key    (:pointer :uint8))
     (klen   :int)
     (input  (:pointer :uint8))
     (ilen   :int)
     (output (:pointer :uint8)) ;; should point to 32 bytes
     (:constant 0 :int))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

;; ------------------------------------------------------------------

(fli:define-c-struct aes_context
  (nr     :int)
  (rk     (:pointer :uint32))
  (buf    (:foreign-array :uint32 (68))))

(fli:define-foreign-function (aes_setkey_enc #.(c-name "aes_setkey_enc") :source)
    ((ctx  (:pointer aes_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (aes_setkey_dec #.(c-name "aes_setkey_dec") :source)
    ((ctx  (:pointer aes_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aes_crypt_cbc #.(c-name "aes_crypt_cbc") :source)
    ((ctx     (:pointer aes_context))
     (mode    :int)
     (length  :int)
     (iv      (:pointer :uint8)) ;; should point to 16 bytes
     (input   (:pointer :uint8))
     (output  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aes_self_test #.(c-name "aes_self_test") :source)
    ((verbose :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

;; ------------------------------------------------------------------

(fli:define-c-struct aesx_context
  (nr     :int)
  (rk     (:pointer :uint32))
  (buf    (:foreign-array :uint32 (128))))

(fli:define-foreign-function (aesx_setkey_enc #.(c-name "aesx_setkey_enc") :source)
    ((ctx  (:pointer aesx_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (aesx_setkey_dec #.(c-name "aesx_setkey_dec") :source)
    ((ctx  (:pointer aesx_context))
     (key  (:pointer :uint8))
     (keysize :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aesx_crypt_cbc #.(c-name "aesx_crypt_cbc") :source)
    ((ctx     (:pointer aesx_context))
     (mode    :int)
     (length  :int)
     (iv      (:pointer :uint8)) ;; should point to 16 bytes
     (input   (:pointer :uint8))
     (output  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
    
(fli:define-foreign-function (aesx_self_test #.(c-name "aesx_self_test") :source)
    ((verbose :int))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

;; ------------------------------------------------------------------

(defun return-digest (dig)
  (let ((lisp-dig (make-ub-array 32)))
    (fli:replace-foreign-array lisp-dig dig
                               :start2 0 :end2 32)
    lisp-dig))

(defun c-sha2-file (fname)
    (fli:with-dynamic-foreign-objects ()
      (let ((dig (fli:allocate-dynamic-foreign-object
                  :type :uint8 :nelems 32)))
        (sha2_file (namestring fname) dig)
        (return-digest dig)) ))

(defun update-digest-from-buffer (ctx dig buf diglen)
  (let* ((len (length buf)))
    (loop for pos from 0 below len by diglen do
          (let ((nb (min diglen (- len pos))))
            (fli:replace-foreign-array dig buf
                                       :start1 0
                                       :end1   nb
                                       :start2 pos
                                       :end2   (+ pos nb))
            (sha2_update ctx dig nb))) ))

(defun c-sha2-buffers (&rest buffers)
  (fli:with-dynamic-foreign-objects ((ctx sha2_context))
    (let ((dig (fli:allocate-dynamic-foreign-object
                :type :uint8  :nelems 32)))
      (sha2_starts ctx)
      (dolist (buf buffers)
        (update-digest-from-buffer ctx dig buf 32))
      (sha2_finish ctx dig)
      (return-digest dig) )))

(defun c-sha2-buffers-into (ans &rest buffers)
  (fli:with-dynamic-foreign-objects ((ctx sha2_context))
    (let ((dig (fli:allocate-dynamic-foreign-object
                :type :uint8  :nelems 32)))
      (sha2_starts ctx)
      (dolist (buf buffers)
        (update-digest-from-buffer ctx dig buf 32))
      (sha2_finish ctx dig)
      (fli:replace-foreign-array ans dig
                                 :start1 0
                                 :end1   32
                                 :start2 0
                                 :end2   32)
      ans)))

#|
;; compare against Ironclad
(let* ((str "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
       #|
       (vec (let ((vec (make-array (length str)
                                   :element-type '(unsigned-byte 8))))
              (loop for ix from 0 below (length str) do
                    (setf (aref vec ix) (char-code (char str ix))))
              vec))
       |#
       (vec  (map '(ub-vector *) #'char-code str))
       (ans1 (c-sha2-buffers vec))
       (ans2 (let ((dig (ironclad:make-digest :sha256)))
               (ironclad:update-digest dig vec)
               (ironclad:produce-digest dig))))
  (terpri) (hex ans1)
  (terpri) (hex ans2)
  (equalp ans1 ans2))

|#

;; Seeing about a 7x speedup going to C
(defun c-kdf (nbits &rest keys)
  (let* ((nbytes (ceiling nbits 8))
         (ans    (make-ub-array nbytes
                                :initial-element 0))
         (ctr    0)
         (keys   (mapcar #'ensure-8bitv (um:flatten keys)))
         (hash   (make-ub-array 32
                                :initial-element 0))
         (rembits (rem nbits 8)))
    (labels ((gen-partial (start end)
               (incf ctr)
               (let ((num (ensure-8bitv
                           (convert-int-to-nbytes ctr 4))))
                 (loop repeat 8192 do
                       (apply #'c-sha2-buffers-into hash num hash keys))
                 (replace ans hash :start1 start :end1 end) )))

      (loop for start from 0 below nbytes by 32 do
            (let ((nb (min 32 (- nbytes start))))
              (gen-partial start (+ start nb))))
      (mask-off ans rembits) )))

#|
(time (dotimes (ix 10)
        (kdf 571
             (ecc-pt-x *ecc-acudora-public-key*)
             (ecc-pt-y *ecc-acudora-public-key*))))
(time (dotimes (ix 10)
        (c-kdf 571
               (ecc-pt-x *ecc-acudora-public-key*)
               (ecc-pt-y *ecc-acudora-public-key*))))
|#

;; ----------------------------------------------------------------

