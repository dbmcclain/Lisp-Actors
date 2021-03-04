;; subkey-derivation.lisp - BTC-style Child Key Derivation
;;
;; DM/Emotiq 05/18
;; ---------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

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

(defpackage :subkey-derivation
  (:use :common-lisp :vec-repr :hash :pbc)
  (:import-from :useful-macros
   :curry
   :nlet)
  (:export
   :ckd-secret-key
   :ckd-public-key
   :ckd-public-key-for-path
   :ckd-secret-key-for-path
   :public-of-secret
   :ckd
   ))

(in-package :subkey-derivation)

#|
NOTE: Callers of child derivation key functions should be prepared for
the rare event that an error is signaled for some choice of keying
index. This has a likelihood of < 1/2^128 of happening, but it could
happen."
|#

(defmethod public-of-secret ((secret-key secret-key))
  "PUBLIC-OF-SECRET returns the public key corresponding to a secret-key."
  (make-instance 'public-key
                 :val (bev-vec (mul-pt-zr (get-g2)
                                          (int secret-key)))))

(defun check-zero (n)
  "Check for n = 0, and abort if so."
  (if (zerop n)
      (error "Invalid CKD index")
    n))

(defmethod ckd ((parent-pkey public-key) (parent-chain bev) (index integer))
  "CKD - child key derivation function"
  (let* ((ix     (bevn index 4))
         (bytes  (bev-vec (hash/512 parent-chain parent-pkey ix)))
         (subpt  (mul-pt-zr (get-g2)
                            (check-zero (int (bev (subseq bytes 0 32))))))
         (sumpt  (add-pts parent-pkey subpt))
         (cpkey  (make-instance 'public-key
                                :val (bev-vec sumpt)))
         (cchain (bev (subseq bytes 32))))
    (values cpkey cchain)))
  
(defmethod ckd-secret-key ((parent-skey secret-key) (parent-chain bev) (index integer))
  "CKD (Child Key Derivation) of child secret key and next chain code.
NOTE: The chain code resulting from this call will match the chain
code that results from calling ckd-public-key with the same
parent-chain and index for same parent keying."
  (let* ((ix     (bevn index 4))
         (data   (if (logbitp 31 index) ;; hard key?
                   parent-skey
                 (public-of-secret parent-skey)))
         (bytes  (bev-vec (hash/512 parent-chain data ix)))
         (cskey  (make-instance 'secret-key
                                :val (bev-vec
                                      (add-zrs (int parent-skey)
                                               (check-zero (int (bev (subseq bytes 0 32))))))))
         (cchain (bev (subseq bytes 32))))
    (values cskey cchain)))

(defmethod ckd-public-key ((parent-pkey public-key) (parent-chain bev) (index integer))
  "CKD (Child Key Derivation) of child public key and next chain code.
NOTE: The chain code resulting from this call will match the chain
code that results from calling ckd-public-key with the same
parent-chain and index for same parent keying."
  (when (logbitp 31 index)
    (error "Can't create hardened public child key"))
  (ckd parent-pkey parent-chain index))

;; ----------------------------------------------------------------------------------

(defun convert-string-path-to-int-path (str)
  (let ((*read-eval* nil))
    (mapcar 'read-from-string (um:split-string str :delims '(#\/)))))

(defmethod ckd-public-key-for-path (parent-pkey parent-chain (path string))
  (ckd-public-key-for-path parent-pkey parent-chain
                           (convert-string-path-to-int-path path)))

(defmethod ckd-pubic-key-for-path ((parent-pkey public-key) (parent-chain bev) (path list))
  (assert (every 'integerp path))
  (values-list
   (reduce (lambda (ans ix)
             (destructuring-bind (pkey chain) ans
                 (multiple-value-list (ckd-public-key pkey chain ix))))
           path
           :initial-value (list parent-pkey parent-chain))))

(defmethod ckd-secret-key-for-path (parent-skey parent-chain (path string))
  (ckd-secret-key-for-path parent-skey parent-chain
                           (convert-string-path-to-int-path path)))

(defmethod ckd-secret-key-for-path ((parent-skey secret-key) (parent-chain bev) (path list))
  (assert (every 'integerp path))
  (values-list
   (reduce (lambda (ans ix)
             (destructuring-bind (skey chain) ans
               (multiple-value-list (ckd-secret-key skey chain ix))))
           path
           :initial-value (list parent-skey parent-chain))))

;; ---------------------------------------------------------------------------------

#|
  ;; test it out...
(let* ((k (make-key-pair :test))
       (c (bev (hash/256 :another-test))))
  (multiple-value-bind (cskey cchain)
      (ckd-secret-key (keying-triple-skey k) c 1)
    (multiple-value-bind (cpkey cchain2)
        (ckd-public-key (keying-triple-pkey k) c 1)
      (assert (vec= (public-of-secret cskey) cpkey))
      (assert (vec= cchain cchain2)))))
|#
