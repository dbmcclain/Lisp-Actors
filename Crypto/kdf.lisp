;; kdf.lisp -- Key Derivation Function
;;
;; -----------------------------------------------
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

;; ------------------------------------------------------------------------

(defun mask-off (arr rembits)
  (when (plusp rembits)
    (setf (aref arr 0) (ldb (byte rembits 0) (aref arr 0))))
  arr)


#-:COM.RAL
(defstub c-kdf)

(defun apply-c-kdf (nbits keys)
  (apply 'c-kdf nbits keys))

(defun kdf (nbits &rest keys)
  (with-fast-impl
   (apply-c-kdf nbits keys)
   (let* ((nbytes (ceiling nbits 8))
          (ans    (make-ub-array nbytes
                                 :initial-element 0))
          (ctr    0)
          (keys   (mapcar #'ensure-8bitv keys))
          (dig    (ironclad:make-digest :sha256))
          (hash   (make-ub-array 32
                                 :initial-element 0))
          (rembits (rem nbits 8)))
     (labels ((gen-partial (start end)
                (incf ctr)
                (let ((num (ensure-8bitv
                            (convert-int-to-nbytes ctr 4))))
                  (loop repeat 8192 do
                        (reinitialize-instance dig)
                        (apply #'safe-update-digest dig num hash keys)
                        (ironclad:produce-digest dig :digest hash))
                  (replace ans hash :start1 start :end1 end) )))
       
       (loop for start from 0 below nbytes by 32 do
             (let ((nb (min 32 (- nbytes start))))
               (gen-partial start (+ start nb))))
       (mask-off ans rembits) ))))


