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

(in-package :gflib)

;; ----------------------------------------------------------------

(fli:define-foreign-function (gf128_add #.(c-name "gf128_add") :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf128_mul #.(c-name "gf128_mul") :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf128_div #.(c-name "gf128_div") :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf128_inv #.(c-name "gf128_inv") :source)
    ((op    (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

;; -------------------------------------------------------------------------------

(defmacro deref64 (buf ix)
  `(fli:dereference ,buf :index ,ix :type :uint64))

(defmacro ldb64 (val ix)
  `(ldb (byte 64 ,(* ix 64)) ,val))

;; -------------------------------------------------------------------------------

(defun convert-gf128-int-to-cbuf (val cbuf)
  (setf (deref64 cbuf 0) (ldb64 val 0)
        (deref64 cbuf 1) (ldb64 val 1)))

(defun convert-cbuf-to-gf128-int (cbuf)
  (let ((val 0))
    (setf (ldb64 val 1) (deref64 cbuf 1)
          (ldb64 val 0) (deref64 cbuf 0))
    val))

(defun foreign-gf-buffer (n)
  (fli:allocate-dynamic-foreign-object
   :type :uint8 :nelems n))

(defun foreign-gf128-buffer ()
  (foreign-gf-buffer 16))

(defmacro with-gf-buffers (alloc-fn cvt-fn bindings &body body)
  `(let ,(mapcar (lambda (binding)
                   `(,(if (consp binding)
                          (car binding)
                        binding)
                     (,alloc-fn)))
                 bindings)
     ,@(mapcan (lambda (binding)
                 (when (consp binding)
                   `((,cvt-fn ,(cadr binding) ,(car binding))) ))
               bindings)
     ,@body))

(defmacro with-gf128-buffers (bindings &body body)
  `(with-gf-buffers foreign-gf128-buffer
                    convert-gf128-int-to-cbuf
                    ,bindings
                    ,@body))

;; ----------------------------------------------------------

(defun gf128-binop (cfn a b)
  (fli:with-dynamic-foreign-objects ()
    (with-gf128-buffers ((opnd-a  a)
                         (opnd-b  b)
                         opnd-dst)
      (funcall cfn opnd-a opnd-b opnd-dst)
      (convert-cbuf-to-gf128-int opnd-dst))))

(defun c-gf128-add (a b)
  (gf128-binop #'gf128_add a b))

(defun c-gf128-mul (a b)
  (gf128-binop #'gf128_mul a b))

(defun c-gf128-div (a b)
  (gf128-binop #'gf128_div a b))

(defun gf128-unop (cfn x)
  (fli:with-dynamic-foreign-objects ()
    (with-gf128-buffers ((opnd  x)
                         opnd-dst)
      (funcall cfn opnd opnd-dst)
      (convert-cbuf-to-gf128-int opnd-dst))))

(defun c-gf128-inv (x)
  (gf128-unop #'gf128_inv x))

;; ----------------------------------------------------------------

(fli:define-foreign-function (gf571_add #.(c-name "gf571_add") :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf571_mul #.(c-name "gf571_mul") :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf571_div #.(c-name "gf571_div") :source)
    ((op1   (:pointer :uint8))
     (op2   (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)
  
(fli:define-foreign-function (gf571_inv #.(c-name "gf571_inv") :source)
    ((op    (:pointer :uint8))
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

;; -------------------------------------------------------------------------------

(defun convert-gf571-int-to-cbuf (val cbuf)
  (assert (<= (integer-length val) 571))
  (setf (deref64 cbuf 0) (ldb64 val 0)
        (deref64 cbuf 1) (ldb64 val 1)
        (deref64 cbuf 2) (ldb64 val 2)
        (deref64 cbuf 3) (ldb64 val 3)
        (deref64 cbuf 4) (ldb64 val 4)
        (deref64 cbuf 5) (ldb64 val 5)
        (deref64 cbuf 6) (ldb64 val 6)
        (deref64 cbuf 7) (ldb64 val 7)
        (deref64 cbuf 8) (ldb64 val 8) ))

(defun convert-cbuf-to-gf571-int (cbuf)
  (let ((val 0))
    (setf (ldb64 val 8) (deref64 cbuf 8)
          (ldb64 val 7) (deref64 cbuf 7)
          (ldb64 val 6) (deref64 cbuf 6)
          (ldb64 val 5) (deref64 cbuf 5)
          (ldb64 val 4) (deref64 cbuf 4)
          (ldb64 val 3) (deref64 cbuf 3)
          (ldb64 val 2) (deref64 cbuf 2)
          (ldb64 val 1) (deref64 cbuf 1)
          (ldb64 val 0) (deref64 cbuf 0))
    val))

(defun foreign-gf571-buffer ()
  (foreign-gf-buffer 72))

(defmacro with-gf571-buffers (bindings &body body)
  `(with-gf-buffers foreign-gf571-buffer
                    convert-gf571-int-to-cbuf
                    ,bindings
                    ,@body))

#|
(list *ecc-acudora-private-key*
      (fli:with-dynamic-foreign-objects ()
        (let ((cbuf (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
          (convert-gf571-int-to-cbuf *ecc-acudora-private-key* cbuf)
          (convert-cbuf-to-gf571-int cbuf))))
|#

;; -------------------------------------------------------------------------------

(defun gf571-binop (cfn a b)
  (fli:with-dynamic-foreign-objects ()
    (with-gf571-buffers ((opnd-a  a)
                         (opnd-b  b)
                         opnd-dst)
      (funcall cfn opnd-a opnd-b opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(defun c-gf571-add (a b)
  (gf571-binop #'gf571_add a b))

(defun c-gf571-mul (a b)
  (gf571-binop #'gf571_mul a b))

(defun c-gf571-div (a b)
  (gf571-binop #'gf571_div a b))

(defun gf571-unop (cfn x)
  (fli:with-dynamic-foreign-objects ()
    (with-gf571-buffers ((opnd  x)
                         opnd-dst)
      (funcall cfn opnd opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(defun c-gf571-inv (x)
  (gf571-unop #'gf571_inv x))

#|
(gfinv *ecc-acudora-private-key*)
(gf571-inv *ecc-acudora-private-key*)
(time (dotimes (ix 1000)
        ;; about 2900 per sec
        (gfinv *ecc-acudora-private-key*)))
(time (dotimes (ix 1000)
        ;; about 9900 per sec
        (gf571-inv *ecc-acudora-private-key*)))
|#

#|
(fli:define-foreign-function (gf571_integer_length #.(c-name "gf571_integer_length") :source)
    ((opnd  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module   :cryptolib)

(defun gf571-intlen (x)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (gf571_integer_length opnd))))
|#
#|
(fli:define-foreign-function (gf571_shiftl #.(c-name "gf571_shiftl") :source)
    ((opnd  (:pointer :uint8))
     (nsh   :int)
     (opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

(defun gf571-shiftl (x n)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72))
          (opnd-dst (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (gf571_shiftl opnd n opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(fli:define-foreign-function (gf571_prim #.(c-name "gf571_prim") :source)
    ((opdst (:pointer :uint8)))
  :result-type :void
  :language    :ansi-c
  :module      :cryptolib)

(defun gf571-prim ()
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd-dst (fli:allocate-dynamic-foreign-object
                     :type :uint8 :nelems 72)))
      (gf571_prim opnd-dst)
      (convert-cbuf-to-gf571-int opnd-dst))))

(fli:define-foreign-function (gf571_is_one #.(c-name "gf571_is_one") :source)
    ((opnd  (:pointer :uint8)))
  :result-type :int
  :language :ansi-c
  :module :cryptolib)

(defun gf571-is-one (x)
  (fli:with-dynamic-foreign-objects ()
    (let ((opnd (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 72)))
      (convert-gf571-int-to-cbuf x opnd)
      (gf571_is_one opnd))))

(fli:define-foreign-function (gf571_sizeof_opnd #.(c-name "gf571_sizeof_opnd") :source)
    ()
  :result-type :int
  :language :ansi-c
  :module :cryptolib)
|#

;; -----------------------------------------------------------------------

(fli:define-foreign-function (c_ecc571_setCurve #.(c-name "c_ecc571_setCurve") :source)
    ((a   (:pointer :uint8))
     (b   (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(defun c-ecc571-setCurve (a b)
  (fli:with-dynamic-foreign-objects ()
    (when a
      (with-gf571-buffers ((ca a))
        (c_ecc571_setCurve ca nil)))
    (when b
      (with-gf571-buffers ((cb b))
        (c_ecc571_setCurve nil cb))) ))


(let ((lock   (mp:make-lock :name "ECC Lib Lock"))
      (curve-a nil)  ;; force lib update on first call
      (curve-b nil)) ;; ...make sure we are on the same footing
  
  (defun do-with-ecc-lib (fn)
    (mp:with-lock (lock)
      (unless (eql curve-a *ecc-a*)
        (setf curve-a *ecc-a*)
        (c-ecc571-setCurve curve-a nil))
      (unless (eql curve-b *ecc-b*)
        (setf curve-b *ecc-b*)
        (c-ecc571-setcurve nil curve-b))
      (funcall fn))) )
            
(defmacro with-ecc-lib (&body body)
  `(do-with-ecc-lib (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-ecc-lib" 1)


(fli:define-foreign-function (c_ecc571_add #.(c-name "c_ecc571_add") :source)
    ((x1   (:pointer :uint8))
     (y1   (:pointer :uint8))
     (x2   (:pointer :uint8))
     (y2   (:pointer :uint8))
     (xdst (:pointer :uint8))
     (ydst (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (c_ecc571_sub #.(c-name "c_ecc571_sub") :source)
    ((x1   (:pointer :uint8))
     (y1   (:pointer :uint8))
     (x2   (:pointer :uint8))
     (y2   (:pointer :uint8))
     (xdst (:pointer :uint8))
     (ydst (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(defun chk-c-inf(pt)
  (um:bind* ((:struct-accessors ecc-pt (x y) pt))
    (if (and (zerop x)
             (zerop y))
        (ecc-infinity)
      pt)))

(defun c-ecc571-binop (fn apt bpt)
  (cond ((ecc-infinite-p apt) bpt)
        ((ecc-infinite-p bpt) apt)
        (t
         (with-accessors ((x1  ecc-pt-x)
                          (y1  ecc-pt-y)) apt
           (with-accessors ((x2  ecc-pt-x)
                            (y2  ecc-pt-y)) bpt
             (fli:with-dynamic-foreign-objects ()
               (with-gf571-buffers ((opnd1-x  x1)
                                    (opnd1-y  y1)
                                    (opnd2-x  x2)
                                    (opnd2-y  y2))
                 (with-ecc-lib
                     (funcall fn opnd1-x opnd1-y opnd2-x opnd2-y opnd1-x opnd1-y))
                 (chk-c-inf
                  (make-ecc-pt
                   :x (convert-cbuf-to-gf571-int opnd1-x)
                   :y (convert-cbuf-to-gf571-int opnd1-y)))
                 )))))
        ))
  
(defun c-ecc571-add (apt bpt)
  (c-ecc571-binop #'c_ecc571_add apt bpt))

(defun c-ecc571-sub (apt bpt)
  (c-ecc571-binop #'c_ecc571_sub apt bpt))


(fli:define-foreign-function (c_ecc571_mul #.(c-name "c_ecc571_mul") :source)
    ((x1   (:pointer :uint8))
     (y1   (:pointer :uint8))
     (n    (:pointer :uint8))
     (xdst (:pointer :uint8))
     (ydst (:pointer :uint8))
     (rand (:pointer :uint8)))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(defun gf-random-k* ()
  (let ((ans (ctr-drbg-int *nbits*)))
    (if (zerop ans)
        (gf-random-k*)
      ans)))

(defun c-ecc571-mul (apt n &key alpha)
  (if (ecc-infinite-p apt)
      apt
    (with-accessors ((x  ecc-pt-x)
                     (y  ecc-pt-y)) apt
      (fli:with-dynamic-foreign-objects ()
        (let ((noise   (or alpha
                           (gf-random-k*)))
              (nn      (mod n *ecc-r*)))
          (with-gf571-buffers ((opnd1-x  x)
                               (opnd1-y  y)
                               (opnd2    nn)
                               (rand     noise))
            (with-ecc-lib
                (c_ecc571_mul opnd1-x opnd1-y opnd2 opnd1-x opnd1-y rand))
            (chk-c-inf
             (make-ecc-pt
              :x (convert-cbuf-to-gf571-int opnd1-x)
              :y (convert-cbuf-to-gf571-int opnd1-y)))
            ))))))


#|
(c-ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)
(ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)

(c-ecc-mul *ecc-acudora-public-key* 15)
(ecc-mul *ecc-acudora-public-key* 15)

(c-ecc-add *ecc-acudora-public-key* *ecc-acudora-public-key*)
(ecc-add *ecc-acudora-public-key* *ecc-acudora-public-key*)

;; seeing about 3x speedup going to C
(time (dotimes (ix 1000)
        ;; about 1700 / sec
        (ecc-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)))
(time (dotimes (ix 1000)
        ;; about 8000 / sec
        (c-ecc571-add *ecc-acudora-public-key* *ecc-vtuning-product-public-key*)))

(time (dotimes (ix 10)
        ;; about 8.7/sec
        (ecc-affine-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*)))
(time (dotimes (ix 10)
        ;; about 7.5/sec
        (ecc-projective-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*)))
(time (dotimes (ix 10)
        ;; about 14/sec
        (c-ecc571-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*)))
|#  
