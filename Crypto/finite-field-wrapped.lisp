;; finite-field-wrapped.lisp -- Prime Field Arithmetic
;; DM/RAL 03/24
;; -----------------------------------------------------
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

;; -----------------------------------------------------

(in-package #:com.ral.crypto.finite-field)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------
;; FFBASE - carries all information about the modular base.  A single
;; instance is referred to by the class of each associated field
;; value.

(defclass ffbase ()
  ((base        :reader ffbase-base        :initarg :base      )
   (inst-class  :reader ffbase-inst-class  :initarg :inst-class)
   (nbits       :reader ffbase-nbits       )
   (bits        :reader ffbase-bits        )
   (wrap        :reader ffbase-wrap        )
   (sqrt-fn     :reader ffbase-sqrt-fn     )
   ))

(defmethod initialize-instance :after ((obj ffbase) &key &allow-other-keys)
  (with-slots (base nbits bits wrap sqrt-fn) obj
    (setf nbits   (integer-length base)
          bits    (byte nbits 0)
          wrap    (- (ash 1 nbits) base)
          sqrt-fn (if (= 3. (ldb (byte 2. 0) base)) ;; base = 3 mod 4?
                      (let ((p  (ash (1+ base) -2.)))
                        (lambda (x)
                          (ff^ x p)))
                    ;; else
                    'tonelli-shanks))
    ))
  
(defmethod print-object ((obj ffbase) stream)
  (if *print-readably*
      (call-next-method)
    (print-unreadable-object (obj stream :type t :identity t)
      (princ (ffbase-base obj) stream))
    ))

(defmethod make-load-form ((obj ffbase) &optional environment)
  (declare (ignore environment))
  `(make-instance 'ffbase
                  :base       ,(ffbase-base obj)
                  :inst-class ',(ffbase-inst-class obj)))

(defmethod hash:hashable ((obj ffbase))
  (hash:hashable (make-load-form obj)))


;; ----------------------------------------------------
;; FFBASE as a verb. Construct or return the associated FFBASE object.
;; Reference is by modular base value, an FFBASE itself, or the symbol
;; designating the field class.

(defgeneric ffbase (x &key inst-class)
  (:method ((base integer) &key inst-class)
   (make-instance 'ffbase
                  :base base
                  :inst-class inst-class))
  (:method ((p ffbase) &key inst-class)
   (declare (ignore inst-class))
   p)
  (:method ((s symbol) &key inst-class)
   (declare (ignore inst-class))
   (ffld-base (make-instance s))))

;; -------------------------------

(defvar *field*)  ;; the current field in use - allows for simple integers in modular expressions

(defmacro with-field (base &body body)
  `(let ((*field*  (ffbase ,base)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-field" 1)

(defun field-base ()
  (ffbase-base *field*))

;; -------------------------------
;;
;; The code in this module allows for indefinite buildup of modular
;; excess on entry and from arithmetic Add/Subtract operations. But
;; products from FMUL and FSQR are always wrapped to no more than the
;; containing size of the modulus. That is, if the prime modulus, q,
;; is 2^(n-1) < q < 2^n, then products are wrapped to no larger than
;; 2^n.
;;
;; To do otherwise, during large exponentiations, which happens for
;; FFSQRT, where exponents are on the order of 2^250 or larger, the
;; gradual buildup of modular excesses, even if only by 1 bit
;; occasionally, end up consuming impossibly large amounts of memory.
;; The system exhibits non-terminating behavior simply because memory
;; quickly exhausts.
;;
;; Actual modulo operations are delayed until you need to read out the
;; value of an FFLD instance, and during comparisons where the true
;; value of the instance is needed.

(defmethod basic-wrap ((p ffbase) (x integer))
  #F
  ;; Simply wrap the hi bits of a large number into the low bits. This
  ;; is not modulo the field base, but rather, modulo 2^n, where
  ;; 2^(n-1) < q < 2^n. Doing so prevents impossibly large buildup of
  ;; modular excesses during large exponentiations.
  ;;
  (let* ((nbits  (ffbase-nbits p))
         (hi     (ash x (the fixnum (- nbits)))))
    (declare (fixnum nbits)
             (integer hi))
    (if (zerop hi)
        x
      (let ((bits (ffbase-bits p))
            (wrap (ffbase-wrap p)))
        (declare (integer wrap))
        (basic-wrap p (+ (the integer (ldb bits x))
                         (the integer (* hi wrap))))
        ))))

(defmethod basic-normalize ((p ffbase) (x integer))
  #F
  ;; Ensure a proper modular value.
  ;;
  ;; ...If you were C and didn't have BIGNUM MOD...
  ;; (Turns out this really is faster than MOD !!
  ;;  this: 0.55 µs, MOD: 1.08 µs, essentially 2x faster)
  ;;
  ;; Iterate wrapping until no more excess MSB, then modulo base.
  ;;
  ;; On entry, integer x may be positive or negative. The LDB
  ;; operation extracts the low bits as an unsigned integer, but the
  ;; ASH is an arithmetic right shift with sign extension.
  ;;
  ;; After enough iterations, the integer argument, x, will be
  ;; positive: 0 <= x < 2^N, and where prime base is:
  ;;    2^(N-1) < BASE < 2^N.
  ;;
  ;; If x is: BASE <= x < 2^N, then DIFF = (x - BASE) will be
  ;; non-negative. Return that DIFF value. Else DIFF will be negative,
  ;; which means that x is already in modular range, so return it..
  ;;
  (let* ((ans  (basic-wrap p x))
         (base (ffbase-base p))
         (diff (- ans base)))
    (declare (integer ans base diff))
    (if (minusp diff)
        ans
      diff)))

;; -----------------------------------------------------
;; FFLD - instance of finite field values. Their class points to the
;; associated FFBASE instance.

(defclass ffld ()
  ;; A basic invariant is that all field values remain properly
  ;; normalized to modular range. However, in a sequence of
  ;; adds/subtracts we permit a modular excess to accumulate in
  ;; intermediate sums. That excess is removed after the sequence has
  ;; ended. Products always remain properly normalized.
  ((val  :accessor ffld-val
         :initarg  :val
         :initform 0)
   ))

(defmethod ffld-val ((x integer))
  x)

(defmethod ffld-val (x)
  (vec-repr:int x))

(defun %wrapped-basic-ffld (class x)
  (make-instance class
                 :val x))

(defgeneric copy-ffld (x)
  ;; Copy a field value, or promote an integer to field form.
  (:method ((x ffld))
   (%wrapped-basic-ffld (class-of x) (ffld-val x)))
  (:method ((x integer))
   (%wrapped-basic-ffld (ffbase-inst-class *field*) x))
  (:method (x)
   (copy-ffld (vec-repr:int x))))

(define-condition subclass-responsibility (error)
  ((fn  :reader subclass-responsibility-fn  :initarg :fn)
   (cls :reader subclass-responsibility-cls :initarg :cls))
  (:report (lambda (cx stream)
             (format stream "METHOD ~A (~A) : Subclass responsiblity"
                     (subclass-responsibility-fn cx)
                     (subclass-responsibility-cls cx)))
   ))

(defun subclass-responsibility (fn-name obj)
  (error 'subclass-responsibility
         :fn  fn-name
         :cls (class-name (class-of obj))))

(define-condition ffield-mismatch (error)
  ())

(defgeneric ffld-base (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-base x))
  (:method (x)
   *field*))

(defgeneric ffld-class (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-class x))
  (:method (x)
   (ffbase-inst-class *field*)))

(defun need-same-ffield (x y)
  ;; Not only same base for field, but also sameness as Montgomery
  ;; mixin or not.
  (unless (eql (ffld-class x) (ffld-class y))
    (error 'ffield-mismatch)))

(defun %basic-ffld (proto x)
  ;; this does not wrap on entry of value x
  (%wrapped-basic-ffld (ffld-class proto) x))

(defgeneric ffld (proto x)
  ;; this wraps on entry of value x
  (:method (proto (x ffld))
   x)
  (:method (proto (x integer))
   (%basic-ffld proto x))
  (:method (proto x)
   (ffld proto (vec-repr:int x))))

(defgeneric ff-normalize (x)
  (:method ((x ffld))
   ;; Returns the integer value modulo base.
   ;; It also replaces the value inside x with its normalized integer.
   (setf (ffld-val x) (basic-normalize (ffld-base x) (ffld-val x))))
  (:method ((x integer))
   (basic-normalize *field* x))
  (:method (x)
   (ff-normalize (vec-repr:int x))))

(defmethod vec-repr:int ((x ffld))
  ;; Normalize, just in case. In most cases this should be gratuitous.
  (ff-normalize x))

(defmethod hash:hashable ((x ffld))
  ;; We want all instances of the same field value to hash to the same
  ;; hash value. Same as a simple integer without field dressing. So
  ;; no indication of elevated status.
  (hash:hashable (vec-repr:int x)))

(defmacro define-ffield1 (name base)
  `(progn
     (defclass ,name (ffld)
       ())
     (defmethod ffld-base ((x ,name))
       ,base)
     (defmethod ffld-class ((x ,name))
       ',name)
     (defgeneric ,name (x)
       (:method ((x ,name))
        x)
       (:method ((x integer))
        (make-instance ',name
                       :val x
                       ))
       (:method (x)
        (,name (vec-repr:int x)))
       )))
  
(defmacro define-ffield (name base)
  ;; Define a modular field. Constructs its FFBASE, define the field
  ;; class, and offer the classname as a constructor verb.
  (let ((base-var (gensym)))
    `(let ((,base-var (ffbase ,base :inst-class ',name)))
       (define-ffield1 ,name ,base-var))
    ))

(defmethod print-object ((x ffld) stream)
  (if *print-readably*
      (call-next-method x stream)
    (print-unreadable-object (x stream :type t :identity t)
      (princ (vec-repr:int x) stream))
    ))

;; --------------------------------------------------

(defun ffadd (x y)
  ;; FFADD allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (need-same-ffield x y)
  (%basic-ffld x
	       (+ (ffld-val x) (ffld-val y))))

(defgeneric ffadd= (x y)
  ;; FFADD= allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (:method ((x ffld) y)
     (need-same-ffield x y)
     (incf (ffld-val x) (ffld-val y))
     x))
    
(defun ffsub (x y)
  ;; FFSUB allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (need-same-ffield x y)
  (%basic-ffld x
	       (- (ffld-val x) (ffld-val y))))

(defgeneric ffsub= (x y)
  ;; FFSUB= allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (:method ((x ffld) y)
   (need-same-ffield x y)
   (decf (ffld-val x) (ffld-val y))
   x))
  
(defun ffneg (x)
  (let ((base (ffbase-base (ffld-base x))))
    (%basic-ffld x (- base (ffld-val x)))))

(defun ffmul (x y)
  (need-same-ffield x y)
  (%basic-ffld x
	       (basic-wrap (ffld-base x)
                           (* (ffld-val x) (ffld-val y)))
	       ))

(defgeneric ffmul= (x y)
  (:method ((x ffld) y)
   (need-same-ffield x y)
   (setf (ffld-val x) (basic-wrap (ffld-base x)
                                  (* (ffld-val x) (ffld-val y))))
   x))

(defun ffsqr (x)
  (ffmul x x))

(defmethod ffsqr= ((x ffld))
  (ffmul= x x))

(defun ffinv (x)
  (um:nlet iter ((v  (ffbase-base (ffld-base x)))
                 (u  (ff-normalize x)) ;; ensure u >= 0
                 (x2 0)
                 (x1 1))
    (declare (integer u v x1 x2))
    (when (zerop u)
      (error "No inverse"))
    (if (= 1 u)
        (ffld x x1)
      ;; else
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (go-iter u r x1 (- x2 (* q x1)))
        ))))

(defun ffdiv (x y)
  (ffmul x (ffinv y)))

;; ---------------------------------------------

(defun ff+ (arg &rest args)
  (reduce #'ffadd= args
          :initial-value (copy-ffld arg)))

(defun ff- (arg &rest args)
  (if args
      (reduce #'ffsub= args
              :initial-value (copy-ffld arg))
    (ffneg arg)))

(defun ff* (arg &rest args)
  (reduce #'ffmul= args
          :initial-value (copy-ffld arg)))

(defun ff/ (arg &rest args)
  (if args
      (ffdiv arg (apply #'ff* args))
    (ffinv arg)))

;; -----------------------------------------------------

(defun ff-compare (test-fn xs)
  (apply test-fn (mapcar #'ff-normalize xs)))

(defun ff= (x &rest ys)
  (ff-compare #'= (cons x ys)))

(defun ff/= (x &rest ys)
  (ff-compare #'/= (cons x ys)))

(defun ff< (x &rest ys)
  (ff-compare #'< (cons x ys)))

(defun ff<= (x &rest ys)
  (ff-compare #'<= (cons x ys)))

(defun ff> (x &rest ys)
  (ff-compare #'> (cons x ys)))

(defun ff>= (x &rest ys)
  (ff-compare #'>= (cons x ys)))

(defun ff0= (x)
  (zerop (ff-normalize x)))

;; ------------------------------------------------------

(defun fabs (x)
  ;; treat high-half of field as negative values
  (let* ((xv    (ff-normalize x))
         (field (ffld-base x))
         (base  (ffbase-base field)))
    (min xv (- base xv))))

;; -----------------------------------------------------
;; Precomputed powers cache

(defclass window-cache ()
  ((precv  :reader   window-cache-precv ;; precomp x^n cache
           :initarg  :precv)
   (x^1    :reader   window-cache-x^1
           :initarg  :x^1)
   (op-sqr :reader   window-cache-op-sqr
           :initarg  :op-sqr)
   (op-mul :reader   window-cache-op-mul
           :initarg  :op-mul)))

(defun make-window-cache (&key nbits x^1 op-mul op-sqr)
  (let ((precv (make-array (ash 1 nbits) :initial-element nil)))
    (setf (aref precv 1) x^1) ;; slot 0 never accessed
    (make-instance 'window-cache
                   :precv  precv
                   :x^1    x^1
                   :op-sqr op-sqr
                   :op-mul op-mul)))

(defmethod get-prec ((wc window-cache) (ix integer))
  ;; compute powers of x^n, n = 1..(2^nbits-1) on demand
  (declare (fixnum ix))
  (with-accessors ((precv  window-cache-precv)
                   (x^1    window-cache-x^1)
                   (op-mul window-cache-op-mul)
                   (op-sqr window-cache-op-sqr)) wc
    (or (aref precv ix)
        (setf (aref precv ix)
              (if (oddp ix)
                  (funcall op-mul (copy-ffld x^1) (get-prec wc (1- ix)))
                (funcall op-sqr (copy-ffld (get-prec wc (ash ix -1)))))
              ))))

;; -----------------------------------------------------
;; Generalized fixed-window exponentiation algorithm
;;
;; Used by both modular exponentiation, and Cipolla algorithm for
;; modular square roots.

(defmethod generalized-windowed-exponentiation (x n &key window-nbits op-mul op-sqr)
  ;; modular x^n using fixed-width window algorithm
  (let* ((ans   nil)
         (wc    (make-window-cache
                 :nbits  window-nbits
                 :x^1    x
                 :op-mul op-mul
                 :op-sqr op-sqr))
         (nbits (integer-length n)))
    (declare (fixnum nbits))
    (loop for pos fixnum from (* window-nbits (floor nbits window-nbits)) downto 0 by window-nbits do
          (when ans
            (loop repeat window-nbits do
                  (setf ans (funcall op-sqr ans))))
          (let ((bits (ldb (byte window-nbits pos) n)))
            (declare (fixnum bits))
            (unless (zerop bits)
              (let ((y (get-prec wc bits)))
                (setf ans (if ans
                              (funcall op-mul ans y)
                            (copy-ffld y))))
              )))
    ans))

;; ---------------------------------------------------------------------------------------------

(defgeneric ff^ (x n)
  (:method ((x ffld) (n integer))
   (cond ((minusp n)
          (ff^ (ffinv x) (- n)))
         ((ff< x 2.) ;; x = 0,1 -- test also normalizes x
          x)
         (t
          ;; we know that x^(q-1) = 1, so x^(n*(q-1) + r) = x^r
          (let* ((base   (ffbase-base (ffld-base x)))
                 (nred   (mod n (1- base))))
            (declare (integer base nred))
            (case nred
              (0  (ffld x 1))
              (1  x)
              (2. (ffsqr x))
              (t  (generalized-windowed-exponentiation x nred
                                                       :window-nbits 4.
                                                       :op-mul       'ffmul=
                                                       :op-sqr       'ffsqr=))
              )))
         ))
  (:method (x (n integer))
   ;; put x into an FFLD to avoid excessive calls to basic-normalize
   ;; on every copy-ffld
   (ff^ (copy-ffld x) n)))

;; -----------------------------------------------------------------

(define-condition non-square-residue (error)
  ((arg :initarg :arg))
  (:report (lambda (cx stream)
             (format stream "Not a square residue: ~A" (slot-value cx 'arg)))
   ))

(defun ffsqrt (x)
  (let* ((ffbase (ffld-base x))
         (root   (funcall (ffbase-sqrt-fn ffbase) x)))
    (if (ff= x (ffsqr root))
        root
      (error 'non-square-residue :arg x))
     ))

;; ----------------------------------------------------

(defun ff-chi (x)
  ;; chi(x) -> {-1,0,+1}
  ;; = +1 when x is square residue
  ;; =  0 when x = 0
  ;; = -1 when x is non-square
  (let ((base (ffbase-base (ffld-base x))))
    (ff-normalize (ff^ x (ash (1- base) -1)))))

(defun ff-quadratic-residue-p (x)
  ;; aka Legendre Symbol (x|m)
  (= 1 (ff-chi x)))
          

#| ;; NOT YET PORTED  DM/RAL 03/24
(defun fast-cipolla (x)
  ;; Cipolla method for finding square root of x over prime field m
  ;; use fixed 4-bit window evaluation
  ;;
  ;; Cipolla defines a quadratic extnsion field, where every value in
  ;; Fq^2 is a square, albeit possibly "imaginary". If a value is a
  ;; square in Fq then it has zero imaginary component in its square
  ;; root in Fq^2. Otherwise, it has zero real part and non-zero
  ;; imaginary part.
  ;;
  ;; This routine will work happily on every field value in Fq, but it
  ;; only returns the real part of the result, which will be zero for
  ;; Fq non-squares.
  ;;
  (declare (integer x))
  (let ((x (mmod x)))
    (declare (integer x))
    (if (< x 2.)
        x
      (multiple-value-bind (re im^2)
          (um:nlet iter ((a  2.))
            ;; look for quadratic nonresidue (the imaginary base)
            ;; where we already know that x must be a quadratic residue
            (declare (integer a))
            (let ((v  (m- (m* a a) x)))
              (declare (integer v))
              (if (quadratic-residue-p v)
                  (go-iter (1+ a))
                (values a v))
              ))
        (declare (integer re im^2))
        (labels
            ;; complex multiplication over the field Fq^2
            ((fq2* (a b)
               (destructuring-bind (are . aim) a
                 (declare (integer are aim))
                 (destructuring-bind (bre . bim) b
                   (declare (integer bre bim))
                   (cons
                    (m+ (m* are bre)
                        (m* aim bim im^2))
                    (m+ (m* are bim)
                        (m* aim bre)))
                   )))
             
             (fq2sqr (a)
               (destructuring-bind (are . aim) a
                 (declare (integer are aim))
                 (cons
                  (m+ (m* are are)
                      (m* aim aim im^2))
                  (m* 2. are aim)))))
          
          (car (generalized-windowed-exponentiation (cons re 1) (m/2u)
                                                    :window-nbits  4.
                                                    :op-mul        #'fq2*
                                                    :op-sqr        #'fq2sqr))
          )))))
|#

;; -----------------------------------------------------------
#| ;; NOT YET PORTED  DM/RAL 03.24
(defun get-tonelli-shanks-params ()
  (get-cached-symbol-data '*m* :tonelli *m*
                          (let ((base *m*))
                            (declare (integer base))
                            (lambda ()
                              (multiple-value-bind (q s)
                                  (um:nlet iter ((q  (1- base))
                                                 (s  0))
                                    (declare (integer q s))
                                    (if (oddp q)
                                        (values q s)
                                      (go-iter (ash q -1)
                                               (1+ s))))
                                (declare (integer q s))
                                (let ((z (um:nlet iter ((x 2.))
                                           ;; on average, about 2 iters
                                           (declare (integer x))
                                           (if (quadratic-residue-p x)
                                               (go-iter (1+ x))
                                             x))))
                                  (declare (integer z))
                                  (list q s z)))))))

(defun tonelli-shanks (arg)
  "Tonelli-Shanks algorithm for Sqrt in prime field"
  (declare (integer arg))
  (let ((x (mmod arg)))
    (declare (integer x))
    (if (< x 2)
        x
      (progn
        #|
        (unless (quadratic-residue-p x)
          (error "Not a quadratic residue"))
        |#
        (destructuring-bind (q s z) (get-tonelli-shanks-params)
          (declare (integer q s z))
          (um:nlet iter ((m  s)
                         (c  (m^ z q))
                         (tt (m^ x q))
                         (r  (m^ x (ash (1+ q) -1))))
            (declare (integer m c tt r))
            (cond ((zerop tt) 0)
                  ((= tt 1)   r)
                  (t
                   (let* ((i  (um:nlet iteri ((i  1)
                                              (x  (msqr tt)))
                                (declare (integer i x))
                                (cond ((= i m)  (error 'non-square-residue :arg arg))
                                      ((= x 1)  i)
                                      (t        (go-iteri (1+ i) (msqr x)))
                                      )))
                          (b  (m^ c (ash 1 (- m i 1))))
                          (new-m  i)
                          (new-c  (msqr b))
                          (new-tt (m* tt new-c))
                          (new-r  (m* r b)))
                     (declare (integer i b new-m new-c new-tt new-r))
                     (go-iter new-m new-c new-tt new-r)))
                  ))
          )))))
|#

#|
  ;; looks like Tonelli-Shanks is the speed winner here, by more than 2:1
  ;; cacheing of precomputed parameters is important, as is the removal of the
  ;; test for quadratic residue, which is detected later.
  
(with-mod 904625697166532776746648320380374280092339035279495474023489261773642975601
  (assert (/= 3 (logand *m* 3))) ;; ensure (q mod 4 != 3)
  (let* ((elts (loop repeat 1000 collect
                    (um:nlet iter ()
                      (let ((x (core-crypto:random-between 1 *m*)))
                        (if (quadratic-residue-p x)
                            x
                          (go-iter)))))))
    (time (map nil 'fast-cipolla elts))
    (time (map nil 'tonelli-shanks elts))))

(unintern '*m*)
 |#

;; ----------------------------------------------------

(defgeneric ffmod (x)
  ;; Returns an integer value in the modular range.
  (:method ((x ffld))
   (ff-normalize x))
  (:method ((x integer))
   (basic-normalize *field* x)))

(defgeneric ff-signed (x)
  ;; return a signed integer corresponding to the treatment of field
  ;; values in the top half modular range as negative values.
  (:method ((x ffld))
   (let ((mxv (vec-repr:int (ffneg x)))
         (xv  (vec-repr:int x)))
     (if (< xv mxv)
         xv
       (- mxv))
     ))
  (:method ((x integer))
   (ff-signed (copy-ffld x))))

#|
(defun msqrt* (arg)
  ;; return an element in the quadratic extension field, if necessary
  (handler-case
      (msqrt arg)
    (non-square-residue ()
      (let* ((im^2 (third (get-tonelli-shanks-params)))
             (k    (msqrt (m/ arg im^2))))
        (cons 0 k)))
    ))
|#

#|
;; interesting...
(defun msqrtx (x)
  (let* ((root     (msqrt x))
         (negroot  (m- root)))
    (if (< (field-reduce root) (field-reduce negroot))
        root
      negroot)))

(defun mmax (a b)
  (m/ (m+ a b
          (msqrtx (msqr (m- a b))))
      2))

(defun mmin (a b)
  (m/ (m- (m+ a b)
          (msqrtx (msqr (m- a b))))
      2))

(defun m>= (a b)
  (m= a (mmax a b)))

(defun m< (a b)
  (not (m>= a b)))
|#
#|
(defun tst (a b)
  (if (< a b)
      (assert (m< a b))
    (if (< b a)
        (assert (m< b a)))))

(with-mod 3618502788666131106986593281521497120414687020801267626233049500247285301239
  (dotimes (ix 1000000)
    (let ((x (core-crypto:random-between 0 *m*))
          (y (core-crypto:random-between 0 *m*)))
      (tst x y))))

(with-mod 3618502788666131106986593281521497120414687020801267626233049500247285301239
  (dotimes (x 1000000)
    (tst x (+ x 123456))))

|#
