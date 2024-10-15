;; finite-field.lisp -- Prime Field Arithmetic
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
   (montgy-ninv :reader ffbase-montgy-ninv )
   (montgy-rsq  :reader ffbase-montgy-rsq  )
   ))

(defmethod initialize-instance :after ((obj ffbase) &key &allow-other-keys)
  (with-slots (base nbits bits wrap sqrt-fn montgy-ninv montgy-rsq) obj
    (setf nbits (integer-length base)
          bits  (byte nbits 0))
    (let ((r  (ash 1 nbits)))
      (multiple-value-bind (gcd ninv rinv)
          (ff-bezout base r)
        (declare (ignore gcd rinv))
        (setf wrap        (- r base)
              montgy-rsq  (mod (* wrap wrap) base)
              montgy-ninv ninv
              sqrt-fn     (if (= 3. (ldb (byte 2. 0) base)) ;; base = 3 mod 4?
                              (let ((p  (ash (1+ base) -2.)))
                                (lambda (x)
                                  (ff^ x p)))
                            ;; else
                            'tonelli-shanks))
        ))))
  
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
  (let* ((nbits  (ffbase-nbits p))
         (hi     (ash x (the fixnum (- nbits))) ))
    (declare (fixnum nbits)
             (integer hi))
    (if (zerop hi)
        (let* ((base (ffbase-base p))
               (diff (- x base)))
          (declare (integer base diff))
          (if (minusp diff)
              x
            diff))
      ;; else
      (let ((bits (ffbase-bits p))
            (wrap (ffbase-wrap p)))
        (declare (integer wrap))
        (basic-normalize p (+ (the integer (ldb bits x))
                              (the integer (* hi wrap)) )))
      )))

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

(defclass ff-montgomery-mixin ()
  ;; These objects have been scaled to Montgomery form, for use with
  ;; Montgomery multiplication.
  ())

(defun %wrapped-basic-ffld (class x)
  (make-instance class
                 :val x))

(defgeneric copy-ffld (x)
  ;; Copy a field value, or promote an integer to field form.
  (:method ((x ffld))
   (%wrapped-basic-ffld (class-of x) (ffld-val x)))
  (:method ((x integer))
   (%wrapped-basic-ffld (ffbase-inst-class *field*) (basic-normalize *field* x))))

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

(defun need-same-ffield (x y)
  ;; Not only same base for field, but also sameness as Montgomery
  ;; mixin or not.
  (unless (eql (class-of x) (class-of y))
    (error 'ffield-mismatch)))

(defgeneric ffld-base (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-base x))
  (:method ((x integer))
   *field*))

(defgeneric ffld-class (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-class x))
  (:method ((x integer))
   (ffbase-inst-class *field*)))

(defgeneric ffld-montgomery-class (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-montgomery-class x))
  (:method ((x integer))
   (ffld-montgomery-class (make-instance (ffld-class x)))))

(defun %basic-ffld (proto x)
  (declare (type ffld proto))
  (%wrapped-basic-ffld (ffld-class proto) x))

(defun %basic-fmfld (proto x)
  ;; For direct construction of Montomery elements from an integer
  ;; value. Contrast with FFLD, which scales the integer.
  (declare (type ffld proto))
  (%wrapped-basic-ffld (ffld-montgomery-class proto) x))

(defgeneric ffld (proto x)
  (:method ((proto ff-montgomery-mixin) (x integer))
   (ff-to-montgy (%basic-ffld proto (basic-normalize (ffld-base proto) x))))
  (:method ((proto ffld) (x integer))
   (%basic-ffld proto (basic-normalize (ffld-base proto) x)))
  (:method ((proto ffld) x)
   (ffld proto (vec-repr:int x))))

(defgeneric ff-normalize (x)
  (:method ((x ffld))
   ;; Returns the integer value modulo base.
   ;; It also replaces the value inside x with its normalized integer.
   (setf (ffld-val x) (basic-normalize (ffld-base x) (ffld-val x))))
  (:method ((x integer))
   (basic-normalize *field* x)))

(defmethod vec-repr:int ((x ff-montgomery-mixin))
  ;; All instances of a field value represents the same naked integer
  ;; value, regardless of Montgomery scaling or not.
  (vec-repr:int (ff-from-montgy x)))

(defmethod vec-repr:int ((x ffld))
  ;; Normalize, just in case. In most cases this should be gratuitous.
  (ff-normalize x))

(defmethod vec-repr:vec ((x ffld))
  (vec-repr:vec (vec-repr:int x)))

(defmethod hash:hashable ((x ffld))
  ;; We want all instances of the same field value to hash to the same
  ;; hash value. Same as a simple integer without field dressing. So
  ;; no indication of elevated status.
  (hash:hashable (vec-repr:int x)))

(defmacro define-ffield1 (name base)
  (let ((montgy-name  (um:symb name "-MONTGOMERY-FORM")))
    `(progn
       (defclass ,name (ffld)
         ())
       (defclass ,montgy-name (ff-montgomery-mixin ,name)
         ())
       (defmethod ffld-base ((x ,name))
         ,base)
       (defmethod ffld-class ((x ,name))
         ',name)
       (defmethod ffld-montgomery-class ((x ,name))
         ',montgy-name)
       (defgeneric ,name (x)
         (:method ((x ,name))
          x)
         (:method ((x integer))
          (make-instance ',name
                         :val (basic-normalize ,base x)
                         ))
         (:method (x)
          (,name (vec-repr:int x)))
         ))
    ))
  
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
      ;; Montgomery scaling, if present, remains hidden from view.
      (princ (vec-repr:int x) stream))
    ))

;; --------------------------------------------------

(defgeneric ffadd (x y)
  ;; FFADD allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym  (ff-to-montgy y)))
     (need-same-ffield x ym)
     (%basic-fmfld x
                   (+ (ffld-val x) (ffld-val ym)))
     ))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (%basic-ffld x
                  (+ (ffld-val x) (ffld-val ynm)))))
  (:method ((x ffld) (y integer))
   (ffadd x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffadd (ffld y x) y)))

(defgeneric ffadd= (x y)
  ;; FFADD= allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (incf (ffld-val x) (ffld-val ym))
     x))
  (:method ((x ffld) (y ffld))
   (let ((yf  (ff-from-montgy y)))
     (need-same-ffield x yf)
     (incf (ffld-val x) (ffld-val yf))
     x))
  (:method ((x ffld) (y integer))
   (let ((yf (ffld x y)))
     (ffadd= x yf))))
    
(defgeneric ffsub (x y)
  ;; FFSUB allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (%basic-fmfld x
                   (- (ffld-val x) (ffld-val ym)))))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (%basic-ffld x
                  (- (ffld-val x) (ffld-val ynm)))))
  (:method ((x ffld) (y integer))
   (ffsub x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffsub (ffld y x) y)))

(defgeneric ffsub= (x y)
  ;; FFSUB= allows buildup of modular overflow.
  ;; User must normalize at some point afterward.
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (decf (ffld-val x) (ffld-val ym))
     x))
  (:method ((x ffld) (y ffld))
   (let ((yf  (ff-from-montgy y)))
     (need-same-ffield x yf)
     (decf (ffld-val x) (ffld-val yf))
     x))
  (:method ((x ffld) (y integer))
   (let ((yf (ffld x y)))
     (ffsub= x yf))))
  
(defgeneric ffneg (x)
  (:method ((x ffld))
   (let ((ffbase (ffld-base x)))
     (with-accessors ((base ffbase-base)) ffbase
       ;; no change of Montgomery or not
       (make-instance (class-of x)
                      :val (- base (ffld-val x))))
     ))
  (:method ((x integer))
   (ffneg (copy-ffld x))))

(defgeneric ffmul (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (ff-montgy-mul x ym)))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (%basic-ffld x
                  (basic-normalize (ffld-base x)
                                   (* (ffld-val x) (ffld-val ynm)))
                  )))
  (:method ((x ffld) (y integer))
   (ffmul x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffmul (ffld y x) y)))

(defgeneric ffmul= (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (ff-montgy-mul= x ym)
     x))
  (:method ((x ffld) (y ffld))
   (let ((ynm  (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (setf (ffld-val x) (basic-normalize (ffld-base x)
                                         (* (ffld-val x) (ffld-val ynm))))
     x))
  (:method ((x ffld) (y integer))
   (ffmul= x (ffld x y))))


(defgeneric ffsqr (x)
  (:method ((x ffld))
   (ffmul x x))
  (:method ((x integer))
   (ffsqr (copy-ffld x))))

(defmethod ffsqr= ((x ffld))
  (ffmul= x x))

(defgeneric ffinv (x)
  (:method ((x ffld))
   (um:nlet iter ((v  (ffbase-base (ffld-base x)))
                  (u  (ffld-val (ff-from-montgy x))) ;; ensure u >= 0
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
  (:method ((x integer))
   (ffinv (copy-ffld x))))

(defgeneric ffdiv (x y)
  (:method ((x ffld) (y ffld))
   (ffmul x (ffinv y)))
  (:method ((x ffld) (y integer))
   (ffdiv x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffdiv (ffld y x) y)))

;; ---------------------------------------------

(defun ff+ (arg &rest args)
  (let ((ans  (reduce #'ffadd= args
                      :initial-value (copy-ffld arg))))
    ;; sum has been permitted to overflow the modular range
    (ff-normalize ans)
    ans))
         

(defun ff- (arg &rest args)
  (if args
      (let ((ans (reduce #'ffsub= args
                         :initial-value (copy-ffld arg))))
        ;; sum has been permitted to overflow the modular range
        (ff-normalize ans)
        ans)
    (ffneg arg)))

(defun ff* (arg &rest args)
  ;; all Montgy forms after prod remain modular normalized
  (reduce #'ffmul= args
          :initial-value (ff-to-montgy (copy-ffld arg))))

(defun ff/ (arg &rest args)
  (if args
      (ffdiv (ff-to-montgy arg) (apply #'ff* args))
    (ffinv arg)))

;; --------------------------------------------------------------

(defun %redc (ffbase val)
  #F
  ;; val/r mod n : Mongomery reduction
  ;;
  ;; From Bezout's Identity: gcd(a, b) = a'*a + b'*b for some a',b'.
  ;; Function BEZOUT takes a, b and returns gcd(a,b), a', b'.
  ;;
  ;; By using this on prime base n, and its containing r,
  ;;
  ;;    r = 2^m > n > 2^(m-1)
  ;;
  ;; We have:
  ;;
  ;;     n'*n + r'*r = gcd(n,r) = 1,  with 0 < n',r' < n < r
  ;;
  ;; In mod n arithmetic this becomes
  ;;
  ;;    (n'*n + r'*r) mod n = r'*r mod n = 1, so r' = inv(r) mod n.
  ;;
  ;; And in mod r arithmetic,
  ;;
  ;;    (n'*n + r'*r) mod r = n'*n mod r = 1, so n' = inv(n) mod r.
  ;;
  ;; So if we need to compute x*inv(r) mod n:
  ;;
  ;;    x*inv(r) mod n = x*inv(r)*r/r mod n    ;; mult by r/r
  ;;                   = x*(inv(r)*r)/r mod n  ;; regroup
  ;;                   = x*(1 - n'*n)/r mod n  ;; subst inv(r)*r from Bezout
  ;;                   = (x - x*n'*n)/r mod n  ;; expand
  ;;                   = (x - x*n'*n + s*r*n)/r mod n, for arb s
  ;;                   = (x - n*(x*n' - s*r)/r mod n ;; factor n
  ;;                   = (x - n*q)/r mod n     ;; subst q = x*n' - s*r, arb s
  ;;
  ;; But since s is arbitrary, we can compute
  ;;
  ;;     q = (x*n' - s*r) => choose s so that q = x*n' mod r
  ;;                                            = (x mod r) * n' mod r,
  ;;
  ;; i.e., q is low m bits of product, using low m bits of x, n' already < r.
  ;;
  ;; Then, (x - n*q) = (x - n*(x*n' mod r))
  ;;                 = (x - (x*n*n') mod r - k*r)     ;; for some k
  ;;                 = (x - (x*n*inv(n)) mod r - k*r) ;; from Bezout relation, mod r
  ;;                 = (x - x mod r - k*r) => low m bits of difference become zero
  ;; So
  ;;    (x - n*q)/r is simply the right shift by m bits of the difference.
  ;; 
  ;;    x*inv(r) mod n = ((x - n*((x mod r)*n' mod r)) >> m) mod n
  ;;
  ;; Since x < n*n < r*n (even if x is the prod of a mult), and q*n <
  ;; r*n, we know that -n < (x - q*n)/r < n. Therefore, final mod n is
  ;; implemented using a single check and one addition.
  ;;
  (with-accessors ((n     ffbase-base)
                   (ninv  ffbase-montgy-ninv)
                   (nbits ffbase-nbits)
                   (bits  ffbase-bits)) ffbase
    (declare (fixnum nbits)
             (integer n ninv))
    (let* ((q  (ldb bits (the integer (* (the integer (ldb bits val)) ninv))))
           (a  (ash (- val (the integer (* q n))) (the fixnum (- nbits)))))
      (declare (integer q a))
      (if (minusp a)
          (+ a n)
        a)
      )))

(defgeneric ff-to-montgy (x)
  (:method ((x ff-montgomery-mixin))
   x)
  (:method ((x ffld))
   ;; x -> r*x
   (let* ((ffbase (ffld-base x))
          (rsq    (ffbase-montgy-rsq ffbase)))
     (make-instance (ffld-montgomery-class x)
                    :val (%redc ffbase (* (ffld-val x) rsq)))
     ))
  (:method ((x integer))
   (ff-to-montgy (copy-ffld x))))

(defgeneric ff-from-montgy (x)
  (:method ((x ff-montgomery-mixin))
   ;; x -> x/r
   (let ((ffbase (ffld-base x)))
     (make-instance (ffld-class x)
                    :val (%redc ffbase (ffld-val x)))))
  (:method ((x ffld))
   x))

(defmethod ff-montgy-mul ((x ff-montgomery-mixin) (y ff-montgomery-mixin))
  (need-same-ffield x y)
  (let ((ffbase (ffld-base x)))
    (make-instance (class-of x)
                   :val (%redc ffbase (* (ffld-val x) (ffld-val y))))
    ))

(defmethod ff-montgy-sqr ((x ff-montgomery-mixin))
  (ff-montgy-mul x x))

(defmethod ff-montgy-mul= ((x ff-montgomery-mixin) (y ff-montgomery-mixin))
  (need-same-ffield x y)
  (let ((ffbase (ffld-base x)))
    (setf (ffld-val x)  (%redc ffbase (* (ffld-val x) (ffld-val y))))
    x ))

(defmethod ff-montgy-sqr= ((x ff-montgomery-mixin))
  (ff-montgy-mul= x x))

;; -----------------------------------------------------

(defun ff-compare (test-fn x y)
  (funcall test-fn (vec-repr:int x) (vec-repr:int y)))

(defun ff= (x y)
  (ff-compare #'= x y))

(defun ff/= (x y)
  (ff-compare #'/= x y))

(defun ff< (x y)
  (ff-compare #'< x y))

(defun ff<= (x y)
  (ff-compare #'<= x y))

(defun ff> (x y)
  (ff-compare #'> x y))

(defun ff>= (x y)
  (ff-compare #'>= x y))

;; ------------------------------------------------------

(defgeneric fabs (x)
  (:method ((x ffld))
   ;; treat high-half of field as negative values
   (let* ((ans   (copy-ffld (ff-from-montgy x)))
          (xv    (ffld-val ans))
          (field (ffld-base ans))
          (base  (ffbase-base field)))
     (setf (ffld-val ans) (min xv (- base xv)))
     ans))
  (:method ((x integer))
   (fabs (copy-ffld x))))

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
         ((ff< x 2.) ;; x = 0,1
          x)
         (t
          ;; we know that x^(q-1) = 1, so x^(n*(q-1) + r) = x^r
          (let* ((ffbase (ffld-base x))
                 (base   (ffbase-base ffbase))
                 (nred   (mod n (1- base))))
            (declare (integer base nred))
            (case nred
              (0  (ffld x 1))
              (1  x)
              (2. (ffsqr x))
              (t  (generalized-windowed-exponentiation (ff-to-montgy x) nred
                                                       :window-nbits 4.
                                                       :op-mul       'ff-montgy-mul=
                                                       :op-sqr       'ff-montgy-sqr=))
              )))
         ))
  (:method ((x integer) (n integer))
   (ff^ (copy-ffld x) n)))

;; -----------------------------------------------------------------

(define-condition non-square-residue (error)
  ((arg :initarg :arg))
  (:report (lambda (cx stream)
             (format stream "Not a square residue: ~A" (slot-value cx 'arg)))
   ))

(defgeneric ffsqrt (x)
  (:method ((x ffld))
   (let* ((ffbase (ffld-base x))
          (root   (funcall (ffbase-sqrt-fn ffbase) x)))
     (if (ff= x (ffsqr root))
         root
       (error 'non-square-residue :arg x))
     ))
  (:method ((x integer))
   (ffsqrt (copy-ffld x))))

;; ----------------------------------------------------

(defun ff-bezout (a b)
  ;; Extended Euclidean algorithm
  ;; Bezout's identity: gcd(a,b) = s*a + t*b
  (declare (integer a b))
  (um:nlet iter ((r0  a)
                 (r1  b)
                 (s0  1)
                 (s1  0)
                 (t0  0)
                 (t1  1))
    (declare (integer r0 r1 s0 s1 t0 t1))
    (if (zerop r1)
        (values r0  ;; gcd
                s0  ;; gcd(a,b) = a*s + b*t
                t0)
      (multiple-value-bind (q r2) (truncate r0 r1)
        (declare (integer q r2))
        (go-iter r1 r2
                 s1 (- s0 (* q s1))
                 t1 (- t0 (* q t1)))
        ))))

(defgeneric ff-chi (x)
  (:method ((x ffld))
   ;; chi(x) -> {-1,0,+1}
   ;; = +1 when x is square residue
   ;; =  0 when x = 0
   ;; = -1 when x is non-square
   (let ((base  (ffbase-base (ffld-base x))))
     (ffld-val (ff-from-montgy (ff^ x (ash (1- base) -1))))
     ))
  (:method ((x integer))
   (ff-chi (copy-ffld x))))

(defgeneric ff-quadratic-residue-p (x)
  (:method ((x ffld))
   ;; aka Legendre Symbol (x|m)
   (= 1 (ff-chi x)))
  (:method ((x integer))
   (ff-quadratic-residue-p (copy-ffld x))))
          

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
