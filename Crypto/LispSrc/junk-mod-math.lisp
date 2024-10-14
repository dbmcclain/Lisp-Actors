;; modular-arith.lisp -- Prime Field Arithmetic
;; DM/RAL 03/18
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

(in-package :crypto/modular-arith)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------

(defvar *m*  1) ;; current mod base

(defstruct monty-form
  val)

;; define primitive access to integer values
;; without conversion to/from monty-form scaled values
(defmethod int-val ((xm monty-form))
  (monty-form-val xm))

(defmethod int-val ((x integer))
  x)

;; ---------------------------------------------

(declaim (integer *m*)
         (inline mmod m-1 m/2l m+1 m/2u rsh))

(defmacro with-mod (base &body body)
  `(let ((*m* ,base))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-mod" 1)

(defun rsh (x nsh)
  ;; right shift x by nsh bits
  (declare (integer x)
           (fixnum  nsh))
  (ash x (- nsh)))

(defmethod range-reduce ((x integer))
  ;; bring x into range 0 <= x < *m* (= mod base)
  (if (and (<= 0 x)
           (< x *m*))
      x
    (mmod x)))

(defun mmod (x)
  (declare (integer x))
  (mod x *m*))

(defun m-1 ()
  (1- *m*))

(defun m/2l ()
  (truncate (m-1) 2))

(defun m+1 ()
  (1+ *m*))

(defun m/2u ()
  (truncate (m+1) 2))

(defun m! (m)
  ;; for REPL convenience, so we don't have to keep doing WITH-MOD
  (setf *m* m))

;; --------------------------------------------------------------
;; Montgomery forms

(defconstant +monty-exp+  30) ;; small enough for squared max to be fixnum

(defun get-monty-info (&rest arglist)
  (let* ((m  *m*)
         (lst  (get-cached-symbol-data '+monty-exp+ :info m
                                       (lambda ()
                                         ;; We code carefully to avoid overt
                                         ;; modular arithmetic functions except
                                         ;; for mod.  The Montgomery primitives
                                         ;; will depend on this info.
                                         (let* ((limit  (ceiling (integer-length m) +monty-exp+))
                                                (byt    (ash 1 +monty-exp+))
                                                (r      (range-reduce (ash 1 (* limit +monty-exp+))))
                                                (r^2    (range-reduce (* r r)))
                                                (byte-0 (byte +monty-exp+ 0))
                                                (mp     (with-mod byt
                                                          (- byt (%minv-int m)))))
                                           ;; we never really need byt and r, but here for debug
                                           (list
                                            :limit  limit   ;; count of 30-bit blocks per number
                                            :mprime mp      ;; = -1/m mod byte  (byte = 30 bit block)
                                            :byte-0 byte-0  ;; = (byte 30 0)
                                            :byte   byt     ;; value of 2^30
                                            :r      (make-monty-form ;; pseudo Monty form for R
                                                                     :val r)
                                            :r^2    (make-monty-form ;; pseudo Monty form for R^2
                                                                     :val r^2))
                                           ))
                                       )))
    (if arglist
        (mapcar (um:curry 'getf lst) arglist)
      lst)))

;; -------------------------------------------------------------------
;; special values in Montgomery form

(defun monty-zero ()
  (make-monty-form
   :val 0))

(defun monty-one ()
  ;; = 1*R in monty-form
  (destructuring-bind (r) (get-monty-info :r)
    r))

(defmethod monty= ((m1 monty-form) (m2 monty-form))
  (= (monty-form-val m1) (monty-form-val m2)))

(defmethod monty= ((m1 integer) (m2 monty-form))
  (= m1 (to-int-form m2)))

(defmethod monty= ((m1 monty-form) (m2 integer))
  (= (to-int-form m1) m2))

(defgeneric monty* (a b))

;; -----------------------------------------------------

(defvar *blinders* (make-hash-table))

(defun create-blinder (m)
  (declare (integer m))
  (* m (ecc-crypto-b571:random-between #.(ash 1 31) #.(ash 1 32))))

(defun get-blinder (&optional (m *m*))
  (declare (integer m))
  (or 0 ;; no blinding...
      (gethash m *blinders*)
      (setf (gethash m *blinders*) (create-blinder m))))

(defun get-monty-blinder (&optional (m *m*))
  (make-monty-form
   :val (get-blinder m)))

(defun reset-blinders ()
  (clrhash *blinders*))

;; -----------------------------------------------------
;; Prime-Field Arithmetic

(defmethod m^ ((base integer) exponent)
  (let ((x (range-reduce base)))
    (declare (integer x))
    (if (< x 2) ;; x = 0,1
        x
      (m^ (to-monty-form (+ x (get-blinder))) exponent))
    ))

(defmethod m^ ((base monty-form) exponent)
  ;; base^exponent mod modulus, for any modulus
  (let* ((exp (+ (to-int-form exponent) (get-blinder (m-1))))
         (n   (integer-length exp)))
    (declare (fixnum n)
             (integer exp))
    (do ((b  base
             (monty* b b))
         (p  (monty-one))
         (ix 0    (1+ ix)))
        ((>= ix n) p)
      (declare (monty-form b p)
               (fixnum ix))
      (when (logbitp ix exp)
        (setf p (monty* p b))))
    ))

;; ------------------------------------------------------------

(defun mchi (x)
  ;; chi(x) -> {-1,+1}
  ;; = +1 when x is square residue
  (m^ x (m/2l)))

(defun quadratic-residue-p (x)
  ;; aka Legendre Symbol (x|m)
    (monty= (monty-one) (mchi x)))

(defvar *fq2-red*)

(defstruct fq2
  x y)

#| ;; not needed...
(defun fq2+ (a b)
  (um:bind* ((:struct-accessors fq2 ((ax x) (ay y)) a)
             (:struct-accessors fq2 ((bx x) (by y)) b))
    (make-fq2
     :x (m+ ax bx)
     :y (m+ ay by))))
|#

(defun fq2* (a b)
  (um:bind* ((:struct-accessors fq2 ((ax x) (ay y)) a)
             (:struct-accessors fq2 ((bx x) (by y)) b))
    (make-fq2
     :x (m+ (m* ax bx)
            (m* ay by (cadr *fq2-red*)))
     :y (m+ (m* ax by)
            (m* ay bx))
     )))

(defun cipolla (x)
  (declare (integer x))
  ;; Cipolla method for finding square root of x over prime field m
  (let* ((*fq2-red* (um:nlet iter ((a  2))
                      (declare (integer a))
                      (let ((v  (range-reduce (- (* a a) x))))
                        (declare (integer v))
                        (if (quadratic-residue-p v)
                            (go-iter (1+ a))
                          (list a v))
                        )))
         ;; exponentiation in Fq^2: (a, sqrt(a^2 - n))^((q-1)/2)
         (one  (monty-one))
         (xx   (make-fq2
                :x (to-monty-form (car *fq2-red*))
                :y one))
         (exp  (m/2u))
         (n    (integer-length exp)))
    (declare (fixnum n)
             (integer exp))
    (do ((b  xx
             (fq2* b b))
         (p  (make-fq2
              :x one
              :y (monty-zero)))
         (ix 0    (1+ ix)))
        ((>= ix n) (fq2-x p))
      (declare (fixnum ix))
      (when (logbitp ix exp)
        (setf p (fq2* p b))))
    ))

(defun get-msqrt-fn ()
  (get-cached-symbol-data '*m* :msqrt *m*
                          (lambda ()
                            (cond
                             ((= 3 (mod *m* 4))
                              (let ((p  (truncate (m+1) 4)))
                                (um:rcurry 'm^ p)))
                             (t 'cipolla)))))

(defun %msqrt (x)
  (declare (integer x))
  (if (< x 2)
      x
    ;; else
    (let ((ix (isqrt x)))
      (declare (integer ix))
      (cond ((= x (* ix ix)) ix)
            ((quadratic-residue-p x)
             (let* ((fn (get-msqrt-fn))
                    (xr (funcall fn x)))
                 (declare (integer xr))
                 (assert (monty= x (m* xr xr)))
                 xr))
            
            (t (error "not a square"))
            ))))

(defmethod msqrt ((x integer))
  ;; assumes m is prime
  ;; a^(m-1) = 1 for m prime
  ;; a^m = a
  ;; a^(m+1) = a^2
  ;; a^((m+1)/4) = a^(1/2) -- works nicely when m = 3 mod 4
  ;; 1/2 = 2/4 = 3/6 = 4/8 = 5/10 = 6/12 = 7/14 = 8/16
  ;; in general:  for m = (2k+1) mod 4k, use (em + (2k-1))/4k, k = 1,2,...
  (%msqrt (range-reduce x)))

(defmethod msqrt ((x monty-form))
  (to-monty-form (%msqrt (to-int-form x))))

;; ------------------------------------------------------------

(defmethod m* ((arg integer) &rest args)
  (if (some 'monty-form-p args)
      (apply 'm* (to-monty-form arg) args)
    ;; else -- all integers
    (let* ((blinder (get-blinder)))
      (declare (integer blinder))
      (dolist (opnd args arg)
        (declare (integer opnd))
        (setf arg (mmod (* arg (+ opnd blinder)))))
      )))

(defmethod m* ((arg monty-form) &rest args)
  (dolist (opnd args arg)
    (setf arg (monty* opnd arg))))

;; ------------------------------------------------------------

(defmethod m+ ((arg integer) &rest args)
  (if (some 'monty-form-p args)
      (apply 'm+ (to-monty-form arg) args)
    ;; else - all integers
    (let* ((blinder (get-blinder))
           (ans     (+ arg blinder)))
      (declare (integer blinder ans))
      (dolist (opnd args)
        (declare (integer opnd))
        (incf ans (+ opnd blinder)))
      (mmod ans))))

(defmethod m+ ((arg monty-form) &rest args)
  (let ((ans (monty-form-val arg)))
    (dolist (opnd (mapcar 'to-monty-form args))
      (declare (monty-form opnd))
      (incf ans (monty-form-val opnd)))
    (unless (< ans *m*)
      (decf ans *m*))
    (make-monty-form
     :val ans)))

(defmethod m- ((arg integer) &rest args)
  (if (some 'monty-form-p args)
      (apply 'm- (to-monty-form arg) args)
    ;; else -- all integers
    (let* ((blinder (get-blinder))
           (ans     (- arg blinder)))
      (declare (integer blinder ans))
      (if args
          (dolist (opnd args)
            (declare (integer opnd))
            (decf ans (+ opnd blinder)))
        (setf ans (- ans)))
      (mmod ans))))

(defmethod m- ((arg monty-form) &rest args)
  (let ((ans (monty-form-val arg)))
    (dolist (opnd (mapcar 'to-monty-form args))
      (declare (monty-form opnd))
      (decf ans (monty-form-val opnd)))
    (when (minusp ans)
      (incf ans *m*))
    (make-monty-form
     :val ans)))

;; ------------------------------------------------------------

(defmethod minv ((a integer))
  (%minv (range-reduce a)))

(defmethod minv ((a monty-form))
  ;; we are given a*R and must produce (1/a)*R
  (%minv (to-int-form a)))

(defun %minv (u)
  ;; Euclid's algorithm
  ;; (or Chinese Remainder Theorem, can't remember which...)
  (declare (integer u))
  (let* ((v  *m*)
         (x1 (monty-one))
         (x2 (monty-zero)))
    (declare (integer v))
    (do ()
        ((= u 1) x1)
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (m- x2 (monty* q x1))))
          (shiftf v u r)
          (shiftf x2 x1 x)) ))
    ))

(defun %minv-int (u)
  ;; special int-only version needed for setting up Montgomery info
  (declare (integer u))
  (let* ((v  *m*)
         (x1 1)
         (x2 0))
    (declare (integer v x1 x2))
    (do ()
        ((= u 1) x1)
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (mmod (- x2 (* q x1)))))
          (shiftf v u r)
          (shiftf x2 x1 x)) ))
    ))

(defun m/ (arg &rest args)
  (dolist (opnd args
                (if args arg (minv arg)))
    (setf arg (m* (minv opnd) arg))))

;; --------------------------------------------------------------

(defmethod monty-reduction ((xm monty-form))
  ;; produce xval/R
  ;; produce plain integer value for xm,
  ;; scales monty-form x*R by 1/R mod q -> x
  (let ((xv (monty-form-val xm))
        (m  *m*))
    (declare (integer xv m))
    (destructuring-bind (limit mp byte-0) (get-monty-info :limit :mprime :byte-0)
      (declare (fixnum limit mp))
      (let ((a  xv))
        (declare (integer a))
        (loop for ix fixnum from 0 below limit
              do
              (let ((u (ldb byte-0 (the fixnum (* (the fixnum (ldb byte-0 a))
                                                  mp)))))
                (declare (fixnum u))
                (setf a (rsh (+ a (* u m)) +monty-exp+))
                ))
        (unless (< a m)
          (decf a m))
        a))))

(defgeneric %monty-rev* (x y))

(defmethod monty* ((x integer) y)
  (%monty-rev* y (to-monty-form x)))

(defmethod monty* ((x monty-form) y)
  (%monty-rev* y x))

(defmethod %monty-rev* ((y integer) x)
  (%monty* x (to-monty-form y)))

(defmethod %monty-rev* ((y monty-form) x)
  (%monty* x y))

(defun %monty* (xm ym)
  ;; produce monty-form for x*R*y*R/R mod q = x*y*R mod q
  (let ((m  *m*)
        (xv (monty-form-val xm))
        (yv (monty-form-val ym)))
    (declare (integer xv yv m))
    (destructuring-bind (limit mp byte-0) (get-monty-info :limit :mprime :byte-0)
      (declare (fixnum limit mp))
      (let ((a  0)
            (y0 (ldb byte-0 yv)))
        (declare (fixnum y0)
                 (integer a))
        (loop for ix fixnum from 0 below limit
              for pos fixnum from 0 by +monty-exp+
              do
              (let* ((xi (ldb (byte +monty-exp+ pos) xv))
                     (ui (ldb byte-0
                              (the fixnum
                                   (* mp
                                      (the fixnum
                                           (+ (the fixnum (ldb byte-0 a))
                                              (the fixnum (* xi y0)))))
                                   ))))
                (declare (fixnum xi ui))
                (setf a (rsh (+ a (* xi yv) (* ui m)) +monty-exp+))
                ))
        (unless (< a m)
          (decf a m))
        (make-monty-form
         :val a)
        ))))

;; --------------------------------------------------------------------
;; Conversion to Montgomery form

(defmethod to-monty-form ((x integer))
  ;; produce Monty form x*R
  (destructuring-bind (r^2) (get-monty-info :r^2)
    (monty* (make-monty-form
              :val (range-reduce x))
             r^2)))

(defmethod to-monty-form  ((x monty-form))
  x)

;; --------------------------------------------------------------------
;; Conversion back to raw integer form

(defmethod to-int-form ((x integer))
  x)

(defmethod to-int-form ((x monty-form))
  ;; produce integer corresponding to monty-form value
  (monty-reduction x))

