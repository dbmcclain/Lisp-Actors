
(in-package :edec)

#|
In the field of curve1174 the first small non-square residue occurs at
11. So define a quadratic extension field based on this as our "β"
value.

That is, define β to be that value for which β^2 = 11 mod r, where r =
*ed-r*. And we know (?!) that β^(r-1) = -1 mod r, whereas for all
other field members x in [1,r) we have x^(r-1) = 1..

Numbers are represented as (a,b) pairs, which means (a + b*β). Then to
multiply two numbers n1 = (a, b) and n2 = (c, d) we distribute their
product of sums to get:

   n1 * n2 = (a + b*β) * (c + d*β) = (a*c + b*d*β^2) + (a*d + b*c)*β = n3

Since we are operating in a prime field, we know that x^r = x, for any
x in [1..r). What happens for our composite numbers?

   (a + b*β)^r = a^r + r*a^(r-1)*b*β + ... + b^r*β^r
               = a + 0 + ... + b*β^r = (a - b*β)

since all interior terms with r* leading become 0 mod r. Binomial
coefficients ( n | m ) = n!/(m!*(n-m)!), all have leading n* except for
the first and last terms, which both have coefficients of 1.


    β^(2*r) = (β^2)^r = 11^r = 11

|#
#|
(modr
  (m^ 11 (/ (1- *ed-r*) 2)))
(modr (mmod -1))
(modr
  (m^ 11 (/ (1+ *ed-r*) 2)))
(modr
  (/ (1+ *ed-r*) 2))
(mod *ed-r* 4)

(defun qfexp (n exp)
  (labels ((cm* (a b)
             (destructuring-bind (ar . ai) a
               (destructuring-bind (br . bi) b
                 (cons
                  (m+ (m* ar br) (m* ai bi 11))
                  (m+ (m* ar bi) (m* ai br)))
                 )))
           (cmsqr (x)
             (destructuring-bind (xr . xi) x
               (cons
                (m+ (m* xr xr) (m* xi xi 11))
                (m* xr xi 2))
               )))
    (crypto/modular-arith::generalized-windowed-exponentiation n exp
                                                               :window-nbits 4
                                                               :op-mul #'cm*
                                                               :op-sqr #'cmsqr)
    ))

(defmethod madj ((x integer))
  (msigned x))

(defmethod madj ((x cons))
  (destructuring-bind (xr . xi) x
    (cons
     (madj xr)
     (madj xi))))

(modr
  (let ((exp *ed-r*))
    (madj (qfexp '(3 . 99) exp))
    ))
        
(modr (msqrt* 11))

(modq
  (loop for ix from 2 below 100 do
          (msqrt ix)))
|#
;; -----------------------------------------------------------
;; Quadratic Extension Fields
;; Arith supports mixed integer/quadratic field elements

(defvar *qf-im^2*)

(defmacro with-qf (base &body body)
  `(with-mod ,base
     (let ((*qf-im^2* (third (crypto/modular-arith::get-tonelli-shanks-params))))
       ,@body)))

(defmacro qfq (&body body)
  `(with-qf *ed-q*
             ,@body))

(defmacro qfr (&body body)
  `(with-qf *ed-r*
             ,@body))

;; -----------------------------------
;; Field Elements

(defstruct (qf
            (:constructor qf (re &optional (im 0)))
            (:print-object (lambda (x stream)
                             (let ((xre  (qf-re x))
                                   (xim  (qf-im x))
                                   (cvt? (boundp 'crypto/modular-arith::*m*)))
                               (when (and cvt? (plusp xre))
                                 (setf xre (msigned xre)))
                               (when (and cvt? (plusp xim))
                                 (setf xim (msigned xim)))
                               (format stream "(QF ~D ~D)" xre xim)))
             ))
  re im)

;; ------------------------------
;; Component Extraction

(defmethod re ((x integer))
  x)

(defmethod re ((x qf))
  (qf-re x))

(defmethod im ((x integer))
  0)

(defmethod im ((x qf))
  (qf-im x))

;; ------------------------------
;; Field Addition

(defmethod qfm+ ((x integer) (y integer))
  (m+ y x))

(defmethod qfm+ ((x qf) (y integer))
  (qf (m+ (qf-re x) y)
      (qf-im x)))

(defmethod qfm+ ((x integer) (y qf))
  (qfm+ y x))

(defmethod qfm+ ((x qf) (y qf))
  (qf (m+ (qf-re x) (qf-re y))
      (m+ (qf-im x) (qf-im y))))

;; ------------------------------
;; Field Negation

(defmethod qfneg ((x integer))
  (m- x))

(defmethod qfneg ((x qf))
  (qf (m- (qf-re x))
      (m- (qf-im x))))

;; ------------------------------
;; Field Subtraction

(defmethod qfm- ((x integer) (y integer))
  (m- y x))

(defmethod qfm- ((x qf) (y integer))
  (qf (m- (qf-re x) y)
      (qf-im x)))

(defmethod qfm- ((x integer) (y qf))
  (qf (m- x (qf-re y))
      (m- (qf-im y))))

(defmethod qfm- ((x qf) (y qf))
  (qf (m- (qf-re x) (qf-re y))
      (m- (qf-im x) (qf-im y))))

;; ------------------------------
;; Field Multiplication

(defmethod qfm* ((x integer) (y integer))
  (m* x y))

(defmethod qfm* ((x qf) (y integer))
  (qf (m* (qf-re x) y)
      (m* (qf-im x) y)))

(defmethod qfm* ((x integer) (y qf))
  (qfm* y x))

(defmethod qfm* ((x qf) (y qf))
  (qf  (m+ (m* (qf-re x) (qf-re y))
           (m* (qf-im x) (qf-im y) *qf-im^2*))
       (m+ (m* (qf-re x) (qf-im y))
           (m* (qf-im x) (qf-re y)))
       ))

;; ------------------------------------
;; Field Conjugation

(defmethod qf-conj ((x integer))
  x)

(defmethod qf-conj ((x qf))
  (qf (qf-re x)
      (m- (qf-im x))))

;; ------------------------------------
;; Magnitude Squared

(defmethod qf-magsq ((x integer))
  (msqr x))

(defmethod qf-magsq ((x qf))
  (m- (msqr (qf-re x))
      (m* (msqr (qf-im x)) *qf-im^2*)))

;; ---------------------------------------
;; Field Inverse

(defmethod qfinv ((x integer))
  (minv x))

(defmethod qfinv ((x qf))
  (let* ((x*    (qf-conj x))
         (magsq (qf-magsq x)))
    (qfm* (minv magsq) x*)))

;; ---------------------------------------
;; Field Division

(defmethod qfm/ ((x integer) (y integer))
  (m/ x y))

(defmethod qfm/ ((x qf) (y integer))
  (qf (m/ (qf-re x) y)
      (m/ (qf-im x) y)))

(defmethod qfm/ (x (y qf))
  (qfm* x (qfinv y)))

;; --------------------------------------------
;; Field Squaring

(defmethod qfsqr ((x integer))
  (msqr x))

(defmethod qfsqr ((x qf))
  (qf (m+ (msqr (qf-re x)) (m* (msqr (qf-im x)) *qf-im^2*))
      (m* 2 (qf-re x) (qf-im x))
      ))

;; --------------------------------------------
;; Field Exponentiation

(defmethod qfm^ ((x integer) y)
  ;; x^y
  (m^ x y))

(defmethod qfm^ ((x qf) y)
  (crypto/modular-arith::generalized-windowed-exponentiation x y
                                                             :window-nbits 4
                                                             :op-mul #'qfm*
                                                             :op-sqr #'qfsqr))
;; ----------------------------------------------
;; Field Signed Repr

(defmethod qf-signed ((x integer))
  (msigned x))

(defmethod qf-signed ((x qf))
  (qf (msigned (qf-re x))
      (msigned (qf-im x))))

;; ------------------------------------------------

(defmethod qf= ((x integer) (y integer))
  (m= x y))

(defmethod qf= ((x integer) (y qf))
  (and (m= x (qf-re y))
       (zerop (qf-im y))))

(defmethod qf= ((x qf) (y integer))
  (qf= y x))

(defmethod qf= ((x qf) (y qf))
  (and (m= (qf-re x) (qf-re y))
       (m= (qf-im x) (qf-im y))))

;; ------------------------------------------------
#|
(qfq
  (qf-signed (qfm^ (qf 1 1) *ed-q*)))

(qfq
  (qf-signed (qfinv (qf 1 1))))

(qfq
  (qf-signed (qfm* (qf 1 1) (qfinv (qf 1 1))) ))

(qfq
  (values (qf-signed (qf-magsq (qf 1 1)))
          (qf-signed (qfm* (qf 1 1) (qf-conj (qf 1 1))))))

(qfq
  (qf-signed (qf-conj (qf 1 1))))

(let ((q 31))
  (with-qf q
    (let ((map (make-array `(,q ,q) :initial-element '_)))
      (loop for ix from 0 below q do
              (let ((root (msqrt* ix)))
                (if (consp root)
                    ;; (incf (aref map (car root) (cdr root)))
                    (setf (aref map (car root) (cdr root)) t)
                  ;; (incf (aref map root 0))
                  (setf (aref map root 0) t)
                  )))
      (loop for ix from 0 below q do
              (print (loop for iy from 0 below q collect
                             (aref map ix iy))))
      )))

(let ((q 31))
  (with-qf q
    (let ((map (make-array `(,q ,q)  :initial-element '_)))
      (loop for ix from 0 below q do
              (loop for iy from 0 below q do
                      (let ((sqr (qfsqr (qf ix iy))))
                        ;; (incf (aref map (re sqr) (im sqr)))
                        (setf (aref map (re sqr) (im sqr)) t)
                        )))
      (loop for ix from 0 below q do
            (print (loop for iy from 0 below q collect
                           (aref map ix iy))))
      )))
|#
