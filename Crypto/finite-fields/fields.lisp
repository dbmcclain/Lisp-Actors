
(defpackage :pbc/x
  (:use :cl :pbc :ro :crypto/modular-arith :vec-repr))

(in-package :pbc/x)

#|  FIXME:  PBC:MAKE-KEY-PAIR cannot be called at compile time for delivery under LispWorks

The following code doesn't appear to be referenced

(defparameter k (make-key-pair :dave))
(defparameter pkey (keying-triple-pkey k))
(defparameter skey (keying-triple-skey k))

|#

(defparameter beta   76600213043964638334639432839350561620586998450651561245322304548751832163977)
(defparameter alpha0 82889197335545133675228720470117632986673257748779594473736828145653330099944)
(defparameter alpha1 66367173116409392252217737940259038242793962715127129791931788032832987594232)
(defparameter q      115792089237314936872688561244471742058375878355761205198700409522629664518163)
(defparameter r      115792089237314936872688561244471742058035595988840268584488757999429535617037)

;; ----------------------------------------------

(defclass field ()
  ((ord   :reader field-ord
          :initarg :ord)))

(defmethod print-object ((obj field) out-stream)
  (format out-stream "#<FIELD ORD = ~A>" (field-ord obj)))

(defmethod make-field ((ord integer))
  (make-instance 'field
                 :ord  ord))

;; -------------------------------------------------

(defclass field-int ()
  ((fld   :reader field-int-fld
          :initarg :fld)
   (val   :reader field-int-val
          :initarg :val)))

(defmethod print-object ((obj field-int) out-stream)
  (format out-stream "#<FIELD-INT VAL = ~A>" (field-int-val obj)))

(defmethod make-field-int ((fld field) (v integer))
  (make-instance 'field-int
                 :fld fld
                 :val (with-mod (field-ord fld)
                        (mmod v))))

(defmethod int ((x field-int))
  (field-int-val x))

(defmethod element-binop (op (x field-int) (y field-int))
  (let ((fld (field-int-fld x)))
    (assert (eq fld (field-int-fld y)))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (funcall op (field-int-val x) (field-int-val y))))
    ))

(defmethod element-binop (op (x field-int) (y integer))
  (let ((fld (field-int-fld x)))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (funcall op (field-int-val x) y)))))
  
(defmethod element-add (x y)
  (element-binop 'm+ x y))

(defmethod element-sub (x y)
  (element-binop 'm- x y))

(defmethod element-mul (x y)
  (element-binop 'm* x y))

(defmethod element-div (x y)
  (element-binop 'm/ x y))

(defmethod element-expt (x y)
  (element-binop 'm^ x y))

(defmethod element-rnd (x)
  (let* ((fld (field-int-fld x))
         (ord (field-ord fld)))
    (make-field-int fld (ecc-crypto-b571:random-between 1 ord))))

(defmethod element-is-sqr ((x field-int))
  (let ((fld (field-int-fld x)))
    (with-mod (field-ord fld)
      (quadratic-residue-p (field-int-val x)))))

(defmethod element-sqrt ((x field-int))
  (let ((fld (field-int-fld x)))
    (make-instance 'field-int
                   :fld  fld
                   :val  (with-mod (field-ord fld)
                           (msqrt (field-int-val x)))
                   )))

(defmethod element-inv ((x field-int))
  (let ((fld (field-int-fld x))
        (v   (field-int-val x)))
    (assert (plusp v))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (m/ v)))
    ))

(defmethod element-neg ((x field-int))
  (let ((fld  (field-int-fld x)))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (m- (field-int-val x))))
    ))

(defmethod element-cmp ((x field-int) (y field-int))
  (let ((fld (field-int-fld x)))
    (assert (eq fld (field-int-fld y)))
    (element-cmp x (field-int-val y))))

(defmethod element-cmp ((x field-int) (y integer))
  (let ((ans (- (field-int-val x) y)))
    (cond ((zerop ans)   0)
          ((minusp ans) -1)
          (t             1)
          )))

(defmethod element-0p ((x field-int))
  (zerop (field-int-val x)))

(defmethod element-1p ((x field-int))
  (= 1 (field-int-val x)))

(defmethod element-0 ((x field-int))
  (make-instance 'field-int
                 :fld (field-int-fld x)
                 :val 0))

(defmethod element-1 ((x field-int))
  (make-instance 'field-int
                 :fld  (field-int-fld x)
                 :val  1))

(defmethod element-conj ((x field-int))
  x)

(defmethod element-re ((x field-int))
  (field-int-val x))

(defmethod element-im ((x field-int))
  0)

;; -------------------------------------------------

(defmethod element-0p ((x integer))
  (zerop x))

(defmethod element-qp ((x integer))
  (= x 1))

(defmethod element-0 ((x integer))
  0)

(defmethod element-1 ((x integer))
  1)

(defmethod element-conj ((x integer))
  x)

(defmethod element-re ((x integer))
  x)

(defmethod element-im ((x integer))
  0)


;; ------------------------------------------------------------
;; ------------------------------------------------------------------------------

(defvar *fld-q* (make-instance 'field
                               :ord 115792089237314936872688561244471742058375878355761205198700409522629664518163))

(defvar *fld-r* (make-instance 'field
                               :ord 115792089237314936872688561244471742058035595988840268584488757999429535617037))

;; --------------------------------------------------------------------
