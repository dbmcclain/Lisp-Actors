;; fr-ver3.lisp -- arbitrary prime fields using short word vectors
;;
;; DM/RAL  03/19
;; ------------------------------------------------------------------------------------------------

(in-package :edec)

;; ------------------------------------------------------------------------

(defconstant +bpw+  30) ;; small enough to allow max squared value to remain a fixnum
(defconstant +low-bits-mask+ (1- (ash 1 +bpw+)))

;; ------------------------------------------------------------------------

(defun to-chunks (x nchunks)
  (let ((ans (make-array nchunks
                         :element-type 'fixnum
                         :initial-element 0)))
    (um:nlet iter ((ix  0))
      (if (< ix nchunks)
          (progn
            (setf (aref ans ix) (ldb (byte +bpw+ (* ix +bpw+)) x))
            (go-iter (1+ ix)))
        ans))
    ))

(defun from-chunks (v)
  (let ((ans     0)
        (nchunks (length v)))
    (um:nlet iter ((ix 0))
      (if (< ix nchunks)
          (progn
            (dpb (aref v ix) (byte +bpw+ (* ix +bpw+)) ans)
            (go-iter (1+ ix)))
        ans))))

(defclass field ()
  ((modulus  :accessor field-modulus  :initarg :modulus)
   (nchunks  :accessor field-nchunks)
   (last-len :accessor field-last-len)
   (modv     :accessor field-modv)
   (last-mul :accessor field-last-mul)
   ))

(defmethod initialize-instance :after ((fld field) &key modulus &allow-other-keys)
  (let ((nbits (integer-length modulus)))
    (multiple-value-bind (nc rem) (ceiling nbits +bpw+)
      (setf (field-nchunks fld)  nc
            (field-last-len fld) (+ rem +bpw+)
            (field-modv fld)     (to-chunks modulus nc)
            (field-last-mul fld) (mod (ash 1 nbits) modulus))
      )))

(defmethod make-field ((modulus integer))
  (make-instance 'field :modulus modulus))


(defclass element ()
  ((field  :accessor element-field  :initarg :field)
   (vec    :accessor element-vec    :initarg :vec)
   ))

(defmethod make-element ((field field) (value integer))
  (make-instance 'element
                 :field field
                 :vec   (to-chunks value (field-nchunks field))))

(defmethod make-element ((field field) (value vector))
  (assert (= (length value) (field-nchunks field)))
  (make-instance 'element
                 :field field
                 :vec   value))

(defmethod assert-same-field ((e1 element) (e2 element))
  (assert (eq (element-field e1) (element-field e2)) nil "Not same field"))

(defmethod basic-f+ ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let* ((fld  (element-field e1))
         (vres (copy-seq (element-vec e1)))
         (v2   (element-vec e2))
         (nc   (field-nchunks fld)))
    (dotimes (ix nc)
      (setf (aref vres ix) (+ (aref vres ix) (aref v2 ix))))
    (make-element fld vres)))

(defmethod basic-f- ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let* ((fld  (element-field e1))
         (vres (copy-seq (element-vec e1)))
         (v2   (element-vec e2))
         (nc   (field-nchunks fld)))
    (dotimes (ix nc)
      (setf (aref vres ix) (- (aref vres ix) (aref v2 ix))))
    (make-element fld vres)))

(defmethod fneg ((e element))
  (let* ((fld  (element-field e))
         (vres (copy-seq (element-vec e)))
         (nc   (field-nchunks fld)))
    (dotimes (ix nc)
      (setf (aref vres ix) (- (aref vres ix))))
    (make-element fld vres)))

(defun f+ (e1 &rest es)
  (let ((ans e1))
    (dolist (e2 es)
      (setf ans (basic-f+ ans e2)))
    ans))

(defun f- (e1 &rest es)
  (if es
      (let ((ans e1))
        (dolist (e2 es)
          (setf ans (basic-f- ans e2)))
        ans)
    ;; else, simple negation
    (fneg e1)))

(defmethod scr ((e element))
  (let* ((fld (element-field e))
         (nc  (field-nchunks fld))
         (v   (element-vec e))
         (v0  (aref v 0)))
    (um:nlet iter ((tmp  v0)
                   (ix   1))
      (if (< ix nc)
          (let ((new-tmp (+ (aref v ix) (ash tmp (- +bpw+)))))
            (setf (aref v ix) (ldb (byte +bpw+ 0) new-tmp))
            (go-iter new-tmp (1+ ix)))
        ;; else
        (setf (aref v 0) (+ (ldb (byte +bpw+ 0) v0)
                            (* (field-last-mul fld)
                               (ash tmp (- (field-last-len fld))))))
        ))
    e))
        

#|
(defun mod-adj (v mod)
  (case (cmpv v mod)
    (:less  v)
    (t      (sub-no-borrow v mod))
    ))

(defun add-no-carry (vres v2)
  (let ((pcy (list 0)))
    (dotimes (ix (length vres))
      (setf (aref vres ix) (adc (aref vres ix) (aref v2 ix) pcy)))))

(defun adc (a64 b64 pcy)
  (multiple-value-bind (ahi alo) (split_u64 a64)
    (multiple-value-bind (bhi blo) (split_u64 b64)
      (multiple-value-bind (c rlo) (split_u64 (+ alo blo (car pcy)))
        (multiple-value-bind (c rhi) (split_u64 (+ ahi bhi c))
          (setf (car pcy) c)
          (combine_u64 rhi rlo))))))

(defun f+ (e1 &rest es)
  (let ((ans e1))
    (dolist (e2 es)
      (setf ans (basic-f+ ans e2)))
    ans))

(defmethod basic-f- ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let ((fld (element-field e1))
        (su  (element-su e1)))
    (cond ((eq su (element-su e2))
           (let* ((vres (copy-seq (element-vec e1)))
                  (v2   (element-vec e2)))
             (sub-no-borrow vres v2 fld)
             (make-element fld vres su)))

        ((eq su :unscaled)
         (let* ((e2   (to-unscaled e2))
                (v2   (element-vec e2))
                (vres (copy-seq (element-vec e1))))
           (sub-no-borrow vres v2 fld)
           (make-element fld vres su)))

        (t
         (let* ((e1   (to-unscaled e1))
                (vres (element-vec e1))
                (v2   (element-vec e2)))
           (sub-no-borrow vres v2 fld)
           e1))
        )))

(defun sub-no-borrow (vres v2 &optional fld)
  (let ((pcy (list 0)))
    (dotimes (ix (length vres))
      (setf (aref vres ix) (sbb (aref vres ix) (aref v2 ix) pcy)))
    (when fld
      (unless (zerop (car pcy))
        (add-no-carry vres (field-modv fld))))))

(defun sbb (a64 b64 pcy)
  (multiple-value-bind (ahi alo) (split_u64 a64)
    (multiple-value-bind (bhi blo) (split_u64 b64)
      (let ((ten #.(ash 1 32)))
        (multiple-value-bind (c rlo) (split_u64 (- (+ ten alo) blo (car pcy)))
          (multiple-value-bind (c rhi) (split_u64 (- (+ ten ahi) bhi (if (zerop c) 1 0)))
            (setf (car pcy) (if (zerop c) 1 0))
            (combine_u64 rhi rlo)))))))

(defmethod fneg ((e element))
  (basic-f- (make-element (element-field e) 0) e))

(defun f- (e1 &rest es)
  (if es
      (let ((ans e1))
        (dolist (e2 es)
          (setf ans (basic-f- ans e2)))
        ans)
    ;; else, simple negation
    (fneg e1)))

(defun cmpv (v1 v2)
  (um:nlet iter ((ix (length v1)))
    (if (zerop ix)
        :equal
      (let* ((i (1- ix))
             (a (aref v1 i))
             (b (aref v2 i)))
        (cond ((< a b) :less)
              ((> a b) :greater)
              (t       (go-iter i))
              )))))

(defmethod f= ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let ((su  (element-su e1)))
    (cond ((eq su (element-su e2))
           (let* ((v1 (element-vec e1))
                  (v2 (element-vec e2)))
             (eq :equal (cmpv v1 v2))))

        ((eq su :unscaled)
         (let* ((e2  (to-unscaled e2))
                (v2  (element-vec e2))
                (v1  (element-vec e1)))
           (eq :equal (cmpv v1 v2))))

        (t
         (let* ((e1  (to-unscaled e1))
                (v1  (element-vec e1))
                (v2  (element-vec e2)))
           (eq :equal (cmpv v1 v2))))
        )))

(defmethod f= ((e1 element) (e2 integer))
  (f= e1 (make-element (element-field e1) e2)))

(defmethod f= ((e1 integer) (e2 element))
  (f= (make-element (element-field e2) e1) e2))

(defmethod basic-f+ ((e1 element) (e2 integer))
  (basic-f+ e1 (make-element (element-field e1) e2)))

(defmethod basic-f+ ((e1 integer) (e2 element))
  (basic-f+ (make-element (element-field e2) e1) e2))

(defmethod basic-f- ((e1 element) (e2 integer))
  (basic-f- e1 (make-element (element-field e1) e2)))

(defmethod basic-f- ((e1 integer) (e2 element))
  (basic-f- (make-element (element-field e2) e1) e2))

(defmethod basic-f* ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let* ((fld  (element-field e1))
         (e1   (to-scaled e1))
         (e2   (to-scaled e2)))
    (make-element fld (mul-reduce (element-vec e1)
                                  (element-vec e2)
                                  (field-modv fld)
                                  (field-minv fld))
                  :scaled)))
  
|#
