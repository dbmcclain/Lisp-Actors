
(in-package :pbc/x)

(defclass qfield-pair ()
  ((fld   :reader qfield-pair-fld
          :initarg :fld)
   (x     :reader qfield-pair-x
          :initarg :x
          :initform 0)
   (y     :reader qfield-pair-y
          :initarg :y
          :initform 0)))


(defmethod print-object ((obj qfield-pair) out-stream)
  (format out-stream "#<QFIELD-PAIR  :X ~A  :Y ~A>"
          (qfield-pair-x obj)
          (qfield-pair-y obj)))

(defmethod make-qfield-pair ((fld field) (x integer) &optional (y 0))
  (make-instance 'qfield-pair
                 :fld fld
                 :x   x
                 :y   y))

(defmethod element-conj ((a qfield-pair))
  (let ((fld (qfield-pair-fld a)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld  fld
                     :x    (qfield-pair-x a)
                     :y    (m- (qfield-pair-y a)))
      )))

(defmethod element-add ((a qfield-pair) (b qfield-pair))
  (let* ((fld (qfield-pair-fld a)))
    (assert (eq fld (qfield-pair-fld b)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld fld
                     :x   (m+ (qfield-pair-x a)
                              (qfield-pair-x b))
                     :y   (m+ (qfield-pair-y a)
                              (qfield-pair-y b)))
      )))

(defmethod element-add ((a qfield-pair) (b integer))
  (let* ((fld (qfield-pair-fld a)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld fld
                     :x   (m+ b (qfield-pair-x a))
                     :y   (qfield-pair-y a))
      )))

(defmethod element-sub ((a qfield-pair) (b qfield-pair))
  (let* ((fld (qfield-pair-fld a)))
    (assert (eq fld (qfield-pair-fld b)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld fld
                     :x   (m- (qfield-pair-x a)
                              (qfield-pair-x b))
                     :y   (m- (qfield-pair-y a)
                              (qfield-pair-y b)))
      )))

(defmethod element-sub ((a qfield-pair) (b integer))
  (let* ((fld (qfield-pair-fld a)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld fld
                     :x   (m- (qfield-pair-x a) b)
                     :y   (qfield-pair-y a))
      )))

(defmethod element-mul ((a qfield-pair) (b qfield-pair))
  (let* ((fld (qfield-pair-fld a)))
    (assert (eq fld (qfield-pair-fld b)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld fld
                     :x   (m- (m* (qfield-pair-x a)
                                  (qfield-pair-x b))
                              (m* (qfield-pair-y a)
                                  (qfield-pair-y b)))
                     :y   (m+ (m* (qfield-pair-x a)
                                  (qfield-pair-y b))
                              (m* (qfield-pair-y a)
                                  (qfield-pair-x b)))
                     ))))

(defmethod element-mul ((a qfield-pair) (b integer))
  (let* ((fld (qfield-pair-fld a)))
    (with-mod (field-ord fld)
      (make-instance 'qfield-pair
                     :fld fld
                     :x   (m* b (qfield-pair-x a))
                     :y   (m* b (qfield-pair-y a)))
      )))

(defmethod element-div ((a qfield-pair) (b integer))
  (let* ((fld (qfield-pair-fld a)))
    (with-mod (field-ord fld)
      (element-mul a (m/ b))
      )))

(defmethod element-div ((a qfield-pair) (b qfield-pair))
  (let* ((fld (qfield-pair-fld a)))
    (assert (eq fld (qfield-pair-fld b)))
    (element-div (element-mul a (element-conj b))
                 (element-magsq b))
    ))

(defmethod element-0 ((a qfield-pair))
  (make-qfield-pair (qfield-pair-fld a) 0))

(defmethod element-1 ((a qfield-pair))
  (make-qfield-pair (qfield-pair-fld a) 1))

(defmethod element-0p ((a qfield-pair))
  (and (zerop (qfield-pair-x a))
       (zerop (qfield-pair-y a))))

(defmethod element-1p ((a qfield-pair))
  (and (= 1 (qfield-pair-x a))
       (zerop (qfield-pair-y a))))

(defmethod element-inv ((a qfield-pair))
  (element-div (element-conj a)
               (element-magsq a)))

(defmethod element-expt ((a qfield-pair) (b integer))
  (let ((ans (element-1 a)))
    (loop for ix from 0 below (integer-length b)
          for p = a then (element-mul p p)
          do
          (when (logbitp ix b)
            (setf ans (element-mul p ans))))
    ans))

(defmethod element-is-sqr ((a qfield-pair))
  (let* ((fld (qfield-pair-fld a)))
    (element-1p (element-expt a (truncate (1- (field-ord fld)) 2)))))

(defmethod element-re ((a qfield-pair))
  (qfield-pair-x a))

(defmethod element-im ((a qfield-pair))
  (qfield-pair-y a))

(defmethod element-magsq ((a qfield-pair))
  (let ((fld (qfield-pair-fld a)))
    (with-mod (field-ord fld)
      (let ((re  (qfield-pair-x a))
            (im  (qfield-pair-y a)))
        (m+ (m* re re) (m* im im)))
      )))

