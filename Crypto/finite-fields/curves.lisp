
(in-package :pbc/x)

;; ---------------------------------------------------------------------

(defclass curve ()
  ((qfld :reader curve-qfld
         :initarg :qfld)
   (rfld :reader curve-rfld
         :initarg :rfld)
   (a    :reader curve-a
         :initform 0
         :initarg :a)
   (b    :reader curve-b
         :initarg :b)
   (h    :reader curve-h
         :initarg :h
         :initform 1))
  (:documentation "Curve: y^2 = x^3 + a*x + b
Curve define over field qfld, order = h*ord(rfld).
X, Y are elements of qfld."))

(defclass pt ()
  ((curve :reader pt-curve
          :initarg :curve)
   (x     :reader pt-x
          :initarg :x)
   (y     :reader pt-y
          :initarg :y)))

(defmethod print-object ((obj pt) out-stream)
  (format out-stream "#<PT X: ~A :Y ~A>"
          (pt-x obj) (pt-y obj)))

(defmethod element-0p ((p pt))
  (element-0p (pt-y p)))

(defmethod element-0 ((p pt))
  (make-instance 'pt
                 :curve (pt-curve p)
                 :x     (element-0 (pt-x p))
                 :y     (element-0 (pt-y p))))

(defmethod element-neg ((p pt))
  (make-instance 'pt
                 :curve (pt-curve p)
                 :x     (pt-x p)
                 :y     (element-neg (pt-y p))))

(defmethod affine-double ((pt pt))
  (with-accessors ((curve  pt-curve)
                   (x      pt-x)
                   (y      pt-y)) pt
    (let* ((s  (element-div
                (element-add
                 (element-mul (element-1 x)
                              (curve-a curve))
                 (element-mul (element-mul x x)
                              3))
                (element-mul y 2)))
           (x2 (element-sub (element-mul s s)
                            (element-add x x)))
           (y2 (element-sub (element-mul s
                                         (element-sub x x2))
                            y)))
      (make-instance 'pt
                     :curve curve
                     :x     x2
                     :y     y2))))

(defmethod element-add ((pt1 pt) (pt2 pt))
  (with-accessors ((curve1  pt-curve)
                   (x1      pt-x)
                   (y1      pt-y)) pt1
    (with-accessors ((curve2 pt-curve)
                     (x2     pt-x)
                     (y2     pt-y)) pt2
      (assert (eq curve1 curve2))
      (cond
       ((element-0p pt1) pt2)
       ((element-0p pt2) pt1)
       ((element-0p (element-sub x1 x2))
        (cond ((element-0p (element-sub y1 y2))
               (affine-double pt1))
              ((element-0p (element-add y1 y2))
               (element-0 pt1))
              (t (error "affine-add: points not on curve"))
              ))
       (t
        (let* ((s   (element-div (element-sub y2 y1)
                                 (element-sub x2 x1)))
               (x3  (element-sub (element-mul s s)
                                 (element-add x1 x2)))
               (y3  (element-sub (element-mul s
                                              (element-sub x1 x3))
                                 y1)))
          (make-instance 'pt
                         :curve curve1
                         :x     x3
                         :y     y3)))
       ))))

(defmethod element-sub ((p1 pt) (p2 pt))
  (element-add p1 (element-neg p2)))

(defmethod element-mul ((p pt) (x integer))
  (let ((ans  (element-0 p)))
    (loop for ix from 0 below (integer-length x)
          for q = p then (element-add q q)
          do
          (when (logbitp ix x)
            (setf ans (element-add ans q))))
    ans))

(defmethod pt-on-curve-p ((p pt))
  (with-accessors ((curve  pt-curve)
                   (x      pt-x)
                   (y      pt-y)) p
    (cond ((element-0p p) nil)
          (t (let* ((yy  (element-mul y y))
                    (one (element-1 x))
                    (gx  (element-add
                          (element-mul x
                                       (element-add
                                        (element-mul x x)
                                        (element-mul one (curve-a curve))))
                          (element-mul one (curve-b curve)))))
               (element-0p (element-sub yy gx))))
          )))

(defmethod validate-pt ((p pt))
  (and (not (element-0p p))
       (pt-on-curve-p p)
       (let* ((curve (pt-curve p))
              (rfld  (curve-rfld curve))
              (h     (curve-h    curve)))
         (element-0p (element-mul p (* h (field-ord rfld)))))
       ))

(defmethod hash-to-curve ((curve curve) (h hash:hash))
  (let* ((x    (hash-to-field (curve-qfld curve) h))
         (one  (element-1 x)))
    (um:nlet-tail iter ((x x))
      (let ((gx (element-add
                 (element-mul x
                              (element-add
                               (element-mul x x)
                               (element-mul one (curve-a curve))))
                 (element-mul one (curve-b curve)))))
        (if (element-is-sqr gx)
            (make-instance 'pt
                           :curve curve
                           :x     x
                           :y     (element-sqrt gx))
          (iter (element-add one
                             (element-mul x x)))
          )))
    ))

(defmethod hash-to-vec ((h hash:hash) nb)
  "short hashes -> H | 0 | H | 1 | H ..."
  (let* ((nel   (hash:hash-length h))
         (bytes (hash:hash-bytes h)))
    (cond ((= nel nb)  bytes)
          ((> nel nb)  (subseq bytes 0 nb))
          (t 
           (let ((vec (make-ub8-vector nb)))
             (um:nlet-tail iter ((pos 0)
                                 (ctr 0))
               (if (>= pos nb)
                   vec
                 (progn
                   (replace vec bytes :start1 pos)
                   (incf pos nel)
                   (when (< pos nb)
                     (setf (aref vec pos) ctr)
                     (incf pos))
                   (iter pos (1+ ctr)))
                 ))))
          )))

(defun shift-to-fit (vec ord)
  (um:nlet-tail iter ((x  (int (bev vec))))
    (if (>= x ord)
        (iter (ash x -1))
      x)))

(defmethod hash-to-field ((qfld field) (h hash:hash))
  (let* ((ord   (field-ord qfld))
         (nb    (ceiling (integer-length ord) 8))
         (xval  (shift-to-fit (hash-to-vec h nb) ord)))
    (make-field-int qfld xval)))

(defmethod hash-to-field ((qfld ext-field) (h hash:hash))
  (let* ((ord    (field-ord (ext-field-fld qfld)))
         (deg    (ext-field-deg qfld))
         (nbpc   (ceiling (integer-length ord) 8))
         (nb     (* deg nbpc))
         (xbytes (hash-to-vec h nb))
         (xcoffs (loop repeat deg
                       for pos from 0 by nbpc
                       collect
                       (shift-to-fit (subseq xbytes pos (+ pos nbpc)) ord))))
    (make-poly qfld xcoffs)))

    
(defvar *curve* (make-instance 'curve
                               :qfld *fld-q*
                               :rfld *fld-r*
                               :b    3))

(defvar *ext-curve* (let ((ext (make-extension-field *fld-q* '(3 0 1))))
                      (make-instance 'curve
                                     :qfld ext
                                     :rfld *fld-r*
                                     :b    (make-poly ext '(-3 3)))))

