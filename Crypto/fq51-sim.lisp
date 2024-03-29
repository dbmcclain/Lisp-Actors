;; fq51-sim.lisp
;;
;; DM/Stegos  03/19
;; ---------------------------------------------------------

;; ------------------------------------------------------------
(in-package :edec)
;; ------------------------------------------------------------

(defun border ()
  (terpri)
  (princ "      ")
  (dotimes (ix 5)
    (princ "+------------------"))
  (princ "+"))
  
(defun show-fq (title v)
  (border)
  (format t "~%~5A ~{| ~16,'_x ~}|"
          title
          (map 'list (lambda (x)
                       (ldb (byte 64 0) x))
               v)))

(defun map-to-field (x)
  ;; for things like hash values
  (let* ((x      (ldb (byte 256 0) x)) ;; get unsigned bits
         (excess (ldb (byte 5 251) x)))
    (ash x (- (integer-length excess)))))

(defun encode-fq51 (x)
  (declare (integer x))
  (let ((ans    (make-array 5 :initial-element 0))
        (x      (map-to-field x)))
    (setf (aref ans 0) (ldb (byte 51   0) x)
          (aref ans 1) (ldb (byte 51  51) x)
          (aref ans 2) (ldb (byte 51 102) x)
          (aref ans 3) (ldb (byte 51 153) x)
          (aref ans 4) (ldb (byte 47 204) x))
    (scr ans)))

(defun decode-fq51 (v)
  (let ((x (gnorm v)))
    (dpb (aref x 0) (byte 51 0)
         (dpb (aref x 1) (byte 51 51)
              (dpb (aref x 2) (byte 51 102)
                   (dpb (aref x 3) (byte 51 153)
                        (dpb (aref x 4) (byte 47 204) 0)))))))

(defun split-bits (n val)
  (values (ash val (- n))
          (ldb (byte n 0) val)))

(defun bits-at (ix)
  (if (< ix 4)
      51
    47))

(defun scr (x)
  (let ((ans (copy-seq x))
        (cy  0))
    (dotimes (ix 5)
      (multiple-value-bind (hi lo) (split-bits (bits-at ix) (+ cy (aref x ix)))
        (setf (aref ans ix) lo
              cy            hi)))
    (incf (aref ans 0) (* 9 cy))
    ans))

#|
(defun scr (x)
  (let ((ans  (copy-seq x)))
    (um:nlet iter ((ix   0)
                   (cy   0))
      (if (< ix 5)
          (multiple-value-bind (hi lo) (split-bits (bits-at ix) (+ cy (aref x ix)))
            (setf (aref ans ix) lo)
            (go-iter (1+ ix) hi))
        (progn
          (incf (aref ans 0) (* 9 cy))
          ans))
      )))
|#

(defun gneg (x)
  (map 'vector '- x))

(defun gsub (a b)
  (map 'vector '- a b))

(defun gnorm (x)
  (scr (scr (gneg (scr (gneg x))))))

(defun show-norm (val &key neg valprint)
  (let* ((fq (if (vectorp val)
                 val
               (encode-fq51 val))))
    (when neg
      (setf fq (gneg fq)))
    (format t "~%Value = ~D, ~@[negated~]" (or valprint val) neg)
    (format t "~%~16t~{~A~^~17@t~}" '(x0 x1 x2 x3 x4))
    (show-fq "fq" fq)
    (show-fq "gneg" (setf fq (gneg fq)))
    (show-fq "scr"  (setf fq (scr fq)))
    (show-fq "gneg" (setf fq (gneg fq)))
    (show-fq "scr"  (setf fq (scr fq)))
    (um:nlet iter ((fq fq))
      (let ((v (aref fq 0)))
        (unless (= v (ldb (byte 51 0) v))
          (let ((new-fq  (scr fq)))
            (show-fq "scr" new-fq)
            (go-iter new-fq)))))
    (border)
    (terpri)))

(defun show-scr (val &key neg valprint)
  (let* ((fq (if (vectorp val)
                 val
               (encode-fq51 val))))
    (when neg
      (setf fq (gneg fq)))
    (format t "~%Value = ~D, ~@[negated~]" (or valprint val) neg)
    (format t "~%~16t~{~A~^~17@t~}" '(x0 x1 x2 x3 x4))
    (show-fq "fq" fq)
    (show-fq "scr" (setf fq (scr fq)))
    (um:nlet iter ((fq fq))
      (let ((v (aref fq 0)))
        (unless (= v (ldb (byte 51 0) v))
          (let ((new-fq  (scr fq)))
            (show-fq "scr" new-fq)
            (go-iter new-fq)))))
    (border)
    (terpri)))



#|
(inspect (gnorm (gneg (encode-fq51 (ash 50 51)))))
(loop for ix from 0 below 1000 do
      (let* ((x  (ash ix (* 4 51)))
             (fq (encode-fq51 x))
             (y  (gnorm (gneg fq))))
        (assert (notany 'minusp y) () "~A" ix)))

(progn
  (show-norm 0)
  (show-norm 1)
  (show-norm 50)
  (show-norm (ash 50 51)   :valprint "50*2^51")
  (show-norm (- *ed-q* 50) :valprint "|Fq|-50")
  (show-norm *ed-q*        :valprint "|Fq|")
  (show-norm (+ *ed-q* 50) :valprint "|Fq|+50")

  (show-norm 1 :neg t)
  (show-norm 50 :neg t)
  (show-norm (ash 50 51)   :neg t :valprint "50*2^51")
  (show-norm (- *ed-q* 50) :neg t :valprint "|Fq|-50")
  (show-norm *ed-q*        :neg t :valprint "|Fq|")
  (show-norm (+ *ed-q* 50) :neg t :valprint "|Fq|+50")

  (show-norm (ash 50 (* 4 51)) :neg t :valprint "50*2^(4*51)")
)

(defun split16 (x)
  (values (ash x -4)
          (logand x 15)))

(defun scr4 (v)
  (let ((ans (copy-seq v)))
    (multiple-value-bind (hi lo) (split16 (aref v 0))
      (setf (aref ans 0) lo)
      (multiple-value-bind (hi lo) (split16 (+ (aref v 1) hi))
        (setf (aref ans 1) lo)
        (incf (aref ans 0) (* 3 hi))
        ans))))

(defun simple (v)
  (print v)
  (print (setf v (scr4 v)))
  (um:nlet iter ((v v))
    (let ((new-v (scr4 v)))
      (unless (equalp new-v v)
        (print new-v)
        (go-iter new-v)))))
|#
#|
SHOW-NORM

Value = 0, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = 1, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | _______________1 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFFF | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFF6 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF800000000000A | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________1 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = 50, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF800000000003B | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = 50*2^51, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | _______________0 | ______________32 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | FFFFFFFFFFFFFFF7 | ___7FFFFFFFFFFCE | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________9 | FFF8000000000032 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | ______________32 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = |Fq|-50, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF800000000003B | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+

Value = |Fq|, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | ___7FFFFFFFFFFF7 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF8000000000009 | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = |Fq|+50, 
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | ______________29 | _______________0 | _______________0 | _______________0 | ____800000000000 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFD7 | _______________0 | _______________0 | _______________0 | FFFF800000000000 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF800000000003B | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = 1, negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | FFFFFFFFFFFFFFFF | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________1 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________1 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFFF | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFF6 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+

Value = 50, negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+

Value = 50*2^51, negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | _______________0 | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | ______________32 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | ______________32 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | FFFFFFFFFFFFFFF7 | ___7FFFFFFFFFFCE | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFF7 | ___7FFFFFFFFFFCD | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+

Value = |Fq|-50, negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | FFF800000000003B | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF800000000003B | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = |Fq|, negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | FFF8000000000009 | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | ___7FFFFFFFFFFF7 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFF7 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFF8000000000009 | FFF8000000000001 | FFF8000000000001 | FFF8000000000001 | FFFF800000000001 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+

Value = |Fq|+50, negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | FFFFFFFFFFFFFFD7 | _______________0 | _______________0 | _______________0 | FFFF800000000000 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | ______________29 | _______________0 | _______________0 | _______________0 | ____800000000000 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ______________32 | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | FFFFFFFFFFFFFFCE | _______________0 | _______________0 | _______________0 | _______________0 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFC5 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFFF |
      +------------------+------------------+------------------+------------------+------------------+

Value = 50*2^(4*51), negated
                X0                 X1                 X2                 X3                 X4
      +------------------+------------------+------------------+------------------+------------------+
fq    | _______________0 | _______________0 | _______________0 | _______________0 | FFFFFFFFFFFFFFCE |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | _______________0 | _______________0 | _______________0 | ______________32 |
      +------------------+------------------+------------------+------------------+------------------+
scr   | _______________0 | _______________0 | _______________0 | _______________0 | ______________32 |
      +------------------+------------------+------------------+------------------+------------------+
gneg  | _______________0 | _______________0 | _______________0 | _______________0 | FFFFFFFFFFFFFFCE |
      +------------------+------------------+------------------+------------------+------------------+
scr   | FFFFFFFFFFFFFFF7 | _______________0 | _______________0 | _______________0 | ____7FFFFFFFFFCE |
      +------------------+------------------+------------------+------------------+------------------+
scr   | ___7FFFFFFFFFFF7 | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ___7FFFFFFFFFFFF | ____7FFFFFFFFFCD |
      +------------------+------------------+------------------+------------------+------------------+
|#
