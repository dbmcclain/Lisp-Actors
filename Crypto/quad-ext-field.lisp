
(in-package :crypto/modular-arith)

(defvar *qfimsq* 0)

(defmacro with-qf (base &body body)
  `(with-mod ,base
     (let ((*qfimsq* (qf-imsq)))
       ,@body)))

(defmethod qf-re ((x integer))
  x)

(defmethod qf-im ((x integer))
  0)

(defmethod qf-re ((z cons))
  (car z))

(defmethod qf-im ((z cons))
  (cdr z))

(defun qf-imsq ()
  (third (get-tonelli-shanks-params)))

(defmethod qfmod ((x integer))
  (mmod x))

(defmethod qfmod ((x cons))
  (cons (mmod (car x))
        (mmod (cdr x))))

(defun qf= (a b)
  (and (m= (qf-re a) (qf-re b))
       (m= (qf-im a) (qf-im b))))

(defmethod qf-conj ((a integer))
  a)

(defmethod qf-conj ((a cons))
  (cons (car a)
        (m- (cdr a))))

(defun qf-add (a b)
  (cons (m+ (qf-re a) (qf-re b))
        (m+ (qf-im a) (qf-im b))))

(defun qf+ (&rest args)
  (reduce #'qf-add args))

(defmethod qf-mul ((a integer) (b integer))
  (m* a b))

(defmethod qf-mul ((a integer) (b cons))
  (cons (m* a (car b))
        (m* a (cdr b))))

(defmethod qf-mul ((a cons) (b integer))
  (qf-mul b a))

(defmethod qf-mul ((a cons) (b cons))
  (let ((are (car a))
        (aim (cdr a))
        (bre (car b))
        (bim (cdr b)))
  (cons
   (m+ (m* are bre)
       (m* aim bim *qfimsq*))
   (m+ (m* are bim)
       (m* aim bre)))
  ))
  
(defun qf* (&rest args)
  (reduce #'qf-mul args))

(defmethod qfsqr ((x integer))
  (msqr x))

(defmethod qfsqr ((x cons))
  (let ((re (car x))
        (im (cdr x)))
    (cons (m+ (m* re re) (m* im im *qfimsq*))
          (m* 2 re im))))

(defun qf-abssqr (x)
  (qf-re (qf-mul x (qf-conj x))))

(defun qf-sub (a b)
  (cons (m- (qf-re a) (qf-re b))
        (m- (qf-im a) (qf-im b))))

(defun qf- (arg &rest args)
  (if args
      (reduce #'qf-sub args :initial-value arg)
    (qf-sub 0 arg)))

(defun qf-div (a b)
  (let* ((cb  (qf-conj b))
         (den (m/ (qf-re (qf* b cb))))
         (num (qf* a cb)))
    (qf-mul den num)))

(defun qf/ (arg &rest args)
  (if args
      (reduce #'qf-div args :initial-value arg)
    (qf-div 1 arg)))

(defun qf^ (x exp)
  ;; for integer exp
  (declare (integer exp))
  (multiple-value-bind (x exp)
      (if (minusp exp)
          (values (qf/ x) (- exp))
        (values (qfmod x) exp))
    (um:nlet iter ((x   x)
                   (exp exp)
                   (ans 1))
      (declare (integer exp))
      (if (zerop exp)
          ans
        ;; In field F(p^2) we know that x^p = conj(x),
        ;; so x^(n*p+r) = conj(x)^n * x^r
        (multiple-value-bind (q r) (truncate exp *m*)
          (declare (integer q r))
          (let ((rans (case r
                        (0  1)
                        (1  x)
                        (2  (qfsqr x))
                        (t  (generalized-windowed-exponentiation x r
                                                                 :window-nbits 4
                                                                 :op-mul #'qf-mul
                                                                 :op-sqr #'qfsqr))
                        )))
            (go-iter (qf-conj x) q (qf-mul ans rans))
            ))))
    ))

;; -------------------------------------------------------------------------
#|
(defun re-im-aref (arr re im)
  ;; fix addressing for Quadrant-I viewing when printing array
  (let ((nrows (array-dimension arr 0)))
    (aref arr (- nrows 1 im) re)))

(defun set-re-im-aref (arr re im val)
  (let ((nrows (array-dimension arr 0)))
    (setf (aref arr (- nrows 1 im) re) val)))

(defsetf re-im-aref set-re-im-aref)

(defun zap (arr)
  (loop for ix from 0 below (array-total-size arr) do
        (when (zerop (row-major-aref arr ix))
          (setf (row-major-aref arr ix) '_))))

;; -----------------------------------------------

(let* ((p 17)
       (arr (make-array (list p p)
                        :initial-element 0))
       (which 'show-sqrs))
  (case which
    ((show-pos)
     (let ((pt  '(4 . 3)))
       (incf (re-im-aref arr (qf-re pt) (qf-im pt)))))

    ((show-pwrs)
     (let ((pt '(3 . 3))) ;; (3 . 3) can serve as generator for P=19
       (with-qf p
         (loop for ix from 0 below (* 19 19) do
               (let ((y (qf^ pt ix)))
                 (incf (re-im-aref arr (qf-re y) (qf-im y)))
                 )))))
    ((show-sqrs)
     (with-qf p
       (loop for ix from 0 below p do
             (loop for iy from 0 below p do
                   (let ((y (qf^ (cons iy ix) 2)))
                     (incf (re-im-aref arr (qf-re y) (qf-im y))))))))
    )
  (zap arr)
  arr)

#|
;; For F(19^2) q = 3 mod 4
;; Squares: non-blank locations are elements with square roots in the field
   0  1 2 2 2 2 2 2 2 2 2 | 2 2 2 2 2 2 2 2 2 1
 #2A((2 \ _ 2 _ 2 _ 2 2 _ | _ 2 2 _ 2 _ 2 _ /)2
  -2 (2 _ \ 2 _ 2 2 _ _ 2 | 2 _ _ 2 2 _ 2 / _)2
  -3 (2 _ 2 \ 2 2 _ _ _ 2 | 2 _ _ _ 2 2 / 2 _)2
  -4 (2 2 _ _ \ _ 2 2 _ 2 | 2 _ 2 2 _ / _ _ 2)2
  -5 (2 _ 2 2 2 \ 2 _ _ _ | _ _ _ 2 / 2 2 2 _)2
  -6 (2 2 _ _ 2 _ \ _ 2 2 | 2 2 _ / _ 2 _ _ 2)2
  -7 (2 2 2 2 _ _ _ \ 2 _ | _ 2 / _ _ _ 2 2 2)2
  -8 (2 2 2 _ _ 2 _ 2 \ _ | _ / 2 _ 2 _ _ 2 2)2
  -9 (2 _ _ _ 2 _ 2 2 2 \ | / 2 2 2 _ 2 _ _ _)2
     ---------------------|--------------------
   9 (2 _ _ _ 2 _ 2 2 2 / | \ 2 2 2 _ 2 _ _ _)2
   8 (2 2 2 _ _ 2 _ 2 / _ | _ \ 2 _ 2 _ _ 2 2)2
   7 (2 2 2 2 _ _ _ / 2 _ | _ 2 \ _ _ _ 2 2 2)2
   6 (2 2 _ _ 2 _ / _ 2 2 | 2 2 _ \ _ 2 _ _ 2)2
 ^ 5 (2 _ 2 2 2 / 2 _ _ _ | _ _ _ 2 \ 2 2 2 _)2
 | 4 (2 2 _ _ / _ 2 2 _ 2 | 2 _ 2 2 _ \ _ _ 2)2
Im 3 (2 _ 2 / 2 2 _ _ _ 2 | 2 _ _ _ 2 2 \ 2 _)2
   2 (2 _ / 2 _ 2 2 _ _ 2 | 2 _ _ 2 2 _ 2 \ _)2
   1 (2 / _ 2 _ 2 _ 2 2 _ | _ 2 2 _ 2 _ 2 _ \)2
   0 (1 2 2 2 2 2 2 2 2 2 | 2 2 2 2 2 2 2 2 2)1) 
      0 1 2 3 4 5 6 7 8 9  -9-8-7-6-5-4-3-2-1 0
             Re ->

When q = 3 mod 4:
 square roots exist for:
   - all purely real, and
   - all purely imag elements, but
   - none exist along the diagonals where re = +/- im

When q = 1 mod 4:
  square roots exist for:
    - all purely real, but
    - none of the purely imag elements, and
    - all diagonal elements where re = +/- im

;; For F(17^2) q = 1 mod 4
;; Squares: non-blank locations are elements with square roots in the field
   0  1 2 2 2 2 2 2 2 2 | 2 2 2 2 2 2 2 2 1
 #2A((_ 2 2 _ 2 _ 2 _ _ | _ _ 2 _ 2 _ 2 2)_
  -2 (_ _ 2 _ 2 2 _ _ 2 | 2 _ _ 2 2 _ 2 _)_
  -3 (_ 2 _ 2 _ 2 2 _ _ | _ _ 2 2 _ 2 _ 2)_
  -4 (_ 2 _ _ 2 _ _ 2 2 | 2 2 _ _ 2 _ _ 2)_
  -5 (_ _ _ 2 2 2 _ 2 _ | _ 2 _ 2 2 2 _ _)_
  -6 (_ _ 2 _ _ 2 2 2 _ | _ 2 2 2 _ _ 2 _)_
  -7 (_ _ _ 2 _ _ 2 2 2 | 2 2 2 _ _ 2 _ _)_
  -8 (_ 2 2 2 _ _ _ _ 2 | 2 _ _ _ _ 2 2 2)_
    -------------------|------------------
   8 (_ 2 2 2 _ _ _ _ 2 | 2 _ _ _ _ 2 2 2)_
   7 (_ _ _ 2 _ _ 2 2 2 | 2 2 2 _ _ 2 _ _)_
   6 (_ _ 2 _ _ 2 2 2 _ | _ 2 2 2 _ _ 2 _)_
   5 (_ _ _ 2 2 2 _ 2 _ | _ 2 _ 2 2 2 _ _)_
   4 (_ 2 _ _ 2 _ _ 2 2 | 2 2 _ _ 2 _ _ 2)_
 ^ 3 (_ 2 _ 2 _ 2 2 _ _ | _ _ 2 2 _ 2 _ 2)_
 | 2 (_ _ 2 _ 2 2 _ _ 2 | 2 _ _ 2 2 _ 2 _)_
Im 1 (_ 2 2 _ 2 _ 2 _ _ | _ _ 2 _ 2 _ 2 2)_
   0 (1 2 2 2 2 2 2 2 2 | 2 2 2 2 2 2 2 2)1)
      0 1 2 3 4 5 6 7 8  -8-7-6-5-4-3-2-1 0
       Re ->
|#
|#

