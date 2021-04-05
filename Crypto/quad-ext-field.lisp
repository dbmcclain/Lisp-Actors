
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

(defun qf^ (qfx exp)
  ;; for integer exp
  (declare (integer exp))
  (multiple-value-bind (qfx exp)
      (if (minusp exp)
          (values (qf/ qfx) (- exp))
        (values (qfmod qfx) exp))
    (um:nlet iter ((qfx qfx)
                   (exp exp)
                   (ans 1))
      (declare (integer exp))
      (if (zerop exp)
          ans
        (multiple-value-bind (q r) (truncate exp *m*)
          (declare (integer q r))
          (let ((rans (case r
                        (0  1)
                        (1  qfx)
                        (2  (qfsqr qfx))
                        (t  (generalized-windowed-exponentiation qfx r
                                                                 :window-nbits 4
                                                                 :op-mul #'qf-mul
                                                                 :op-sqr #'qfsqr))
                        )))
            (go-iter (qf-conj qfx) q (qf-mul ans rans))
            ))))
    ))

#|
(let* ((p 19)
       (arr (make-array (list p p)
                        :initial-element 0))
       (pt  '(4 . 3)))
  ;; (incf (aref arr (qf-im pt) (qf-re pt)))
  #|
  (with-qf p
    (loop for ix from 0 below (* 19 19) do
          (let ((y (qf^ pt ix)))
            (incf (aref arr (qf-im y) (qf-re y)))
            )))
  |#
  #||#
  (with-qf p
    (loop for ix from 0 below p do
          (loop for iy from 0 below p do
                (let ((y (qf^ (cons iy ix) 2)))
                  (incf (aref arr (qf-im y) (qf-re y)))))))
  #||#
  (loop for ix from 0 below p do
        (loop for iy from 0 below p do
              (when (zerop (aref arr ix iy))
                (setf (aref arr ix iy) '_))))
  arr)
|#
