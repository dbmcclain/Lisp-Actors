
(in-package :modmath)

(defclass fq2 ()
  ((val  :reader  fq2-val
         :initarg :val)))

(defun get-im-unit (base)
  (get-cached-symbol-data '*m* :im^2 base
                          (lambda ()
                            (um:nlet iter ((a -2))
                              (declare (integer a))
                              (if (quadratic-residue-p a)
                                  (go-iter (1- a))
                                a)))
                          ))

(defmethod fq2conj ((a cons))
  (destructuring-bind (are . aim) a
    (declare (integer are aim))
    (cons are (m- aim))))

(defmethod fq2tr ((a cons))
  (fq2+ a (fq2conj a)))

(defmethod fq2norm ((a cons))
  (fq2* a (fq2conj a)))

(defmethod fq2+ ((a cons) (b cons))
  (destructuring-bind (are . aim) a
    (declare (integer are aim))
    (destructuring-bind (bre . bim) b
      (declare (integer bre bim))
      (cons (m+ are bre)
            (m+ aim bim)))))


(defmethod fq2- ((a cons) (b cons))
  (destructuring-bind (are . aim) a
    (declare (integer are aim))
    (destructuring-bind (bre . bim) b
      (declare (integer bre bim))
      (cons (m- are bre)
            (m- aim bim)))))


(defmethod fq2* ((a cons) (b cons))
  (destructuring-bind (are . aim) a
    (declare (integer are aim))
    (destructuring-bind (bre . bim) b
      (declare (integer bre bim))
      (let ((im^2 (get-im-unit *m*)))
        (declare (integer im^2))
        (cons
         (m+ (m* are bre)
             (m* aim bim im^2))
         (m+ (m* are bim)
             (m* aim bre)))
        ))))

(defmethod fq2sqr ((a cons))
  (destructuring-bind (are . aim) a
    (declare (integer are aim))
    (let ((im^2 (get-im-unit *m*)))
      (declare (integer im^2))
      (cons
       (m+ (m* are are)
           (m* aim aim im^2))
       (m* 2 are aim)))))

(defmethod fq2^ ((x cons) (y integer))
  (generalized-windowed-exponentiation x y
                                       :window-nbits 4
                                       :op-mul 'fq2*
                                       :op-sqr 'fq2sqr))

(defmethod fq2inv ((x cons))
  (fq2^ x (- (* *m* *m*) 2)))

(defmethod fq2/ ((a cons) (b cons))
  (let ((im^2 (get-im-unit *m*)))
    (declare (integer im^2))
    (destructuring-bind (are . aim) a
      (declare (integer are aim))
      (destructuring-bind (bre . bim) b
        (declare (integer bre bim))
        (let ((sf (m/ (m- (m* bre bre)
                          (m* bim bim im^2)))))
          (declare (integer sf))
          (cons (m* sf
                    (m- (m* are bre)
                        (m* aim bim im^2)))
                (m* sf
                    (m- (m* aim bre)
                        (m* are bim)))
                ))))))

