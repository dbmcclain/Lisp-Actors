
(In-package #:com.ral.rb-trees.sets)

(defun make-set-type (&rest args &key compare-fn)
  (declare (ignore compare-fn))
  (apply #'make-tree-type args))

(defmethod set-type-compare-fn ((s set))
  (tree-type-compare-fn s))

(defun make-set (&rest args
                       &key
                       tree-type
                       compare-fn)
  (declare (ignore tree-type compare-fn))
  (apply #'make-instance 'set args))

(defmethod make-set-like ((s set))
  (make-tree-like s))

(defmethod set-type ((s set))
  (tree-type s))

(defmethod add ((set set) key)
  (trees:add set key))

(define-modify-macro addf (key)
  add)

#+:LISPWORKS
(defmethod view-set ((s set) &rest args
                     &key key layout)
  (declare (ignore key layout))
  (apply #'view-tree s args))
