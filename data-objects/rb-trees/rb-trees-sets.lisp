
(In-package #:com.ral.rb-trees.sets)

(defclass set (tree)
  ()
  (:metaclass
   #+:LISPWORKS clos:funcallable-standard-class
   #+:SBCL      sb-mop:funcallable-standard-class))

;; --------------------------------------------

(defun make-set-type (&rest args &key compare-fn)
  (declare (ignore compare-fn))
  (apply #'make-tree-type args))

(defmethod set-type-compare-fn ((s set))
  (tree-type-compare-fn s))

;; --------------------------------------------

(defun make-set (&rest args &key set-type compare-fn)
  ;; Args can be :SET-TYPE, :COMPARE-FN, or none for defaults.
  (declare (ignore compare-fn))
  (apply #'make-instance 'set (if set-type
                                  (list* :tree-type set-type args)
                                args)))

(defmethod make-set-like ((s set))
  (make-tree-like s))

(defmethod set-type ((s set))
  (tree-type s))

;; --------------------------------------------

(defmethod add ((set set) key)
  (trees:add set key))

(define-modify-macro addf (key)
  add)

(defun unary-intf (fn)
  (lambda (k v)
    (declare (ignore v))
    (funcall fn k)))

(defmethod iter ((s set) fn)
  (call-next-method s (unary-intf fn)))

(defmethod fold ((s set) fn accu)
  (call-next-method s (lambda (k v acc)
                        (declare (ignore v))
                        (funcall fn k acc))
                    accu))

(defmethod every ((s set) pred)
  (call-next-method s (unary-intf pred)))

(defmethod some ((s set) pred)
  (call-next-method s (unary-intf pred)))

(defmethod filter ((s set) pred)
  (call-next-method s (unary-intf pred)))

(defmethod partition ((s set) pred)
  (call-next-method s (unary-intf pred)))


(defmethod elements ((s set))
  (mapcar #'car (call-next-method)))

#+:LISPWORKS
(defmethod view-set ((s set) &rest args
                     &key key layout)
  (declare (ignore key layout))
  (apply #'view-tree s args))
