
(in-package :com.ral.rb-trees.sets)

;; --------------------------------------------------
;; Unshared variant

(um:make-encapsulated-type UE UE? UD)

(defun make-unshared-set (&key (tree-type +default-tree-type+))
  (UE (make-tree :tree-type tree-type)))

(defmethod copy ((set UE))
  (UE (UD set)))

(defmethod add ((set UE) k &optional v)
 (addf (UD set) k v))

(defmethod min-elt ((set UE))
  (min-elt (UD set)))

(defmethod max-elt ((set UE))
  (max-elt (UD set)))

(defmethod mem ((set UE) k)
  (mem (UD set) k))

(defmethod remove ((set UE) k)
  (setf (UD set) (remove (UD set) k)))

(defmethod union ((set1 UE) (set2 UE))
  (UE (union (UD set1) (UD set2))))

(defmethod intersection ((set1 UE) (set2 UE))
  (UE (intersection (UD set1) (UD set2))))

(defmethod diff ((set1 UE) (set2 UE))
  (UE (diff (UD set1) (UD set2))))

(defmethod ord:compare ((set1 UE) (set2 UE))
  (ord:compare (UD set1) (UD set2)))

(defmethod subset ((set1 UE) (set2 UE))
  (subset (UD set1) (UD set2)))

(defun unary-fn (fn)
  (lambda (k v)
    (declare (ignore v))
    (funcall fn k)))

(defmethod iter ((set UE) fn)
  (iter (UD set) (unary-fn fn)))

(defmethod fold ((set UE) fn accu)
  (fold (UD set) (lambda (k v acc)
                   (declare (ignore v))
                   (funcall fn k acc))
        accu))

(defmethod every ((set UE) pred)
  (every (UD set) (unary-fn pred)))

(defmethod some ((set UE) pred)
  (some (UD set) (unary-fn pred)))

(defmethod filter ((set UE) pred)
  (UE (filter (UD set) (unary-fn pred))))

(defmethod partition ((set UE) pred)
  (destructuring-bind (tp fp)
      (partition (UD set) (unary-fn pred))
    (list (UE tp) (UE fp))
    ))

(defmethod cardinal ((set UE))
  (cardinal (UD set)))

(defmethod elements ((set UE))
  (let ((elts (elements (UD set))))
    (map-into elts #'car elts)))

(defmethod choose ((set UE))
  (choose (UD set)))

(defmethod view-set ((set UE) &rest args &key &allow-other-keys)
  (apply #'view-set (UD set) args))

(defmethod erase ((set UE))
  (setf (UD set) (um:with (UD set)
                   :nodes (empty))))


