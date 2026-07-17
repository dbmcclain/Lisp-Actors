
(in-package :com.ral.rb-trees.sets)

;; --------------------------------------------------
;; Shared variant - lock free

(um:make-encapsulated-type SE SE? SD)

(defun make-shared-set (&rest args
                              &key
                              tree-type
                              compare-fn
                              replace-p-fn)
  (declare (ignore tree-type compare-fn replace-p-fn))
  (SE (apply #'make-tree args)))

(defun rd-set (set)
  (um:rd (SD set)))

(defmacro with-set ((s set) &body body)
  `(let ((,s (rd-set ,set)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-set" 1)

(defun %rmw-set (set fn)
  (um:rmw (SD set) fn))

(defmacro rmw-set ((s set) &body body)
  `(%rmw-set ,set (lambda (,s)
                    ,@body)))

#+:LISPWORKS
(editor:setup-indent "rmw-set" 1)

(defmethod copy ((set SE))
  (with-set (s set)
    (SE s)))

(defmethod add ((set SE) key &optional val)
  (rmw-set (s set)
    (add s key val)))

(defmethod min-elt ((set SE))
  (with-set (s set)
    (min-elt s)))

(defmethod max-elt ((set SE))
  (with-set (s set)
    (max-elt s)))

(defmethod mem ((set SE) key)
  (with-set (s set)
    (mem s key)))

(defmethod remove ((set SE) key)
  (rmw-set (s set)
    (remove s key)))

(defmethod union ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (SE (union s1 s2)))))

(defmethod intersection ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (SE (intersection s1 s2)))))

(defmethod diff ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (SE (diff s1 s2)))))

(defmethod ord:compare ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (ord:compare s1 s2))))

(defmethod subset ((set1 SE) (set2 SE))
  (with-set (s1 set1)
    (with-set (s2 set2)
      (subset s1 s2))))

(defmethod iter ((set SE) fn)
  (with-set (s set)
    (iter s (unary-fn fn))))

(defmethod fold ((set SE) fn accu)
  (with-set (s set)
    (fold s (lambda (k v acc)
              (declare (ignore v))
              (funcall fn k acc))
          accu)))

(defmethod every ((set SE) pred)
  (with-set (s set)
    (every s (unary-fn pred))))

(defmethod some ((set SE) pred)
  (with-set (s set)
    (some s (unary-fn pred))))

(defmethod filter ((set SE) pred)
  (with-set (s set)
    (SE (filter s (unary-fn pred)))))

(defmethod partition ((set SE) pred)
  (with-set (s set)
    (destructuring-bind (tp fp)
        (partition s (unary-fn pred))
      (list (SE tp) (SE fp))
      )))

(defmethod cardinal ((set SE))
  (with-set (s set)
    (cardinal s)))

(defmethod elements ((set SE))
  (with-set (s set)
    (let ((elts (elements s)))
      (map-into elts #'car elts)
      )))

(defmethod choose ((set SE))
  (with-set (s set)
    (choose s)))

(defmethod view-set ((set SE) &rest args &key &allow-other-keys)
  (with-set (s set)
    (apply #'view-set s args)))

(defmethod erase ((set SE))
  (rmw-set (s set)
    (funcall s :clone-with (empty))
    ))

;; ------------------------------------------
;; Encapsulation coercion

(defmethod copy-as-shared ((set SE))
  (copy set))

(defmethod copy-as-shared ((set UE))
  (SE (UD set)))

(defmethod copy-as-unshared ((set SE))
  (with-set (s set)
    (UE s)))

(defmethod copy-as-unshared ((set UE))
  (copy set))


