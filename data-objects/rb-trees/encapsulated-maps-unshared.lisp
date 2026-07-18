
(in-package :com.ral.rb-trees.maps)

;; ----------------------------------------------
;; Unshared variant

(um:make-encapsulated-type UE UE? UD)

(defun make-unshared-map (&rest args
                                &key
                                tree-type
                                compare-fn
                                replace-p-fn)
  (declare (ignore tree-type compare-fn replace-p-fn))
  (UE (apply #'make-map args)))

(defmethod copy ((map UE))
  (UE (UD map)))

(defmethod is-empty ((map UE))
  (is-empty (UD map)))

(defmethod add ((map UE) key val)
  (setf (UD map) (add (UD map) key val)))

(defmethod remove ((map UE) key)
  (setf (UD map) (remove (UD map) key)))

(defmethod mem ((map UE) key)
  (mem (UD map) key))

(defmethod diff ((map1 UE) (map2 UE))
  (UE (diff (UD map1) (UD map2))))

(defmethod intersection ((map1 UE) (map2 UE))
  (UE (intersection (UD map1) (UD map2))))

(defmethod union ((map1 UE) (map2 UE))
  (UE (union (UD map1) (UD map2))))

(defmethod cardinal ((map UE))
  (cardinal (UD map)))

(defmethod view-set ((map UE) &rest args &key &allow-other-keys)
  (apply #'view-set (UD map) args))

(defmethod find ((map UE) key &optional default)
  (find (UD map) key default))

(defmethod fold ((map UE) f accu)
  (fold (UD map) f accu))

(defmethod mapi ((map UE) f)
  (mapi (UD map) f))

(defmethod map ((map UE) f)
  (map (UD map) f))

(defmethod iter ((map UE) f)
  (iter (UD map) f))

(defmethod add-plist ((map UE) plist)
  (setf (UD map) (add-plist (UD map) plist)))

(defmethod add-alist ((map UE) alist)
  (setf (UD map) (add-alist (UD map) alist)))

(defmethod add-hashtable ((map UE) hashtable)
  (setf (UD map) (add-hashtable (UD map) hashtable)))

(defmethod add-keys-vals ((map UE) keys vals)
  (setf (UD map) (add-keys-vals (UD map) keys vals)))

(defmethod erase ((map UE))
  (setf (UD map) (funcall (UD map) :clone-with (empty))))

