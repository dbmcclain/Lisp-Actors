
(in-package :com.ral.rb-trees.maps)

;; ----------------------------------------------
;; Shared variant - lock free

(um:make-encapsulated-type SE SE? SD)

(defun make-shared-map (&rest args
                              &key
                              tree-type
                              compare-fn
                              replace-p-fn)
  (declare (ignore tree-type compare-fn replace-p-fn))
  (SE (apply #'make-tree args)))

(defun rd-map (map)
  (um:rd (SD map)))

(defmacro with-map ((m map) &body body)
  `(let ((,m (rd-map ,map)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-map" 1)

(defun %rmw-map (map fn)
  (um:rmw (SD map) fn))

(defmacro rmw-map ((m map) &body body)
  `(%rmw-map ,map (lambda (,m)
                    ,@body)))

#+:LISPWORKS
(editor:setup-indent "rmw-map" 1)

(defmethod copy ((map SE))
  (with-map (m map)
    (SE m)))

(defmethod remove ((map SE) key)
  (rmw-map (m map)
    (remove m key)))

(defmethod mem ((map SE) key)
  (with-map (m map)
    (mem m key)))

(defmethod diff ((map1 SE) (map2 SE))
  (with-map (m1 map1)
    (with-map (m2 map2)
      (SE (diff m1 m2)))))

(defmethod intersection ((map1 SE) (map2 SE))
  (with-map (m1 map1)
    (with-map (m2 map2)
      (SE (intersection m1 m2)))))

(defmethod union ((map1 SE) (map2 SE))
  (with-map (m1 map1)
    (with-map (m2 map2)
      (SE (union m1 m2)))))

(defmethod cardinal ((map SE))
  (with-map (m map)
    (cardinal m)))

(defmethod view-set ((map SE) &rest args &key &allow-other-keys)
  (with-map (m map)
    (apply #'view-set m args)))

(defmethod add ((map SE) key &optional val)
  (rmw-map (m map)
    (add m key val)))

(defmethod find ((map SE) key &optional default)
  (with-map (m map)
    (find m key default)))

(defmethod fold ((map SE) f accu)
  (with-map (m map)
    (fold m f accu)))

(defmethod mapi ((map SE) f)
  (with-map (m map)
    (mapi m f)))

(defmethod map ((map SE) f)
  (with-map (m map)
    (map m f)))

(defmethod iter ((map SE) f)
  (with-map (m map)
    (iter m f)))

(defmethod add-plist ((map SE) plist)
  (rmw-map (m map)
    (add-plist m plist)))

(defmethod add-alist ((map SE) alist)
  (rmw-map (m map)
    (add-alist m alist)))

(defmethod add-hashtable ((map SE) hashtable)
  (rmw-map (m map)
    (add-hashtable m hashtable)))

(defmethod add-keys-vals ((map SE) keys vals)
  (rmw-map (m map)
    (add-keys-vals m keys vals)))

(defmethod erase ((map SE))
  (rmw-map (m map)
    (funcall m :clone-with (empty))
    ))

;; -----------------------------------------
;; Encapsulation coercion

(defmethod copy-as-shared ((map SE))
  (copy map))

(defmethod copy-as-unshared ((map SE))
  (with-map (m map)
    (UE m)))

(defmethod copy-as-unshared ((map UE))
  (copy map))

(defmethod copy-as-shared ((map UE))
  (SE (UD map)))

