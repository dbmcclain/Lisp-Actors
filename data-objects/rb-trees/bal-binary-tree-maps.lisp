;; --------------------------------------------
(in-package :com.ral.rb-tree.maps)
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------

(defstruct (map-cell
            (:constructor map-cell (key val)))
  key val)

(defmethod ord:compare ((a map-cell) (b map-cell))
  ;; for comparing two map cells
  ;; used by sets:add
  (ord:compare (map-cell-key a) (map-cell-key b)))

(defmethod ord:compare (a (b map-cell))
  ;; for comparing keys against map-cells
  (ord:compare a (map-cell-key b)))

(defmethod ord:compare ((a map-cell) b)
  ;; for comparing map-cells against keys
  ;; allows us to perform (set:diff map set) to remove keys
  (ord:compare (map-cell-key a) b))

;; ----------------------------------------------

(defmethod different-value ((a map-cell) (b map-cell))
  (not (eql (map-cell-val a) (map-cell-val b))))

(defmethod different-value (a b)
  t)

(defmethod add ((map tree) key val &key (replace t))
  (sets:with-replacement-p (and replace 'different-value)
    (sets:add map
              (map-cell key val))
    ))

(define-modify-macro addf (key value &rest args)
  add)

(defmethod find ((map tree) key &optional default)
  ;; eval with contant stack space - S(1)
  (multiple-value-bind (found cell) (mem map key)
    (if found
        (values (map-cell-val cell) t)
      (values default nil))))

(defmethod fold ((map tree) f accu)
  ;; eval with S(Log2(N))
  (sets:fold map
             #'(lambda (cell accu)
                 (funcall f (map-cell-key cell) (map-cell-val cell) accu))
             accu))

(defmethod mapi ((map tree) f)
  ;; eval with S(Log2(N))
  (let ((new-map (empty)))
    (sets:iter map
               #'(lambda (cell)
                 (let ((key (map-cell-key cell)))
                   (setf new-map (add new-map key (funcall f key (map-cell-val cell)))
                         ))))
    new-map))

(defmethod map ((map tree) f)
  (mapi map
        #'(lambda (k v)
            (declare (ignore k))
            (funcall f v))
        ))

(defmethod iter ((map tree) f)
  (sets:iter map
             #'(lambda (cell)
                 (funcall f (map-cell-key cell) (map-cell-val cell)))
             ))

(defmethod add-plist ((map tree) plist)
  ;; plist = alternating key,value pairs
  (um:nlet iter ((lst plist))
    (when lst
      (addf map (car lst) (cadr lst))
      (go-iter (cddr lst)))
    map))

(defmethod add-alist ((map tree) alist)
  ;; alist list of pairs (key value)
  (um:nlet iter ((lst alist))
    (when lst
      (addf map (caar lst) (cadar lst))
      (go-iter (cdr lst)))
    map))

(defmethod add-hashtable ((map tree) hashtable)
  (maphash (um:curry 'addf map) hashtable)
  map)

(defmethod add-keys-vals ((map tree) keys vals)
  ;; keys and vals are lists of correspoinding key/val pairs
  (um:nlet iter ((ks keys)
                 (vs vals))
    (when (and ks vs)
      (addf map (car ks) (car vs))
      (go-iter (cdr ks) (cdr vs)))
    map))

;; ----------------------------------------------
;; Unshared variant

(um:make-encapsulated-type UE UE? UD)

(defun make-unshared-map ()
  (UE (empty)))

(defmethod copy ((map UE))
  (UE (UD map)))

(defmethod is-empty ((map UE))
  (is-empty (UD map)))

(defmethod add ((map UE) key val &key (replace t))
  (setf (UD map) (add (UD map) key val :replace replace)))

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
  (setf (UD map) (empty)))

;; ----------------------------------------------
;; Shared variant - lock free

(um:make-encapsulated-type SE SE? SD)

(defun make-shared-map ()
  (SE (empty)))

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

(defmethod add ((map SE) key val &key (replace t))
  (rmw-map (m map)
    (add m key val :replace replace)))

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
    (declare (ignore m))
    (empty)))

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

;; ----------------------------------------------

#|
;; test code for map
(let ((x (empty)))
  (setf x (add 'this 15 x))
  (setf x (add 'that 32 x))
  (setf x (add 'thother 64 x))
  (inspect x)
  (sets:iter x #'print)
  (find 'else x :not-found)
  (fold x
        (lambda (k v acc)
          (cons (print (list k (+ v 3))) acc))
        nil))
  |#
#||#
;; ----------------------------------------------

#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'list-form)))
  (declare (ignore mode))
  (let* ((elts (sets:elements map)))
    (if (every #'map-cell-p elts)
        (let ((keys (mapcar #'map-cell-key elts))
              (vals (mapcar #'map-cell-val elts)))
          (values keys vals)) ;; :entries (list (mapcar #'cons keys vals))))
      (values :elements (list elts)))))

(defmethod key-fn ((cell map-cell))
  (map-cell-key cell))

#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'graph-form)))
  (declare (ignore mode))
  (values :graph (sets:view-set map)))
#||#  
