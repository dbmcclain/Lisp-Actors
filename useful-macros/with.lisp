;; with.lisp - OCaml-style WITH for structs, class instances, condition objects
;;
;; This helps us write FPL style code by not mutating an original
;; object, but rather constructing a copy with modifications.
;;
;; DM/RAL  2024/03/09 12:51:19 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.with
  (:use #:common-lisp #:com.ral.useful-macros))

(in-package #:com.ral.useful-macros.with)

;; ----------------------------------

#+:LISPWORKS
(defmethod with ((obj structure-object) &rest props)
  (let* ((new  (copy-structure obj)))
    (nlet iter ((props props))
      (if (endp props)
          new
        ;; else
        (destructuring-bind (sym val . rest) props
          (let ((slot-name (find-symbol (symbol-name sym)
                                        (symbol-package (class-name (class-of obj))))))
            (setf (slot-value new slot-name) val)
            (go-iter rest)))
        ))))

(defgeneric copyable-slots-using-class (obj class)
  (:method (obj (class standard-class))
   (class-slots class))
  #+(or sbcl cmu openmcl)
  (:method (obj (class structure-class))
   (class-slots class))
  #+allegro
  (:method (obj (class clos::structure-class))
   (class-slots class))
  #+sbcl
  (:method (obj (class sb-pcl::condition-class))
   (class-slots class))
  #+cmu
  (:method (obj (class pcl::condition-class))
   (class-slots class)))

(defgeneric copyable-slots (obj)
  (:method ((obj standard-object))
   (copyable-slots-using-class obj (class-of obj)))
  #+(or sbcl cmu openmcl allegro)
  (:method ((obj structure-object))
   (copyable-slots-using-class obj (class-of obj)))
  (:method ((obj condition))
   (copyable-slots-using-class obj (class-of obj))))

(defun copy-obj (obj &rest props)
  (let* ((class-name (class-name (class-of obj)))
         (pkg        (symbol-package class-name))
         (slot-names (remove-if
                      (complement
                       (lambda (slot)
                         (let ((slot-name (slot-definition-name slot)))
                           (and (slot-boundp obj slot-name)
                                (not (eql (slot-definition-allocation slot) :class)))
                           )))
                      (copyable-slots obj)))
         (extant-props (mapcan (lambda (slot-name)
                                 (list slot-name (slot-value obj slot-name)))
                               slot-names))
         (new-props (nlet iter ((props (reverse props))
                                (ans   nil))
                      (if (endp props)
                          ans
                        ;; translate slot keyword to slot name
                        (destructuring-bind (val name . rest) props
                          (go-iter rest (list* (find-symbol (symbol-name name) pkg)
                                               val
                                               ans)))
                        ))))
    (apply #'make-instance class-name (append new-props extant-props))))
    
(defmethod with ((obj standard-object) &rest props)
  (apply #'copy-obj obj props))

(defmethod with ((obj condition) &rest props)
  (apply #'copy-obj obj props))

#|
(defstruct my-thing
  (a nil :read-only t)
  b c)

(let* ((x  (make-my-thing :a 1 :b 2 :c 3)))
  (with x
    :a 15))
|#