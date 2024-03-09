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

(eval-always
  (import '(closer-mop:class-slots
            closer-mop:slot-definition-name
            closer-mop:slot-definition-allocation
            closer-mop:slot-definition-initargs
            )))

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
  (let* ((class (class-of obj))
         (prop-names (nlet iter ((props props)
                                 (ans   nil))
                       (if (endp props)
                           ans
                         (destructuring-bind (key val . rest) props
                           (declare (ignore val))
                           (go-iter rest (cons key ans)))
                         )))
         (slots (remove-if
                 (complement
                  (lambda (slot)
                    (let ((slot-name (slot-definition-name slot)))
                      (and (slot-boundp obj slot-name)
                           (slot-definition-initargs slot)
                           (null (intersection (slot-definition-initargs slot) prop-names)))
                      )))
                 (copyable-slots obj)))
         (extant-props (mapcan (lambda (slot)
                                 (list (car (slot-definition-initargs slot))
                                       (slot-value obj (slot-definition-name slot))))
                               slots)))
    (apply #'make-instance class (append props extant-props))))
    
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

(defclass my-obj ()
  ((a   :accessor my-obj-a :initarg :a)
   (b   :accessor my-obj-b :initarg :b)))

(let ((x (make-instance 'my-obj
                        :a  1
                        :b  2)))
  (inspect (list x (with x :a 15)))
  ;; (inspect (mapcar #'slot-definition-name (copyable-slots x)))
  )
|#