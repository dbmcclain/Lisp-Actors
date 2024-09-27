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

(defmethod with ((obj structure-object) &rest props)
  (let* ((new  (copy-structure obj))
         (pkg  (symbol-package (class-name (class-of obj))) ))
    (loop for (key val) on props by #'cddr do
            (let ((slot-name (find-symbol (symbol-name key) pkg)))
              (setf (slot-value new slot-name) val)))
    new))

(eval-always
  (import '(closer-mop:class-slots
            closer-mop:slot-definition-name
            closer-mop:slot-definition-initargs
            )))

(defgeneric all-slots-using-class (obj class)
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

(defgeneric all-slots (obj)
  (:method ((obj standard-object))
   (all-slots-using-class obj (class-of obj)))
  #+(or sbcl cmu openmcl allegro)
  (:method ((obj structure-object))
   (all-slots-using-class obj (class-of obj)))
  (:method ((obj condition))
   (all-slots-using-class obj (class-of obj))))

(defun copy-obj (obj &rest props)
  (let* ((class (class-of obj))
         (prop-names (loop for key in props by #'cddr collecting key))
         (extant-props (mapcan
                        (lambda (slot)
                          (let ((name     (slot-definition-name slot))
                                (initargs (slot-definition-initargs slot))
                                val)
                            ;; This ought to work for 99% of class instances.
                            ;; But, being Lisp, there will surely be exceptions.
                            (when (and initargs
                                       (null (intersection initargs prop-names))
                                       (ignore-errors
                                         ;; check that we can actually read the slot
                                         ;; it might be unbound, or computed
                                         (setf val (slot-value obj name))
                                         t))
                              (list (car initargs) val))
                            ))
                        (all-slots obj))))
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

(let ((x (make-my-thing
          :a 1
          :b 2
          :c 3)))
  (with-smart-binding x
    (x :a)))
|#

(defmacro with-smart-binding (name &body body)
  ;; Turn a struct or class instance binding into a function that can either
  ;; make a copy of itself using WITH, or else access a slot value.
  `(macrolet ((,name (cmd &rest args)
                (case cmd
                  (with
                      ;; args should be a property list of slot names and values
                      `(with ,',name ,@args))
                  (otherwise
                   ;; cmd should be a slot name for structs & classes
                   `(slot-value ,',name ',(find-symbol (string cmd))))
                  )))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-smart-binding" 1)

(defmacro with-smart-array (name &body body)
  `(macrolet ((,name (ix &rest ixs))
              `(aref ,',name ,ix ,@ixs))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-smart-binding" 1)
