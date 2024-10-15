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
    (with-smart-object x
      (x :a))))
|#
;; ------------------------------------

(defun slot-ref (obj sym)
  ;; This probably represents a substantial slowdown over using direct
  ;; accessors.
  ;;
  ;; The advantage is that we don't need to know the package where the
  ;; struct class was defined, in order to refer to its slots.
  ;;
  (let* ((pkg       (symbol-package (class-name (class-of obj))))
         (slot-name (or  ;; for decent error reporting on missing slots
                     (find-symbol (symbol-name sym) pkg)
                     sym)))
    (slot-value obj slot-name)))

(defmacro slot-refmaker (name sym &rest args)
  (case (kwsymb sym)
    (:with
        `(with ,name ,@args))
    (otherwise
     `(slot-ref ,name ',sym))
    ))

(defmacro with-smart-objects (names &body body)
  `(macrolet ,(mapcar (lambda (name)
                        `(,name (sym &rest args)
                                `(slot-refmaker ,',name ,sym ,@args) ))
                      names)
     ,@body))

(defmacro with-smart-object (name &body body)
  ;; Turn a struct or class instance binding into a function that can either
  ;; make a copy of itself using WITH, or else access a slot value.
  `(with-smart-objects (,name) ,@body))

;; ------------------------------------
;; Do something Fortran'ish for arrays and vectors

(defmacro with-smart-arrays (names &body body)
  `(macrolet ,(mapcar (lambda (name)
                        `(,name (ix &rest ixs)
                            `(aref ,',name ,ix ,@ixs)))
                      names)
     ,@body))

(defmacro with-smart-array (name &body body)
  `(with-smart-arrays (,name) ,@body))


(defmacro with-smart-row-major-arrays (names &body body)
  `(macrolet ,(mapcar (lambda (name)
                        `(,name (ix)
                            `(row-major-aref ,',name ,ix)))
                      names)
     ,@body))

(defmacro with-smart-row-major-array (name &body body)
  `(with-smart-row-major-arrays (,name) ,@body))


#+:LISPWORKS
(progn
  (editor:setup-indent "with-smart-object"  1)
  (editor:setup-indent "with-smart-objects" 1)
  (editor:setup-indent "with-smart-array"   1)
  (editor:setup-indent "with-smart-arrays"  1)
  (editor:setup-indent "with-smart-row-major-array"  1)
  (editor:setup-indent "with-smart-row-major-arrays" 1))

;; --------------------------------------------

(defmacro with-plist (bindings plist &body body)
  (let ((gplist (gensym)))
    `(let ((,gplist ,plist))
       (symbol-macrolet ,(mapcar (lambda (a1)
                                   `(,(car a1) (getf ,gplist ,@(cdr a1))))
                                 bindings)
         ,@body))
    ))

(defmacro with-alist (bindings alist &body body)
  (let ((galist (gensym)))
    `(let ((,galist ,alist))
       (symbol-macrolet ,(mapcar (lambda (a1)
                                   `(,(car a1) (cdr (assoc ,(cadr a1) ,galist ,@(cddr a1)))))
                                 bindings)
         ,@body))
    ))
