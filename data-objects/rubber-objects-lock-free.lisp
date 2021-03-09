;; --------------------------------------------------------------------
;; rubber-objects.lisp -- Create a stupid-simple flexible object system with single inheritance
;; "Self" without optimizations.
;;
;; DM/RAL 07/16
;; --------------------------------------------------------------------
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

(defpackage :rubber-objects
  (:use #:common-lisp)
  (:nicknames #:ro)
  (:import-from #:useful-macros
   #:defmacro!
   #:nlet
   #:symb
   #:when-let)
  (:export
   #:rubber-object
   #:parent
   #:props
   #:=top=
   #:prop
   #:set-prop ;; (set-prop obj key val), also (setf (prop obj key) val)
   #:make-prop-accessor
   #:make-prop-accessors
   #:direct-prop-keys
   #:prop-keys
   #:has-direct-prop
   #:has-prop
   #:remove-direct-prop
   #:is-a
   #:copy-obj
   #:inherit-from
   #:call
   #:super
   #:defslotfn
   ))

(in-package :rubber-objects)

;; equiv to #F
(proclaim '(optimize (speed  3)
                     ;; (safety 0)
                     (float  0)))

;; ------------------------------------------------------------------
(define-condition no-property (error)
  ((key :reader no-property-key :initarg :key)
   (obj :reader no-property-obj :initarg :obj))
  (:report report-no-property))

(defun report-no-property (cx stream)
  (format stream "No property ~S on ~A"
          (no-property-key cx)
          (no-property-obj cx)))

(define-condition no-next-property (error)
  ((key  :reader next-property-key :initarg :key)
   (obj  :reader next-property-obj :initarg :obj))
  (:report report-no-next-property))

(defun report-no-next-property (cx stream)
  (format stream "No next property ~S on ~A"
          (next-property-key cx)
          (next-property-obj cx)))
  
;; ------------------------------------------------------------------
(defclass rubber-object ()
  ((parent     :reader   parent      :initarg :parent     :initform nil)
   (props-ref  :accessor props-ref   :initarg :props-ref  :initform (ref:ref (maps:empty)))))

(defmethod props ((obj rubber-object))
  (ref:val (props-ref obj)))

(defvar =top= (make-instance 'rubber-object))

(defmethod is-a ((obj rubber-object) archetype)
  (nlet iter ((obj obj))
    (when obj
      (if (eq obj archetype)
          t
        (go-iter (parent obj))))))

(defmethod props (obj)
  nil) ;; default for all other objects

(defmethod parent (obj)
  nil)

(defmethod prop ((obj rubber-object) key &optional default)
  ;; return the direct or ancestor property value
  ;; as a secondary value we return the object in which the prop was found
  (nlet iter ((obj  obj))
    (if obj
        (multiple-value-bind (ans found)
            (maps:find (props obj) key)
          (if found
              (values ans obj)
            ;; else
            (go-iter (parent obj))))
      ;; else
      (values default nil))
    ))

(defmethod set-prop ((obj rubber-object) key val)
  ;; copy-on-write semantics. Any changes to properties
  ;; occur in the direct object, not in any of the inheritaned ancestors
  (um:rmw (props-ref obj) (lambda (map)
                            (maps:add map key val))))

(defsetf prop set-prop)

(defmacro make-prop-accessor (key &optional accessor-name)
  (let ((obj    (gensym (string :obj-)))
        (val    (gensym (string :val-)))
        (reader (symb (or accessor-name key)))
        (writer (symb (gensym (string :set-)))))
    `(progn
       ;; by creating methods here, we allow for the possibility
       ;; that the user could define the same accessors on other types.
       (defmethod ,reader ((,obj rubber-object))
         #F
         (prop ,obj ,key))
       (defmethod ,writer ((,obj rubber-object) ,val)
         #F
         (setf (prop ,obj ,key) ,val))
       (defsetf ,reader ,writer))
    ))

(defmacro make-prop-accessors (&rest keys)
  `(progn
     ,@(mapcar (lambda (key)
                 (cond ((consp key)
                        `(make-prop-accessor ,(car key) ,(cadr key)))
                       (t
                        `(make-prop-accessor ,key))
                       ))
               keys)))

(defmethod print-object ((obj rubber-object) out-stream)
  (handler-case
      (call obj :print-object out-stream)
    (no-property ()
      (call-next-method))))

(defun %direct-prop-keys (obj accum)
  (maps:fold (props obj)
             (lambda (k v accu)
               (declare (ignore v))
               (sets:add accu k))
             accum))

(defmethod direct-prop-keys ((obj rubber-object))
  (sets:elements (%direct-prop-keys obj (sets:empty))))

(defmethod prop-keys ((obj rubber-object))
  (nlet collect ((obj   obj)
                 (accum (sets:empty)))
    (if obj
        (go-collect (parent obj) (%direct-prop-keys obj accum))
      (sets:elements accum))))

(defmethod has-direct-prop ((obj rubber-object) key)
  (second (multiple-value-list (maps:find (props obj) key))))

(defmethod has-prop ((obj rubber-object) key)
  (second (multiple-value-list (prop obj key))))

(defmethod remove-direct-prop ((obj rubber-object) key)
  (um:rmw (props-ref obj)
          (lambda (map)
            (maps:remove map key))))

(defun %merge-props (new-props old-props)
  ;; reversing here removes duplicates by taking the
  ;; earliest as final
  (assert (evenp (length new-props)))
  (um:nlet iter ((lst (reverse new-props)))
    (when lst
      (maps:addf old-props (cadr lst) (car lst))
      (go-iter (cddr lst))))
  old-props)

(defmethod copy-obj ((obj rubber-object) &rest new-props)
  ;; make a copy of an object, possibly with new or modified properties
  ;; result has same parent as source object
  (make-instance (class-of obj)
                 :parent (parent obj)
                 :props  (ref:ref (%merge-props new-props (props obj)))))

(defmethod inherit-from ((obj rubber-object) &rest new-props)
  ;; make a child object with obj as its parent, possibly with
  ;; new or modified properties
  (make-instance (class-of obj)
                 :parent obj
                 :props  (ref:ref (%merge-props new-props (maps:empty)))))

(defmethod inherit-from ((obj null) &rest new-props)
  (apply #'inherit-from =top= new-props))


#| -----------------------------------------------------------------------------------------------
  ;; compare speed of access between property lists and hashtables
  ;; Speed of hashtable is relatively constant for any number of entries in the table (as expected)
  ;; Speed of property list is head:head with hashtable for fewer than 100 elements,
  ;; about half as fast at 500 elements.
(let* ((nel  1000)
       (niter 1000000)
       (keys (loop repeat nel collect (lw:mt-random (* 5 nel))))
       (vals (loop repeat nel collect (lw:mt-random 1000)))
       (ht   (make-hash-table))
       (lst  (mapcan 'list keys vals))
       (map  (maps:empty))
       (queries (loop repeat niter collect (lw:mt-random (* 5 nel)))))
  (loop for key in keys
        for val in vals
        do
        (setf (gethash key ht) val)
        (maps:addf map key val))
  (print "Timing HT")
  (time (dolist (query queries)
          (gethash query ht)))
  (print "Timing Lst")
  (time (dolist (query queries)
          (getf lst query)))
  (print "Timing Map")
  (time (dolist (query queries)
          (maps:find map query)))
  )
|#

(defvar *responder* nil)
(defvar *key*       nil)

(defgeneric call (obj key &rest args)
  (:method ((obj rubber-object) *key* &rest args)
   (multiple-value-bind (fn *responder*) (prop obj *key*)
     (if *responder*
         (if (functionp fn)
             (apply fn obj args)
           fn)
       (error 'no-property :key *key* :obj obj)))))

(defun #1=%call-next (&rest args)
  (when-let (par (parent *responder*))
    (multiple-value-bind (fn *responder*) (prop par *key*)
      (when *responder*
        (return-from #1#
          (if (functionp fn)
              (apply fn args)
            fn))
        )))
  (error 'no-next-property :key *key* :obj *responder*))
    
(defmacro! defslotfn (key obj (&rest args) &body body)
  ;; slot functions can refer to inherited slot functions for the same
  ;; slot key by calling CALL-SUPER
  `(let ((,g!obj ,obj)
         (,g!key ,key))
     (setf (prop ,g!obj ,g!key)
           (lambda (&rest ,g!org-args)
             (flet ((super (&rest ,g!args)
                      (apply #'%call-next (or ,g!args ,g!org-args))))
               (destructuring-bind ,args ,g!org-args
                 ,@body)))
           )))

(editor:setup-indent "defslotfn" 3)

(defslotfn :print-object =top= (obj &optional (stream *standard-output*))
  (print-unreadable-object (obj stream :identity t)
    (format stream "~:(~S~)"
            (class-name (class-of obj))))
  (terpri stream)
  (princ "Properties:" stream)
  (pprint (nreverse
           (maps:fold (props obj)
                      (lambda (k v accum)
                        (acons k v accum))
                     nil))
          stream)
  obj)
