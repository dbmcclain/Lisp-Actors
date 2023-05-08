;; option.lisp
;;
;; DM/RAL  2023/05/06 09:58:20
;; ----------------------------------

(defpackage #:com.ral.useful-macros.option
  (:use #:common-lisp #:um)
  (:export
   #:thing
   #:nothing
   #:something
   #:thing-p
   #:nothing-p
   #:something-p
   #:mapfn))

(in-package #:com.ral.useful-macros.option)

;; ----------------------------------
;; Classes

(defclass thing ()
  ())

(defclass nothing (thing)
  ())

(defclass something (thing)
  ((obj  :reader something-obj :initarg :obj)))

;; ---------------------------------------------
;; Constructors

(deflex nothing  ;; singleton instance
  (make-instance 'nothing))

(defun nothing (&rest args)
  (declare (ignore args))
  nothing)

(defun something (x)
  (make-instance 'something :obj x))

;; ---------------------------------------------
;; Operators

(defgeneric thing-p (x)
  (:method ((x thing))
   t)
  (:method (x)
   nil))

(defgeneric nothing-p (x)
  (:method ((x nothing))
   t)
  (:method (x)
   nil))

(defgeneric something-p (x)
  (:method ((x something))
   t)
  (:method (x)
   nil))

(defgeneric mapfn (fn obj)
  (:method ((fn function) (obj nothing))
   nothing)
  (:method ((fn function) (obj something))
   (something (funcall fn (something-obj obj)))))

(defgeneric thing (x &optional default)
  (:method ((x nothing) &optional default)
   default)
  (:method ((x something) &optional default)
   (declare (ignore default))
   (something-obj x)))
