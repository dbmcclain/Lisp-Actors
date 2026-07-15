;; singleton-class.lisp - adapted fromn Didier Verna
;;
;; DM/RAL  2026/05/18 23:20:25 UTC
;; ----------------------------------

(defpackage #:com.ral.singleton-class
  (:use #:common-lisp #:com.ral.useful-macros)
  (:import-from #:closer-mop
   #:validate-superclass))

(in-package #:com.ral.singleton-class)

;; ----------------------------------

(defclass singleton-class (standard-class)
  ((instance :initform nil)))

(defmethod validate-superclass ((class singleton-class) (superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class) (superclass singleton-class))
  nil)

(defvar *singleton-lock*  (mpc:make-lock))

(defmethod make-instance ((class-obj singleton-class) &key)
  (with-slots (instance) class-obj
    (or instance
        (mpc:with-lock (*singleton-lock*)
          (or instance
              (setf instance (call-next-method))
              ))
        )))

;; (defclass foobar (#|...|#)
;; (#|...|#)
;; (:metaclass singleton-class))
#|
(defclass foobar ()
  ()
  (:metaclass singleton-class))

(defclass foobar2 ()
  ()
  (:metaclass singleton-class))
(inspect (find-class 'foobar))
(inspect (find-class 'foobar2))

(defparameter *foo* (make-instance 'foobar))
(make-instance 'foobar2)
|#

