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

(defmethod make-instance ((singleton-class singleton-class) &key)
  (or (slot-value singleton-class 'instance)
      (setf (slot-value singleton-class 'instance) (call-next-method))))

;; (defclass foobar (#|...|#)
;; (#|...|#)
;; (:metaclass singleton-class))
