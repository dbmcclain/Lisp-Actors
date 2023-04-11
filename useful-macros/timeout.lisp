
(defpackage :com.ral.useful-macros.timeout
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.timeout)

(defvar *timeout*  nil)

(define-condition timeout (error)
  ()
  (:report report-timeout-error))

(defun report-timeout-error (err stream)
  (declare (ignore err))
  (format stream "Timeout"))

(defmacro with-timeout (dt &rest body)
  `(let ((*timeout*  ,dt))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-timeout" 1)
