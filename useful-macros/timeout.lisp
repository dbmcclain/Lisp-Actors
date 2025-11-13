
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

(defmacro with-default-timeout (dt &rest body)
  `(with-timeout (or *timeout* ,dt)
     ,@body))

#+:LISPWORKS
(progn
  (editor:setup-indent "with-timeout" 1)
  (editor:setup-indent "with-default-timeout" 1))

(deflex +timed-out+
  (make-condition 'timeout))

