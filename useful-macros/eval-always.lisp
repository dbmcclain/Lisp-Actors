
(defpackage :com.ral.useful-macros.eval-always
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.eval-always)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
