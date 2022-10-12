
(defpackage :com.ral.useful-macros.nbr-cpus
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.nbr-cpus)

(defconstant +sc-nprocessors-onln+ 58)

#+:MACOSX
(cffi:defcfun "sysconf" :long
  (name :int))

(defun get-number-of-processors ()
  (sysconf +sc-nprocessors-onln+))


