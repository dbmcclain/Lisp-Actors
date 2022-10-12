
(in-package :useful-macros)

(defconstant +sc-nprocessors-onln+ 58)

#+:MACOSX
(cffi:defcfun "sysconf" :long
  (name :int))

(defun get-number-of-processors ()
  (sysconf +sc-nprocessors-onln+))


