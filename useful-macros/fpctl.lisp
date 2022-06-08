
(in-package :fpctl)

(defconstant +FE_TONEAREST+    #x0000)
(defconstant +FE_DOWNWARD+     #x0400)
(defconstant +FE_UPWARD+       #x0800)
(defconstant +FE_TOWARDZERO+   #x0c00)

(fli:define-foreign-function fegetround
    ()
  :result-type :int)

(fli:define-foreign-function fesetround
    ((mode :int))
  :result-type :int)


