#| DATE           : 28Dec1 
 | USER           : Dave 
 | PROCESSED FILE : D:\Projects\LispWorks\vmath\fft_intf.h
 |#

(in-package "FFT")

;;; Derived from file : "C:\\TEMP\\PCL188.h"
;;; Hand edited for module linkage DM/MCFA 12/01

(defvar *fftx-library* (translate-logical-pathname "PROJECTS:LIB;fftx.dll"))

"linkage-specifier -> \"C\" (NIL NIL) "
(fli:define-foreign-function (getversionstring
                              "GetVersionString"
                              :source)
    ((buf (:pointer :char)) (nbuf :long))
  :result-type :void
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (d2zfftf "d2zfftf" :source)
    ((nx :long)
     (ny :long)
     (src (:pointer :double))
     (dst (:pointer :double)))
  :result-type :long
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (z2dfftf "z2dfftf" :source)
    ((nx :long)
     (ny :long)
     (src (:pointer :double))
     (dst (:pointer :double)))
  :result-type :long
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (z2zfftf "z2zfftf" :source)
    ((nx :long)
     (ny :long)
     (src (:pointer :double))
     (dst (:pointer :double))
     (direction :long))
  :result-type :long
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (r2cfftf "r2cfftf" :source)
    ((nx :long)
     (ny :long)
     (src (:pointer :float))
     (dst (:pointer :float)))
  :result-type :long
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (c2rfftf "c2rfftf" :source)
    ((nx :long)
     (ny :long)
     (src (:pointer :float))
     (dst (:pointer :float)))
  :result-type :long
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (c2cfftf "c2cfftf" :source)
    ((nx :long)
     (ny :long)
     (src (:pointer :float))
     (dst (:pointer :float))
     (direction :long))
  :result-type :long
  :module *fftx-library*
  :language :c
  :calling-convention :cdecl)
"end-of-linkage-specifier (NIL)"
