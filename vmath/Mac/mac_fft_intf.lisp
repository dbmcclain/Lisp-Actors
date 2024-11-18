#| DATE           : 28Dec1 
 | USER           : Dave 
 | PROCESSED FILE : D:\Projects\LispWorks\vmath\fft_intf.h
 |#

(in-package #:com.ral.fft)

;;; Derived from file : "C:\\TEMP\\PCL188.h"
;;; Hand edited for module linkage DM/MCFA 12/01

(defvar *fftx-library*
  (fli:register-module :fftlib
                       :real-name
                       (translate-logical-pathname
                        "PROJECTS:LIB;libLispFFT.dylib")))

;; ------------------------------------------------------------
(fli:define-foreign-function (getfftversionstring
                              "GetFFTVersionString"
                              :source)
    ((buf  (:reference-return (:ef-mb-string :limit 256)))
     (:constant 256 :int))
  :result-type :void
  :language :ansi-c
  :module *fftx-library*)

;; ------------------------------------------------------------

(defvar $fftw-forward 0)
(defvar $fftw-inverse 1)

;; ------------------------------------------------------------
;; Double-precision 1D FFTW routines

(fli:define-foreign-function (d2zfft "d2zfft" :source)
    ((nx :int)
     (src (:pointer :double))
     (dst (:pointer :double))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (z2dfft "z2dfft" :source)
    ((nx :int)
     (src (:pointer :double))
     (dst (:pointer :double))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (z2zfft "z2zfft" :source)
    ((nx :int)
     (src (:pointer :double))
     (dst (:pointer :double))
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (unsafe-z2zfft "unsafe_z2zfft" :source)
    ((nx :int)
     (src-r :uintptr)
     (src-i :uintptr)
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

;; ------------------------------------------------------------
;; Double precision 2-D routines

(fli:define-foreign-function (d2zfft2d "d2zfft2d" :source)
    ((nx :int)
     (ny :int)
     (src (:pointer :double))
     (dst (:pointer :double))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (z2dfft2d "z2dfft2d" :source)
    ((nx :int)
     (ny :int)
     (src (:pointer :double))
     (dst (:pointer :double))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (z2zfft2d "z2zfft2d" :source)
    ((nx :int)
     (ny :int)
     (src (:pointer :double))
     (dst (:pointer :double))
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (unsafe-z2zfft2d "unsafe_z2zfft2d" :source)
    ((nx :int)
     (ny :int)
     (src-r :uintptr)
     (src-i :uintptr)
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

;; ------------------------------------------------------------
;; Single-precision Altivec 1D routines
(fli:define-foreign-function (r2cfft "r2cfft" :source)
    ((nx :int)
     (src (:pointer :float))
     (dst (:pointer :float))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (c2rfft "c2rfft" :source)
    ((nx :int)
     (src (:pointer :float))
     (dst (:pointer :float))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (c2cfft "c2cfft" :source)
    ((nx :int)
     (src (:pointer :float))
     (dst (:pointer :float))
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (unsafe-c2cfft "unsafe_c2cfft" :source)
    ((nx :int)
     (src-r :uintptr)
     (src-i :uintptr)
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

;; ------------------------------------------------------------
;; Single-precision Altivec 2D routines
(fli:define-foreign-function (r2cfft2d "r2cfft2d" :source)
    ((nx :int)
     (ny :int)
     (src (:pointer :float))
     (dst (:pointer :float))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (c2rfft2d "c2rfft2d" :source)
    ((nx :int)
     (ny :int)
     (src (:pointer :float))
     (dst (:pointer :float))
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (c2cfft2d "c2cfft2d" :source)
    ((nx :int)
     (ny :int)
     (src (:pointer :float))
     (dst (:pointer :float))
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (unsafe-c2cfft2d "unsafe_c2cfft2d" :source)
    ((nx :int)
     (ny :int)
     (src-r :uintptr)
     (src-i :uintptr)
     (direction :int)
     (tmp-r :uintptr)
     (tmp-i :uintptr)
     (twids :uintptr))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

;; -------------------------------------------------------

(fli:define-foreign-function (create-fft-setup "create_fft_setup" :source)
    ((lg2n :int))
  :result-type :uintptr
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (create-fft-setupD "create_fft_setupD" :source)
    ((lg2n :int))
  :result-type :uintptr
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (destroy-fft-setup "destroy_fft_setup" :source)
    ((twids :uintptr))
  :result-type :void
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (destroy-fft-setupD "destroy_fft_setupD" :source)
    ((twids :uintptr))
  :result-type :void
  :language :ansi-c
  :module *fftx-library*)

;; -------------------------------------------------------

(fli:define-foreign-function (get-align16-offset "get_align16_offset" :source)
    ((buf  :lisp-simple-1d-array))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (get-align32-offset "get_align32_offset" :source)
    ((buf  :lisp-simple-1d-array))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (get-align64-offset "get_align64_offset" :source)
    ((buf  :lisp-simple-1d-array))
  :result-type :int
  :language :ansi-c
  :module *fftx-library*)

(fli:define-foreign-function (get-c-address "get_c_address" :source)
    ((buf  :lisp-simple-1d-array))
  :result-type :uintptr
  :language :ansi-c
  :module *fftx-library*)

;; -------------------------------------------------------

(fli:define-foreign-function (siglab_sbFFT
                              "siglab_sbFFT"
                              :source)
    ((rsrc (:pointer :float))
     (cdst (:pointer :float))
     (nfft :int))
  :result-type :void
  :language :ansi-c
  :module *fftx-library*)

;; --------------------------------------------------------

(in-package #:com.ral.vectorized-math)

(defvar *siglab-library*
  (fli:register-module :siglab
                       :real-name
                       (translate-logical-pathname
                        "PROJECTS:DYLIB;libLispSigLab.dylib")))

(fli:define-foreign-function (disable-denormals
                              "siglab_disable_denormals"
                              :source)
    ()
  :result-type :int
  :language :ansi-c
  :module *siglab-library*)

(fli:define-foreign-function (restore-denormals
                              "siglab_restore_denormals"
                              :source)
    ((savemxcsr :int))
  :result-type :void
  :language :ansi-c
  :module *siglab-library*)

