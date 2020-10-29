;; ctypes.lisp -- extended C typedefs for general use
;; An attempt to avoid the use of keyword symbol names for commonly used C typdefs
;; All FLI users should "use" this package to gain common access to these C typdefs.
;;
;; 12/01  DM/MCFA
;; ------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defpackage "CTYPES"
  (:nicknames "CT")
  (:use "COMMON-LISP")
  (:export
   "UCHAR"
   "OUT-CSTRING"
   "IN-CSTRING"
   ))

(in-package "CTYPES")

(fli:define-c-typedef uchar        (:unsigned :char))
(fli:define-c-typedef out-cstring  (:pointer uchar))
(fli:define-c-typedef in-cstring   (:pointer uchar))

;; -- end of ctypes.lisp -- ;;
