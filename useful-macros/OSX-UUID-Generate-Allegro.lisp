;; OSX-UUID-Generate-Allegro.lisp
;;
;; DM/SD  08/08
;; --------------------------------------------------
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

(defpackage :uuidgen
  (:use #:common-lisp)
  (:export
   #:generate))

(in-package :uuidgen)

;; (require :foreign)

(ff:def-foreign-type <c-uuid> (:array :unsigned-char 16))

(ff:def-foreign-call uuid_generate ((uuid (* <c-uuid>)))
  :returning :int)

(defun generate ()
"return a 16 element vector of unsigned 8-bit bytes
containing an OS X UUID generated from the /dev/urandom entropy store
if it is available and of high enough quality. Otherwise a time-based UUID
will be generated and returned."
  
  (ff:with-stack-fobject (uuid '<c-uuid>)
      (uuid_generate uuid)
      (let ((ans (make-array 16
                             :element-type '(unsigned-byte 8))))
        (dotimes (ix 16)
          (setf (aref ans ix) (ff:fslot-value-typed '<c-uuid> nil uuid ix)))
        ans)))
                             