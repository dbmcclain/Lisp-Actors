;; usec.lisp -- timestamps to the nearest microsecond, when supported
;; DM/RAL  08/09
;; ----------------------------------------------------------------
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

(defpackage :com.ral.usec
  (:use #:common-lisp)
  (:export
   #:get-time-usec
   #:adjust-to-standard-universal-time-usec
   #:get-universal-time-usec
   #:_getTickCount
   ))

;; ----------------------------------------------------------------
(in-package #:com.ral.usec)
;; ----------------------------------------------------------------
;; ----------------------------------------------------------------
;; Timestamps to the nearest microsecond

;;--- MAC OS/X ---

(defun adjust-to-standard-universal-time-usec (tm)
  (declare (integer tm))
  (+ tm #.(* 1000000 (encode-universal-time 0 0 0 1 1 1970 0))))


(cffi:defcfun ("gettimeofday" _get-time-of-day) :int64
  (tsinfo  :pointer :uint64)
  (tzinfo  :pointer :void))

(defun get-time-usec ()
   ;; time since midnight Jan 1, 1970, measured in microseconds
   (cffi:with-foreign-pointer (tsinfo (* 2 8))
     (setf (cffi:mem-aref tsinfo :uint64 0) 0
           (cffi:mem-aref tsinfo :uint64 1) 0)
     (if (zerop (_get-time-of-day tsinfo (cffi:null-pointer)))
         (+ (the integer (* 1000000 (the integer (cffi:mem-aref tsinfo :uint64 0))))
            (the integer (cffi:mem-aref tsinfo :uint64 1)))
       (error "Can't perform Posix gettimeofday()"))
     ))

(defun get-universal-time-usec ()
  (adjust-to-standard-universal-time-usec (get-time-usec)))

