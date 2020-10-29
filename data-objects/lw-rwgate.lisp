;; lw-rwgate.lisp -- R/W Gate done with LW sharing locks
;;
;; DM/RAL 02/17
;; ------------------------------------------------------------
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

(defpackage #:lw-rwgate
  (:use #:common-lisp)
  (:export
   #:make-rwgate
   #:with-read-lock
   #:with-write-lock))

(in-package #:lw-rwgate)

(defun make-rwgate ()
  (mp:make-lock :sharing t))

(defun do-with-lock (lock lock-type timeout fn abortfn)
  (let (has-lock)
    (multiple-value-bind (lockfn unlockfn)
        (case lock-type
          (:read  (values #'mp:process-sharing-lock
                          #'mp:process-sharing-unlock))
          (:write (values #'mp:process-exclusive-lock
                          #'mp:process-exclusive-unlock)))
      
      (labels ((ok-to-proceed ()
                 (mp:with-interrupts-blocked
                   (setf has-lock (funcall lockfn lock :waiting 0)))))
        
        (hcl:unwind-protect-blocking-interrupts-in-cleanups
            (when (or (ok-to-proceed)
                      (mp:wait-processing-events timeout
                                                 :wait-function #'ok-to-proceed))
              (funcall fn))
          (if has-lock
              (funcall unlockfn lock)
            ;; else
            (when abortfn
              ;; abortfn called with interrupts disabled
              (funcall abortfn))
            ))
        ))))

(defmacro with-read-lock ((lock &key timeout abortfn) &body body)
  `(do-with-lock ,lock :read ,timeout (lambda () ,@body) ,abortfn))

(defmacro with-write-lock ((lock &key timeout abortfn) &body body)
  `(do-with-lock ,lock :write ,timeout (lambda () ,@body) ,abortfn))

