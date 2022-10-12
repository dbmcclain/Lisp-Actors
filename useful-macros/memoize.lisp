;; memoize.lisp -- Make memoized functions
;;
;; DM/HMSC  04/09
;; -----------------------------------------------------------
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

(defpackage :com.ral.useful-macros.memo
  (:use :common-lisp :com.ral.useful-macros)
  (:export
   #:memo
   #:memoize
   #:un-memoize
   #:clear-memoize
   #:defun-memo
   ))

(in-package :com.ral.useful-macros.memo)

;; -----------------------------------------------------------

(defun memo (fn &key name (key 'first) (test 'eql))
  ;; for ad-hoc memoization of arbitrary functions
  (let ((cache (make-hash-table :test test)))
    (setf (get name 'memo) cache)
    (lambda (&rest args)
      (let ((k (funcall key args)))
        (multiple-value-bind (val found) (gethash k cache)
          (values-list
           (if found
               val
             (setf (gethash k cache) (multiple-value-list (apply fn args)))))
          )))))

(defun memoize (fn-name &key (key 'first) (test 'eql))
  (unless (get fn-name 'memoized)
    (let ((fn (symbol-function fn-name)))
      (setf (get fn-name 'memoized)   fn
            (symbol-function fn-name) (memo fn :name fn-name :key key :test test))
      )))

(defun un-memoize (fn-name)
  (when-let (fn (get fn-name 'memoized))
    (setf (symbol-function fn-name) fn)
    (remprop fn-name 'memoized)
    (remprop fn-name 'memo)))

(defun clear-memoize (fn-name)
  (when-let (tbl (get fn-name 'memo))
    (clrhash tbl)))

(defmacro defun-memo (fn args &body body)
  `(memoize
    (defun ,fn ,args . ,body)))

#|
(defun fib (n)
  #F
  (declare (fixnum n))
  (if (< n 2)
      1
    (+ (fib (the fixnum (- n 1)))
       (fib (the fixnum (- n 2))))))

(time (fib 35))
(memoize 'fib)
(time (loop repeat #N1_000_000 do (fib 35)))
|#

;; ----------------------------------------------------------

#|
(defun fact (n)
  #f
  (declare (fixnum n))
  (format t "~&compute fact(~D)" n)
  (if (<= n 1)
      1
    (* n (fact (1- n)))))
(memoize 'fact)
|#
