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

(defpackage :com.ral.useful-macros.sharp-quasiquote-reader
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.sharp-quasiquote-reader)

;; --------------------------------------------
;; Reader macro for #` for producing parameterized BQ lists
;; Produces a function that can be applied to arguments
  
(defun |reader-for-#`| (stream sub-char numarg)
  (declare (ignore sub-char))
  (let* ((numarg (or numarg 1))
         (a-args (loop for i from 1 to numarg
                       collect (symb 'a i)))
         (ans  `(lambda ,a-args
                  (declare (ignorable ,@a-args))
                  ,(funcall
                    (get-macro-character #\`) stream nil))))
    (unless *read-suppress*
      ans)))

(set-dispatch-macro-character
 #\# #\` '|reader-for-#`|)
  

#| ;; example -- a1 is first parameter
(mapcar #`(,a1 (intern ,(bang-symbol-name a1))) syms)
|#
