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

(defpackage :com.ral.useful-macros.ppcre-reader
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.ppcre-reader)

;; ---------------------------------------
;; This part from Doug Hoyte using Edi's ppcre

(defun make-char-buffer (&optional (nel 16))
  (make-array nel :element-type 'character
              :adjustable t :fill-pointer 0))

(defun read-chars-till-delim (stream delim)
  (let ((buffer (make-char-buffer)))
    (prog ()
      again
      (let ((ch  (read-char stream t nil t)))
        (unless (eql ch delim)
          (vector-push-extend ch buffer)
          (go again))
        ))
    (coerce buffer 'string)))

(defun segment-reader (stream delim n)
  (when (plusp n)
    (cons (read-chars-till-delim stream delim)
          (segment-reader stream delim (1- n)))
    ))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       (load-time-value (ppcre:create-scanner ,(car ,g!args)) t)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       (load-time-value (ppcre:create-scanner ,(car ,g!args)) t)
       ,',g!str
       ,(cadr ,g!args))))

;; --------------------------------------------
;; Reader macro for #~ for pattern matching/substitution
;; Produces a function that can be applied to strings
  
#+cl-ppcre
(defun |reader-for-#~| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((mode-char (read-char stream))
         (ans   (cond
                 
                 ((char= mode-char #\m)
                  (match-mode-ppcre-lambda-form
                   (segment-reader stream
                                   (read-char stream)
                                   1)))
                   
                 ((char= mode-char #\s)
                  (subst-mode-ppcre-lambda-form
                   (segment-reader stream
                                   (read-char stream)
                                   2)))
                 
                 (t
                  (unless *read-suppress*
                    (error "Unknown #~~ mode character")))
                 )))
    (unless *read-suppress*
      ans)))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ '|reader-for-#~|)

;; Examples you can input to READ:
;;
;;   Pattern Matching:
;;
;;     (#~m/^[+-]?[0-9][0-9_,]*(\.[0-9_,]*([eEdD][+-]?[0-9]+)?)?/ s)
;;
;;     =>
;;
;;       ((LAMBDA (#:STR8281) (CL-PPCRE:SCAN "^[+-]?[0-9][0-9_,]*(\\.[0-9_,]*([eEdD][+-]?[0-9]+)?)?" #:STR8281)) S)
;;
;;
;;   Pattern Substitution:
;;
;;     (#~s/[0-9]/N/ s)
;;
;;     =>
;;
;;      ((LAMBDA (#:STR8282) (CL-PPCRE:REGEX-REPLACE-ALL "[0-9]" #:STR8282 "N")) S)
;;
;; (Note: the uninterned symbols in the examples are not exact, just representative gensyms.)
