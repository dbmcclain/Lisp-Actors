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

(in-package :useful-macros)

;; ---------------------------------------
;; This part from Doug Hoyte using Edi's ppcre

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

;; ---------------------------------------

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

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
