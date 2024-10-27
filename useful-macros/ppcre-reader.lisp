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

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

;; --------------------------------------------
;; Cached compiled scanners - compile on first use.
;; DM/RAL  2024/10/27 00:12:06 UTC

#|
(defvar *ppcre-cache*  (make-hash-table
                        :test #'string=))

(defun get-cache (str)
  ;; Get cached scanner, or compile on demand and cache for future
  ;; needs.
  (or (gethash str *ppcre-cache*)
      (setf (gethash str *ppcre-cache*)
            (ppcre:create-scanner str))
      ))
|#

(defun get-cache (cache str)
  ;; Use private local cache instead of hash-table
  (or (car cache)
      (setf (car cache)
            (ppcre:create-scanner str))
      ))

;; ---------------------------------------
#|
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
|#

;; --------------------------------------------
;; Use cached compiled scanners
;; DM/RAL  2024/10/27 00:12:41 UTC
;;
;; The #~M, #~S, reader macros are used in function position - meaning
;; they expect to produce a lambda function.
;;
;; A let-over-lambda is a natural solution to having a local cache - a
;; simple CONS cell whose CAR contains NIL on first use, and which
;; gets replaced by the compiled scanner function. But we cannot do...
;;
;; You cannot use a let-over-lambda in function position. That would
;; require construction of a functional closure to capture the
;; let-binding surrounding the lambda function. And that is a stretch
;; too far for Lisp. It needs a function, or a previously closed over
;; functional closure. It cannot perform that closing-over at the time
;; it needs it in function position.
;;
;; So, instead, we have our manufacturer in the reader macro build us
;; a cache CONS cell and provide that to us on construction of the
;; lambda. It is private, unseen by anyone else, no binding holds it,
;; except for the function call referring to it with 'SCANNER in
;; argument position. That same CONS cell gets used every time the
;; lambda executes. And it was freshly made, as (LIST NIL), for our
;; private use, not a reference to some constant value like '(NIL).


#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!scanner o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       (get-cache ',,g!scanner ,(car ,g!args))
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!scanner o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       (get-cache ',,g!scanner ,(car ,g!args))
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
                   (list nil) ;; private scanner cache
                   (segment-reader stream
                                   (read-char stream)
                                   1)))
                   
                 ((char= mode-char #\s)
                  (subst-mode-ppcre-lambda-form
                   (list nil) ;; private scanner cache
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
