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

(defpackage :com.ral.useful-macros.safe-read-from-string
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.safe-read-from-string)

#|
(defvar *safe-read-from-string-blacklist*
  '(#\# #\: #\|))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safe-read-from-string failure"))

  (dolist (c *safe-read-from-string-blacklist*)
    (set-macro-character
      c #'safe-reader-error nil rt))

  (defun safe-read-from-string (s &optional fail)
    (if (stringp s)
      (let ((*readtable* rt) *read-eval*)
        (handler-bind
          ((error (lambda (condition)
                    (declare (ignore condition))
                    (return-from
                      safe-read-from-string fail))))
          (read-from-string s)))
      fail)))
|#

;; Using Michal's improved, extensive, version

(defvar *whitespace* '(#\Space #\Newline #\Backspace #\Tab
                               #\Linefeed #\Page #\Return #\Rubout))

(define-condition malformed-input (error) ())

(define-condition input-size-exceeded (error) ())

(defvar *max-input-size* (* 128 1024))

(defun whitespace-p (char)
  (member char *whitespace*))

(defun trim-leading-whitespace (string)
  (string-left-trim *whitespace* string))

;; Special Restricted ReadTable
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter %safe-readtable% (copy-readtable))
  (defparameter %max-safe-char% 256)
  (let ((*readtable* %safe-readtable%))
    (flet ((signal-malformed-input (stream char)
             (declare (ignore stream char))
             (error 'malformed-input))
           (eat-colon (stream char)
             (declare (ignore char))
             (if (eq #\: (read-char-no-hang stream))
                 (read stream)
                 (error 'malformed-input))))
      (dotimes (i %max-safe-char%)
        (let* ((char (code-char i))
               (macro-char (get-macro-character char)))
          (unless (or (null char)
                      (member char '(#\( #\) #\"))
                      (null macro-char))
            (set-macro-character char #'signal-malformed-input))))
      (set-macro-character #\: #'signal-malformed-input)
      (set-macro-character #\# #'eat-colon))))

;; Utility macro - temporary packages
(defmacro with-temp-package (&body body)
  (let* ((now (format nil "~S" (local-time:now)))
         (package-name (gensym (uiop:strcat "TEMP-PKG-" now "-")))
         (package-var (gensym))
         (use (if (eq (first body) :use-list)
                  (prog1 (second body)
                    (setf body (cddr body))))))
                    
    `(let ((,package-var (or (find-package ',package-name)
                             (make-package ',package-name :use ,use))))
       (unwind-protect (let ((*package* ,package-var)) ,@body)
         (delete-package ,package-var)))))

;; Handler-case and macro-wrapper for safe reading
(defmacro safe-read-string-handler-case (&body body)
  `(with-temp-package ,@(if (eq (first body) :use-list)
                            (prog1 (list :use-list (second body))
                              (setf body (cddr body))))
     (handler-case
         (let* ((*readtable* %safe-readtable%))
           (values
            (progn
              ,@body)))
       (error (e)
         (values nil e)))
     ))

#|
(defun %safe-read-from-string (s)
  ;; Michal's reader only accepts S-exprs. We dummy up an S-expr
  ;; covering all arguments in the string and return the list of them.
  (if (> (length str) *max-input-size*)
      (values nil (make-condition 'input-size-exceeded))
    ;; else
    (let ((line (trim-leading-whitespace str)))
      (if (char/= #\( (char line 0))
          (values nil (make-condition 'malformed-input))
        ;; else
        (safe-read-string-handler-case :use-list use-list
          (read-from-string line)))
      )))

(defun safe-read-from-string (s)
  ;; Michal's reader only accepts S-exprs. We dummy up an S-expr
  ;; covering all arguments in the string and return the list of them.
  (let ((trimmed (concatenate 'string
                              "(" (trim-leading-whitespace s) ")" )))
    (%safe-read-from-string trimmed :use-list '(:common-lisp))
    ))
|#

(defun safe-read-from-string (s)
  ;; Michal's reader only accepts S-exprs. We dummy up an S-expr
  ;; covering all arguments in the string and return the list of them.
  (let ((trimmed (concatenate 'string
                              "(" (trim-leading-whitespace s) ")" )))
    (safe-read:safe-read trimmed '(:common-lisp))
    ))

#|
(Safe-read-from-string "(values \
                                x15)")
(safe-read-from-string (concatenate 'string "(values" " x15"))
(safe-read-from-string "32.0")

(with-input-from-string (s "xyxz")
  (print (class-of s)))

(progn
  (with-input-from-string (s "xyxz")
    (print s))
  (unwind-protect
      (with-input-from-string (s "xxx")
        (print s))
    (print :unwinding)))
 |#
