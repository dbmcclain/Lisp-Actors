(cl:defpackage #:json-reader
  (:use #:cl)
  (:export #:enable-json-syntax
           #:disable-json-syntax))
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

(cl:in-package #:json-reader)

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +comma+ #\,)
(defconstant +colon+ #\:)

(defun transform-primitive (value)
  (if (symbolp value)
      (cond
        ((string-equal (symbol-name value) "true") t)
        ((string-equal (symbol-name value) "false") nil)
        ((string-equal (symbol-name value) "null") nil)
        (t value))
      value))

(defun create-json-hash-table (&rest pairs)
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop for (key . value) in pairs
       do (setf (gethash key hash-table) value))
    hash-table))

(defun read-next-object (separator delimiter
                         &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
        (let* ((object (read input-stream t nil t))
               (next-char (peek-next-char)))
          (cond
            ((char= next-char separator) (discard-next-char))
            ((and delimiter (char= next-char delimiter)) nil)
            (t (error "Unexpected next char: ~S" next-char)))
          object))))

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    (loop
       for object = (read-next-object +comma+ +right-bracket+ stream)
       while object
       collect (transform-primitive object) into objects
       finally (return `(vector ,@objects)))))

(defun stringify-key (key)
  (etypecase key
    (symbol (string-downcase (string key)))
    (string key)))

(defun read-left-brace (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    (set-macro-character +colon+ 'read-separator)
    (loop
       for key = (read-next-object +colon+ +right-brace+ stream)
       while key
       for value = (read-next-object +comma+ +right-brace+ stream)
       collect `(cons ,(stringify-key key) ,(transform-primitive value)) into pairs
       finally (return `(create-json-hash-table ,@pairs)))))

(defvar *previous-readtables* nil)

(defmacro enable-json-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +left-bracket+ 'read-left-bracket)
    (set-macro-character +right-bracket+ 'read-delimiter)
    (set-macro-character +left-brace+ 'read-left-brace)
    (set-macro-character +right-brace+ 'read-delimiter)))

(defmacro disable-json-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
