;; remembered-filenames.lispp -- convenience macros for remembering
;; prompted filenames
;;
;; DM/RAL 11/10
;; -------------------------------------------------------------
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

(defpackage :com.ral.useful-macros.remembered-filenames
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.remembered-filenames)

;; ----------------------------------------------------------

(defvar *remembered-filenames*
  (make-hash-table))

(defun remember-filename (key fname)
  (setf (gethash key *remembered-filenames*) fname)
  (when key
    ;; remember last one, for context-free queries
    (setf (gethash nil *remembered-filenames*) fname)))

(defun remembered-filename (key)
  (or
   (gethash key *remembered-filenames*)
   (and key
        ;; No last remembered by key.
        ;; So start with last one, regardless of keyed context
        (values (gethash nil *remembered-filenames*)))))

(defun do-with-remembered-filename (message key prompt-keys init fn)
  ;; can use null key for non-selective recall - starts with last
  ;; prompted filename regardless of former keyed context.
  (let* ((prev-name  (remembered-filename key))
         (init-name  (or init ""))
         (trial-name (if prev-name
                         (merge-pathnames init-name prev-name)
                       init-name)))
    (when-let (fname (or init
                         (multiple-value-call #'capi:prompt-for-file
                           message
                           (values-list prompt-keys)
                           :pathname trial-name)))
      (remember-filename key fname)
      (funcall fn fname))))

(defmacro with-remembered-filename ((fname message
                                           &optional key init
                                           &rest prompt-keys
                                           &key (filter "*.*")
                                           &allow-other-keys)
                                    &body body)
  "KEY indicates a context for the lookup. Can be NIL for \
\"use last lookup\" as context, regardless of its keying.\
\
If INIT is null, then use CAPI:PROMPT-FOR-FILE with MESSAGE \
and any PROMPT-KEYS. Otherwise, INIT should be a Lisp form \
that uses FNAME as a bound argument, and computes and returns \
the new filename."
  `(do-with-remembered-filename ,message ,key
                                (list ,@prompt-keys :filter ,filter)
                                ,init
                                (lambda (,fname)
                                  (declare (ignorable ,fname))
                                  ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-remembered-filename" 1)

#|
(defmacro with-remembered-prompting ((fname &optional
                                            key
                                            init
                                            (prompt "Pick a file")
                                            &rest
                                            prompt-keys)
                                     &body body)
  `(with-remembered-filename (,fname ,key ,init)
       (multiple-value-call #'capi:prompt-for-file ,prompt
         (values ,@prompt-keys)
         :pathname ,fname
         :filter   "*.*")
     ,@body))
                                     
#+:LISPWORKS
(editor:setup-indent "with-remembered-prompting" 1)
|#

;; ----------------------------------------------------------

(defvar *last-timestamp* nil)
(defvar *timestamp-index* 0)

(defun filename-timestamp-string ()
  (let ((now (get-universal-time)))
    (unless (eql now *last-timestamp*)
      (setf *last-timestamp*  now
            *timestamp-index* 0))
    (multiple-value-bind (ss mm hh dd mon yr)
        (decode-universal-time now 0)
      (format nil "~A~{~{~2,'0d~}-~}~d"
              yr (list
                  (list mon dd)
                  (list hh mm ss))
              (incf *timestamp-index*)))))

(defun add-timestamp-to-filename (fname)
  (concatenate 'string
               (pathname-name fname)
               "-"
               (filename-timestamp-string)
               "."
               (pathname-type fname)))
