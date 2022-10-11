;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READ
;;;; © Michał "phoe" Herda 2016
;;;; safe-read.lisp

;; Lisp reader hackery below. Beware.
(in-package #:safe-read)

;; Exported conditions and parameters
(define-condition incomplete-input ()
  ((buffer  :accessor buffer-of  :initarg :buffer  :initform nil)))

(define-condition malformed-input (error) ())

(define-condition input-size-exceeded (error) ())

(defvar *max-input-size* (* 128 1024))

;; Utility functions
(defun condition-key (condition)
  (intern (string (type-of condition)) (find-package :keyword)))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)))

(defun trim-leading-whitespace (string)
  (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout)))
    (string-left-trim whitespace string)))

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

;; Utility macro - creating a safe readtable at compile-time
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

(defgeneric safe-read-method (stream use-list buffer))

;; Main exported function
(defun safe-read (&optional (stream *standard-input*) use-list buffer)
  ;; NOTE: third arg, BUFFER, defaults to NIL. But on rereads against
  ;; the same stream, it should be furnished as being the third return
  ;; value from the prior call to SAFE-READ.
  (safe-read-method stream use-list buffer))

(defmethod safe-read-method ((stream stream) use-list buffer)
  (handler-case
      (if (or (null buffer)
              (string= "" buffer))
          (safe-read-no-buffer stream use-list)
        (safe-read-buffer stream use-list buffer))
    (incomplete-input (c)
      (values nil :incomplete-input (buffer-of c)))
    (end-of-file (e)
      (error e))
    (error (error)
      (error error))))

(defmethod safe-read-method ((s string) use-list buffer)
  (declare (ignore buffer))
  (safe-read-from-string s use-list))

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

(defun safe-read-from-string (str use-list)
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

;; Handler-case and macro-wrapper for safe reading
(defmacro safe-read-handler-case (&body body)
  ;; NOTE: I generally frown on intentional lexical capture inside of
  ;; macros. But since you already adopted the style with binding
  ;; "line", I will join you with binding "buffer". (DM/RAL 11/22)
  (let ((gensym (gensym)))
    `(with-temp-package ,@(if (eq (first body) :use-list)
                              (prog1 (list :use-list (second body))
                                (setf body (cddr body))))
       (handler-case
           (flet ((clear-buffer (e)
                    (declare (ignore e))
                    (setf buffer "")))
               (handler-bind ((malformed-input #'clear-buffer))
                 (let* ((*readtable* %safe-readtable%)
                        (,gensym (progn ,@body)))
                   (values ,gensym nil ""))))
         (end-of-file ()
           (unless (string= line "")
             (setf buffer
                   (uiop:strcat buffer line (string #\Newline))))
           (signal (make-condition 'incomplete-input :buffer buffer)))))))

;; Safe read - no buffer
(defun safe-read-no-buffer (stream use-list)
  (let ((buffer "")
        (line (trim-leading-whitespace (read-limited-line stream))))
    (safe-read-handler-case :use-list use-list
      (read-from-string line))))

;; Safe read - buffer
(defun safe-read-buffer (stream use-list buffer)
  (let ((line (read-limited-line stream (length buffer))))
    (safe-read-handler-case :use-list use-list
      (read-from-string (uiop:strcat buffer line)))))

;; Reading from string with a maximum size limit
(defun read-limited-line (&optional (stream *standard-input*) (buffer-length 0))
  (with-output-to-string (result)
    (let ((char-counter buffer-length) char)
      (loop
        (setf char (read-char-no-hang stream nil :eof))
        (cond ((null char)
               (return))
              ((eq char #\Newline)
               (return))
              ((and (eq char :eof) (= 0 char-counter))
               (error 'end-of-file :stream stream))
              ((and (eq char :eof) (/= 0 char-counter))
               (return))
              ((and (= 0 buffer-length) (= 0 char-counter) (whitespace-p char))
               nil)
              ((and (= 0 buffer-length) (= 0 char-counter) (char/= #\( char))
               (error (make-condition 'malformed-input)))
              ((< *max-input-size* (incf char-counter))
               (error (make-condition 'input-size-exceeded)))
              (t (princ char result)))))))
