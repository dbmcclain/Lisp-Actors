;; -*- mode: Lisp; coding: UTF-8 -*-
;; Define-Extensions for Common Lisp
;; DM/RAL 02/21
;; ----------------------------------

(in-package :cl)

(defmacro λ (&body body)
  `(lambda ,@body))

(defmacro λ* (&body body)
  `(lambda* ,@body))

;; Scheme did some things right...
(defmacro define (item &body body)
  (if (consp item)
      `(defun ,(car item) ,(cdr item) ,@body)
    `(defparameter ,item ,@body)))

(defmacro define-macro ((name &rest args) &body body)
  `(defmacro ,name ,args ,@body))

(defmacro define-generic ((name &rest args) &body body)
  `(defgeneric ,name ,args ,@body))

(defmacro define-method ((name &rest args) &body body)
  (if (keywordp (car args))
      `(defmethod ,name ,(car args) ,(cdr args) ,@body)
    `(defmethod ,name ,args ,@body)))

(progn
  (editor:bind-string-to-key "λ" #\not-sign) ;; Option-L on Mac keyboard
  (editor:setup-indent "λ" 1)
  (editor:setup-indent "λ*" 1)
  (editor:setup-indent "define" 1)
  (editor:setup-indent "define-macro" 1)
  (editor:setup-indent "define-generic" 1)
  (editor:setup-indent "define-method" 1))

(export '(λ
          define
          define-macro
          define-generic
          define-method
          λ*
          lambda*
          define*
          defun*
          labels*
          flet*))
