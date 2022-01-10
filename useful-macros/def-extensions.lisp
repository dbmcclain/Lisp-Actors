;; -*- mode: Lisp; coding: UTF-8 -*-
;; Define-Extensions for Common Lisp
;; DM/RAL 02/21
;; ----------------------------------

(in-package #:def*)

(defmacro µ ((name . args) &body body)
  `(defmacro ,name ,args ,@body))

(µ (∂ (name . args) &body body)
  `(defun ,name ,args ,@body))

(µ (∂* (name . args) &body body)
  `(defun* ,name ,args ,@body))

#||#
(µ (λ &body body)
  `(lambda ,@body))

(µ (λ* &body body)
  `(lambda* ,@body))
#||#

;; Scheme did some things right...
(µ (define item &body body)
  (if (consp item)
      `(defun ,(car item) ,(cdr item) ,@body)
    `(defparameter ,item ,@body)))
  
(µ (define-macro (name . args) &body body)
  `(defmacro ,name ,args ,@body))

(µ (define-generic (name . args) &body body)
  `(defgeneric ,name ,args ,@body))

(µ (define-method (name . args) &body body)
  (if (keywordp (car args))
      `(defmethod ,name ,(car args) ,(cdr args) ,@body)
    `(defmethod ,name ,args ,@body)))

(progn
  (editor:bind-string-to-key "λ" #\not-sign) ;; Option-L on Mac keyboard
  (editor:setup-indent "µ" 1)
  (editor:setup-indent "∂" 1)
  (editor:setup-indent "∂*" 1)
  (editor:setup-indent "λ" 1)
  (editor:setup-indent "λ*" 1)
  (editor:setup-indent "define" 1)
  (editor:setup-indent "define-macro" 1)
  (editor:setup-indent "define-generic" 1)
  (editor:setup-indent "define-method" 1)
  (editor:setup-indent "lambda*" 1 2 8)
  (editor:setup-indent "defun*"  2 2 7)
  (editor:setup-indent "labels*" 1 2 4 'flet)
  (editor:setup-indent "flet*"   1 2 4 'flet)
  (editor:setup-indent "define*" 1))

