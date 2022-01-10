;; -*- mode: Lisp; coding: UTF-8 -*-
;; Define-Extensions for Common Lisp
;; DM/RAL 02/21
;; ----------------------------------

(in-package #:def*)

(progn
  (editor:bind-string-to-key "λ" #\not-sign) ;; Option-L on Mac keyboard
  (editor:setup-indent "µ" 1)
  (editor:setup-indent "∂" 1)
  ;; (editor:setup-indent "∂*" 1)
  (editor:setup-indent "λ" 1)
  ;; (editor:setup-indent "λ*" 1)
  (editor:setup-indent "define" 1)
  (editor:setup-indent "define-macro" 1)
  (editor:setup-indent "define-generic" 1)
  (editor:setup-indent "define-method" 1)
  (editor:setup-indent "lambda*" 1 2 8)
  (editor:setup-indent "defun*"  2 2 7)
  (editor:setup-indent "labels*" 1 2 4 'flet)
  (editor:setup-indent "flet*"   1 2 4 'flet)
  (editor:setup-indent "define*" 1))

;; --------------------------------------------------

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
     (setf current (car body))
     (when (and documentation (stringp current) (cdr body))
       (if doc
           (error "Too many documentation strings in ~S." (or whole body))
         (push (pop body) doc)) ;; Alexandira fails here...
       (go :declarations))
     (when (and (listp current) (eql (first current) 'declare))
       (push (pop body) decls)
       (go :declarations)))
    (values body (nreverse decls) doc)))

(defun wrap-assembly (name args &rest body)
  (if (destr-lambda-list-p args)
      (let ((g!args (gensym)))
        (multiple-value-bind (body-forms decls docstr)
            (parse-body body :documentation t)
          `(,name (&rest ,g!args)
                  ,@docstr
                  (destructuring-bind ,args ,g!args
                    ,@decls
                    ,@body-forms))
          ))
    ;; else
    `(,name ,(us-conv args) ,@(decl-us args) ,@body)))

(defun is-underscore? (x)
  (and (symbolp x)
       (string= "_" (string x))))

(defun decl-us (args)
  (when (is-underscore? args)
      `((declare (ignore ,args)))))

(defun us-conv (args)
  (cond ((eq nil args)  nil)
        ((symbolp args) `(&rest ,args))
        (t              args)))

(defun is-lambda-list-keyword? (arg)
  (member arg lambda-list-keywords))

(defun destr-lambda-list-p (args)
  (and (consp args)
       (or (eq (car args) '&whole)
           (lw:dotted-list-p args)
           (some 'consp (subseq args 0
                                (position-if #'is-lambda-list-keyword? args))
                 ))))

(defun wrap-bindings (hd bindings body)
  `(,hd ,(mapcar (lambda (form)
                   (apply #'wrap-assembly form))
                 bindings)
        ,@body))

(defmacro µ ((name . args) &body body)
  `(defmacro ,name ,args ,@body))

;; ---------------------------------------

(µ (defun* name args &body body)
  `(defun ,@(apply #'wrap-assembly name args body)))

(µ (∂ (name . args) &body body)
  `(defun* ,name ,args ,@body))

#|
(µ (∂ (name . args) &body body)
  `(defun ,name ,args ,@body))

(µ (∂* (name . args) &body body)
  `(defun* ,name ,args ,@body))
|#

(µ (lambda* args &body body)
  (apply #'wrap-assembly 'lambda args body))
  
(µ (λ &body body)
  `(lambda* ,@body))

#|
(µ (λ &body body)
  `(lambda ,@body))

(µ (λ* &body body)
  `(lambda* ,@body))
|#

;; Scheme did some things right...
(µ (define item &body body)
  (if (consp item)
      `(defun ,(car item) ,(cdr item) ,@body)
    `(defparameter ,item ,@body)))
  
(µ (define* (name . args) &body body)
  `(defun* ,name ,args ,@body))

(µ (define-macro (name . args) &body body)
  `(defmacro ,name ,args ,@body))

(µ (define-generic (name . args) &body body)
  `(defgeneric ,name ,args ,@body))

(µ (define-method (name . args) &body body)
  (if (keywordp (car args))
      `(defmethod ,name ,(car args) ,(cdr args) ,@body)
    `(defmethod ,name ,args ,@body)))

(µ (labels* bindings &body body)
  (wrap-bindings 'labels bindings body))
  
(µ (flet* bindings &body body)
  (wrap-bindings 'flet bindings body))
  
