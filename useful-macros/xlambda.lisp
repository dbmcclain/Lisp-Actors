;; -*- Mode: Lisp; Coding: UTF-8 -*-
;; xlambda.lisp -- Convenience auto destructuring argument lists for
;; DEFUN* and LAMBDA*
;;
;; DM/RAL 08/20
;; ------------------------------------------------------------------

(in-package :xlambda)

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

(defun is-lambda-list-keyword (arg)
  (member arg lambda-list-keywords))

(defun destr-lambda-list-p (args)
  (and (consp args)
       (or (eq (car args) '&whole)
           (some 'consp (subseq args 0
                                (position-if #'is-lambda-list-keyword args))
                 ))))

(defun wrap-assembly (name args &rest body)
  (if (destr-lambda-list-p args)
      (let ((g!args (gensym)))
        (multiple-value-bind (body-forms decls docstr)
            (um:parse-body body :documentation t)
          `(,name (&rest ,g!args)
                  ,@docstr
                  (destructuring-bind ,args ,g!args
                    ,@decls
                    ,@body-forms))
          ))
    ;; else
    `(,name ,(us-conv args) ,@(decl-us args) ,@body)))

(defun wrap-bindings (hd bindings body)
  `(,hd ,(mapcar (lambda (form)
                   (apply #'wrap-assembly form))
                 bindings)
        ,@body))
  
(defmacro lambda* (args &body body)
  (apply #'wrap-assembly 'lambda args body))
  
(defmacro defun* (name args &body body)
  `(defun ,@(apply #'wrap-assembly name args body)))
  
(defmacro labels* (bindings &body body)
  (wrap-bindings 'labels bindings body))
  
(defmacro flet* (bindings &body body)
  (wrap-bindings 'flet bindings body))
  
(define-macro (define* name args &body body)
  `(defun* ,name ,args ,@body))

