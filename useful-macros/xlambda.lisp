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

(defun destr-lambda-list-p (args)
  (and (consp args)
       (or (eq (car args) '&whole)
           (some 'consp (subseq args 0
                                (position-if (um:rcurry 'find lambda-list-keywords) args))))))

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

(let ((lw:*handle-warn-on-redefinition* nil))
  
  (defmacro cl:lambda* (args &body body)
    (apply 'wrap-assembly 'lambda args body))
  
  (defmacro cl:defun* (name args &body body)
    `(defun ,@(apply 'wrap-assembly name args body)))
  
  
  (defun wrap-bindings (hd bindings body)
    `(,hd ,(mapcar (lambda (form)
                     (apply 'wrap-assembly form))
                   bindings)
          ,@body))
  
  (defmacro cl:labels* (bindings &body body)
    (wrap-bindings 'labels bindings body))
  
  (defmacro cl:flet* (bindings &body body)
    (wrap-bindings 'flet bindings body))
  
  (define-macro (cl::define* name args &body body)
    `(defun* ,name ,args ,@body)))

#+:LISPWORKS
(progn
  (editor:setup-indent "lambda*" 1 2 8)
  (editor:setup-indent "defun*"  2 2 7)
  (editor:setup-indent "labels*" 1 2 4 'flet)
  (editor:setup-indent "flet*"   1 2 4 'flet)
  (editor:setup-indent "define*" 1))
