;; xlambda.lisp -- Convenience auto destructuring argument lists for
;; DEFUN* and LAMBDA*
;;
;; DM/RAL 08/20
;; ------------------------------------------------------------------

(in-package :xlambda)

(defun destr-lambda-list-p (args)
  (or (eq (car args) '&whole)
      (some 'consp (subseq args 0
                           (position-if (um:rcurry 'find lambda-list-keywords) args)))))

(defun wrap-assembly (name args &rest body)
  (if (destr-lambda-list-p args)
      (let ((g!args (gensym)))
        (multiple-value-bind (body-forms decls docstr)
            (alexandria:parse-body body)
          `(,name (&rest ,g!args)
                  ,@docstr
                  (destructuring-bind ,args ,g!args
                    ,@decls
                    ,@body-forms))
          ))
    ;; else
    `(,name ,args ,@body)))

(defmacro lambda* (args &body body)
  (apply 'wrap-assembly 'lambda args body))

(defmacro defun* (name args &body body)
  `(defun ,@(apply 'wrap-assembly name args body)))


(defun wrap-bindings (hd bindings body)
  `(,hd ,(mapcar (lambda (form)
                   (apply 'wrap-assembly form))
                 bindings)
        ,@body))

(defmacro labels* (bindings &body body)
  (wrap-bindings 'labels bindings body))

(defmacro flet* (bindings &body body)
  (wrap-bindings 'flet bindings body))

#+:LISPWORKS
(progn
  (editor:setup-indent "lambda*" 1 2 8)
  (editor:setup-indent "defun*"  2 2 7)
  (editor:setup-indent "labels*" 1 2 4 'flet)
  (editor:setup-indent "flet*"   1 2 4 'flet))
  

