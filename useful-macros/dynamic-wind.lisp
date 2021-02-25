;; dynamic-wind.lisp -- managing dynamic environments
;;
;; DM/RAL 11/20 -- I learned this from Pascal Costanza
;; -------------------------------------------------------
(in-package :um)
;; -------------------------------------------------

(defvar *dynamic-wind-stack* nil)

(defstruct (dynamic-environment
            (:constructor make-dynamic-environment (wrapper-fns)))
  (wrapper-fns nil :read-only t))

(defun capture-dynamic-environment ()
  (make-dynamic-environment (reverse *dynamic-wind-stack*)))

(defmacro dynamic-wind (&body body)
  (lw:with-unique-names (wrapper-fn fn args proceed-body)
    `(flet ((,wrapper-fn (,fn &rest ,args)
              (macrolet ((proceed (&body ,proceed-body)
                           `(if ,',fn
                                (apply ,',fn ,',args)
                              (progn
                                ,@,proceed-body))))
                ,@body)))
       (let ((*dynamic-wind-stack* (cons #',wrapper-fn *dynamic-wind-stack*)))
         (,wrapper-fn nil)))
    ))

(defun call-with-dynamic-environment (env fn &rest args)
  (labels ((wrap-env (env)
             (if env
                 (let* ((wrapper-fn (first env))
                        (*dynamic-wind-stack* (cons wrapper-fn *dynamic-wind-stack*)))
                   (funcall wrapper-fn (lambda ()
                                         (wrap-env (rest env)))))
               ;; else
               (apply fn args))))
    (with-slots (wrapper-fns) env
      (wrap-env wrapper-fns))))

(defmacro with-dynamic-environment ((env) &body body)
  `(call-with-dynamic-environment ,env (lambda ()
                                         ,@body)))

#+LISPWORKS
(editor:setup-indent "with-dynamic-environment" 1)

