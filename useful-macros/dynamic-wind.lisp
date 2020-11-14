;; dynamic-wind.lisp -- managing dynamic environments
;;
;; DM/RAL 11/20 -- I learned this from Pascal Costanza
;; -------------------------------------------------------
(in-package :um)
;; -------------------------------------------------

(defvar *dynamic-wind-stack* nil)

(defstruct (dynamic-environment
            (:constructor make-dynamic-environment (dynamic-winds)))
  (dynamic-winds nil :read-only t))

(defun capture-dynamic-environment ()
  (make-dynamic-environment (reverse *dynamic-wind-stack*)))

(defmacro dynamic-wind (&body body)
  (lw:with-unique-names (wind-fn thunk proceed-body)
    `(flet ((,wind-fn (,thunk)
              (macrolet ((proceed (&body ,proceed-body)
                           `(if ,',thunk
                                (funcall ,',thunk)
                              (progn
                                ,@,proceed-body))))
                ,@body)))
       (let ((*dynamic-wind-stack* (cons #',wind-fn *dynamic-wind-stack*)))
         (,wind-fn nil)))
    ))

(defun call-with-dynamic-environment (env thunk)
  (labels ((instantiate (env)
             (cond (env
                    (let* ((wind-fn (first env))
                           (*dynamic-wind-stack* (cons wind-fn *dynamic-wind-stack*)))
                      (funcall wind-fn (lambda ()
                                         (instantiate (rest env))))
                      ))
                   (t
                    (funcall thunk))
                   )))
    (with-slots (dynamic-winds) env
      (instantiate dynamic-winds))))

(defmacro with-dynamic-environment ((env) &body body)
  `(call-with-dynamic-environment ,env (lambda ()
                                         ,@body)))

#+LISPWORKS
(editor:setup-indent "with-dynamic-environment" 1)

