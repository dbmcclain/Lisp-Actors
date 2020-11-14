;; dynamic-wind.lisp -- managing dynamic environments
;;
;; DM/RAL 11/20 -- I learned this from Pascal Costanza
;; -------------------------------------------------------

(in-package :cps)

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
  (labels ((instantiate (env thunk)
             (cond (env
                    (let* ((wind-fn (first env))
                           (*dynamic-wind-stack* (cons wind-fn *dynamic-wind-stack*)))
                      (funcall wind-fn (lambda ()
                                         (instantiate (rest env) thunk)))))
                   (t (funcall thunk))
                   )))
    (with-slots (dynamic-winds) env
      (instantiate dynamic-winds thunk))))

(defmacro with-dynamic-environment ((env) &body body)
  `(call-with-dynamic-environment ,env (lambda ()
                                         ,@body)))

#+LISPWORKS
(editor:setup-indent "with-dynamic-environment" 1)

(defmacro =handler-case (form &rest handlers)
  `(dynamic-wind
    (handler-case
        (proceed ,form)
      ,@handlers)))

(defmacro =handler-bind (handler-bindings &body body)
  `(dynamic-wind
    (handler-bind
        ,handler-bindings
      (proceed
       ,@body))))

(defmacro =catch (tag &body body)
  `(dynamic-wind
    (catch ,tag
      (proceed
       ,@body))))

(defmacro =restart-case (restartable-form &rest clauses)
  `(dynamic-wind
    (restart-case
        (proceed ,restartable-form)
      ,@clauses)))

(defmacro =restart-bind (restart-bindings &body body)
  `(dynamic-wind
    (restart-bind
        ,restart-bindings
      (proceed
       ,@body))))

(defmacro =with-simple-restart (args &body body)
  `(dynamic-wind
    (with-simple-restart ,args
      (proceed
       ,@body))))

(defmacro =ignore-errors (&rest body)
  `(dynamic-wind
    (ignore-errors
      (proceed
       ,@body))))

(defmacro =handler-bind* (handler-bindings &body body)
  `(dynamic-wind
    (um:handler-bind*
        ,handler-bindings
      (proceed
       ,@body))))

(defmacro =handler-case* (form &rest handler-cases)
  `(dynamic-wind
    (um:handler-case*
        (proceed ,form)
      ,@handler-cases)))

(defmacro =handler-bind-case (form &rest handler-cases)
  `(dynamic-wind
    (um:handler-bind-case
        (proceed ,form)
      ,@handler-cases)))

;; -------------------------------------------
;; Turn a simple closure into a Continuation

(defun =cont (fn)
  (let ((dyn-env (capture-dynamic-environment)))
    (lambda (&rest args)
      (with-dynamic-environment (dyn-env)
        (apply fn args)))))

;; -------------------------------------------
#|
(=handler-case
    (=bind (x)
        (=values 15)
      (print x)
      (error "make an error"))
  (error ()
    (print "error correctly intercepted")
    t))

(=handler-case
    (=bind (x)
        (ac:spawn-worker (=lambda ()
                           (=values 15))
                         =bind-cont)
      (print x)
      (error "make an error"))
  (error ()
    (print "error correctly intercepted")
    t))

(let ((tester (make-instance 'ac:actor)))
  (actors.base:perform-in-actor tester
    (=handler-case
        (=bind (x)
            (ac:spawn-worker (=lambda ()
                               (=values 15))
                             =bind-cont)
          (print x)
          (error "make an error"))
      (error ()
        (print "error correctly intercepted")
        t))))
|#
