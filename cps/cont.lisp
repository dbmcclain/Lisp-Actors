;; cont.lisp -- managing dynamic environments
;;
;; DM/RAL 11/20 -- using dynamic-wind for this
;; -------------------------------------------------------
(in-package :cps)
;; -------------------------------------------------

(um:eval-always
  (import '(um:dynamic-wind
            um:proceed
            um:capture-dynamic-environment
            um:call-with-dynamic-environment)))

;; -------------------------------------------------------
;; CPS Continuation Operators
;;
;; If there is any chance that the enclosing function has exited the
;; dynamic chain before a continuation is invoked, we need to capture
;; that chain so that it can be re-instituted before calling the
;; continuation closure. Use these =Handlers insetead of the usual
;; Lisp forms to enable that action. (This most likely occurs in Actor
;; code.)

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

(defmacro =unwind-protect (form &rest exit-forms)
  `(dynamic-wind
     (unwind-protect
         (proceed ,form)
       ,@exit-forms)))

(defmacro =let (bindings &body body)
  `(dynamic-wind
     (let ,bindings
       (proceed
        ,@body))))

(defmacro =let* (bindings &body body)
  `(dynamic-wind
     (let* ,bindings
       (proceed
        ,@body))))

;; -------------------------------------------
;; Turn a simple closure into a Continuation

(defun =cont (fn)
  (let ((dyn-env (capture-dynamic-environment)))
    (lambda (&rest args)
      (apply #'trampoline #'call-with-dynamic-environment dyn-env fn args))
    ))

(defun =fut (fn)
  (let ((dyn-env (capture-dynamic-environment)))
    (lambda (&rest args)
      (apply #'call-with-dynamic-environment dyn-env fn args))
    ))

;; -------------------------------------------
#+:LISPWORKS
(progn
  (editor:setup-indent "=handler-bind" 1 2 4)
  (editor:setup-indent "=handler-case" 1 2 4 'handler-case)
  (editor:setup-indent "=catch" 1 2 4)
  (editor:setup-indent "=restart-bind" 1 2 4)
  (editor:setup-indent "=restart-case" 1 2 4 'handler-case)
  (editor:setup-indent "=with-simple-restart" 1 2 4)
  (editor:setup-indent "=ignore-errors" 0 2 4)
  (editor:setup-indent "=handler-bind*" 1 2 4)
  (editor:setup-indent "=handler-case*" 1 2 4 'handler-case)
  (editor:setup-indent "=handler-bind-case" 1 2 4 'handler-case))

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
        (=handler-case
            (=bind (x)
                (ac:spawn-worker (=lambda ()
                                   (=values 15))
                                 =bind-cont)
              (print x)
              (error "make an error"))
          (error ()
            (print "error correctly intercepted")
            (error "make another error")))
      (error ()
        (print "error correctly intercepted again")
        t))))

(let ((final nil)
      (ctr 0))
  (=unwind-protect
   (=bind (x)
       (ac:spawn-worker (=lambda ()
                          (=values 15))
                        =bind-cont)
     (print x)
     (setf final t))
   (incf ctr)
   (format t "~&Unwind-exit ctr = ~D" ctr)
   (when final
     (print "yes - we are final"))
   ))

(block top
  (=bind (x)
      (=values 15)
    (print x)
    (return-from top 32)))

(catch 'tag
  (ac:spawn-worker (lambda ()
                     (throw 'tag 32))))

(dynamic-wind
  (print
   (proceed
    (=catch 'top
      (=bind (x)
          (ac:spawn-worker (=lambda ()
                             (=values 15))
                           =bind-cont)
        (print x)
        (throw 'top 32))))))
|#
