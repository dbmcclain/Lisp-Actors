
(in-package :actors-base)

;; ------------------------------------------------------
;; Macro for creating actors with the behavior specified by body

(defmacro become (behavior &environment env)
  ;; change the behavior of the Actor for future messages (ugh!)
  (let ((a!self (anaphor 'self)))
    (ensure-self-binding
     `(setf (actor-behavior ,a!self) ,behavior)
     env)
    ))

(defmacro behav (args state &body body &environment env)
  ;; can only be used inside actor's body code to produce a new actor
  ;; state
  (let* ((a!self  (anaphor 'self))
         (a!me    (anaphor 'me))
         (subst   `(let ,state
                     (labels ((,a!me ,(cons a!self ,args)
                                (declare (ignorable ,a!self))
                                ,@body))
                       #',a!me))
                  ))
    (ensure-thread-eval subst env)))

(defun generate-actor (behav-fn)
  (let ((actor (make-instance 'Actor
                              :behav behav-fn)))
    (add-actor actor)
    actor))

(defmacro def-factory (name args state &body body)
  ;; state is like a LET binding
  ;; args is a list of args expected by the outer behavior.
  ;;
  ;; This macro builds a function that can be called to make multiple
  ;; instances of the same behavior (same kind of Actor), each with
  ;; their own private copy of internal state.
  ;;
  ;; within body you can refer to symbols ME = body code function,
  ;; SELF = current actor, and any of the symbols named in the binding
  ;; forms of the initial state
  `(defun ,name (&key ,@state)
     (generate-actor (behav ,args () ,@body))) )

(defmacro make-actor (args state &body body)
  ;; MAKE-ACTOR is to the Actor system what LAMBDA is to Lisp. It
  ;; constructs an anonymous embodiment of behaior with a partially
  ;; known argument list. (i.e., the SELF argument is always sent as
  ;; the first arg)
  `(generate-actor (behav ,args ,state ,@body)))

(defun spawn (behavior &rest args)
  ;; SPAWN - like MAKE-ACTOR combined with SEND
  (let ((actor (make-actor (&rest run-args)
                   ()
                 (apply behavior run-args))))
    (apply #'send actor args)
    actor))

;; ----------------------------------------------------------------------------

(Defmacro lambdac (args &body body &environment env)
  (ensure-thread-eval `(lambda ,args ,@body) env))

(defmacro defunc (name args &body body &environment env)
  (let ((subst `(defun ,name ,args ,@body)))
    (if (in-eval-mode-p env)
        `(progn
           ,subst
           (compile ',name))
      subst)))
