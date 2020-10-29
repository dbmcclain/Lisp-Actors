;; actors-macros.lisp -- actually, defuns that need to be in place for the macros to be defined later

(in-package #:actors-macros)

;; --------------------------------------------------------

(defun anaphor (sym)
  ;; ensure that a like symbol is interned into the user's current
  ;; package, e.g., (let ((a!self (anaphor 'self)) ...)
  (intern (string sym)))

(defun gensym-like (sym)
  (gensym (string sym)))


;; --------------------------------------------------------------------

;; LW Locks are more than twice as fast as spin-locks in this application
(defmacro make-lock (&rest args)
  `(mp:make-lock ,@args))

(defmacro with-lock ((lock) &body body)
  `(mp:with-lock (,lock)
     ,@body))


;; --------------------------------------------------------------------

(defun split-bindings (bindings)
  (values (mapcar #'car bindings)
          (mapcar #'cadr bindings)))

(defmacro def-alias (sym fn-sym)
  `(setf (symbol-function ',sym) (symbol-function ',fn-sym)))

;; --------------------------------------------------------------------
#|
Assuming your ANAPHOR function is defined something like:

(defun anaphor (name)
 (intern (symbol-name name)))

then I think all the problems are caused by having different values of
*PACKAGE* at different times and in different threads.

Note that the definition of ANAPHOR above has a dependency on the value of
*PACKAGE*, so using it at macroexpansion time will create a macroexpansion
time dependency on the value of *PACKAGE*.  This is probably OK when compiling
everything because all macros are fully expanded at compile time.  However,
when using the interpreter, macros are expanded at run time (as they are
encountered), so you will have a run time dependency on the value of
*PACKAGE*.

Here are two problems with this for interpreted code **within BEHAV**:

1. Any code that statically references SELF will refer to the symbol in
*PACKAGE* at read time, which may be different from the symbol bound by
MAKE-ACTOR at macroexpansion time.

2. If some other macro calls (anaphor 'self) hoping to pick up the binding
made by MAKE-ACTOR, then it will get a symbol named SELF in *PACKAGE* at the
run time (i.e. macroexpansion time) of that code.  If that code runs at the
same time as MAKE-ACTOR, then it will work.  However, code within a LAMBDA or
LABELS form might run later, in particular on the Executive thread.

Now for the fun part: the value of *PACKAGE* on the Executive thread will
depend on when the thread is created!

This is because the initial value of *PACKAGE* is defined by:

(assoc '*package* mp:*process-initial-bindings*) => (*package* . *package*)

so will capture the value in the thread that calls MP:PROCESS-RUN-FUNCTION.

I think that might explain all of the quirks you are seeing.

-- 
Martin Simmons
LispWorks Ltd
http://www.lispworks.com/
 |#

#|
(defun in-eval-mode-p (env)
  (or (null env)
      (notany (um:curry #'slot-value env)
              '(compiler::compilation-env
                compiler::remote-env
                compiler::benv
                compiler::fenv
                compiler::tenv
                compiler::venv))))

(defun ensure-thread-eval (form env)
  (if (in-eval-mode-p env)
      `(funcall (compile nil (lambda () ,form)))
    form))
  
(defun self-visible-p (anaphoric-self env)
  (and env
       (find anaphoric-self (slot-value env 'compiler::venv)
             :key (lambda (venv)
                    (slot-value venv 'compiler::name)))))

(defun ensure-self-binding (form env)
  (let ((a!self (anaphor 'self)))
    (if (self-visible-p a!self env)
        form
    `(let ((,a!self (current-actor)))
       ,form)
    )))

|#
