#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defpackage :com.ral.useful-macros.pandoric
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.pandoric)

;; Macros which make use of anaphoric symbols may sometimes fail with
;; unbound symbol when evaluated in intrpreter mode in some thread
;; other than the thread in use when the form or definition was
;; initially constructed.
;;
;; This happens most often when some chunk of code is stashed away for
;; later evaluation. If that later interpreter evaluation occurs in a
;; different thread, it might not properly resolve the anaphoric
;; symbols. (see the THIS binding in PANDORICLET)
;;
;; We can overcome this by always compiling the form in the defining
;; thread. Compiled code always works properly because the symbols are
;; fully resolved at compile time, even for stashed bits of code.

#+:LISPWORKS
(defun in-eval-mode-p (env)
  ;; return true if we are in the outer global environment
  (or (null env)
      (notany (curry #'slot-value env)
              '(compiler::compilation-env
                compiler::remote-env
                compiler::benv
                compiler::fenv
                compiler::tenv
                compiler::venv))))

#-:LISPWORKS
(defun in-eval-mode-p (env)
  ;; return true if we are in the outer global environment
  t)

(defun ensure-thread-eval (form env)
  (if (in-eval-mode-p env)
      `(funcall (compile nil (lambda () ,form)))
    form))

(defun ensure-thread-eval-def (name def-form env)
  (if (in-eval-mode-p env)
      `(progn
         ,def-form
         (compile ',name))
    def-form))

#+:LISPWORKS
(editor:setup-indent "ensure-thread-eval-def" 1)

;; ----------------------------------------------------------------------

(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env ,a!this))
     (setq ,a!this
       (lambda (&rest ,g!temp-args)
         ,@body
         (apply ,g!indir-env
                ,g!temp-args)))))

#|
(ichain-before (doit))
(alet ((a 1) (b 2)) (alambda (&rest args) body))
(let-hotpatch ((a 1) (b 2)) (lambda (x y &rest args) (print (list a b))))
 |#

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env ,a!this))
     (setq ,a!this
       (lambda (&rest ,g!temp-args)
         (multiple-value-prog1
           (apply ,g!indir-env
                  ,g!temp-args)
           ,@body)))))

(defmacro! ichain-intercept (&rest body)
  `(let ((,g!indir-env ,a!this))
     (setq ,a!this
       (lambda (&rest ,g!temp-args)
         (block ,g!intercept
           (macrolet ((,a!intercept (v)
                       `(return-from
                          ,',g!intercept
                          ,v)))
             (multiple-value-prog1
               (apply ,g!indir-env
                      ,g!temp-args)
               ,@body)))))))

(defmacro! alet-hotpatch (letargs &rest body)
  `(let ((,a!this) ,@letargs)
     (setq ,a!this ,@(last body))
     ,@(butlast body)
     (dlambda
       (:hotpatch (closure)
        (setq ,a!this closure))
       (t (&rest args)
          (apply ,a!this args)))))

(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
       (:hotpatch (closure)
        (setq ,g!this closure))
       (t (&rest args)
          (apply ,g!this args)))))

(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))

;; ----------------------------------------------------------------------

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree) 
          (cons
           (tree-leaves%% (car tree) test result)
           (tree-leaves%% (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
          tree))))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
     ,tree
     (lambda (x)
       (declare (ignorable x))
       ,test)
     (lambda (x)
       (declare (ignorable x))
       ,result)))

(defmacro sublet (bindings% &rest body)
  (let ((bindings (let-binding-transform
                    bindings%)))
    (setq bindings
      (mapcar
        (lambda (x)
          (cons (gensym (symbol-name (car x))) x))
        bindings))
    `(let (,@(mapcar #'list
                     (mapcar #'car bindings)
                     (mapcar #'caddr bindings)))
       ,@(tree-leaves
           body
           #1=(member x bindings :key #'cadr)
           (caar #1#)))))

(defmacro sublet* (bindings &rest body)
  `(sublet ,bindings
     ,@(mapcar #'macroexpand-1 body)))

;; ----------------------------------------------------

(defun pandoric-body (letargs)
  `(dlambda
     (:pandoric-get (sym)
      ,(pandoriclet-get letargs))
     (:pandoric-set (sym val)
      ,(pandoriclet-set letargs))
     (:pandoric-syms ()
      ',(mapcar (compose #'kwsymbol #'car) letargs))
     (:pandoric-vals ()
      (list ,@(mapcan #`(,(kwsymbol (car a1)) ,(car a1)) letargs)))
     (t (&rest args)
        (apply this args))
     ))

(defmacro! pandoriclet (lexref letargs &rest body &environment env)
  (let* ((letargs (cons
                   '(this)
                   (let-binding-transform
                    letargs)))
         (panargs (append letargs
                          (let-binding-transform lexref))))
    (ensure-thread-eval
     `(let* (,a!self ,@letargs)
        (setq ,a!self ,(pandoric-body panargs)
              this    ,@(last body))
        ,@(butlast body)
        ,a!self)
     env)))

#+:LISPWORKS
(editor:setup-indent "pandoriclet" 1)

#|
(pandoriclet ((a 1) (b 2)) () (lambda (x y) body))
|#

(defun pandoric-get-error (sym)
  (error "Unknown pandoric get: ~s" sym))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(kwsymbol (car a1))) ,(car a1)) letargs)
     (t (pandoric-get-error sym))
     ))

(defun pandoric-set-error (sym)
  (error "Unkown pandoric set: ~s" sym))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(kwsymbol (car a1)))
                   (setq ,(car a1) val))
               letargs)
     (t (pandoric-set-error sym))
     ))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro! with-pandoric (syms o!box &rest body)
  ;; the syms is a list of symbols stated in the manner you want to
  ;; use them, i.e., most like local package symbols. These match the
  ;; pandoric bindings, regardless of what package in which they had
  ;; been desfined. True because we convert to keyword as the medium
  ;; of exchange.
  `(symbol-macrolet
     (,@(mapcar #`(,a1 (get-pandoric ,g!box ,(kwsymbol a1)))
                syms))
     (declare (ignorable ,@syms))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-pandoric" 2)

#|
(with-pandoric (a b c) thebox (dobody a b))

(setf (symbol-function 'pantest)
      (pandoriclet ((acc 0))
          (lambda (n) (incf acc n))))
(pantest 3)
(pantest 5)
(pantest :pandoric-get 'acc)
(pantest :pandoric-set 'acc 100)
(pantest 3)
(pantest :pandoric-get 'this)
(get-pandoric #'pantest 'acc)
(setf (get-pandoric #'pantest 'acc) -10)
(pantest 3)
(with-pandoric (acc) #'pantest
  (format t "Value of acc: ~A~%" acc))
(with-pandoric (acc) #'pantest
  (setq acc 5))
(pantest 1)
 |#

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))

#+:LISPWORKS
(editor:setup-indent "pandoric-hotpatch" 1)

#|
(pantest 0)
(pandoric-hotpatch #'pantest
  (let ((acc 100))
    (lambda (n) (decf acc n))))
(pantest 3)
(with-pandoric (acc) #'pantest
  acc)
 |#

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setq this ,new)))

#+:LISPWORKS
(editor:setup-indent "pandoric-recode" 2)

#|
(pandoric-recode (acc) #'pantest
  (lambda (n)
    (decf acc (/ n 2))))
(pantest 2)
(with-pandoric (acc) #'pantest
  acc)
 |#

(defmacro! plambda (largs pargs &body body &environment env)
  (let ((pargs (mapcar #'list `(this ,@pargs))))
    (ensure-thread-eval
     `(let (this ,a!self)
        (setq ,a!self ,(pandoric-body pargs)
              this    (lambda ,largs ,@body))
        ,a!self)
     env)))

#+:LISPWORKS
(editor:setup-indent "plambda" 2)
        
#|
(plambda (x y) ((a 1) (b 2))
  (print (list a b x y)))
(plambda (n) ((acc 0)) (incf acc n))
|#

(defmacro! defpan (name args pargs &rest body &environment env)
  (ensure-thread-eval-def name
    `(defun ,name (,a!self ,@args)
       ,(if pargs
            `(with-pandoric ,pargs ,a!self
               ,@body)
          `(progn ,@body)))
    env))

(defmacro! defpan-method (name args pargs &rest body &environment env)
  (ensure-thread-eval-def name
    `(defmethod ,name (,a!self ,@args)
       ,(if pargs
            `(with-pandoric ,pargs ,a!self
               ,@body)
          `(progn ,@body)))
    env))

#+:LISPWORKS
(editor:setup-indent "defpan" 3)
#+:LISPWORKS
(editor:setup-indent "defpan-method" 3)

(defvar *pandoric-eval-tunnel*)

(defmacro pandoric-eval (vars expr)
  `(let ((*pandoric-eval-tunnel*
           (plambda () ,vars t)))
     (eval `(with-pandoric
              ,',vars *pandoric-eval-tunnel*
              ,,expr))))

;; ----------------------------------------------------------------------

(defun pinspect (box)
  (mapcar (lambda (key)
            (cons key (get-pandoric box key)))
          (funcall box :pandoric-syms)))

;; ----------------------------------------------------------------------
#|
(um:defpan handle-state-machine-message (&rest msg) (state state-handlers)
  ;; GETF uses EQ comparison -- identical
  (um:if-let (fn (getf state-handlers state)) ;; get state handlers
      ;; we got the handler function. It takes a message and
      ;; returns a closure to call for that message, or nil
      ;; on no handler.
      (funcall fn msg) ;; this might change state...
    ;; else
    (error "Invalid state")))

(defmacro! make-pandoric-state-machine (lex-refs state-bindings initial-state &body body)
  ;; Every Actor has a name, possibly some non-empty initial
  ;; internal-state bindings and a body.  A state-machine also takes
  ;; the name used to refer to messages in the body, an initial state
  ;; value for machine state, and a collection of handlers for each
  ;; state. The body will be synthesized for use by the state machine
  ;; grinder.
  `(pandoriclet ,lex-refs
     (,@state-bindings
      (state          ,initial-state)
      (state-handlers (macrolet ((,a!next-state (state)
                                   `(setf state ,state)))
                        ,@(last body))))
     ,@(butlast body)
     (um:curry #'handle-state-machine-message ,a!self)))

(defmacro defstates (&rest clauses)
  `(list ,@(mapcan #`(,(car a1) (optima.extra:lambda-match
                                  ,@(cdr a1)))
                   clauses)))

#+:LISPWORKS
(editor:setup-indent "make-state-machine" 3)
|#

#|
(let ((x  (make-pandoric-state-machine () (val) :initial
            (defstates
             (:initial
              ((list :echo x) (ac:pr x))
              ((list :who)    (ac:pr self))
              ((list :test x)
               (setf val x)
               (next-state :one)))
             
             (:one
              ((list :try x)
               (ac:pr (+ x val))
               (next-state :initial)))
             ))))
  (funcall x :echo :This)
  (funcall x :who)
  (funcall x :test 15)
  (funcall x :who)
  (funcall x :echo :That)
  (funcall x :try 32)
  (funcall x :quit))
|#