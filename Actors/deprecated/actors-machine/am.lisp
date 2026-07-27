;; am.lisp
;;
;; DM/RAL  2023/10/07 01:10:20 UTC
;; ----------------------------------

(defpackage #:am
  (:use #:common-lisp :ac))

(in-package #:am)

;; ----------------------------------
Msg:   target, args
          |
          +-- SELF

Actor: Behavior
         |
         +-- Code
         |
         +-- Create Data, Env

(defstruct beh
  code env)

(defun collect-arg-names (args)
  (um:nlet iter ((arg    args)
                 (names  nil))
    (cond ((null arg)  names)
          ((consp arg)
           (cond ((member (car arg) '(&optional &key &aux))
                  (append names
                          (mapcan (lambda (arg)
                                    (if (consp arg)
                                        (list (car arg))
                                      (unless (or (member arg lambda-list-keywords)
                                                  (string= arg "_"))
                                        (list arg))))
                                  (cdr arg))))
                 (t
                  (append (iter (car arg) names)
                          (iter (cdr arg) nil)))
             ))
          ((or (member arg lambda-list-keywords)
               (string= arg "_"))
           names)
          (t
           (cons arg names))
          )))

(collect-arg-names '((a . b) &rest c &key (kw1 a) (kw2 2)))

(defmacro defbeh (args &body body)
  (let ((names (collect-arg-names args)))
    `(lambda ,args
       (values ',body
               (pairlis ',names
                        (list ,@names))))
    ))

(defstruct closure
  code env)

(defun close-over (factory args env)
  (multiple-value-bind (code binds)
      (apply factory args)
    (make-closure
     :code code
     :env  (cons binds env))
    ))

(defbeh (a b . c)
  a b + c)

(defun find-binding (env glb name)
  (um:nlet iter ((env  env))
    (cond ((null env)
           (let ((pair (assoc name glb)))
             (when pair
               (values (cdr pair) t))
             ))
          (t
           (let ((pair (assoc name (car env))))
             (if pair
                 (values (cdr pair) t)
               (go-iter (cdr env)))
             ))
          )))

(defun interp-beh (&optional glb-env)
  (alambda
   ((:interp (instr . instrs) env stk old-beh new-beh msgs)
    (case instr
      (:become
       (let ((clos  (pop stk)))
         (send self :interp instrs env stk old-beh clos msgs)))
      
      (:send
       (let* ((target (pop stk))
              (msg    (pop stk)))
         (send self :interp instrs env stk old-beh new-beh (cons (cons target msg) msgs))))
      
      (:create
       (let* ((clos (pop stk))
              (act  (new-actor clos)))
         (push act stk)
         (send self :interp instrs env stk old-beh new-beh msgs)))
      
      (:commit
       (let ((itself (find-in-env env 'self)))
         (if (or (eq old-beh new-beh)
                 (set-beh! itself old-beh new-beh))
             (dolist (msg msgs)
               (send* self :actor-dispatch msg))
           ;; else
           (let ((self-msg (find-in-env env 'self-msg)))
             (send* self :actor-dispatch itself self-msg))
           )))
      
      (:push-env
       (let ((new-env (cons (pop stk) env)))
         (send self :interp instrs new-env stk old-beh new-beh msgs)))
      
      (:restore-env
       (send self :interp instrs (pop stk) stk old-beh new-beh msgs))
      
      (:add-global
       (let ((name (pop stk))
             (val  (pop stk)))
         (become (interp-beh (acons name val glb-env)))
         (send self :interp instrs env stk old-beh new-beh msgs)))
      
      (otherwise
       (exec-instr instr stk env)
       (send self :interp instrs env stk old-beh new-beh msgs))
     ))
    
   ((:actor-dispatch target . msg)
    (let* ((beh     (actor-beh target))
           (code    (beh-code beh))
           (state   (beh-env  beh)) ;; a list of alists of bindings
           (env     (cons `((self-msg . ,msg)
                            (self     . ,target))
                          state)))
      (send self :interp code env nil beh beh nil)))
   ))

(deflex amachine (interp-beh))

'( '(cust) '(self-msg @ cust @ send* sink-beh @ become) defbeh once-beh add-global)


Env:   locals, current message, create args
                    
(defun cpu-beh ()
  (lambda (instr sp ep next)
    (