
(in-package :com.ral.actors.macros)

#|
(defmacro behav (args &body body)
  `(protecting-variables ()
     (lambda* ,args
       ,@body)))
|#

(defmacro behav (args &body body)
  `(lambda* ,args
       ,@body))

#+:LISPWORKS
(editor:indent-like 'behav 'lambda)

;; ------------------------------------------------------
;; ALAMBDA -- a behavior lambda for Actors with pattern matching on
;; messages

(defmacro alambda (&rest clauses)
  (um:with-unique-names (msg)
    `(behav (&rest ,msg)
       (match ,msg ,@clauses))))

;; ----------------------------------------------------
;; ACTORS -- like LABELS, but for Actors.
;; Allows for construction of multiple Actors that reference each other.
;;
;; So what's the problem here? Why can't we just use LETREC?
;;
;; LETREC allows for the declaration of bindings that reference each
;; other, but only if such references are found within lambda
;; closures, not at top level.
;;
;; Example:
;;    (letrec  ((FACTO  (lambda (n)
;;                         (if (> n 1)
;;                              (* n (funcall FACTO (1- n)))
;;                            1)) ))
;;         ...
;;
;; This translates into:
;;
;;    (let (FACTO)
;;      (setf FACTO (lambda (n) ...))
;;      ...
;;
;; So, as long as LETREC bindings are referred to only from inside of
;; a lambda closure, a delayed reference, this will work. But if one
;; wants to use the value of a binding before all the SETF's have
;; completed, then you run into trouble.
;;
;; Now consider the following:
;;
;;   (actors ((tag1  (tag ans))
;;            (tag2  (tag ans))
;;            (ans   (gate tag1 tag2 (acurry fmt-println "Answer from ~S"))))
;;      ...)
;;
;;  Here we want to set up a race between two Actors that will
;;  respond, respectively, to TAG1 and TAG2, which then send
;;  themselves to ANS.
;;
;;  But because we are referencing ANS, in the value of bindings for
;;  TAG1 and TAG2, at top-level, ANS must be an Actor at the time of
;;  top-level reference, not later as from inside some lambda closure.
;;  But ANS cannot be defined until TAG1 and TAG2 have been defined.
;;
;;  So the initial value given to each binding must be an Actor. Their
;;  behaviors can be filled in later, which does not change their
;;  identity.
;;

(defmacro actors (bindings &body body)
  ;; For cross- and self-referential Actor constructions...
  ;;
  ;; All binding values should represent fresh Actor constructions.
  ;;
  ;; Thanks to the Transactional Actor Behavior Protocol, no other
  ;; thread can learn the identity of any freshly constructed Actor
  ;; until we exit the behavior code.
  ;;
  ;; Since we are freshly constructing Actors for all the names in the
  ;; binding list, we are the only executing thread that knows about
  ;; them here, and it is therefore safe to immediately and directly
  ;; mutate their behavior slots.
  ;;
  `(let ,(mapcar #`(,(first a1) (create)) bindings)
     (setf ,@(mapcan #`((actor-beh ,(first a1)) (actor-beh ,(second a1))) bindings))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "actors" 1)

;; -------------------------------------------------------
;;
;; BETA Forms - syntactically similar to MULTIPLE-VALUE-BIND, but
;; instead of a values-generating expression following the binding
;; args, we use a SEND with BETA in customer position of the message.
;;
;; The forms following the SEND become the body forms of an unnamed
;; Actor bound to lexical symbol BETA. That Actor is a continuation of
;; sorts, which will be activated on receipt of a message. The args
;; list will be bound to the parameters of the message.
;;
;;  (BETA (args)
;;     (SEND diddly BETA)
;;    body-of-Actor-behavior)
;;
;; ------------------------------------------------------------------
;; NOTE: Watch out for the following trap... A BETA form in an Actor
;; defines a new Actor, and the containing host Actor simply skips
;; over the BETA form. The host Actor and the BETA actor can run
;; concurrently and share LET bindings that were in effect at the
;; position of the SEND.
;;

(defmacro beta (args form &body body)
  `(let ((beta (create
                (behav ,args ,@body))))
     ,form))

#+:LISPWORKS
(editor:indent-like "beta" 'destructuring-bind)

;; ----------------------------------------------------

(µ α (args &body body)
  ;; α is to actor, what λ is to lambda
  `(create
    (behav ,args ,@body)))

(µ αα (&rest clauses)
  `(create
    (alambda
     ,@clauses)))

(µ β (args form &body body)
  ;; β is a BETA
  `(let ((β (create
             (behav ,args ,@body))))
     ,form))

#+:LISPWORKS
(progn
  (editor:setup-indent "α" 1)
  (editor:indent-like "β" 'destructuring-bind))

;; --------------------------------------------------------

(defmacro par (args exprs &body body)
  (let ((cust (gensym)))
    `(β ,args
         (send (fork ,@(mapcar (lambda (e)
                                 `(create
                                   (behav (,cust)
                                     (send ,cust ,e))))
                               exprs))
               β)
       ,@body)))

#+:LISPWORKS
(editor:indent-like "par" 'destructuring-bind)

;; --------------------------------------------------

;; ...this also happens to be one way to perform a switch from
;; imperative foreign thread code to Actors-land. So here we can
;; defne:

(defmacro with-actors (&body body)
  `(β _
       (send β)
     ,@body))

;; -----------------------------------------------------------

(defmacro on-commit (&body body)
  ;; Synonym for RESTARTABLE and NON-IDEMPOTENT.
  ;; Meaning - the body action will only happen on
  ;; successful commit of the Actor body actions.
  `(send (create
          (behav ()
            ,@body))) )

;; -------------------------------------------------------
;; My convenience macros...

(defun >> (&rest args)
  (send* args))

(defun >>* (&rest args)
  (apply #'send* args))

(defun << (&rest args)
  (apply #'funcall args))

(defun <<* (&rest args)
  (apply #'apply args))

(defmacro ! (&rest args)
  `(setf ,@args))

(defun β! (arg)
  (become arg))

(defun !β (arg) ;; for dyslexics, like me
  (become arg))

(defmacro αλ (&rest clauses)
  `(alambda ,@clauses))

