
(in-package :com.ral.actors.macros)

;; ------------------------------------------------------
;; ALAMBDA -- a behavior lambda for Actors with pattern matching on
;; messages

(defmacro alambda (&rest clauses)
  (um:with-unique-names (msg)
    `(lambda (&rest ,msg)
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
  ;; All binding values should represent fresh Actor constructions.
  ;; Violations may lead to undefined behavior.
  ;;
  ;; Macros are syntax engines and cannot infer semantic actions.
  ;; Hence, there is no way to ensure proper use of this macro.
  ;;
  ;; Thanks to the Transactional Actor Behavior Protocol, no other
  ;; thread can learn the identity of any freshly constructed Actor
  ;; until we exit the behavior code.
  ;;
  ;; Since we are freshly constructing Actors for all the names in the
  ;; binding list, we are the only executing thread that knows about
  ;; them here, and it is therefore safe to immediately directly
  ;; mutate their behavior slots.
  ;;
  ;; As for the binding values, if they are freshly constructed Actors
  ;; then we can safely copy their behavior function, because, again,
  ;; nobody else has ever seen them. Their Actor envelopes will be
  ;; discarded by the GC.
  ;;
  ;; But if they are extant Actors, not freshly constructed, then
  ;; copying their behavior function will then have two Actors with
  ;; the same behavior and same shared state slots in the functional
  ;; closure. The possibility of future divergence between them
  ;; exists. That might be okay, but beware.
  ;;
  ;; Note that making our names into Forwarding Actors, to avoid
  ;; divergence, will not work correctly for common use cases. A TAG
  ;; is expected to furnish itself to the customer. A (FWD TAG) will
  ;; not present the correct identity to a customer who was only told
  ;; the identity of the FWD Actor.
  ;;
  `(let ,(mapcar #`(,(first a1) (create)) bindings)
     (setf ,@(mapcan #`((actor-beh ,(first a1)) (actor-beh ,(second a1))) bindings))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "actors" 1)

;; ----------------------------------------------------

(µ α (args &body body)
  ;; α is to actor, what λ is to lambda
  `(create
    (lambda* ,args ,@body)))

(µ αα (&rest clauses)
  `(create
    (alambda
     ,@clauses)))

(µ β (args form &body body)
  ;; β is a BETA - see below
  `(let ((β  (create (lambda* ,args
                       ,@body))))
     ,form))

#+:LISPWORKS
(progn
  (editor:setup-indent "α" 1)
  (editor:indent-like "β" 'destructuring-bind))

;; ----------------------------------------------

(defun parse-list-pat (pat)
  ;; Convert a proper list to itself and return LIST as the pattern
  ;; combiner. Convert an improper list to a proper list and furnish
  ;; LIST* as the pattern combiner.
  (um:nlet iter ((pat  pat)
                 (ans  nil))
    (if (cdr pat)
        (if (consp (cdr pat))
            (go-iter (cdr pat) (cons (car pat) ans))
          (values (nreverse (cons (cdr pat) (cons (car pat) ans)))
                  'list*))
      (values (nreverse (cons (car pat) ans))
              'list))
    ))

(defun parse-beta-args (args)
  (when (consp args)
    (let* ((proper-list (parse-list-pat args))
           (pos         (position-if (lambda (x)
                                       (and (symbolp x)
                                            (string= (symbol-name x) "/")))
                                     proper-list)))
      
      (when pos
        (values (um:take pos args)         ;; parms
                (um:drop (1+ pos) args)))  ;; fn args
      )))

;; -------------------------------------------------------
;; BETA Forms - A BETA form takes advantage of a Common Lisp pun, to
;; define an anonymous Actor which can be referred to in a subsequent
;; SEND, or some function which will eventually SEND, by the name
;; BETA, up until the next BETA form, which rebinds BETA. (BETA, cf. B
;; for Binding)
;;
;; ACTOR serves the same purpose for Actors that LAMBDA serves for
;; functions. ACTOR generates an anonymous Actor that can be LET bound
;; to some named binding.
;;
;; BETA extends this by providing a binding form for message arguments
;; that would arrive from a subsequent SEND, and then allows the body
;; function of the Actor to describe actions utilizing those message
;; args when they arrive. So BETA sets up an Actor continuation.
;;
;; BETA forms are nothing more than syntax sugar to make the use of
;; continuation Actors more readable. You could instead just define
;; the continuation Actor with a LET binding and then SEND a message
;; referring to it as an eventual message customer. But that separates
;; the Actor from the SEND which generates an eventual message to the
;; Actor. The BETA form allows their relationship to be more directly
;; discerned.
;;
;; BETA forms have a syntax reminiscent of DESTRUCTURING-BIND and
;; MULTIPLE-VALUE-BIND. The bindings are on the first line, followed
;; by a generator form - a SEND which will use this BETA Actor as a
;; message argument - and then follows the body of the Actor's
;; behavior function.
;;
;;  (BETA (args)
;;     (SEND diddly BETA)
;;    body-of-Actor-behavior)
;;
;; BETA forms come it two varieties. Just as Actors can be
;; parameterized by local state, as well as from message arguments, so
;; too can BETA forms. The normal argument list, like that just shown
;; above, for a BETA form describes an Actor which can only accept
;; message arguments for its parameterization - in addition to those
;; also provided by capture from the enclosing lexical context.
;;
;; But when an argument list contains a group of symbols followed by
;; "/" then those symbols before the slash separator are taken to mean
;; Actor parameters furnished at Actor construction time. Those args
;; following the separator represent message args as a result of a
;; SEND.
;;
;;   (BETA (param / arg)
;;       (SEND diddly (BETA paramVal))
;;     body-of-Actor-behavior)
;;
;; For the unparameterized version of BETA forms, you mention the Actor
;; by BETA in a SEND. But in a parameterized version you must make a
;; function call to BETA with parameter args, as in (BETA arg1 arg2 ...). In
;; effect, the parameterized version of BETA actually represents an Actor
;; generating function.
;;
;; When you need to refer explicitly to that generating function it
;; goes by the name #'BETA-GEN. But in normal use a function call form
;; with BETA in first position serves to call #'BETA-GEN. The values
;; of BETA and #'BETA-gen are available to the body code of the Actor
;; up until they are rebound by an inner BETA form.
;;
;; For use by BECOME, the parameterized behavior generator is named
;; #'BETA-BEH.
;;
;; ------------------------------------------------------------------
;; NOTE: Watch out for the following trap... A BETA form in an Actor
;; effectively spawns a new Actor, and the containing host Actor
;; simply skips over the BETA form. The host Actor and the BETA actor
;; run concurrently.
;;
;; The containing Actor could exit, then respond to a new message in
;; parallel with a waiting BETA Actor.  That could also modify the
;; conditions, e.g., external hardware ports could get closed, before
;; the BETA form executes. And that BETA form might be expecting a
;; continuity of external conditions. You might need IGNORE-ERRORS
;; inside that BETA form.
;;
;; A BETA form *does not* force serialized execution!! They operate
;; concurrently with the host Actor. Nested BETA forms, however, do
;; operate sequentially.
;;
;; See also: SUSPENDED-BEH, and SUSPEND, in prim-actors.lisp
;;
;; β could also be named α-bind, or ACTOR-BIND -- in harmony with
;; multiple-value-bind.

(defmacro beta (args form &body body)
  (multiple-value-bind (params binding-args)
      (parse-beta-args args)
    (if params
        ;; must use LET not LABELS here, because we still need beta as a
        ;; macro function
        `(labels ((beta-beh ,params
                    (lambda* ,binding-args
                      ,@body))
                  (beta-gen ,params
                    (create (beta-beh ,@params))))
           (macrolet ((beta (&rest args)
                        `(beta-gen ,@args)))
             ;; this beta redef lasts only for the next form
             ,form))
      ;; else
      `(let ((beta (create (lambda* ,args ,@body))))
         ,form)
      )))

#+:LISPWORKS
(editor:indent-like "beta" 'destructuring-bind)

;; ------------------------------------------------------

#|
(defmacro sequential-β (send-forms body-form)
  ;; Macro to generate β-forms to cause the Actor system to perform a
  ;; series of message sends sequentially before running the body
  ;; form. Each send should refer to β as the customer. Each message
  ;; target should respond to their customer, to permit the next send
  ;; to execute.
  ;;
  ;; Without these β-forms all messages would be sent at once at exit
  ;; of Actor body code. With sequential delivery we may avoid race
  ;; conditions.
  (um:nlet iter ((forms send-forms))
    (if forms
        `(β _
             ,(car forms)
           ,(iter (cdr forms)))
    ;; else
    body-form)))

;; ------------------------------------------------------

(defmacro concurrently (&body body)
  ;; spawn a new concurrent Actor to perform the body
  `(send (create (lambda () ,@body))))
|#

;; --------------------------------------------------------
;; Lisp is already inherently sequential, so a verb like SEQUENTIALLY
;; is redundant.

(defmacro concurrently (&rest exprs)
  `(tagbody
     ,@(mapcar (lambda (e)
                 `(send (create
                         (lambda ()
                           ,e))))
               exprs)
     ))

;; -----------------------------------------------------

(defmacro par (args exprs &body body)
  (lw:with-unique-names (cust)
    `(β ,args
         (send (fork ,@(mapcar (lambda (e)
                                 `(create
                                   (lambda (,cust)
                                     (send ,cust ,e))))
                               exprs))
               β)
       ,@body)))

#+:LISPWORKS
(editor:indent-like "par" 'destructuring-bind)

;; ------------------------------------------------------

(defmacro ret (&rest ans)
  `(send γ ,@ans))

(defmacro ret* (&rest ans)
  `(send* γ ,@ans))

(defmacro γlambda (&rest clauses)
  (um:with-unique-names (msg)
    `(lambda (γ &rest ,msg) ;; use RET or RET* instead of SEND CUST
       (match ,msg ,@clauses))
    ))

(defmacro γactor (args &body body)
  ;; use RET or RET* instead of SEND CUST
  `(create
    (lambda* (γ ,@(if (consp args)
                      args
                    `(&rest ,args)))
      ,@body)))

;; ----------------------------------------------------
;; Like NLET for functions, only this defines a locally named Actor
;; and then SENDs to it immediately.

(defmacro actor-nlet (name bindings &body body)
  `(let (,name)
     (setf ,name (create
                     (lambda ,(um:firsts bindings)
                       ,@body)))
     (send ,name ,@(um:seconds bindings))))

#+:LISPWORKS
(editor:indent-like "actor-nlet" 'nlet)

;; --------------------------------------------------

(defmacro yield (&body body)
  ;; exit Actor to allow concurrent actions, then resume
  ;; forces our continuation to the back of the event queue.
  `(β _
       (send β)
     ,@body))

;; ...this also happens to be one way to perform a switch from
;; imperative foreign thread code to Actors-land. So here we can
;; defne:

(defmacro with-actors (&body body)
  `(β _
       (send β)
     ,@body))

;; --------------------------------------------------------
;; Just as we have a need for cross-referrential Actors within code
;; bodies, and we have the ACTORS macro to provide them for us, we
;; need something at global lexical level to allow as-yet undefined
;; Actors to be declared, and then later fill in their behaviors.
;; Forward references, if you will...
;;
;;  (def-actor Actor-A)             ;; declare here so Actor-B can see it.
;;
;;  (def-actor Actor-B
;;    (lambda (cust &rest msg)
;;      (send Actor-A cust ....)))  ;; use Actor-A
;;
;;  (define-behavior Actor-A        ;; now fill in the behavior
;;    (lambda (cust &rest msg)
;;       ....))
;;
;; Unlike a Lisp function, an Actor is a lexical binding with a value
;; cell (beh) that is free to change. But the outer envelope - the
;; Actor identity - remains constant.
;;
;; So if we don't provide a forward reference with DEF-ACTOR, then any
;; code referring to it will be presumed by Lisp to be an, as yet
;; undefined, special-binding.  An Actor at global level is *NOT* a
;; special binding, it is a lexical binding, just like Lisp functions.
;;
;; It would generally be a grave mistake to allow that to happen. We
;; never want to change the identity of an Actor, just its behavior,
;; through BECOME.

;; ----------------------------------------------------------

(defun %becomer-beh (&rest msgs)
  (alambda
   (('%become arg)
    (become (screened-beh arg))
    (send-all-to self msgs))
   (msg
    (become (apply #'%becomer-beh msg msgs)))
   ))

(defmacro def-actor (name &optional beh)
  `(deflex ,name 
     (create ,(or beh
                  `(%becomer-beh))) ))

(defmacro define-behavior (name fn)
  `(progn
     (assert (actor-p ,name))
     (send ,name '%become ,fn)
     ))

#+:LISPWORKS
(progn
  (editor:setup-indent "def-actor" 0)
  (editor:setup-indent "define-behavior" 0))

#|
(defmacro def-beh (name args &rest clauses)
  `(defun ,name ,args
     (alambda
      ,@clauses)))

#+:LISPWORKS
(progn
  ;; (editor:setup-indent "def-ser-beh" 2)
  (editor:setup-indent "def-beh" 2))
|#
;; -----------------------------------------------------------

(defmacro restartable (&body body)
  ;; Make a body of code restartable alongside potentially failing
  ;; BECOME, which will auto-retry. The code might be non-idempotent,
  ;; but it won't execute unless the BECOME succeeds without retry.
  `(send (create (lambda ()
                   ,@body))) )

(defmacro non-idempotent (&body body)
  ;; A synonym for RESTARTABLE but marks the source code in a more
  ;; obvious manner. Meaning - this body action is non-idempotent, ans
  ;; so should only run if the Actor behavior is not retried.
  `(restartable ,@body))

(defmacro on-commit (&body body)
  ;; Synonym for RESTARTABLE and NON-IDEMPOTENT.
  ;; Meaning - the body action will only happen on
  ;; successful commit of the Actor body actions.
  `(restartable ,@body))

;; -------------------------------------------------------
;; My convenience macros...

(defmacro >> (&rest args)
  `(send ,@args))

(defmacro >>* (&rest args)
  `(send* ,@args))

(defmacro << (&rest args)
  `(funcall ,@args))

(defmacro <<* (&rest args)
  `(apply ,@args))

(defmacro ! (&rest args)
  `(setf ,@args))

(defmacro β! (arg)
  `(become ,arg))

(defmacro !β (arg) ;; for dyslexics, like me
  `(become ,arg))

(defmacro αλ (&rest clauses)
  `(alambda ,@clauses))

;; -------------------------------------------------------
;; Codify a recurring pattern - MP-safe system wide cached binding

(defvar *sys-cached-lock*  (mpc:make-lock))

(defmacro sys-cached (place val-expr)
  ;; PLACE must be previously defined and initially bound to NIL.
  ;; Val-Expr will only ever be executed just once, on demand, for the
  ;; Place binding value.
  `(or #1=,place
       (mpc:with-lock (*sys-cached-lock*)
         (or #1#
             (setf #1# ,val-expr)
             ))) )
         
