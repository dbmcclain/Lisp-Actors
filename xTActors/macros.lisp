
(in-package :com.ral.actors.macros)

;; --------------------------------------
;; ACTOR in function position acts like a higher level LAMBDA expression

(defmacro actor (args &body body)
  `(create
    (lambda* ,args
      ,@body)))

#+:LISPWORKS
(editor:setup-indent "actor" 1)

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
      `(let (beta)
         (setf beta  (actor ,args ,@body))
         ,form)
      )))

#+:LISPWORKS
(editor:indent-like "beta" 'destructuring-bind)

(defmacro concurrently (&body body)
  ;; spawn a new concurrent Actor to perform the body
  `(send (actor () ,@body)))

;; ------------------------------------------------------
;; ALAMBDA -- a behavior lambda for Actors with pattern matching on
;; messages

(defmacro alambda (&rest clauses)
  (lw:with-unique-names (msg)
    `(lambda (&rest ,msg)
       (match ,msg ,@clauses))))

;; ------------------------------------------------------

(defmacro ret (&rest ans)
  `(send γ ,@ans))

(defmacro ret* (&rest ans)
  `(send* γ ,@ans))

(defmacro γlambda (&rest clauses)
  (lw:with-unique-names (msg)
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
     (setf ,name (actor ,(um:firsts bindings)
                   ,@body))
     (send ,name ,@(um:seconds bindings))))

#+:LISPWORKS
(editor:indent-like "actor-nlet" 'nlet)

;; ----------------------------------------------------

(µ α (args &body body)
  ;; α is to actor, what λ is to lambda
  `(actor ,args ,@body))

(µ αα (&rest clauses)
  `(create
    (alambda
     ,@clauses)))

(µ β (args form &body body)
  ;; β is to beta
  `(let (β)
     (setf β  (α ,args ,@body))
     ,form))

#+:LISPWORKS
(progn
  (editor:setup-indent "α" 1)
  (editor:indent-like "β" 'destructuring-bind))

;; ---------------------------------------------------

(defun do-with-custom-error-response (cust fn-err fn)
  ;; Defined such that we don't lose any debugging context on errors.
  ;;
  ;; Function fn-err takes an error condition as argument and returns
  ;; the message to be sent back in event of error abort.
  ;;
  ;; Function fn is a thunk.
  ;;
  (let (err)
    (restart-case
        (handler-bind ((error (lambda (e)
                                (setf err e))))
          (funcall fn))
      (abort ()
        :report "Handle next event, reporting"
        ;; generalized for use by ERL
        (abort-beh) ;; NOP unless within an Actor
        (apply #'send-to-all (um:mklist cust) (um:mklist (funcall fn-err err)))
        ))
    ))

(defmacro with-custom-error-response ((cust fn-err) &body body)
  ;; Handler that guarantees a customized message sent back to cust in
  ;; event of error. This version finds most utility with services
  ;; invoked by FORK.
  ;;
  ;; Function fn-err should accept an error condition arg and compute
  ;; a response message.
  ;;
  ;; Cust can be a single customer or a list of customers.
  ;;
  `(do-with-custom-error-response ,cust ,fn-err (lambda () ,@body)))

(defun err-from (e)
  `(:error-from ,self ,e))

(defmacro with-basic-error-response (cust &body body)
  ;; Handler that guarantees a message sent back to cust in event of
  ;; error. This version finds greatest utility for services guarded
  ;; behind a SERIALIZER. The Erlang layer also uses this version.
  ;;
  ;; Cust can be a single customer or a list of customers.
  ;;
  `(with-custom-error-response (,cust #'err-from) ,@body))

#+:LISPWORKS
(progn
  (editor:setup-indent "with-basic-error-response" 1)
  (editor:setup-indent "with-custom-error-response" 1))

;; ---------------------------------------------------

(defmacro def-ser-beh (name args &rest clauses)
  ;; For Actors behind a SERIALIZER, define their behaviors so that,
  ;; in any event, a response is sent to cust. The cust must be the
  ;; first arg of any message. Use ALAMBDA-style handler clauses.
  ;;
  ;; It becomes *Your* responsibilty to eventually respond to cust
  ;; from each of your handler clauses.
  ;;
  (lw:with-unique-names (cust msg)
    `(defun ,name ,args
       (lambda (,cust &rest ,msg)
         (with-basic-error-response ,cust
           (match (cons ,cust ,msg)
             ,@clauses
             (_
              (send ,cust :unhandled-message ,msg))
             ))
         ))
    ))

(defmacro def-beh (name args &rest clauses)
  `(defun ,name ,args
     (alambda
      ,@clauses)))

#+:LISPWORKS
(progn
  (editor:setup-indent "def-ser-beh" 2)
  (editor:setup-indent "def-beh" 2))

