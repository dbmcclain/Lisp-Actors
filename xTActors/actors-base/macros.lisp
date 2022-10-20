
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
;; Like LETREC, but for Actor defs, cross-referential

(defmacro actors (bindings &body body)
  `(let ,(mapcar #`(,(first a1) (create)) bindings)
     ,@(mapcar #`(%set-beh ,(first a1) ,(second a1)) bindings)
     ,@body))

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
  (um:with-unique-names (msg)
    `(lambda (&rest ,msg)
       (match ,msg ,@clauses))))

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

;; --------------------------------------------------

(defmacro yield (&body body)
  ;; exit Actor to allow concurrent actions, then resume
  ;; forces our continuation to the back of the event queue.
  `(β _
       (send β)
     ,@body))

;; ---------------------------------------------------
;; Macros to assist in the conversion of CALL/RETURN style into Actors code.
;;
;; Whereas, in CALL/RETUN you could do:
;;
;;   (when (and A B C)
;;     (do-something...))
;;
;; In Actors code, where A, B, or C is a SEND to an Actor, there is no
;; immediate value to test. Instead, we need to construct continuation
;; Actors along the way. We would have to write:
;;
;;    (β (ans)
;;        (send A β)
;;      (when ans
;;        (β (ans)
;;            (send B β)
;;          (when ans
;;            (β (ans)
;;                (send C β)
;;              (when ans
;;                 (do-something...)) )) )))
;;
;; To make this less painful, we can use these macros to write:
;;
;;    (WHEN-β-AND (ans (send A β)
;;                     (send B β)
;;                     (send C β))
;;      (do-something...))

(defmacro with-β-and ((ans &rest clauses) &body body)
  ;; Short-circuit eval of clauses. Ans will be the value produced by
  ;; the last clause, or nil if any of the clauses produce nil.
  (lw:with-unique-names (proc)
    (let ((decl (when (um:is-underscore? ans)
                  ;; We have to diddle the _ to prevent its being
                  ;; discarded in the β forms that follow below.
                  (let ((gans (gensym)))
                    (setf ans gans)
                    `((declare (ignore ,gans)))))
                ))
      `(flet ((,proc (,ans)
                ,@decl
                ,@body))
         ,(um:nlet iter ((clauses (reverse clauses))
                         (xform   `(,proc ,ans)))
            (if (endp clauses)
                xform
              (go-iter (cdr clauses)
                       (let ((clause (car clauses)))
                         (if (and (consp clause)
                                  (or (eql 'send  (car clause))
                                      (eql 'send* (car clause))))
                             `(β (,ans)
                                  ,clause
                                (if ,ans
                                    ,xform
                                  (,proc nil)))
                           ;; else
                           `(let ((,ans ,clause))
                              (if ,ans
                                  ,xform
                                (,proc nil)))
                           )))
              )))
      )))

(defmacro with-β-or ((ans &rest clauses) &body body)
  ;; Short-circuit eval of clauses. Ans will be the value of the first
  ;; clause to produce non-nil, or else nil if none do.
  (lw:with-unique-names (proc)
    (let ((decl (when (um:is-underscore? ans)
                  ;; We have to diddle the _ to prevent its being
                  ;; discarded in the β forms that follow below.
                  (let ((gans (gensym)))
                    (setf ans gans)
                    `((declare (ignore ,gans)))))))
      `(flet ((,proc (,ans)
                ,@decl
                ,@body))
         ,(um:nlet iter ((clauses (reverse clauses))
                         (xform   `(,proc nil)))
            (if (endp clauses)
                xform
              (go-iter (cdr clauses)
                       (let ((clause (car clauses)))
                         (if (and (consp clause)
                                  (or (eql 'send  (car clause))
                                      (eql 'send* (car clause))))
                             `(β (,ans)
                                  ,clause
                                (if ,ans
                                    (,proc ,ans)
                                  ,xform))
                           ;; else
                           `(let ((,ans ,clause))
                              (if ,ans
                                  (,proc ,ans)
                                ,xform
                                (proc nil)))
                           )))
              )))
      )))

(defmacro if-β (test iftrue &optional iffalse)
  (lw:with-unique-names (ans)
    `(with-β-and (,ans ,test)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

#+:LISPWORKS
(progn
  (editor:indent-like 'if-β     'if)
  (editor:indent-like 'if-β-and 'if)
  (editor:indent-like 'if-β-or  'if))

(defmacro when-β (test &body body)
  (lw:with-unique-names (ans)
    `(with-β-and (,ans ,test)
       (when ,ans
         ,@body))))

(defmacro unless-β (test &body body)
  (lw:with-unique-names (ans)
    `(with-β-and (,ans ,test)
       (unless ,ans
         ,@body))))

(defmacro if-β-and ((&rest clauses) iftrue &optional iffalse)
  (lw:with-unique-names (ans)
    `(with-β-and (,ans ,@clauses)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro if-β-or ((&rest clauses) iftrue &optional iffalse)
  (lw:with-unique-names (ans)
    `(with-β-or (,ans ,@clauses)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro when-β-and ((ans &rest clauses) &body body)
  `(if-β-and (,ans ,@clauses) (progn ,@body)))

(defmacro when-β-or ((ans &rest clauses) &body body)
  `(if-β-or (,ans ,@clauses) (progn ,@body)))

(defmacro unless-β-and ((ans &rest clauses) &body body)
  `(if-β-and (,ans ,@clauses) 'nil (progn ,@body)))

(defmacro unless-β-or ((ans &rest clauses) &body body)
  `(if-β-or (,ans ,@clauses) 'nil (progn ,@body)))

#|
(if (and a b c)
    iftrue
  iffalse)

(with-β-and (ans (send a β)
                  (send b β)
                  (symbolp x)
                  (send* c β args))
  (if ans
      iftrue
    iffalse))

                              
|#
