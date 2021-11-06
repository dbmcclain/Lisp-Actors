
(in-package :actors/macros)

(defun %become (new-beh)
  (check-type new-beh function)
  (check-type self actor)
  (locally
    (declare (actor self))
    (setf (actor-beh self) new-beh)))

(defmacro %using-become (where &body body)
  (lw:with-unique-names (spon)
    `(let ((,spon (or ,where base-sponsor)))
       (if (eq self-sponsor ,spon)
           (macrolet ((become (new-beh)
                        `(%become ,new-beh)))
             ,@body)
         ;; else
         (send* ,spon self actors/base:*whole-message*)))
    ))

#+:LISPWORKS
(editor:setup-indent "using-become" 1)

(defun %send (actor &rest msg)
  (check-type actor actor)
  (check-type self actor) ;; check we are running in an Actor behavior
  (actors/base:add-evq actors/base:*evt-queue* (cons actor msg)))

(defmacro %send* (actor &rest msg)
  `(apply #'%send ,actor ,@msg))

(defun %repeat-send (actor)
  (%send* actor actors/base:*whole-message*))

(defun %send-combined-msg (cust msg1 msg2)
  (multiple-value-call #'%send cust (values-list msg1) (values-list msg2)))
  
(defmacro behavior (&body body)
  `(macrolet ((send (actor &rest msg)
                `(%send ,actor ,@msg))
              (send* (actor &rest msg)
                `(%send* ,actor ,@msg))
              (repeat-send (actor)
                `(%repeat-send ,actor))
              (send-combined-msg (cust msg1 msg2)
                `(%send-combined-msg ,cust ,msg1 ,msg2))
              (using-become ((&optional where) &body body)
                `(%using-become ,where ,@body)))
     ,@body))

(defmacro def-beh (name args &body body)
  `(defun ,name ,args
     (behavior ,@body)))

;; --------------------------------------
;; ACTOR in function position acts like a higher level LAMBDA expression

(defmacro actor (args &body body)
  `(make-actor
    (lambda* ,args
      (behavior
        ,@body))))

#+:LISPWORKS
(editor:setup-indent "actor" 1)

;; ------------------------------------
;; ACTORS macro allows for defining new Actors which recursively
;; reference each other in their initial state. Like LETREC, but one
;; more layer of indirection here.

(defmacro actors (bindings &body body)
  ;; Binding values should be behavior closures
  `(let ,(mapcar #`(,(car a1) (make-actor)) bindings)
     ,@(mapcar #`(setf (actor-beh ,(car a1)) ,(cadr a1)) bindings)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "actors" 1)

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
        (values (subseq proper-list 0 pos)
                (subseq proper-list (1+ pos)))
        ))))

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
                    (once
                     (in-this-sponsor
                      (make-actor (beta-beh ,@params))))))
           (macrolet ((beta (&rest args)
                        `(beta-gen ,@args)))
             ;; this beta binding lasts only for the next form
             ,form))
      ;; else
      `(let ((beta  (once
                     (in-this-sponsor
                      (actor ,args ,@body)))))
         ,form)
      )))

#+:LISPWORKS
(editor:indent-like "beta" 'destructuring-bind)

(defmacro with-worker (&body body)
  `(beta _
       (send beta)
     ,@body))

;; ------------------------------------------------------
;; ALAMBDA -- a behavior lambda for Actors with pattern matching on
;; messages

(defmacro list-match (lst &rest clauses)
  `(optima:match ,lst
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (pat . body)
                     clause
                   (if (consp pat)
                       (multiple-value-bind (elts list-kind)
                           (parse-list-pat pat)
                         `((,list-kind ,@elts) ,@body))
                     `((list* ,pat) ,@body))
                   ))
               clauses)))

#+:LISPWORKS
(editor:setup-indent "list-match" 1)
  
(defmacro alambda (&rest clauses)
  (lw:with-unique-names (msg)
    `(lambda (&rest ,msg)
       (list-match ,msg ,@clauses))
    ))

;; ------------------------------------------------------

(defmacro ret (&rest ans)
  `(send γ ,@ans))

(defmacro ret* (&rest ans)
  `(send* γ ,@ans))

(defmacro γlambda (&rest clauses)
  (lw:with-unique-names (msg)
    `(lambda (γ &rest ,msg) ;; use RET or RET* instead of SEND CUST
       (list-match ,msg ,@clauses))
    ))

(defmacro γactor (args &body body)
  ;; use RET or RET* instead of SEND CUST
  `(make-actor
    (lambda* (γ ,@(if (consp args)
                      args
                    `(&rest ,args)))
      ,@body)))

;; ----------------------------------------------------
;; Like NLET for functions, only this defines a locally named Actor
;; and then SENDs to it immediately.

(defmacro actor-nlet (name bindings &body body)
  `(let (,name)
     (setf ,name (actor ,(mapcar #'first bindings)
                   ,@body))
     (send ,name ,@(mapcar #'second bindings))))

#+:LISPWORKS
(editor:indent-like "actor-nlet" 'nlet)

;; ----------------------------------------------------

#|
(behavior
  (using-become ()
    (become (make-beh 15))))
|#
