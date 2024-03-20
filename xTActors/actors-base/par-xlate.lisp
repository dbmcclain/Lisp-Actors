;; par-xlate.lisp -- Conversion of Call/Return into Parallel Actors
;;
;; DM/RAL  2022/10/22 05:51:07
;; ----------------------------------

(defpackage #:com.ral.actors.par-xlate
  (:use #:common-lisp #:actors))

(in-package #:com.ral.actors.par-xlate)

;; ----------------------------------
;; General translations of imperative Call/Return code into Actors
;; Parallel code.
;;
;; We make use of SERVICE and β forms. A SERVICE Actor is an Actor
;; that expects only a customer in a message, performs its service,
;; and sends its result onward to that customer. The result should
;; be a single value. β-forms allow for destructuring args.
;;
;; Anything can be converted into a Service. For existing Actors which
;; expect a customer in their messages, simply state their name and
;; message form, eliding the customer argument:
;;
;;   If the send form is:
;;     (SEND Actor cust e1 e2 ...)
;;
;;   then their service form is:
;;     (SERVICE Actor e1 e2 ...) ;; note elided cust
;;
;; The convention with Actors is that the cust arg is always in first
;; position in any message.
;;
;;
;;  For functions whose call form is:
;;     (fn e1 e2 ...)
;;
;;  their service form is:
;;     (SERVICE #'fn e1 e2 ...)
;;
;;
;;  And for general value forms:
;;     expr
;;
;;  their service form is:
;;     (SERVICE expr)
;; 
;; ---------------------------
;; A β form is an Actor-continuation style, where the syntax of the β
;; form mimics the syntax of a DESTRUCTURING-BIND form:
;;
;;   (DESTRUCTURING-BIND (n1 n2 ...)
;;      (funcall fn e1 e2 ...)
;;    body)
;; =>
;;   (β (n1 n2 ...)
;;      (SEND Actor cust e1 e2 ...)
;;     body)
;;
;; Note that arg lists can be general trees, just like for
;; DESTRUCTURING-BIND.
;;
;; The dynamics of the β form are that you are in execution #1 into
;; the SEND, and in execution #2 (the continuation) in the body, with
;; the bindings (n1 n2 ...) having been established on resumption of
;; the continuation for the body form.
;;
;; The β form is just a convenient representation for the equivalent
;; form:
;;
;;  (let ((β  (CREATE (LAMBDA (n1 n2 ...) body))))
;;     (SEND Actor β e1 e2 ...)
;;
;; Just be on guard - while you can refer to SELF in the outer parts
;; of Execution #1, the value of SELF later refers to the anonymous
;; continuation Actor inside the body form of the continuation. So if
;; you need to send to the outer SELF Actor, be sure to capture it in
;; a lexical binding:
;;
;;   (let ((ME SELF))
;;     (β  (...)
;;        (SEND Actor β .. SELF ...)
;;       (body-form ... ME ...)))
;;       
;; ---------------------------------------------
;; Imperative Conversions to Parallel Forms:
;;
;; Just be advised - since Lisp does not offer CALL/RETURN
;; Continuations, as with CALL/CC, you cannot peform β conversions in
;; mid-arguments stream. You must perform them prior to use of thier
;; resulting bindings.
;;
;;  That is, you cannot hope to do something like:
;;
;;    (funcall e1 e2 (β-form e3) e4 e5)
;;
;;  Doing that would require that the Lisp compiler convert the
;;  expression to CALL/RETURN CPS-style, moving the funcall to after
;;  the argument eval with the β-form. Lisp cannot do this.
;;
;;   So, instead, you must do:
;;
;;     (β-form (n3)
;;            (SEND e3 β)
;;        (funcall e1 e2 n3 e4 e5))
;;
;; ---------------------------------------------------
;; (funcall fn e1 e2 ...)
;; =>
;; (β (v1 v2 ...)
;;     (send (fork (service ,@e1)
;;                 (service ,@e2)
;;                 ...)
;;           β)
;;   (send fn cust v1 v2 ...))

;; -----------------

;; (begin
;;  e1
;;  e2
;;  ...
;;  en)
;; =>
;; (β _
;;     (send (service ,@e1) β)
;;   (β _
;;       (send (service ,@e2) β)
;;     ...
;;     (β (vn)
;;         (send (service ,@en) β)
;;       (send cust vn))))

;; ---------------

;; (if e1
;;     e2
;;   e3)
;; =>
;; (β (v1)
;;     (send (service ,@e1) β)
;;   (if v1
;;       (send (service ,@e2) cust)
;;     (send (service ,@e3) cust)))
;; ---------------------------

;; (lambda args . body)
;; => ;; for Services
;; (create (lambda (cust ,@args)
;;           ,@body))
;; => ;; for Sinks
;; (create (lambda ,args ,@body))

;; ----------------------------

;; (and e1 e2 .. en)
;; =>
;; (send (and-β
;;        (service ,@e1)
;;        (service ,@e2)
;;        ...
;;        (service ,@en))
;;       cust)

;; ----------------------------

;; (or e1 e2 .. en)
;; =>
;; (send (or-β
;;        (service ,@e1)
;;        (service ,@e2)
;;        ...
;;        (service ,@en))
;;       cust)
;; ------------------------------
;; (let ((n1 e1)
;;       (n2 e2)
;;       ..
;;       (nn en))
;;   eb1
;;   eb2
;;   ...
;;   ebn)
;; =>
;; (β  (n1 n2 .. nn)
;;     (send (fork (service ,@e1)
;;                 (service ,@e2)
;;                 ...
;;                 (service ,@en))
;;           β)
;;   (β (_ _ .. vbn)
;;       (send (fork (service ,@eb1)
;;                   (service ,@eb2)
;;                   ...
;;                   (service ,@ebn))
;;             β)
;;     (send cust vbn)))
;; ------------------------------
;; (let* ((n1 e1)
;;        (n2 e2)
;;        ..
;;        (nn en))
;;   eb1
;;   eb2
;;   ...
;;   ebn)
;; =>
;; (β  (n1)
;;     (send (service ,@e1) β)
;;   (β (n2)
;;       (send (service ,@e2) β)
;;     ...
;;     (β (nn)
;;         (send (service ,@en) β)
;;       (β (_ _ .. vbn)
;;           (send (fork (service ,eb1)
;;                       (service ,eb2)
;;                       ...
;;                       (service ,@ebn))
;;                 β)
;;         (send cust vbn)))))
;;
;; -----------------------------------------------

(defgeneric service (x &rest args)
  ;; Accept an Actor, Function, or Item, along with other args.
  ;; Produce a Service Actor that expects a message containing an
  ;; eventual customer.
  ;;
  ;; If x is an Actor, then we get an Actor that expects a customer in
  ;; a message, then forwards that customer and the construction args
  ;; to x.
  ;;
  ;; If x is a Function, the we get an Actor that expects at least a
  ;; customer in a message, applies the function to the construction
  ;; args, and sends the result to that customer Actor.
  ;;
  ;; If x is anything else, say a number, the we get an Actor that
  ;; expects a customer in a message, and then sends x and
  ;; construction args to that customer.
  ;;
  (:method ((ac actor) &rest args)
   (if args
       (apply #'racurry ac args)
     ac))
  (:method ((fn function) &rest args)
   (create
    (alambda
     ((cust . _)
      (send* cust (multiple-value-list
                   (apply fn args))))
     )))
  (:method (x &rest args)
   (apply #'const x args)))

;; ----------------------------------------

(deflex null-service
  ;; A Service Actor that expects at least a customer in a message,
  ;; then perform a bare SEND (no message args) to that customer.
  (create
   (alambda
    ((cust . _)
     (send cust))
    )))

(deflex echo-service
  ;; A Service Actor that simply sends back to the customer in the
  ;; message, the rest of the message. Like an on-the-fly FWD.
  (create #'send))
     
;; ------------------------------------------------

(defun const-beh (&rest msg)
  (alambda
   ((cust . _)
    (send* cust msg))
   ))

(defun const (&rest msg)
  (create (apply #'const-beh msg)))

(deflex true
  (const t))

(deflex false
  (const nil))

;; ---------------------------------------------------
;; Fork/Join against zero or more services

#|
(defun join2 (cust tag1)
  ;; Wait for two answers and then forward them in proper order to the
  ;; customer.
  (create
   (lambda* (tag . ans1)
     (become (lambda* (_ . ans2)
               (become-sink)
               (send* cust (if (eq tag tag1)
                               (append ans1 ans2)
                             (append ans2 ans1)) )))
     )))

(defun fork2 (service1 service2)
  ;; Produce a single service which fires both services in parallel
  ;; and sends their results in the same order to customer.
  (create
   (lambda (cust)
     (actors ((tag1   (tag joiner))
              (tag2   (tag joiner))
              (joiner (join2 cust tag1)))
       (send service1 tag1)
       (send service2 tag2)
       ))
   ))

(defun abstract-forker (svc-fn &rest args)
  ;; Abstraction of FORK -- svc-fn should accept a specialization arg and
  ;; return a service Actor that expects a customer as it message.
  (or (reduce (lambda (arg tail)
                (fork2 (funcall svc-fn arg) tail))
              (butlast args)
              :initial-value (apply svc-fn (last args))
              :from-end t)
      null-service))
|#

(defun abstract-forker (svc-fn &rest args)
  (if args
      (let ((svcs   (mapcar svc-fn args))
            (no-ans (vector :no-answer))) ;; globally unique value
        (assert (every #'actor-p svcs))
        (create
         (alambda
          ((cust)
           (loop for ix from 0
                 for svc in svcs
                 do
                   (send svc (label self ix)))
           (labels ((beh (ansv)
                      (alambda
                       ((ix . ans) / (eq no-ans (aref ansv ix))
                        ;; guard against repeated replies
                        (let ((new-ansv (copy-seq ansv)))
                          (setf (aref new-ansv ix) (car ans))
                          (become (beh new-ansv))
                          (unless (find no-ans new-ansv)
                            (send* cust (coerce new-ansv 'list)))
                          ))
                       )))
             (become (beh (make-array (length svcs)
                                      :initial-element no-ans)))
             ))
          )))
    ;; else
    null-service))


(defun fork (&rest services)
  ;; Produces a single service from a collection of them. Will exec
  ;; each in parallel, sending accumulated results to eventual
  ;; customer, in the same order as stated in the service list.
  ;;
  ;; All services in the list must send a result to their customer.
  ;;
  (apply #'abstract-forker #'identity services))

;;
;; We get for (FORK A B C):
#|
                              +----+                                              +----------+             
                          +-->| A  |--------------------------------------------->|          |             
                          |   +----+                                              |  JOIN2   |--- cust --->
            +----------+  |                                                  +--->|          |             
            |          |--+                                                  |    +----------+             
 -- cust -->|  FORK2   |                        +----+                       |                             
            |          |--+                 +-->| B  |---+                   |                             
            +----------+  |   +----------+  |   +----+   |    +----------+   |                             
                          |   |          |--+            +--->|          |   |                             
                          +-->|  FORK2   |                    |  JOIN2   |---+                             
                              |          |--+            +--->|          |                                 
                              +----------+  |   +----+   |    +----------+                                 
                                            +-->| C  |---+                                                 
                                                +----+

|# 
;; ----------------------------------------------------------------------------------
;; PAR-MAP -- produce a service Actor that forks a bunch of Actors to
;; perform some action against each from a list of args.
;;
;; (Recall that a Service is an Actor that expects a customer as its
;; only message arg. Any specialization args are pre-cooked into its
;; behavior closure.)

(defun par-map (action &rest args)
  ;; Produces a single service performing ACTION on each of a
  ;; collection of args. Will exec each in parallel, sending
  ;; accumulated results to eventual customer, in the same order as
  ;; stated in the args list.
  ;;
  ;; Action must be an Actor that always sends a result to its
  ;; customer. Action must accept a customer and an arg. (I.e., Action
  ;; is not a Service, but gets turned into one by way of SERVICE and
  ;; Actor-Currying.)
  ;;
  (apply #'abstract-forker (um:curry #'service action) args))

;; ------------------------------------------------

(defun condenser (cust)
  ;; convert a cust Actor, which expects a single list arg, into an
  ;; Actor that accepts many args.
  (create
   (lambda (&rest args)
     (send cust args))
   ))

(defun spreader (cust)
  ;; convert a cust Actor, which expects many args, into an Actor
  ;; which can accept a single list arg.
  (create
   (alambda
    ((arg-list) 
     (send* cust (um:mklist arg-list)))
    )))

(deflex map-reduce
  ;; MAP-REDUCE - an Actor that accepts a customer, an action Actor, a
  ;; filtering function, and any number of args.
  ;;
  ;; Fork the action across all the args, and filter the accumulated
  ;; answers before sending on to customer.
  ;;
  ;; Action is an Actor that expects a customer and an arg. Action
  ;; should always send a result to its customer.
  ;;
  ;; Customer should accept any number of args. Use a CONDENSER on the
  ;; cust if it only accepts a single list result.
  (create
   (alambda
    ((cust action filter-fn . args)
     (send (apply #'par-map action args)
           (create (lambda (&rest ans)
                     (send* cust (um:collect-if filter-fn ans)))
                   )))
    )))

;; ------------------------------------------------------------------
;; Extend LET+ into β-forms

(defmethod do-let+ ((fst (eql :β)) binding form)
  (destructuring-bind (list-form verb-form) (rest binding)  
    `(β ,list-form
         (send ,verb-form β)
       ,@(um:maybe-ignore_ list-form)
       ,form)
    ))

(defmethod do-let+ ((fst (eql :beta)) binding form)
  (do-let+ :β binding form))


;; -----------------------------------------------

(defun prog1-β (&rest services)
  ;; Produce an Actor Service that performs a sequence of Services and
  ;; sends the result of the first Service to the customer after
  ;; completing the remaining Services.  The sequence is performed
  ;; serially and in-order specified.
  (cond ((endp services)
         false)
        ((endp (cdr services))
         (car services))
        (t
         (um:letrec ((iter (create
                            (lambda (cust svcs ans)
                              (if (endp svcs)
                                  (send* cust ans)
                                (β _
                                    (send (car svcs) β)
                                  (send iter cust (cdr svcs) ans))
                                )))))
           (create
            (lambda (cust)
              (β ans
                  (send (car services) β)
                (send iter cust (cdr services) ans) ) ))
           ))
        ))
                     
(defun progn-β (&rest services)
  ;; Produce an Actor Service that performs a sequence of Services
  ;; and sends the result of the last Service to the customer.
  ;; The sequence is performed serially and in-order specified.
  (cond ((endp services)
         false)
        ((endp (cdr services))
         (car services))
        (t
         (um:letrec ((iter (create
                            (lambda (cust svcs)
                              (if (cdr svcs)
                                  (β _
                                      (send (car svcs) β)
                                    (send iter cust (cdr svcs)))
                                (send (car svcs) cust))
                              ))))
           (create
            (lambda (cust)
              (send iter cust services)))
           ))
        ))

#|
  Use as:

  (β (ans)
      (send (progn-β (service ...)
                     (service ...)
                     (service ...))
            β)
    ...)
 |#
;; -----------------------------------------------
;; So now... when it comes to AND and OR as short-circuit operations
;; against Actors, we have two ways to go here:
;;
;; We can perform each service serially in the order stated and look
;; for a short-circuit send back to customer. This corresponds to the
;; Lisp AND and OR operators.
;;
;; Or we can perform all services in parallel and accept a
;; short-circuit from the first to respond appropriately.

;; -----------------------------------------------------
;; Serial Forms

(defun ser-and/or (test-fn zero-act services)
  (if services
      (let+ (( (svc . svcs) services))
        (if svcs
            (create
             (alambda
              ((cust)
               (β (ans . anss)
                   (send svc β)
                 (if (funcall test-fn ans)
                     (send* cust ans anss)
                   (send (ser-and/or test-fn zero-act svcs) cust))))
              ))
          ;; else
          svc))
    ;; else
    zero-act))

(defun or-ser-β (&rest services)
  ;; Construct a service Actor that performs in-order messaging of
  ;; services until one of them replies non-nil, then send reply to
  ;; cust.
  (ser-and/or #'identity false services))

(defun and-ser-β (&rest services)
  ;; Construct a service Actor that performs in-order messaging of
  ;; services until one of them replies nil. If that happens, send nil
  ;; to the service customer. Else, send the final reply to customer.
  (ser-and/or #'not true services))

;; -----------------------------------------------------
;; Parallel Forms

(defun par-and/or (test-fn final-ans zero-act services)
  (if services
      (if (cdr services)
          (create
           (alambda
            ((cust)
             (labels ((beh (tags)
                        (alambda
                         ((tag ans . anss) / (find tag tags)
                          (if (funcall test-fn ans)
                              (progn
                                (become-sink)
                                (send* cust ans anss))
                            ;; else
                            (let ((new-tags (remove tag tags)))
                              (become (beh new-tags))
                              (unless new-tags
                                (send cust final-ans)))
                            ))
                         )))
               (let ((tags (mapcar (lambda (svc)
                                     (let ((tag  (tag self)))
                                       (send svc tag)
                                       tag))
                                   services)))
                 (become (beh tags)))
               ))
            ))
        ;; else
        (car services))
    ;; else
    zero-act))

(defun or-par-β (&rest services)
  ;; Construct a service Actor that launches all services in parallel.
  ;; The first to reply non-nil has its full reply sent to customer.
  (par-and/or #'identity nil false services))

(defun and-par-β (&rest services)
  ;; Construct a service Actor that launches all services in parallel.
  ;; The first to reply nil has its full reply sent to customer.
  (par-and/or #'not t true services))

;; -----------------------------------------------

(defmacro with-and-β ((ans &rest clauses) &body body)
  `(β (,ans)
       (send (and-ser-β ,@clauses) β)
     ,@body))

(defmacro with-or-β ((ans &rest clauses) &body body)
  `(β (,ans)
       (send (or-ser-β ,@clauses) β)
     ,@body))

;; -----------------------------------------------

(defmacro if-β (test iftrue &optional iffalse)
  (let ((ans (gensym (string :ans))))
    `(with-and-β (,ans ,test)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro when-β (test &body body)
  (let ((ans (gensym (string :ans))))
    `(with-and-β (,ans ,test)
       (when ,ans
         ,@body))))

(defmacro unless-β (test &body body)
  (let ((ans (gensym (string :ans))))
    `(with-and-β (,ans ,test)
       (unless ,ans
         ,@body))))

;; -----------------------------------------------

(defmacro if-and-β ((&rest clauses) iftrue &optional iffalse)
  (let ((ans (gensym (string :ans))))
    `(with-and-β (,ans ,@clauses)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro if-or-β ((&rest clauses) iftrue &optional iffalse)
  (let ((ans (gensym (string :ans))))
    `(with-or-β (,ans ,@clauses)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro when-and-β ((ans &rest clauses) &body body)
  `(if-and-β (,ans ,@clauses) (progn ,@body)))

(defmacro when-or-β ((ans &rest clauses) &body body)
  `(if-or-β (,ans ,@clauses) (progn ,@body)))

(defmacro unless-and-β ((ans &rest clauses) &body body)
  `(if-and-β (,ans ,@clauses) 'nil (progn ,@body)))

(defmacro unless-or-β ((ans &rest clauses) &body body)
  `(if-or-β (,ans ,@clauses) 'nil (progn ,@body)))

#+:LISPWORKS
(progn
  (editor:setup-indent "with-and-β" 1)
  (editor:setup-indent "with-or-β"  1)
  (editor:indent-like 'if-β     'if)
  (editor:indent-like 'if-and-β 'if)
  (editor:indent-like 'if-or-β  'if))

;; -----------------------------------------------
;; Experience shows that, while you can generally convert imperative
;; CALL/RETURN style into Parallel Actors form, you cannot have the
;; ideal of "everything" is an Actor when feeding Actors as data items
;; into existing imperative CALL/RETURN code.
;;
;; As an illustration, the RB-Tree (imperative) code has numerous
;; comparisons of key forms when reconstructing trees after
;; insertions. They use calls like:
;;
;;     (IF (PLUSP (ORD:COMPARE A B))
;;        ...)
;;
;; So, if A or B were data Actors, who need to be queried as to their
;; current sort-value, then this is impossible. Lisp's lack of CALL/CC
;; means that we need to extract the sort-value before entering the
;; (IF (ORD:COMPARE ...)).
;;
;; So conversion to data Actors requires a total rewrite of any client
;; imperative code. Using these macros makes that easier than not. But
;; it must still be done. Data-Actors requires Parallel Actors code.
;; While Parallel Actors code can work with either Data-Actors or
;; conventional data structs.

