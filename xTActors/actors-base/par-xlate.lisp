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
;; (β (_ _ ... vn)
;;     (send (fork (service ,@e1)
;;                 (service ,@e2)
;;                 ...
;;                 (service ,@en))
;;           β)
;;   (send cust vn))

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

(defun const-beh (&rest msg)
  (lambda (cust)
    (send* cust msg)))

(defun const (&rest msg)
  (create (apply #'const-beh msg)))

;; ---------------------------------------------------
;; Service -- offer up a parameterized service once the customer is
;; known

(defun service (server &rest args)
  (cond ((actor-p server)
         (create
          (lambda (cust)
            (send* server cust args))))
        ((functionp server)
         (create
          (lambda (cust)
            (send cust (apply server args)))))
        (t
         (const server))
        ))

(deflex null-service
  (create
   (lambda (cust)
     (send cust))))

;; ---------------------------------------------------
;; Fork/Join against an arbitrary number of services

(defun join2 (cust tag1)
  (create
   (alambda
    ((tag . ans) when (eql tag tag1)
     (become (lambda (tag &rest ans2)
               (declare (ignore tag))
               (send* cust (append ans ans2)))))
    ((_ . ans)
     (become (lambda (tag &rest ans1)
               (declare (ignore tag))
               (send* cust (append ans1 ans)))))
    )))

(defun fork2 (service1 service2)
  ;; Produce a single services which fires both in parallel and sends
  ;; their results in the same order to eventual customer.
  (create
   (lambda (cust)
     (actors ((tag1   (tag joiner))
              (tag2   (tag joiner))
              (joiner (join2 cust tag1)))
       (send service1 tag1)
       (send service2 tag2)
       ))))

(defun fork (&rest services)
  ;; Produces a single service from a collection of them. Will exec
  ;; each in parallel, returning all of their results to eventual
  ;; customer, in the same order as stated in the service list.
  (or (reduce (lambda (svc tail)
                (fork2 svc tail))
              (butlast services)
              :initial-value (car (last services))
              :from-end t)
      null-service))
;;
;; We get for (FORK A B C):
;;
;;                    +---+
;;                    | A |
;;                    +---+     +---+
;;         +------+  /          | B |
;;      -->| FORK |/            +---+
;;         +------+\           /
;;                   \+------+/
;;                    | FORK |
;;                    +------+\
;;                             \
;;                              +---+
;;                              | C |
;;                              +---+
;;
;; -----------------------------------------------

(defmacro let-β (bindings &body body)
  ;; bindings should be to services as in:
  ;; 
  ;;   (let-β ((n1 (service ,@e1))
  ;;           (n2 (service ,@e2))
  ;;           ... )
  ;;      ,@body)
  ;;
  ;; FORK works properly for zero or more services.
  ;;
  `(β ,(mapcar #'car bindings)
       (send (fork ,@(mapcar #'cadr bindings)) β)
     ,@body))

#+:LISPWORKS
(progn
  (editor:setup-indent "let-β"  1)
  (editor:setup-indent "let-β*" 1))

(defmacro let-β* (bindings &body body)
  ;; bindings should be to services
  (if bindings
      `(let-β (,(car bindings))
         (let-β* ,(cdr bindings) ,@body))
    `(progn
       ,@body)))

;; -----------------------------------------------

(defmacro prog1-β (first-val services &body body)
  (um:with-unique-names (ns)
    `(β ,ns
         (send (fork ,@services) β)
       (let ((,first-val (car ,ns)))
         ,@body))))

(defmacro progn-β (final-val services &body body)
  (um:with-unique-names (ns)
    `(β ,ns
         (send (fork ,@services) β)
       (let ((,final-val (car (last ,ns))))
         ,@body))))

;; -----------------------------------------------

(deflex true  (const t))
(deflex false (const nil))

(defun or2 (service1 service2)
  (create
   (lambda (cust)
     (β (ans)
         (send service1 β)
       (if ans
           (send cust ans)
         (send service2 cust))))
   ))

(defun or-β (&rest services)
  (if services
      (reduce (lambda (head svc)
                (or2 head svc))
              services)
    false))
        
;;
;; We get for (OR-β A B C):
;;
;;                          +---+
;;                          | A |
;;                          +---+
;;               +------+  /
;;               |  OR  |/
;;               +------+\ 
;;                  /      \+---+
;;                 /        | B |
;;                /         +---+
;;      +------+ /
;;   -->|  OR  |/
;;      +------+\
;;               \+---+
;;                | C |
;;                +---+
;;
;; -----------------------------------------------

(defun and2 (service1 service2)
  (create
   (lambda (cust)
     (β (ans)
         (send service1 β)
       (if ans
           (send service2 cust)
         (send cust nil))))
   ))
   
(defun and-β (&rest services)
  (if services
      (reduce (lambda (head svc)
                (and2 head svc))
              services)
    true))

;;
;; We get for (AND-β A B C):
;;
;;                          +---+
;;                          | A |
;;                          +---+
;;               +------+  /
;;               |  AND |/
;;               +------+\ 
;;                  /      \+---+
;;                 /        | B |
;;                /         +---+
;;      +------+ /
;;   -->|  AND |/
;;      +------+\
;;               \+---+
;;                | C |
;;                +---+
;;
;; -----------------------------------------------

(defmacro with-and-β ((ans &rest clauses) &body body)
  `(β (,ans)
       (send (and-β ,@clauses) β)
     ,@body))

(defmacro with-or-β ((ans &rest clauses) &body body)
  `(β (,ans)
       (send (or-β ,@clauses) β)
     ,@body))

;; -----------------------------------------------

(defmacro if-β (test iftrue &optional iffalse)
  (lw:with-unique-names (ans)
    `(with-and-β (,ans ,test)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro when-β (test &body body)
  (lw:with-unique-names (ans)
    `(with-and-β (,ans ,test)
       (when ,ans
         ,@body))))

(defmacro unless-β (test &body body)
  (lw:with-unique-names (ans)
    `(with-and-β (,ans ,test)
       (unless ,ans
         ,@body))))

;; -----------------------------------------------

(defmacro if-and-β ((&rest clauses) iftrue &optional iffalse)
  (lw:with-unique-names (ans)
    `(with-and-β (,ans ,@clauses)
       (if ,ans
           ,iftrue
         ,iffalse))
    ))

(defmacro if-or-β ((&rest clauses) iftrue &optional iffalse)
  (lw:with-unique-names (ans)
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
