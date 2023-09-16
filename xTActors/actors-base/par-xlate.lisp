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

(defmethod service ((ac service) &rest args)
  ac)

(defmethod service ((ac actor) &rest args)
  (create-service
   (lambda (cust)
     (send* ac cust args))))

(defmethod service ((fn function) &rest args)
  (create-service
   (lambda (cust)
     (send cust (apply fn args)))))

(defmethod service (x &rest args)
  (apply #'const x args))

(def-actor null-service
  (create-service
   (lambda (cust)
     (send cust))))

;; ------------------------------------------------

(defun const-beh (&rest msg)
  (lambda* (cust . _)
    (send* cust msg)))

(defun const (&rest msg)
  (create-service (apply #'const-beh msg)))

(def-actor true  (const t))
(def-actor false (const nil))

;; ---------------------------------------------------
;; Fork/Join against zero or more services

(defun join2-beh (cust tag1)
  (alambda
   ((tag . ans) when (eql tag tag1)
    (become (lambda (tag &rest ans2)
              (declare (ignore tag))
              (send* cust (append ans ans2)))))
   ((_ . ans)
    (become (lambda (tag &rest ans1)
              (declare (ignore tag))
              (send* cust (append ans1 ans)))))
   ))

(defun fork2 (service1 service2)
  ;; Produce a single service which fires both in parallel and sends
  ;; their results in the same order to eventual customer.
  (create-service
   (lambda (cust)
     (actors ((tag1   (tag-beh joiner))
              (tag2   (tag-beh joiner))
              (joiner (join2-beh cust tag1)))
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
  (cond ((cdr bindings) ;; more than one
         `(β ,(mapcar #'car bindings)
              (send (fork ,@(mapcar #'cadr bindings)) β)
            ,@body) )
        
        (bindings ;; only one
         `(β (,@(um:mklist (caar bindings)) )
              (send ,(cadar bindings) β)
            ,@body) )
        
        (t  ;; none
         `(progn
            ,@body) )
        ))

#+:LISPWORKS
(progn
  (editor:setup-indent "let-β"  1)
  (editor:setup-indent "let*-β" 1))

(defmacro let*-β (bindings &body body)
  ;; bindings should be to services
  (if bindings
      `(let-β (,(car bindings))
         (let*-β ,(cdr bindings)
           ,@body))
    ;; else
    `(progn
       ,@body) ))

;; -----------------------------------------------

(defun prog1-β (&rest services)
  ;; Produce an Actor Service that performs a sequence of Services and
  ;; sends the result of the first Service to the Service customer.
  ;; The sequence is performed serially and in-order specified.
  (cond ((endp services)
         false)
        ((endp (cdr services))
         (car services))
        (t
         (labels ((beh1 (cust)
                    ;; using a private continuation Actor keeps us
                    ;; from becoming a 1-use only Service.
                    (let ((cont (create #'beh2)))
                      (β (ans)
                          (send (car services) β)
                        (send cont cust ans (cdr services)))
                      ))
                  (beh2 (cust ans services)
                    (if (endp services)
                        (send cust ans)
                      (let ((me self))
                        (β _
                            (send (car services) β)
                          (send me cust ans (cdr services)))
                        ))))
           (create-service #'beh1)))
        ))
                     
(defun progn-β (&rest services)
  ;; Produce an Actor Service that performs a sequence of Services
  ;; and sends the result of the last Service to the Service customer.
  ;; The sequence is performed serially and in-order specified.
  (cond ((endp services)
         false)
        ((endp (cdr services))
         (car services))
        (t
         (labels ((beh1 (cust)
                    ;; using a private continuation Actor keeps us
                    ;; from becoming a 1-use only Service.
                    (let ((cont (create #'beh2)))
                      (β _
                          (send (car services) β)
                        (send cont cust (cdr services)))
                      ))
                  (beh2 (cust services)
                    (if (endp (cdr services))
                        (send (car services) cust)
                      (let ((me self))
                        (β _
                            (send (car services) β)
                          (send me cust (cdr services))))
                      )))
           (create-service #'beh1)))
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

(defun or2 (service1 service2)
  (create-service
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
  (create-service
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

