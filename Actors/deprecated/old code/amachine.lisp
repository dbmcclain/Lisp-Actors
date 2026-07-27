;; amachine.lisp -- Define a restricted machine more suited to Actors programming.
;;
;; It may look like Lisp, and to a large degree, it still is, but
;; watch out... What looks like a function call is most likely a
;; message SEND.
;;
;; DM/RAL 12/21
;; ---------------------------------------------------------------------

(defpackage com.ral.actors.language
  (:use :cl)
  (:local-nicknames (#:ac #:com.ral.actors))
  (:shadow if quote apply cons car cdr list list* 1+ 1-)
  (:import-from :com.ral.actors
   #:self)
  (:export
   #:self
   #:actor-speak
   #:if
   #:val
   #:par
   #:send
   #:send*
   #:become
   #:create
   #:beh
   #:def
   #:def-actor
   #:actor
   #:match
   #:defprim
   #:list
   #:list*
   #:cons
   #:car
   #:cdr
   #:1+
   #:1-
   ))

(in-package :com.ral.actors.language)

;; ---------------------------------------------------------------------

(defmacro actor-speak (&rest forms)
  (wrap-progn (mapcar 'parse forms)))

(defun wrap-progn (clauses)
  (cl:if (cl:cdr clauses)
      `(progn ,@clauses)
    (cl:car clauses)))

(defvar *macs*  nil)

(defun is-mac (e)
  (and (symbolp e)
       (find e *macs*)))

(defun mac-expand (form)
  (macroexpand form))

(defvar *special-predef*
  `(lisp quote if par def macro
         create beh send send* become
         apply match val lambda
         send-after))

(defun parse (e)
  (cond ((atom e)
         e)
        (t
         (let ((lead (cl:car e)))
           (cond ((is-mac lead)
                  (parse (mac-expand e)))
                 ((member lead *special-predef*)
                  e)
                 (t (parse-form e))
                 )))
        ))

(defun parse-to-list-of-factors (e)
  (let (terms)
    (flet ((winnow (ee)
             (cond ((atom ee)
                    ee)
                   (t
                    (let ((lead (cl:car ee)))
                      (cond ((eql 'lisp lead)
                             (cadr ee))
                            ((member lead '(create cl:lambda cl:funcall cl:apply))
                             ee)
                            (t
                             (let ((sym (gensym)))
                               (push (cl:list sym
                                              (parse `(,lead ,sym ,@(cl:cdr ee))))
                                     terms)
                               sym))
                            )))
                   )))
      (let ((body (mapcar #'winnow e)))
        (values terms body))
      )))

(defun reduce-binding-terms (terms initial)
  (reduce (lambda (ans term)
            (destructuring-bind (rx val) term
              `(let ((,rx (ac:actor (,rx)
                            ,ans)))
                 ,val)
              ))
          terms
          :initial-value initial))
  
(defun parse-form (e)
  (multiple-value-bind (terms lst)
      (parse-to-list-of-factors e)
    (reduce-binding-terms terms
                          `(ac:send ,lst))
    ))

;; ---------------------------------------------------------
;; ActorSpeak Language Elements

(defmacro if (test tform &optional fform)
  (multiple-value-bind (terms body)
      (parse-to-list-of-factors (cl:list test))
    (reduce-binding-terms terms
                          `(cl:if ,(cl:car body)
                               ,(parse tform)
                             ,@(when fform
                                 `(,(parse fform)))
                             ))))
#|
(actor () (doit tform))
(send (actor () e))
(send self e)
(actor-speak (self (1+ e)))
(actor-speak (self (lisp (cl:1+ e))))
(actor-speak (self e))

(if (1+ test) (doit tform))
(if (lisp (1+ test)) (doit tform) (doit fform))
(val ((x 15)) e)
 |#

(defmacro delayed-bindings (bindings body)
  (destructuring-bind ((sym val) . rest) bindings
    `(let ((,sym (ac:actor (,sym)
                   ,@(cl:if rest
                         `((delayed-bindings ,rest ,body))
                       (mapcar 'parse body)))))
       (ac:send ,(cl:car val) ,sym ,@(cl:cdr val)))
    ))

(defmacro val (bindings &body body)
  (let (immed delayed)
    (dolist (binding bindings)
      (cond ((consp binding)
             (multiple-value-bind (sym val)
                 (values-list binding)
               (cond ((or (atom val)
                          (eql 'create (cl:car val)))
                      (push binding immed))
                     ((eql 'lisp (cl:car val))
                      (push (cl:list sym (cadr val)) immed))
                     (t
                      (push binding delayed))
                     )))
            (t
             (push (cl:list binding) immed))
            ))
    (setf immed   (nreverse immed)
          delayed (nreverse delayed))
    (cl:if immed
        `(let ,(mapcar (lambda (binding)
                         (destructuring-bind (sym val) binding
                           (cl:if (eql 'create (cl:car val))
                              `(,sym (ac:make-actor))
                             sym)))
                       immed)
           ,@(mapcar (lambda (binding)
                       (destructuring-bind (sym val) binding
                         (cl:if (eql 'create (cl:car val))
                             `(setf (ac:actor-beh ,sym) ,(cadr val))
                           `(setf ,sym ,val))))
                     (remove-if #'null immed :key 'cadr))
           ,@(cl:if delayed
                 `((delayed-bindings ,delayed ,body))
               (mapcar 'parse body)))
      (cl:if delayed
          `(delayed-bindings ,delayed ,body)
        (mapcar 'parse body))
      )))

(defmacro par (&rest forms)
  (wrap-progn
   (mapcar 'parse forms)))

(defmacro send (actor &rest args)
  (multiple-value-bind (terms body)
      (parse-to-list-of-factors (cl:cons actor args))
    (reduce-binding-terms terms
                          `(ac:send ,@body))
    ))

(defmacro apply (actor &rest args)
  (multiple-value-bind (terms body)
      (parse-to-list-of-factors (cl:cons actor args))
    (reduce-binding-terms terms
                          `(cl:apply 'ac:send ,@body))
    ))

(defmacro send* (actor &rest args)
  `(apply ,actor ,@args))
         
(defmacro become (fn)
  (multiple-value-bind (terms body)
      (parse-to-list-of-factors fn)
    (cl:if terms
        (lw:with-unique-names (me)
          `(let ((,me  self))
             ,(reduce-binding-terms terms
                                    `(setf (ac:actor-beh ,me) ,body))
             ))
      `(ac:become ,body)
      )))

(defmacro create (beh)
  (multiple-value-bind (terms body)
      (parse-to-list-of-factors beh)
    (reduce-binding-terms terms
                          `(ac:make-actor ,body))
    ))

#|
(create (once-beh must-do))
  |#

(defmacro beh (args &body body)
  `(cl:lambda ,args ,@(mapcar 'parse body)))

(defmacro def (name params form)
  `(defun ,name ,params
     ,form)) ;; should be a BEH

(defmacro def-actor (name form)
  `(um:deflex ,name (create ,form)))

(defmacro actor (args &body body)
  `(create (beh ,args ,@body)))

(defmacro match (arg &rest clauses)
  (multiple-value-bind (terms match-arg)
      (parse-to-list-of-factors (cl:list arg))
    (reduce-binding-terms
     terms
     `(ac:match ,@match-arg
        ,@(mapcar (lambda (clause)
                    (cl:if (eql 'when (cadr clause))
                        (destructuring-bind (pat _ test . body) clause
                          (declare (ignore _))
                          `(,pat when ,test
                                 ,@(mapcar 'parse body)))
                      (destructuring-bind (pat . body) clause
                        `(,pat ,@(mapcar 'parse body)))))
                  clauses)))))

;; --------------------------------------------------
;; Primitive Actors

(defmacro defprim (name args form)
  (lw:with-unique-names (cust)
    `(um:deflex ,name
             (ac:actor (,cust ,@args)
               (ac:send ,cust ,form)))
    ))

(dolist (triple '((list  (&rest args)  args)
                  (list* (&rest args)  (cl:apply 'cl:list args))
                  (cons  (x &rest lst) (cl:cons x lst))
                  (car   (lst)         (cl:car lst))
                  (cdr   (lst)         (cl:cdr lst))
                  (1+    (x)           (cl:1+ x))
                  (1-    (x)           (cl:1- x))))
  (destructuring-bind (name args form) triple
    (eval `(defprim ,name ,args ,form))))

;; --------------------------------------------------

