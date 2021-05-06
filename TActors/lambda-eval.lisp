;; lambda-eval.lisp -- Lambda Calculus Evaluator
;;
;; Translated from Humus, by Dale Shumacher, at:
;; http://www.dalnefre.com/wp/2010/08/evaluating-expressions-part-1-core-lambda-calculus/
;;
;; DM/RAL 05/21
;; ------------------------------------------------

(defpackage #:lambda-eval
  (:use #:common-lisp #:actors)
  (:nicknames #:le)
  (:export
   ))

;; ------------------------------------
(in-package #:lambda-eval)
;; ------------------------------------
;; Chapter 1 - Core Lambda Calculus

(defun const-expr-beh (val)
  (lambda (cust op _)
    (declare (ignore _))
    (when (eq op :eval)
      (send cust val))
    ))

(defun $const (val)
  (make-actor (const-expr-beh val)))

;; --------------------------

(defun ident-expr-beh (id)
  (lambda (cust op env)
    (when (eq op :eval)
      (send env cust id))
    ))

(defun $ident (id)
  (make-actor (ident-expr-beh id)))

;; --------------------------

(defvar ? (actor _))

(defvar *empty-env*
  (actor (cust &rest _)
    (declare (ignore _))
    (send cust ?)))

(defun empty-env ()
  *empty-env*)

(defun env-beh (id val next)
  (lambda (cust req)
    (cond ((eq id req)
           (send cust val))
          (t
           (repeat-send next))
          )))

;; --------------------------

(defun closure-beh (id body env)
  (lambda (cust op arg)
    (when (eq op :apply)
      (let ((envx (make-actor (env-beh id arg env))))
        (send body cust :eval envx))
      )))

(defun abs-expr-beh (id expr)
  (lambda (cust op env)
    (when (eq op :eval)
      (send cust (make-actor (closure-beh id expr env))))
    ))

(defun $lam (id expr)
  (make-actor (abs-expr-beh id expr)))

;; --------------------------

(defun app-expr-beh (abs-expr arg-expr)
  (lambda (cust op env)
    (when (eq op :eval)
      (let ((k-abs (actor (abs)
                     (let ((k-arg (actor (arg)
                                    (send abs cust :apply arg))))
                       (send arg-expr k-arg :eval env)))
                   ))
        (send abs-expr k-abs :eval env))
      )))

(defun $apply (abs-expr arg-expr)
  (make-actor (app-expr-beh abs-expr arg-expr)))

#|
;; (\x.x)(42)
(let ((example ($apply
                ($lam 'x ($ident 'x))
                ($const 42))
               ))
  (send example (println) :eval *empty-env*))
|#

;; -----------------------------------------------------
;; Chapter 2 - Conditional Special Form

(defun case-expr-beh (val-expr choices)
  (lambda (cust op env)
    (when (eq op :EVAL)
      (let ((k-val (actor (val)
                     (send choices cust :MATCH val env))))
        (send val-expr k-val :EVAL env)))
    ))

(defmacro $case (x &rest cases)
  `(make-actor (case-expr-beh ($IDENT ',x)
                              ,(reduce (lambda* ((pat expr) ans)
                                         `(make-actor
                                           (case-choice-beh ,pat ,expr ,ans)))
                                       cases
                                       :initial-value *case-end*
                                       :from-end      t))))

(defun case-choice-beh (pat expr next)
  (lambda (cust op val env)
    (when (eq op :MATCH)
      (let ((k-match (actor (envx)
                       (cond ((eq envx ?)
                              (send next cust op val env))
                             (t
                              (send expr cust :EVAL envx))
                             ))))
        (send pat k-match :MATCH val env)))
    ))

(defvar *case-end*
  (actor (cust op _)
    (declare (ignore _))
    (when (eq op :MATCH)
      (send cust ?))
    ))

(defun ident-pat-beh (id)
  (lambda (cust op val env)
    (if (eq op :MATCH)
        (let ((envx (make-actor (env-beh id val env))))
          (send cust envx))
      ;; else
      (send cust ?))
    ))

(defun $pat-ident (id)
  (make-actor (ident-pat-beh id)))

(defun const-pat-beh (value)
  (lambda (cust op val env)
    (if (and (eq op :MATCH)
             (eql val value))
        (send cust env)
      ;; else
      (send cust ?))
    ))

(defun $pat-const (val)
  (make-actor (const-pat-beh val)))

(defun any-pat ()
  (lambda (cust op _ env)
    (declare (ignore _))
    (if (eq op :MATCH)
        (send cust env)
      (send cust ?))
    ))

(defun $pat-wild ()
  (make-actor (any-pat)))

(defun value-pat-beh (expr)
  (lambda (cust op val env)
    (if (eq op :MATCH)
        (let ((k-val (actor (valx)
                       (if (eql val valx)
                           (send cust env)
                         (send cust ?))
                       )))
          (send expr k-val :EVAL env))
      ;; else
      (send cust ?))
    ))

(defun $pat-val (expr)
  (make-actor (value-pat-beh expr)))

(defun pat-abs-expr-beh (pat body-expr)
  (lambda (cust op env)
    (when (eq op :EVAL)
      (send cust (make-actor (pat-closure-beh pat body-expr env)))
      )))

(defun pat-closure-beh (pat body env)
  (lambda (cust op arg)
    (when (eq op :APPLY)
      (let ((k-env (actor (envx)
                     (if (eq envx ?)
                         (send cust ?)
                       (send body cust :EVAL envx))
                     )))
        (send pat k-env :MATCH arg env)))
    ))

(defun assert-eq-beh (tstval)
  (lambda (val)
    (send (println) val)
    (assert (eql val tstval))))

(defun $assert-eq (tstval)
  (make-actor (assert-eq-beh tstval)))

#|
($case x
       (($PAT-IDENT 'y) Expr1)
       (($PAT-CONST 15) Expr2)
       (($PAT-WILD)     Expr3))

(let ((global-env (make-actor
                   (env-beh 'zero?
                            (make-actor (closure-beh 'x
                                                     ($CASE x
                                                            (($PAT-CONST 0) ($CONST 'TRUE))
                                                            (($PAT-WILD)    ($CONST 'FALSE)))
                                                     *empty-env*))
                            *empty-env*))
                  ))
  (send ($apply ($IDENT 'zero?) ($CONST 0)) (println) :eval global-env))


 |#
