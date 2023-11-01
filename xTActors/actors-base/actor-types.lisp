;; actor-types.lisp
;;
;; DM/RAL  2022/10/24 16:56:15
;; ----------------------------------

(defpackage #:com.ral.actors.types
  (:use #:common-lisp #:ac))

(in-package #:com.ral.actors.types)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 2) #+:LISPWORKS (FLOAT 0)))

;; ----------------------------------

(unless (fboundp 'do-nothing)
  ;; needed for SBCL
  (defun* do-nothing _
    (values)))

;; -----------------------------------------------------
;; Actors are simply indirect refs to a beh closure (= function + state).
;;
;; Actor behavior/state can change without affecting the identity of
;; the Actor.
;;               +------+-----+
;;  Actor Ref -->| Type | Beh |
;;               +------+-----+
;;                  |      |
;;                  |      v      Closure
;;                  |    +----+-------+
;;                  v    | Fn | State |
;;             T(Actor)  +----+-------+     Bindings
;;                         |      |      +------+-----+-----+---
;;                         |      +----->| Data | ... | ... |
;;                         |             +------+-----+-----|---
;;                         |    +------+-----+-----+---
;;                         +--->| Code | ... | ... |
;;                              +------+-----+-----+---
;; ------------------------------------------------------------------
;;
;; An Actor can use any function as its behavior. But by convention, a
;; Service Actor is one which takes a message containing only a
;; customer.  We cannot enforce this convention, so we have to trust
;; the programmer.
;;
;; But Actors which comply with this convention, and which promise to
;; reply to the customer, can take advantage of some parallel
;; invocation macros, such as FORK/JOIN and others.

(defclass actor ()
  ;; CAS needs to access a CONS cell, not an instance slot
  ((beh-cons
    :accessor actor-beh-cons))
  (:metaclass
   #+:LISPWORKS clos:funcallable-standard-class
   #+:SBCL      sb-mop:funcallable-standard-class
   ))

(defmethod initialize-instance :after ((ac actor) &key (beh #'do-nothing) &allow-other-keys)
  (setf (actor-beh-cons ac) (list beh))
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:SBCL      sb-mop:set-funcallable-instance-function             
   ac
   (lambda* args
     (send* ac args))))

(defun actor (&optional (beh #'do-nothing))
  (make-instance 'actor
                 :beh (screened-beh beh)))

(defun create (&optional (beh #'do-nothing))
  (actor beh))

(defgeneric actor-p (arg)
  (:method ((arg actor)) ;; as distinct from Function
   t)
  (:method (arg)
   nil))

(defmethod actor-beh ((ac actor))
  (car (the cons (actor-beh-cons ac))))

;; ----------------------------------------------------------

(defgeneric screened-beh (arg)
  (:method ((arg function))
   ;; Catches both Actors and Functions. Actors as behavior are
   ;; self-forwarding.
   arg)
  (:method (arg)
   (error "Invalid behavior: ~S" arg)))

;; -------------------------------------------

(declaim (inline %actor-cas))

(defun %actor-cas (actor old-beh new-beh)
    (mpc:compare-and-swap (car (the cons (actor-beh-cons (the actor actor))))
                          old-beh new-beh))

;; -----------------------------------------------------

