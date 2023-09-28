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

(defstruct (actor
               (:constructor %create (beh)))
  (beh #'do-nothing :type function))

;; ----------------------------------------------------------

(defun fwd-beh (actor)
  (lambda (&rest msg)
    (send* actor msg)))

(defun fwd (actor)
  (create (fwd-beh actor)))

;; ----------------------------------------------------------

(defgeneric screened-beh (arg)
  (:method ((fn function))
   fn)
  (:method ((ac actor))
   ;; (actor-beh ac) ;; don't steal their beh, become a forwarding Actor
   (fwd-beh ac))
  (:method (x)
   (error "Invalid behavior: ~S" x)))

;; ----------------------------------------------------------

(defun create (&optional (fn #'do-nothing))
  (%create (screened-beh fn)))

;; -------------------------------------------

(declaim (inline %actor-cas))

(defun %actor-cas (actor old-beh new-beh)
    (mpc:compare-and-swap (actor-beh (the actor actor)) old-beh new-beh))

;; -----------------------------------------------------

