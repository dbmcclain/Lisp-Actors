;; actor-types.lisp
;;
;; DM/RAL  2022/10/24 16:56:15
;; ----------------------------------

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
  (beh  #'do-nothing))

(defun create (&optional (beh #'do-nothing))
  (%create (screened-beh beh)))

;; ----------------------------------------------------------

(defgeneric screened-beh (arg)
  #F
  (:method ((arg actor))
   (fwd-beh arg))
  (:method ((arg function))
   arg)
  (:method (arg)
   #'do-nothing))

;; -------------------------------------------

(declaim (inline %actor-cas))

(defun %actor-cas (actor old-beh new-beh)
    (mpc:compare-and-swap (actor-beh (the actor actor))
                          old-beh new-beh))

