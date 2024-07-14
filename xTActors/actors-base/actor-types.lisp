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
  (beh  #'do-nothing  :type function))

(defun create (&optional (beh #'do-nothing))
  (%create (screened-beh beh)))

;; ----------------------------------------------------------

(defgeneric screened-beh (arg)
  (:method ((arg function))
   arg)
  (:method ((arg actor))
   (fwd-beh arg))
  (:method (arg)
   (error "Invalid behavior: ~S" arg)))

;; -------------------------------------------

(declaim (inline %actor-cas))

(defun %actor-cas (actor old-beh new-beh)
    (mpc:compare-and-swap (actor-beh (the actor actor))
                          old-beh new-beh))

;; -----------------------------------------------------
;; Sending messages is not supported until MultiProcessing is running.
;;
;; The only truly safe way to do this...  Actors are inviolable,
;; immutable, bindings. Any reference to SELF from within an Actor
;; behavior must forever continue to point to the same object.
;;
;; You can refer, freely, to an Actor's behavior, but only the Actor
;; can mutate its own behavior, using BECOME.

(defun %patch-beh (actor target)
  ;; Actor is presumed to be a %BECOMER
  ;; Target is an Actor or a behavior function
  (if (mpc:get-current-process) ;; MP running?
      ;; yes - could have accessed SELF, or received messages already
      (send actor '%become target)
    ;; else - could not have accessed SELF, nor received any messages
    (setf (actor-beh actor) (cond
                             ((functionp target) target)
                             ((actor-p target)   (actor-beh target))
                             (t (error "Actor or behavior function expected."))
                             ))
    ))

