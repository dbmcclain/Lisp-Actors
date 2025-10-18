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
;;                  |      v    Closure
;;                  |    +----+-------+
;;                  v    | Fn | State |
;;             T(Actor)  +----+-------+  Bindings
;;                         |      |      +------+-----+-----+---
;;                         |      +----->| Data | ... | ... |
;;                         |             +------+-----+-----|---
;;                         |    +------+-----+-----+---
;;                         +--->| Code | ... | ... |
;;                              +------+-----+-----+---
;; ------------------------------------------------------------------
;;
;; An Actor can use any function as its behavior. And, by convention,
;; messages have, as their first element, a reference to a customer
;; Actor to which results should be sent.
;;
;; We cannot enforce this convention, so we have to trust the
;; programmer.
;;
;; A Service Actor is one which takes a message containing only a
;; customer, and whose behavior code sends a result to this customer.
;; Actors which comply with this Service convention can take advantage
;; of some parallel invocation macros, such as FORK/JOIN and others.
;;
;; Any Actor which accepts more arguments in messages can be converted
;; into a Service Actor by placing it behind an invocation forwarding
;; Actor which accepts the single customer argument, but which has
;; been created with values for the other message args and the target
;; Actor. It then forwards the invocation to the target Actor using
;; the customer supplied in the message, concatenated with the other
;; args specified in its creation state.
;;
;; This can be automated using the macro RACURRY, supplying the target
;; Actor and the remaining message args to supply on invocation of the
;; target Actor. RACURRY works like RCURRY, but for Actors instead of
;; Functions. RACURRY creates a forwarding Actor to which messages
;; containing only the customer can be sent.
;;
;; -------------------------------------------
;; DM/RAL  2025/10/18 02:38:52 UTC
;;
;; Everyone decries the creation of NIL. I like NIL. It enables so
;; many idiomatic Lisp expressions that would be much more cumbersome
;; without it.
;;
;; So in the interest of saving space, we will denote SINK with NIL so
;; that messages to SINK targets never take any space across the
;; network.
;;
;; And by allowing a NIL function pointer in the behavior slot of an
;; Actor, we enable BECOME-SINK to free up the memory stored in their
;; closure bindings.

(defstruct (actor
            (:constructor %create (beh)))
  (beh  nil  :type (or null function)))


(defgeneric screened-beh (arg)
  (:method ((arg function))
   arg)
  (:method ((arg actor))
   (fwd-beh arg))
  (:method (arg) ;; anything other than a function or an Actor becomes SINK
   nil))


(defun create (&optional beh)
  (%create (screened-beh beh)))

;; ----------------------------------------------------------

