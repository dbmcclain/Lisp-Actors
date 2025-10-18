;; shunting-become.lisp -- a BECOME based on an extended computation
;;
;; DM/RAL  08/25
;; ----------------------------------------------

(in-package :com.ral.actors.macros)

;; ----------------------------------------------------------
;; SHUNTING-BECOME - a useful idiom for when non-idemptent actions
;; produce information that needs to become part of an evolving Actor
;; state.
;;
;; Non-idempotent behavior needs to be relegated to edge Actors
;; (Actors without BECOME). You can acheive that by wrapping the
;; non-idempotent actions with ON-COMMIT, which only executes if
;; successful commit of BECOME.
;;
;; But sometimes, those non-idempotent actions produce information
;; that needs to be folded into the evolving Actor state. The solution
;; for this is:
;;
;;   (A) Use a SERIALIZER around the Actor, then go ahead and perform
;;   BECOME along with non-idempotent actions - but this penalizes all
;;   message handlers in the Actor.
;;
;; Or else,
;;
;;   (B) Temporarily BECOME a shunting behavior which shunts all
;;   futures messages to a queue while looking for specific update
;;   information from an ON-COMMIT action.
;;
;;   The ON-COMMIT action must compute its non-idempotent information
;;   and produce a new behavior function which is sent to the shunting
;;   Actor behavior. The shunting behavior then becomes the new
;;   behavior and it can release the pending messages for re-delivery
;;   to SELF.
;;
;; This is all elegantly produced by macro SHUNTING-BECOME, which
;; carries the surface syntax of MULTIPLE-VALUE-BIND.
#|
(defun shunting-beh (tag err sav-beh &optional pending)
  (flet ((exit (beh)
           (become beh)
           (send-all-to self pending)))
    (alambda
     ((atag) / (eq atag err)
      (exit sav-beh))
     ((atag new-beh) / (eq atag tag)
      (exit new-beh))
     (msg
      (become (shunting-beh tag err sav-beh
                            (cons msg pending))))
     )))
|#

(defun shunting-beh (tag err sav-beh &optional pending)
  (with-contention-free-semantics
    (flet ((exit (beh)
             (without-contention
              (become beh)
              (send-all-to self pending))))
      (alambda
       ((atag) / (eq atag err)
        (exit sav-beh))
       ((atag new-beh) / (eq atag tag)
        (exit new-beh))
       (msg
        (without-contention
         (become (shunting-beh tag err sav-beh (cons msg pending)))
         ))
       ))))

(defun do-shunting-become (fn)
  ;; Works hand-in-glove with SHUNTING-BEH to catch error conditions
  ;; in the non-idempotent action, and cause the Actor to revert back
  ;; to its former behavior.
  ;;
  ;; The non-idempotent expr develops a new behavior function for the
  ;; Actor.
  (let ((tag  (tag self))
        (err  (tag self)))
    (become (shunting-beh tag err self-beh))
    (on-commit
      (handler-bind
          ((error (lambda (c)
                    (declare (ignore c))
                    (send-to-pool err)) ;; unconditional immediate SEND
                  ))
        (send tag (funcall fn))
        ))
    ))

(defmacro shunting-become (expr)
  ;; Expr must produce a new behavior function. As always, any errors
  ;; along the way cancels the BECOME.
  ;;
  `(do-shunting-become (lambda ()
                         ,expr)))

(defmacro β-become (expr)
  `(shunting-become ,expr))

#+:LISPWORKS
(progn
  (editor:indent-like "shunting-become" 'destructuring-bind)
  (editor:indent-like "β-become" 'destructuring-bind))
