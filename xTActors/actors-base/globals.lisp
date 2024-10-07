;; globals.lisp
;;
;; DM/RAL  2022/12/14 06:20:16
;; ----------------------------------

(in-package #:com.ral.actors.base)

;; ----------------------------------

;; DEFGLOBAL - intended for global bindings that never get rebound
;; during execution

(mpc:defglobal *central-mail*         (mpc:make-mailbox :lock-name "Central Mail"))
(mpc:defglobal *nbr-pool*               8)  ;; nbr threads in dispatch pool
(mpc:defglobal *ASK-TIMEOUT*          0.1)  ;; period of goal checking
(mpc:defglobal *actors-grace-period*  5f0)  ;; period before forced shutdown termination

;; --------------------------------------------
;; Per-Thread for Activated Actors
;;
;; May become rebound during execution. Different for every Dispatcher
;; thread, one shared collection for all non-Dispatcher threads.
;;
;; User code should treat all of these dynamic globals as read-only!
;;
;; Group them all together into one single dynamic binding for speed.

(defstruct dyn-specials
  (send-hook      nil)
  (become-hook    (lambda (new-beh)
                    (declare (ignore new-beh))
                    (error "Not in an Actor")))
  (abort-beh-hook #'do-nothing)
  self self-beh self-msg msg-parent)

(defvar *dyn-specials* (make-dyn-specials))

(define-symbol-macro *send-hook*
  (dyn-specials-send-hook      (the dyn-specials *dyn-specials*)))
(define-symbol-macro *become-hook*
  (dyn-specials-become-hook    (the dyn-specials *dyn-specials*)))
(define-symbol-macro *abort-beh-hook*
  (dyn-specials-abort-beh-hook (the dyn-specials *dyn-specials*)))
(define-symbol-macro self
  (dyn-specials-self           (the dyn-specials *dyn-specials*)))
(define-symbol-macro self-beh
  (dyn-specials-self-beh       (the dyn-specials *dyn-specials*)))
(define-symbol-macro self-msg
  (dyn-specials-self-msg       (the dyn-specials *dyn-specials*)))
(define-symbol-macro msg-parent
  (dyn-specials-msg-parent     (the dyn-specials *dyn-specials*)))

#|
 *SEND-HOOK*, *BECOME-HOOK*, *ABORT-BEH-HOOK*
   - dynamic runtime hooks for SEND, BECOME, ABORT-BEH
 
     SELF = current Actor during behavior execution
 SELF-MSG = current message during behavior execution
 SELF-BEH = Actor behavior function when its execution was initiated
MSG-FRAME = Parent message frame for current message (used for message tracing)
|#

