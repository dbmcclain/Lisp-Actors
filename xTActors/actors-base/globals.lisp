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

#+:LISPWORKS
(defstruct user-dyn-specials
  ;; User level bindings are read-only
  (self            nil :read-only t)
  (self-beh        nil :read-only t)
  (self-msg        nil :read-only t)
  (self-msg-parent nil :read-only t))

#+:LISPWORKS
(defstruct (sys-dyn-specials
            (:include user-dyn-specials
             (self            nil :read-only nil)
             (self-beh        nil :read-only nil)
             (self-msg        nil :read-only nil)
             (self-msg-parent nil :read-only nil)))
  ;; Sys level access has both readers and writers.
  ;; We need this to be a STRUCT so that we can
  ;; emply CAS against a slot.
  (send-hook      nil)
  (become-hook    (lambda (new-beh)
                    (declare (ignore new-beh))
                    (error "Not in an Actor")))
  (abort-beh-hook #'do-nothing))


#-:LISPWORKS
(defstruct user-dyn-specials
  ;; User level bindings are read-only
  self self-beh self-msg
  self-msg-parent)

#-:LISPWORKS
(defstruct (sys-dyn-specials
            (:include user-dyn-specials))
  ;; Sys level access has both readers and writers.
  ;; We need this to be a STRUCT so that we can
  ;; emply CAS against a slot.
  (send-hook      nil)
  (become-hook    (lambda (new-beh)
                    (declare (ignore new-beh))
                    (error "Not in an Actor")))
  (abort-beh-hook #'do-nothing))

(defvar *dyn-specials*  (make-sys-dyn-specials))

;; --------------------------------------------
;; R/W Access for the system level code

(define-symbol-macro *send-hook*
  (sys-dyn-specials-send-hook (the sys-dyn-specials *dyn-specials*)))

(define-symbol-macro *become-hook*
  (sys-dyn-specials-become-hook (the sys-dyn-specials *dyn-specials*)))

(define-symbol-macro *abort-beh-hook*
  (sys-dyn-specials-abort-beh-hook (the sys-dyn-specials *dyn-specials*)))

(define-symbol-macro *self*
  (sys-dyn-specials-self (the sys-dyn-specials *dyn-specials*)))

(define-symbol-macro *self-beh*
  (sys-dyn-specials-self-beh (the sys-dyn-specials *dyn-specials*)))

(define-symbol-macro *self-msg*
  (sys-dyn-specials-self-msg (the sys-dyn-specials *dyn-specials*)))

(define-symbol-macro *self-msg-parent*
  (sys-dyn-specials-self-msg-parent (the sys-dyn-specials *dyn-specials*)))

;; --------------------------------------------
;; Read-only versions for user code

(define-symbol-macro self
  (user-dyn-specials-self (the user-dyn-specials *dyn-specials*)))

(define-symbol-macro self-beh
  (user-dyn-specials-self-beh (the user-dyn-specials *dyn-specials*)))

(define-symbol-macro self-msg
  (user-dyn-specials-self-msg (the user-dyn-specials *dyn-specials*)))

(define-symbol-macro self-msg-parent
  (user-dyn-specials-self-msg-parent (the user-dyn-specials *dyn-specials*)))

;; --------------------------------------------
#|
 *SEND-HOOK*, *BECOME-HOOK*, *ABORT-BEH-HOOK*
   - dynamic runtime hooks for SEND, BECOME, ABORT-BEH
 
     SELF = current Actor during behavior execution
 SELF-MSG = current message during behavior execution
 SELF-BEH = Actor behavior function when its execution was initiated
MSG-FRAME = Parent message frame for current message (used for message tracing)
|#

