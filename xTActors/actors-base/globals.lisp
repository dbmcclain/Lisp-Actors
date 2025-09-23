;; globals.lisp
;;
;; DM/RAL  2022/12/14 06:20:16
;; ----------------------------------

(in-package #:com.ral.actors.base)

;; ----------------------------------

;; DEFGLOBAL - intended for global bindings that never get rebound
;; during execution

(mpc:defglobal *central-mail*         nil)
(mpc:defglobal *central-mail-lock*    (mpc:make-lock))
(mpc:defglobal *nbr-pool*               8)  ;; nbr threads in dispatch pool
(mpc:defglobal *ASK-TIMEOUT*          0.1)  ;; period of goal checking
(mpc:defglobal *actors-grace-period*  5f0)  ;; period before forced shutdown termination

;; --------------------------------------------

(defun %send-to-pool (msg)
  (unless *central-mail*
    (mpc:with-lock (*central-mail-lock*)
      (unless *central-mail*
        (setf *central-mail* (mpc:make-mailbox :lock-name "Central Mail"))
        (restart-actors-system *nbr-pool*)
        )))
  (mpc:mailbox-send *central-mail* msg))

(defun not-in-actor (&rest ignored)
  (declare (ignore ignored))
  (error "BECOME while not in an Actor"))
  
;; --------------------------------------------
;; Per-Thread for Activated Actors
;;
;; May become rebound during execution. Different for every Dispatcher
;; thread, one shared collection for all non-Dispatcher threads.
;;
;; User code should treat all of these dynamic globals as read-only!
;; --------------------------------------------

(defvar *self*            nil)
(defvar *self-beh*        nil)
(defvar *self-msg*        nil)
(defvar *self-msg-parent* nil)

(defvar *send-hook*      #'%send-to-pool)
(defvar *become-hook*    #'not-in-actor)
(defvar *become-bak*     #'not-in-actor)
(defvar *abort-beh-hook* #'do-nothing)
  
;; --------------------------------------------
;; User level has Read-Only access
;;
;; (In general, to make something read-only, place it within a function.)

(declaim (inline self self-beh self-msg self-msg-parent))

(defun self ()
  *self*)

(defun self-beh ()
  *self-beh*)

(defun self-msg ()
  *self-msg*)

(defun self-msg-parent ()
  *self-msg-parent*)

(define-symbol-macro self             (self))
(define-symbol-macro self-beh         (self-beh))
(define-symbol-macro self-msg         (self-msg))
(define-symbol-macro self-msg-parent  (self-msg-parent))

;; --------------------------------------------
#|
 *SEND-HOOK*, *BECOME-HOOK*, *ABORT-BEH-HOOK*
   - dynamic runtime hooks for SEND, BECOME, ABORT-BEH
 
           SELF = current Actor during behavior execution
       SELF-MSG = current message during behavior execution
       SELF-BEH = Actor behavior function when its execution was initiated
SELF-MSG-PARENT = Parent message frame for current message (used for message tracing)
|#

