;; globals.lisp
;;
;; DM/RAL  2022/12/14 06:20:16
;; ----------------------------------

(in-package #:com.ral.actors.base)

;; ----------------------------------

;; DEFGLOBAL - intended for global bindings that never get dynamically
;; rebound during execution

(mpc:defglobal *central-mail*         nil)
(mpc:defglobal *central-mail-lock*    (mpc:make-lock))
(mpc:defglobal *nbr-pool*               8)  ;; nbr threads in dispatch pool
(mpc:defglobal *ASK-TIMEOUT*          0.1)  ;; period of goal checking
(mpc:defglobal *actors-grace-period*  5f0)  ;; period before forced shutdown termination

;; --------------------------------------------
;; Self-init on first SEND

(aop:defdynfun %send-to-pool (msg)
  (%initial-send-to-pool msg))

(defun %ss-send-to-pool (msg)
  (mpc:mailbox-send *central-mail* msg))
  
(defun %initial-send-to-pool (msg)
  (unless *central-mail*
    (mpc:with-lock (*central-mail-lock*)
      (unless *central-mail*
        (setf *central-mail* (mpc:make-mailbox :lock-name "Central Mail"))
        (aop:rebind %send-to-pool #'%ss-send-to-pool)
        (restart-actors-system *nbr-pool*)
        )))
  (%ss-send-to-pool msg))

(defun reset-send-to-pool ()
  (mpc:with-lock (*central-mail-lock*)
    (setf *central-mail*  nil)
    (aop:rebind %send-to-pool #'%initial-send-to-pool)
    ))

;; --------------------------------------------
;; Per-Thread for Activated Actors
;;
;; May become dynamically rebound during execution. Different for
;; every Dispatcher thread, one shared collection for all
;; non-Dispatcher threads.
;;
;; User code should treat all of these dynamic globals as read-only!
;; --------------------------------------------

(defvar *self*            nil)  ;; the current Actor
(defvar *self-beh*        nil)  ;; the behavior closure of the current Actor
(defvar *self-msg*        nil)  ;; the full message
(defvar *self-msg-parent* nil)  ;; for debugging, the parent message of the current message

(defvar *state*           nil)

;; --------------------------------------------
;; User level has Read-Only access
;;
;; (In general, to make something read-only, place it within a function.)

(declaim (inline fn-self fn-self-beh fn-self-msg fn-self-msg-parent))

(defun fn-self ()
  *self*)

(defun fn-self-beh ()
  *self-beh*)

(defun fn-self-msg ()
  *self-msg*)

(defun fn-self-msg-parent ()
  *self-msg-parent*)

(define-symbol-macro self             (fn-self))
(define-symbol-macro self-beh         (fn-self-beh))
(define-symbol-macro self-msg         (fn-self-msg))
(define-symbol-macro self-msg-parent  (fn-self-msg-parent))

;; --------------------------------------------
#|
           SELF = current Actor during behavior execution
       SELF-MSG = current message during behavior execution
       SELF-BEH = Actor behavior function when its execution was initiated
SELF-MSG-PARENT = Parent message frame for current message (used for message tracing)
|#

;; --------------------------------------------

