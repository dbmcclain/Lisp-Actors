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
;; --------------------------------------------

;; --------------------------------------------
;; Oh, SBCL -- too much!! Let's see if we can write something that
;; works for both of us...
;; --------------------------------------------

(defconstant +self-off+             0                         )
(defconstant +self-beh-off+         (1+ +self-off+)           )
(defconstant +self-msg-off+         (1+ +self-beh-off+)       )
(defconstant +self-msg-parent-off+  (1+ +self-msg-off+)       )
(defconstant +send-hook-off+        (1+ +self-msg-parent-off+))
(defconstant +become-hook-off+      (1+ +send-hook-off+)      )
(defconstant +abort-beh-hook-off+   (1+ +become-hook-off+)    )
(defconstant +dyn-specials-size+    (1+ +abort-beh-hook-off+) )

(defun make-dyn-specials (&key send-hook become-hook abort-beh-hook)
  (let ((v  (make-array +dyn-specials-size+
                        :initial-element nil)))
    (setf (aref v +send-hook-off+) send-hook
          (aref v +become-hook-off+)
          (or become-hook
              (lambda (new-beh)
                (declare (ignore new-beh))
                (error "Not in an Actor")))
          (aref v +abort-beh-hook-off+)
          (or abort-beh-hook
              #'do-nothing))
    v))

;; --------------------------------------------

(defvar *dyn-specials*  (make-dyn-specials))

;; --------------------------------------------
;; System level has R/W access

(define-symbol-macro *self*
  (svref (the simple-vector *dyn-specials*) +self-off+))

(define-symbol-macro *self-beh*
  (svref (the simple-vector *dyn-specials*) +self-beh-off+))

(define-symbol-macro *self-msg*
  (svref (the simple-vector *dyn-specials*) +self-msg-off+))

(define-symbol-macro *self-msg-parent*
  (svref (the simple-vector *dyn-specials*) +self-msg-parent-off+))

(define-symbol-macro *send-hook*
  (svref (the simple-vector *dyn-specials*) +send-hook-off+))

(define-symbol-macro *become-hook*
  (svref (the simple-vector *dyn-specials*) +become-hook-off+))

(define-symbol-macro *abort-beh-hook*
  (svref (the simple-vector *dyn-specials*) +abort-beh-hook-off+))

;; --------------------------------------------
;; User level has Read-Only access

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
MSG-FRAME = Parent message frame for current message (used for message tracing)
|#

