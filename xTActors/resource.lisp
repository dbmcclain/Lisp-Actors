;; resource.lisp -- Guarded resources that need reclamation, no matter what
;;  e.g., open files need to be closed.
;;
;; This attempts to be the Actor analog to UNWIND-PROTECT.
;;
;; DM/RAL 12/21
;; ------------------------------------------------------------------

(in-package :com.ral.actors.base)

(defun side-job (cust side-action &rest side-args)
  (actor (&rest msg)
    (send* side-action side-args)
    (send* cust msg)))

(defun guard (action must-do timeout)
  (actor (cust rsrc . args)
    (let* ((must-do-once  (once must-do))
           (fail          (side-job sink must-do-once rsrc)))
      (send-after timeout fail)
      (send* action
             (side-job cust must-do-once rsrc) ;; cust for success
             fail ;; cust for failure
             rsrc args))
    ))

(defvar close-file (actor (fp)
                     (close fp)))

(defvar secure-erase (actor (buf)
                       (fill buf 0)))

