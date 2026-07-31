;; cancellable.lisp -- Cancellable Logical Tasks
;;
;; DM/RAL  2026/07/25T09:56:47U
;; --------------------------------------------

(in-package #:com.ral.actors.base)

;; --------------------------------------------
;; Cancellable Tasks...
;;
;; Actors cooperatively perform operation cancellation. We shouldn't
;; forcibly cancel anything because we don't fully understand the
;; intermediate contexts, and could possbly damage global state.
;;
;; A :CANCEL flag may exist in the *SELF-CONTEXT* of a running Actor.
;; Actors can query with (CANCELLED?), and if an Actor has access to a
;; CANCEL-FLAG, then it can (CANCEL flag). Any downstream Actors that
;; have a reference to the flag can query and choose to cancel their
;; operation.
;;
;; A CANCEL-FLAG is linked to any upstream CANCEL-FLAGS so that the
;; query (CANCELLED?) checks an entire chain of CANCEL-FLAGs before
;; returning.
;;
;; MAKE-CANCEL-FLAG constructs a CANCEL-FLAG and links it to all
;; upstream CANCEL-FLAGs.
;;

(defun cancel-flag ()
  (getf self-context :cancel))

(defstruct (cancel-flag
            (:constructor %make-cancel-flag))
  cancelled?
  (link (cancel-flag) :read-only t))

(defun make-cancel-flag ()
  (%make-cancel-flag))

(defun cancel (flag)
  (setf (cancel-flag-cancelled? flag) t))

(defun cancelled? (&optional (flag (cancel-flag)))
  (and flag
       (or (cancel-flag-cancelled? flag)
           (cancelled? (cancel-flag-link flag)))))



(defmacro with-context (ctxt &body body)
  `(let ((*self-context*  ,ctxt))
     ,@body))

(defmacro with-cancel-flag ((cf ctxt) &body body)
  `(do-with-cancel-flag (lambda (,cf ,ctxt)
                          ,@body)))

(defun do-with-cancel-flag (fn)
  (let ((flag  (make-cancel-flag))
        (sav   self-context))
    (with-context (list* :cancel flag self-context)
      (funcall fn flag sav))))


