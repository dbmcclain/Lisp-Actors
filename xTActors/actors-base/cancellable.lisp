;; cancellable.lisp -- Cancellable Logical Tasks
;;
;; DM/RAL  2026/07/25T09:56:47U
;; --------------------------------------------

(in-package #:com.ral.actors.base)

;; --------------------------------------------
;; Cancellable Tasks...
;;
;; In order to propagate a cancel condition to Actors involved in some
;; coordinated activity (a Logical Task), we allow the customer field
;; of a message to contain a customer Actor/cancel-flag pair.
;;
;; Messages can be sent to the pair, meaning the Actor of the pair, as
;; well as to ordinary Actors.
;;
;; Cancellation is indicated by the cancel flag, but what an Actor
;; does with this information is strictly voluntary. No errors are
;; thrown when the cancellation flag is checked with CANCELLED?. It
;; merely returns a boolean result.
;;
;; In effect, you ask "if the customer of the Actor has been
;; cancelled?" The cancel flag gets set by someone calling CANCEL on
;; the pair, or on the flag itself.
;;
;; A cancel flag can be propagated to other Actors by calling
;; MAKE-CANCELLABLE on an Actor, with a customer pair, or a cancel
;; flag, as the second argument.
;;

(defstruct (cancel-flag
            (:constructor %make-cancel-flag (link)))
  cancelled?
  (link nil :read-only t))

(defun make-cancel-flag (&optional link)
  (%make-cancel-flag (cancel-flag link)))

(defstruct cust-can-pair
  ;; used to convey a customer actor and cancellation flag to a
  ;; service Actor
  (customer     nil :read-only t)
  (cancel-flag  nil :read-only t))

(defgeneric make-cancellable (cust cf)
  ;; Make a cust Actor cancellable if cf is.
  ;; If cust is already cancellable, no change.
  (:method ((cust actor) cf)
   (let ((flag (cancel-flag cf)))
     ;; can inherit from a cancel flag or another cancellable
     (if flag
         (make-cust-can-pair
          :customer    cust
          :cancel-flag flag)
       cust)))
  (:method (cust cf)
   (declare (ignore cf))
   cust))

(defgeneric cancelled? (x)
  (:method (x)
   nil)
  (:method ((x cancel-flag))
   (or (cancel-flag-cancelled? x)
       (cancelled? (cancel-flag-link x))))
  (:method ((x cust-can-pair))
   (cancelled? (cust-can-pair-cancel-flag x))))

(defgeneric cancel (x)
  (:method (x)
   ;; do nothing...
   )
  (:method ((x cancel-flag))
   (setf (cancel-flag-cancelled? x) t))
  (:method ((x cust-can-pair))
   (cancel (cust-can-pair-cancel-flag x))))

(defgeneric cancel-flag (x)
  ;; Extract the cancel-flag from the argument.
  (:method (x)
   nil)
  (:method ((x cancel-flag))
   x)
  (:method ((x cust-can-pair))
   (cust-can-pair-cancel-flag x)))

