;; resource.lisp -- Guarded resources that need reclamation, no matter what
;;  e.g., open files need to be closed.
;;
;; This attempts to be the Actor analog to UNWIND-PROTECT.
;;
;; DM/RAL 12/21
;; ------------------------------------------------------------------
;;
;; Actors vs UNWIND-PROTECT and RESTART... Actors do not retain a
;; useable dynamic environment, since they exit immediately after
;; sending messages. By the time the sent-to receiver gets activated,
;; the sending Actor is long gone, and there if no record of the
;; sender. So UNWIND-PROTECT, RESTART-HANDLER, etc. are ineffective.
;;
;; Instead, when we have actions that may need to restart, we send
;; along a customer for the normal case, and an on-fail Actor to
;; handle the need for restart.
;;
;; By wrapping both the customer and on-fail Actors with SIDE-JOB, we
;; can also ensure that vital actions happen, regardless of outcome,
;; much like UNWIND-PROTECT. But in this case, it could happen that
;; neither receiver gets sent a message. For that situation we also
;; have a timeout that will ensure that on-fail gets sent a message.

(in-package :com.ral.actors.base)

(defun guard-selector-beh (must-do must-args)
  (lambda (cust &rest msg)
    (send* cust msg)
    (send* must-do must-args)
    (become-sink)))

(defun guard-selector (cust fail must-do must-args)
  (letrec ((sel      (create
                      (guard-selector-beh must-do must-args)))
           (lbl-ok   (label sel cust))
           (lbl-fail (label sel fail)))
    (values lbl-ok lbl-fail)))
  
(defun guard (action timeout must-do &rest must-args)
  ;; Guard manages the ultimate disposition of valued resources, such
  ;; as a file pointer that must be eventually closed.
  ;;
  ;; The action will receive the customer, the sender's fail portal,
  ;; and args. We augment both customer and fail portals with a
  ;; side-job wrapper to ensure that must-do gets called with
  ;; must-args when a message is sent to either of the original
  ;; receivers. Action message signature: (cust on-fail . args)
  ;;
  ;; We guarantee that if a message is sent to either the customer, or
  ;; the fail portal, then the must-do will be sent the must-args. But
  ;; if neither of these targets are sent a message, then we have a
  ;; timeout backup to send to the fail portal. In any case, must-do
  ;; is called only once on the must-args.
  ;;
  ;; Guards can be nested for cases with multiple resources.  The
  ;; action of the first guard can be another guard.
  ;;
  (actor 
      (lambda* ((cust fail) &rest args)
        (multiple-value-bind (lbl-ok lbl-fail)
            (guard-selector cust fail must-do must-args)
          (send-after timeout lbl-fail +timed-out+)
          (send* action `(,lbl-ok ,lbl-fail) args))
        )))

;; Some must-do's
(deflex close-file
  (actor 
      (lambda (fp)
        (close fp))))
  
(deflex secure-erase
  (actor 
      (lambda (buf)
        (fill buf 0))))

(defun perform (fn)
  (actor 
      (lambda (&rest args)
        (apply fn args))))

#|
;; Example - carefully controlling the use of a secret key: Copy key
;; from environment into buffer, use the key, ensure buffer is zapped
;; after use.
;;
(defun with-open-vault (vault)
  (actor 
      (lambda* ((cust on-fail) &rest args)
        (let ((key (copy-seq (get-env "MySecretKey"))))
          (send* (guard unlock-vault 10 secure-erase key)
                 `(,cust ,on-fail) vault key args)))
    ))
  |#