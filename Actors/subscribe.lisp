;; subscribe.lisp - A Subscription/Notification System
;;
;; Built on a purely functional system, where the only mutation
;; is via BECOME.
;;
;; DM/RAL 04/21
;; ------------------------------------------------------

(in-package #:actors/notifications)

(um:eval-always
  (import '(um:dlambda
            )))

;; ------------------------------------------------------
;; A self organizing chain of event handlers

(defun respond-to-prune (from)
  (send from :pruned (current-behavior)))

(defun make-empty-subject-beh ()
  (dlambda
    (:attach (about from)
     (let ((next (make-actor (current-behavior))))
       (become (make-subject-beh about from next))))
    
    (:prune (prev)
     (respond-to-prune prev))
    ))

(defun make-subject-beh (about from next)
  (dlambda
    (:attach (an-about a-from)
     (unless (and (eq an-about about)
                  (eq a-from   from))
       (repeat-send next)))
    
    (:detach (an-about a-from)
     (cond ((and (eq about an-about)
                 (eq from  a-from))
            (become (make-prune-beh next))
            (send next :prune (current-actor)))
           
           (t
            (repeat-send next))
           ))

    (:notify (an-about &rest args)
     (declare (ignore args))
     (when (eq an-about about)
       (repeat-send from))
     (repeat-send next))

    (:prune (prev)
     (respond-to-prune prev))
    ))

(defun make-prune-beh (next)
  (dlambda
    (:pruned (beh)
     (become beh))

    (t (&rest _)
       ;; simple forwarding
       (declare (ignore _))
       (repeat-send next))
    ))

(defvar *subscribers* (make-actor (make-empty-subject-beh)))

(defun subscribe (about from)
  (send *subscribers* :attach about from))

(defun unsubscribe (about from)
  (send *subscribers* :detach about from))

(defun notify (about &rest args)
  (apply #'send *subscribers* :notify about args))
