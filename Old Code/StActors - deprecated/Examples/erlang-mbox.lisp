;; erlang-mbox.lisp - using micro Actors to effect an Erlang-style mailbox with selective RECV
;;
;; DM/RAL 04/21
;; ----------------------------------------------

(defpackage #:micro-erl
  (:use #:common-lisp #:stactors)
  (:export
   ))

;; --------------------------------------------
(in-package #:micro-erl)
;; --------------------------------------------

(defun make-erl-mbox ()
  (make-actor (make-root-mailbox-beh)))

(defun make-root-mailbox-beh ()
  (let ((next (make-end-mailbox-beh)))
    (lambda (&rest msg)
      (declare (ignore msg))
      (repeat-send next))))
     
(defun make-end-mailbox-beh ()
  (um:dlambda
    (:recv (cust pred)
      (let ((next (make-actor self-beh)))
        (become (make-recv-mailbox-beh cust pred next))))
    (:send (&rest m)
     (let ((next (make-actor self-beh)))
       (become (make-send-mailbox-beh m next))))
    (:prune (prev)
     (send prev :pruned self-beh))
    ))

(defun make-pruned-beh (next)
  (um:dlambda
    (:pruned (beh)
     (become beh))
    (t (&rest _)
       (declare (ignore _))
       (repeat-send next))
    ))

(defun prune-myself (next)
  (become (make-pruned-beh next))
  (send next :prune self))

(defun make-recv-mailbox-beh (cust pred next)
  (um:dlambda
    (:send (&rest m)
     (if (apply pred m)
         (progn
           (apply #'send cust m)
           (prune-myself next))
       ;; else
       (repeat-send next)))
    (:prune (prev)
     (send prev :pruned self-beh))
    (t (&rest _)
       (declare (ignore _))
       (repeat-send next))
    ))

(defun make-send-mailbox-beh (m next)
  (um:dlambda
    (:recv (cust pred)
      (if (apply pred m)
          (progn
            (apply #'send cust m)
            (prune-myself next))
        (repeat-send next)))
    (:prune (prev)
     (send prev :pruned self-beh))
    (t (&rest _)
       (declare (ignore _))
       (repeat-send next))
    ))

(defun test-mailbox ()
  (let ((mbox (make-erl-mbox))
        (cust (um:curry #'log-info :system-log "From RECV: ~S")))
    (watch mbox)
    (send mbox :recv cust (lambda (&rest msg)
                            (not (and (eq :foo (car msg))
                                      (null (cdr msg))))))
    (send mbox :send :foo)
    (send mbox :send :bar)
    (send mbox :send :baz)
    (send mbox :recv cust (lambda (&rest msg)
                            (and (eq :foo (car msg))
                                 (null (cdr msg)))))
    (inspect mbox)
    ))
#|
(test-mailbox)
|#



            
