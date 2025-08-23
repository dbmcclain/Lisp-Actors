;; anti-collision.lisp --
;;
;; A wrapper for Actor behavior code to prevent collisions by multiple
;; threads all attempting, in parallel, to mutate Actor state via
;; BECOME. In such cases, the default action is to grant the change to
;; only one thread and force an immediate message retry for the other
;; threads.
;;
;; There is no getting around the retry, unless you do something
;; disgusting like dropping the messages on the floor. But there are
;; different ways of performing message delivery retry.
;;
;; Here the wrapper has colliding threads simply re-enqueue the
;; message for later delivery. This recycling can be performed prior
;; to investing, and avoiding, a lot of compute cycles in what would
;; become a wasted effort.
;;
;; If the lead up to the BECOME is lengthy, then placing the BECOME
;; inside a WITHOUT-CONTENTION may save considerable wasted CPU cycles
;; during parallel or concurrent access to a highly contentious Actor.
;;
;; The WITHOUT-CONTENTION macro could also be used to enforce local
;; serialized execution, in the event of functionally impure Actor
;; code. (I hate to encourage this...)
;;
;; DM/RAL 08/25
;; -----------------------------------------------------------

(in-package #:com.ral.actors.base)

;; --------------------------------------------
;; Do it without disturbing the Actor itself.
;;
;; We handle it with a private closure binding. All clauses in the
;; behavior which might call BECOME should be encapsulated by macro
;; WITHOUT-CONTENTION.
;;
;; Doing it this way allows non-mutating clauses to execute in
;; parallel, instead of forcing single-thread behavior for all
;; clauses.

(defun do-without-contention (guard fn)
  (let ((me  (mpc:get-current-process))
        (sav (car (the cons guard))))
    (cond ((or (eq me sav)
               (mpc:compare-and-swap (car (the cons guard)) nil me))
           (if sav
               (funcall fn)
             (unwind-protect
                 (funcall fn)
               (setf (car (the cons guard)) nil))))
          
          (t
           (%send-to-pool (msg self self-msg))
           (abort))
          )))

(defun do-become (guard fn)
  (do-without-contention guard (lambda ()
                                 (become fn))))
  
(defmacro with-contention-free-semantics (&body body)
  ;; Should be used at the level of closure vars for the behavior
  ;; which follows, e.g, after DEFUN and before ALAMBDA.
  (let  ((cfguard  (gensym)))
    `(let ((,cfguard  (list nil)))
       (macrolet ((become (fn)
                    `(do-become ,',cfguard ,fn))
                  (without-contention (&body body)
                    `(do-without-contention ,',cfguard (lambda () ,@body))))
         ,@body))))

