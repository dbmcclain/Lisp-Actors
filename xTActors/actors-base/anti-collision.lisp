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
;; code. (I hate to encourage this...) AKA WITH-SERIALIZED-EXECUTION.
;;
;; DM/RAL 08/25
;; -----------------------------------------------------------

(in-package #:com.ral.actors.base)

#|
(defun do-with-serialized-execution (guard fn)
  ;; SELF is a global construct (a global binding visible to all threads)
  ;; *SELF* and *SELF-BEH* are thread-local bindings, different for each thread.
  ;; SAV-BEH is local to the execution, once past the CAS operation.
  (symbol-macrolet ((beh  (actor-beh (the actor *self*))))
    (if (mpc:compare-and-swap beh *self-beh* *my-go-around*)
        (let ((sav-beh (shiftf *self-beh* *my-go-around*)))  ;; allow nested WITHOUT-CONTENTION
          (unwind-protect
              (funcall fn)
            (setf *self-beh* sav-beh)
            (mpc:compare-and-swap beh *my-go-around* sav-beh)
            ))
      ;; else
      (apply *my-go-around* *self-msg*))
    ))

(defmacro with-serialized-execution (&body body)
  `(let ((,cfguard  (list nil)))
     (do-with-serialized-execution guard (lambda ()
                                           ,@body))))
|#

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

