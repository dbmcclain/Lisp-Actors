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

(defun do-without-contention (fn)
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

(defmacro without-contention (&body body)
  ;; Should be placed around BECOME clauses.
  `(do-without-contention (lambda () ,@body)))
  
(defmacro with-serialized-execution (&body body)
  `(without-contention ,@body))
