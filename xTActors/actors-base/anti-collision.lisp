;; anti-collision.lisp -- Sometimes it is better to avoid contention races...
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
;; So what's the difference between using
;; WITH-COLLISION-FREE-SEMANTICS and using a Serializer?
;;
;; Ah well, a Serialilzer prevents all activity except one thread from
;; entering the Actor. That includes mutating as well as non-mutating
;; actions.
;;
;; But it is really okay for non-mutating actions to occur
;; simultaneously with mutating actions, *PROVIDED* that the code is
;; functionally pure. Using a Serializer would prevent many concurrent
;; activities that could otherwise occur under
;; WITH-COLLISION-FREE-SEMANTICS.
;;
;; So the purpose of WITH-COLLISION-FREE-SEMANTICS isn't to prevent
;; concurrent actions. It simply seeks to reduce the wasted compute
;; cycles resulting from mutating action contention, where only one
;; thread will win the round, and the other threads will be
;; automatically retried. Guarding the BECOME clauses with
;; WITHOUT-CONTENTION allows us to forego the wasted compute cycles
;; leading up to a failed BECOME, and force an early go-around for all
;; the non-winning threads.
;;
;; DM/RAL 08/25
;; -----------------------------------------------------------

(in-package #:com.ral.actors.base)

;; --------------------------------------------

#|
(defun bad-become (beh)
  (declare (ignore ben))
  (error "Unguarded BECOME in contention-free semantics"))
|#

(defun do-with-disallowed-contention (msg guard fn)
  ;; If user forgets to surround their BECOME clause with
  ;; WITHOUT-CONTENTION, then we still let them try, but they have to go
  ;; through the same protection protocol as all the other clauses
  ;; using WITHOUT-CONTENTION.
  (let ((*become-hook* (lambda (beh)
                         (warn "Calling BECOME outside of WITHOUT-CONTENTION")
                         (do-without-contention guard (lambda ()
                                                        (become beh))))
                       ))
    (apply fn msg)
    ))

(defun go-around ()
  (%send-to-pool (msg self self-msg))
  (abort))

;; --------------------------------------------
;; RELEASE-CONTENTION - needed for Actor system shutdown

(define-condition release-contention ()
  ())

(defun release-contention ()
  (signal 'release-contention))

;; --------------------------------------------

(defun do-without-contention (guard thunk)
  (declare (cons guard)
           (function thunk))
  (symbol-macrolet ((owner  (car (the cons guard))))
    (let ((*become-hook* *ac-become-hook*)
          (me  (mpc:get-current-process)))
          
      (flet ((acquire ()
               (mpc:compare-and-swap owner nil me))
             (release (&rest ignored)
               (declare (ignore ignored))
               (mpc:compare-and-swap owner me nil)))
        (declare (dynamic-extent #'acquire))
        ;;
        ;; First thread to attempt WITHOUT-CONTENTION takes it,
        ;; preemptively blocking all other threads from mutating the
        ;; Actor. Other threads simply put their message back on the
        ;; queue for later delivery.
        ;;
        ;; Non-mutating threads continue to execute in parallel
        ;; concurrent manner. So it continues to be a good idea to
        ;; keep the behavior code functionally pure.
        ;;
        (when (acquire)
          (on-commit
            (release)))
        (if (eq me owner)
            (handler-bind
                ((error              #'release)
                 (release-contention #'release))
              (funcall thunk))
          ;; else, Re-enqueue our message for later delivery,
          ;; and go process the next available message. This
          ;; drops all pending BECOME and SEND.
          (go-around))
        ))
    ))

(defmacro with-contention-free-semantics (beh-fn)
  ;; Should wrap a behavior function.
  ;;
  ;; WITHOUT-CONTENTION available only within
  ;; WITH-CONTENTION-FREE-SEMANTICS.
  ;;
  ;; All BECOME clauses should be wrapped inside of
  ;; WITHOUT-CONTENTION. And that includes any calls to external
  ;; functions that might call BECOME.
  ;;
  ;; While it might be nice to catch this at compile time, that is
  ;; impossible when external functions could call BECOME. With
  ;; macrology you could catch the overt BECOMEs in the Actor body
  ;; code. But you would miss the ones in external functions. We are
  ;; left with runtime error trapping.
  ;;
  (um:with-unique-names (g!guard g!msg)
    `(let ((,g!guard (list nil)))
       (lambda (&rest ,g!msg)
         (do-with-disallowed-contention
          ,g!msg
          ,g!guard
          (macrolet ((without-contention (&body body)
                       `(do-without-contention ,',g!guard (lambda ()
                                                            ,@body))
                       ))
            ,beh-fn))
         ))
    ))
