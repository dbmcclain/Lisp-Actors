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
;; DM/RAL 08/25
;; -----------------------------------------------------------

(in-package #:com.ral.actors.base)

;; --------------------------------------------

(defvar *become-disabled*  nil)

(defmacro with-become-disabled (&body body)
  `(let ((*become-disabled* t))
     ,@body))

(defmacro with-become-enabled (&body body)
  `(let ((*become-disabled* nil))
     ,@body))

(defun bad-become ()
  (error "Unguarded BECOME in contention-free semantics"))

;; --------------------------------------------

(defun go-around ()
  ;; Re-enqueue our message for later delivery, and go process the
  ;; next available message. This drops all pending BECOME and SEND.
  (when self
    ;; pointless unless we are in an Actor.
    (%send-to-pool (msg self self-msg))
    (abort)))

(defun make-cf-closure (beh-fn)
  (let ((proc  (list nil)))
    (symbol-macrolet ((owner  (car (the cons proc))))
      (alambda
       (('contention-free fn)
        (let ((me  (mpc:get-current-process)))
          (flet ((acquire ()
                   (mpc:compare-and-swap owner nil me))
                 (release ()
                   (mpc:compare-and-swap owner me nil)))
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
                (let ((normal-exit nil))
                  (unwind-protect
                      (with-become-enabled
                        (funcall fn)
                        (setf normal-exit t))
                    (unless normal-exit
                      ;; Normal exit uses a commit action to clear
                      ;; ownership. This avoids a race condition at
                      ;; commit time between competing BECOMEs.
                      ;;
                      ;; But here, on abnormal exit, we must clear
                      ;; the owner on the way to an ABORT restart.
                      ;;
                      ;; We need to use CAS because we might have
                      ;; had nested WITHOUT-CONTENTION, in which
                      ;; case another thread might have grabbed the
                      ;; Actor after the inner abnormal exit
                      ;; cleared the owner.
                      (release))
                    ))
              ;; else
              (go-around))
            )))
       (msg
        (with-become-disabled
          (apply beh-fn msg)))
       ))))

(defun do-without-contention (fn)
  (funcall self-beh 'contention-free fn))

(defmacro with-contention-free-semantics (fn-form)
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
  `(make-cf-closure
    (macrolet ((without-contention (&body body)
                 `(do-without-contention (lambda ()
                                           ,@body))))
      ,fn-form)))

