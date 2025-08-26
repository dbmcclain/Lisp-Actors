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
  (when *self*
    ;; pointless unless we are in an Actor.
    (%send-to-pool (msg *self* *self-msg*))
    (abort)))

(defun make-cf-closure (beh-fn)
  (let ((proc  (list nil)))
    (alambda
     (('contention-free fn)
      (with-become-enabled
        (symbol-macrolet ((owner  (car (the cons proc))))
          (let ((me     (mpc:get-current-process))
                (holder owner))
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
            (when (and (null holder)
                       (mpc:compare-and-swap owner nil me))
              (setf holder me)
              ;; this BECOME resets in absence of other BECOMEs
              (become (make-cf-closure beh-fn)))
            (cond ((eq me holder)
                   (let ((aborting t))
                     (unwind-protect
                         (progn
                           (funcall fn)
                           (setf aborting nil))
                       (when aborting
                         (setf owner nil))
                       )))
                  (t
                   (go-around))
                  )))))
     (msg
      (with-become-disabled
        (apply beh-fn msg)))
     )))

(defun do-without-contention (fn)
  (funcall *self-beh* 'contention-free fn))

(defmacro with-contention-free-semantics (fn-form)
  ;; Should wrap a behavior function.
  ;; WITHOUT-CONTENTION available only within WITH-CONTENTION-FREE-SEMANTICS.
  ;; All BECOME clauses should be wrapped inside of WITHOUT-CONTENTION.
  `(make-cf-closure
    (macrolet ((without-contention (&body body)
                 `(do-without-contention (lambda ()
                                           ,@body))))
      ,fn-form)))

