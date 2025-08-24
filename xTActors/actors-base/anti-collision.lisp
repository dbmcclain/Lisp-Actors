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

#|
(defun do-without-contention (guard fn)
  (symbol-macrolet ((gcar  (car (the cons guard))))
    (let ((me  (mpc:get-current-process))
          (sav gcar))
    (cond ((or (eq me sav)
               (mpc:compare-and-swap gcar nil me))
           (if sav
               (funcall fn)
             (unwind-protect
                 (funcall fn)
               (setf gcar nil))))
          
          (t
           (%send-to-pool (msg self self-msg))
           (abort))
          ))))
|#

(defun do-become (fn)
  (become fn))

(defun do-become-sink ()
  (become-sink))

(defun bad-become ()
  (error "Unguarded BECOME in contention-free semantics"))

#|
(defmacro with-contention-free-semantics (&body body)
  ;; Should be used at the level of closure vars for the behavior
  ;; which follows, e.g, after DEFUN and before ALAMBDA.
  (let  ((cfguard  (gensym)))
    `(let ((,cfguard  (list nil)))
       (macrolet ((become (fn)
                    (declare (ignore fn))
                    (bad-become))
                  (become-sink ()
                    (bad-become))
                  (without-contention (&body body)
                    `(do-without-contention ,',cfguard
                                            (lambda ()
                                              (macrolet ((become (fn)
                                                           `(do-become ,fn))
                                                         (become-sink ()
                                                           `(do-become-sink)))
                                                ,@body)))))
         ,@body))
    ))
|#
#|
(defun tst (x)
  (with-contention-free-semantics
    (alambda
     ((cust :ok)
      (without-contention
       (become (sink))))
     )))
|#

(defclass cf-closure ()
  ((beh-fn    :reader cf-closure-beh-fn  :initarg :beh)
   (cell      :reader cf-closure-cell    :initform (list nil)))
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((cf cf-closure) &key beh (proc nil) &allow-other-keys)
  (setf (car (cf-closure-cell cf)) proc)
  (clos:set-funcallable-instance-function cf beh))

(defmethod cf-closure-proc ((beh cf-closure))
  (let ((me      (mpc:get-current-process))
        (sav-beh (make-instance 'cf-closure
                                :beh (cf-closure-beh-fn beh))))
    (symbol-macrolet ((proc (car (cf-closure-cell beh))))
      (when (mpc:compare-and-swap proc nil me)
        (become sav-beh))
      (values me proc sav-beh))
    ))
         
(defun do-without-contention (fn)
  (symbol-macrolet ((beh  (actor-beh (the actor *self*))))
    (multiple-value-bind (me proc sav-beh)
        (cf-closure-proc *self-beh*)
      (cond ((eq me proc)
             (handler-bind
                 ((error (lambda (c)
                           (mpc:compare-and-swap beh *self-beh* sav-beh)
                           (error c))
                         ))
               (funcall fn)))

            (t
             (%send-to-pool (msg *self* *self-msg*))
             (abort))
            ))
    ))

(defmacro with-contention-free-semantics (&body body)
  ;; Should be used at the level of closure vars for the behavior
  ;; which follows, e.g, after DEFUN and before ALAMBDA.
  `(macrolet ((become (fn)
                (declare (ignore fn))
                (bad-become))
              (become-sink ()
                (bad-become))
              (without-contention (&body body)
                `(do-without-contention (lambda ()
                                          (macrolet ((become (fn)
                                                       `(do-become ,fn))
                                                     (become-sink ()
                                                       `(do-become-sink)))
                                            ,@body)))))
     (make-instance 'cf-closure :beh ,@body)))
