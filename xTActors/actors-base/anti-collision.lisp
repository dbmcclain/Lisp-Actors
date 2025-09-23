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

(defvar *become-bak*  nil)

(defun do-without-contention (guard clos)
  (declare (cons guard)
           (function clos))
  (symbol-macrolet ((owner  (car (the cons guard))))
    (let ((*become-hook* *become-bak*)
          (me  (mpc:get-current-process)))
          
      (flet ((go-around ()
               (%send-to-pool (msg self self-msg))
               (abort))
             (acquire ()
               (mpc:compare-and-swap owner nil me))
             (release (&rest ignored)
               (declare (ignore ignored))
               (mpc:compare-and-swap owner me nil)))
        (declare (dynamic-extent #'go-around #'acquire))
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
          (send (create #'release)))
        (if (eq me owner)
            (handler-bind
                ((error #'release))
              (funcall clos))
          ;; else, Re-enqueue our message for later delivery,
          ;; and go process the next available message. This
          ;; drops all pending BECOME and SEND.
          (go-around))
        ))))

(defun do-without-become (beh-fn msg)
  (flet ((bad-become (&rest ignored)
           (declare (ignore ignored))
           (error "Unguarded BECOME in contention-free semantics")))
    (let ((*become-bak*  *become-hook*)
          (*become-hook* #'bad-become))
      (apply beh-fn msg))))
  
(defmacro with-contention-free-semantics (beh-fn-form)
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
  (um:with-unique-names (g!guard g!msg g!body)
    `(let ((,g!guard (list nil)))
       (lambda (&rest ,g!msg)
         (do-without-become
          (macrolet ((without-contention (&body ,g!body)
                       `(do-without-contention ,',g!guard (lambda ()
                                                            ,@,g!body))))
            ,beh-fn-form)
          ,g!msg)))
    ))

