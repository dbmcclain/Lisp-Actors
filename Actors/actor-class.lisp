;; actor-class.lisp -- A definer of class instances with private
;; single-thread state and a controlling Actor
;;
;; DM 09/20
;; ------------------------------------------------------------------------

(in-package :actors/base)

;; ----------------------------------------------------------------------------

(defun inject (actor fn &rest args)
  ;; dispatch as a continuation to Actor, bypassing message filtering
  (if (eq actor (current-actor))
      (apply fn args)
    ;; else
    (apply 'send actor 'actors/internal-message:continuation fn args)))

(defun exec (actor fn &rest args)
  ;; Dispatch as a normal message, and like SELF-CALL, subjects thunk
  ;; to Actor message filtering. But if already running, acts like a
  ;; continuation or direct call, since we will have already gone
  ;; through message filtering.
  ;;
  ;; If you want ordered delivery and filtering when called from Self,
  ;; then use a SEND instead of calling directly.
  (if (eq actor (current-actor))
      (apply fn args)
    ;; else
    (apply 'send actor fn args)))

(defmacro perform-in-actor (actor &body body)
  `(exec ,actor (lambda ()
                  ,@body)))

(defmacro inject-into-actor (actor &body body)
  `(inject ,actor (lambda ()
                    ,@body)))

(defmacro query-actor (actor &body body)
  `(ask ,actor (lambda ()
                   ,@body)))

(defmacro with-as-current-actor (actor &body body)
  `(let ((*current-actor* ,actor))
     ,@body))

(define-actor-class hoare-monitor ()
  ())

#+:LISPWORKS
(progn
  (editor:setup-indent "perform-in-actor" 1)
  (editor:setup-indent "inject-into-actor" 1)
  (editor:setup-indent "with-as-current-actor" 1))

(define-actor-class printer ()
  ())

(defvar *printer* (make-instance 'printer))

(defmacro prt (&body body)
  `(perform-in-actor *printer*
     ,@body))

(defun pr (&rest args)
  (prt
    (stream:with-stream-output-lock *standard-output*
      (if (cdr args)
          (print args)
        (print (car args)))
      )))