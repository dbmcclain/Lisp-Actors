;; Actors.lisp -- An implementation of Actors
;;
;; Single thread semantics across multithreaded and SMP systems
;;
;; DM/RAL  12/17
;; -----------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#


(in-package #:stactors/base)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 2) #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------------------------------

(defgeneric send (obj &rest msg))
;; (defgeneric ask  (obj &rest msg))
(defgeneric get-property (obj key &optional default))
(defgeneric set-property (obj key value))
(defgeneric remove-property (obj key))

(defsetf get-property set-property)

;; ------------------------------------------------------

(defvar *self* nil)

(define-symbol-macro self     *self*)
(define-symbol-macro self-beh (actor-beh self))

;; -----------------------------------------------------
;; Version 3... make the Actor's internal state more readily visible
;; to debuggers. Use a CLOS object instead of closed over lambda vars.

(defmacro define-actor-class (name superclasses slots &rest options)
  ;; Superclass defaults to 'ACTOR
  `(defclass ,name ,(cond
                     ((equalp superclasses '(t)) nil)
                     ((null superclasses)        '(actor))
                     (t                          superclasses))
     ,slots
     ,@options
     ))

#+:LISPWORKS
(editor:indent-like 'define-actor-class 'defclass)

;; -----------------------------------------------------
;; Actor class heirarchy

(define-actor-class actor (t)
  ;; Actor with indefinite-size message queue. A kind of closure
  ;; object that responds to messages sent to it. Actors enforce
  ;; single-thread semantics on all of the code that runs under their
  ;; supervision - including callback continuations.
  ;;
  ;; By default, it expects messages to be executable functions with
  ;; args. Users can specify other behavior at construction time.
  ((properties
    ;; globally visible properties on this Actor. SMP-safe methods are
    ;; provided for getting / setting
    :reader    actor-properties
    :initform  (maps:make-shared-map))
   (behavior
    ;; points to the user code describing the behavior of this Actor.
    ;; This pointer is changed when the Actor performs a BECOME. Only
    ;; the Actor queries this slot so SMP safety not a concern.
    :accessor  actor-beh
    :initarg   :beh))
  (:default-initargs
   :beh #'funcall
   ))

;; -----------------------------------------------------
;; Actor construction

(defmethod initialize-instance :after ((actor actor) &key properties &allow-other-keys)
  (when properties
    (maps:add-plist (actor-properties actor) properties)))

(defun make-actor (&optional fn &key properties)
  (make-instance 'actor
                 :beh        (or fn #'funcall)
                 :properties properties))

;; -----------------------------------------------------
;; These methods can be called from any thread. SMP safe.

(defmethod get-property ((actor actor) key &optional default)
  ;; SMP-safe
  (maps:find (actor-properties actor) key default))

(defmethod set-property ((actor actor) key value)
  ;; SMP-safe
  (maps:add (actor-properties actor) key value)
  value)

(defmethod remove-property ((actor actor) key)
  (maps:remove (actor-properties actor) key))

;; ----------------------------------------------------------------
;; Toplevel Actor / Worker behavior

(defvar *event-lock*   (mp:make-lock))
(defvar *event-signal* (mp:make-condition-variable))
(defvar *event-queue*  (finger-tree:make-unshared-queue))

(defvar *run-thread* nil)

(defun get-next-event ()
  (mp:with-lock (*event-lock*)
    (when (finger-tree:is-empty? *event-queue*)
      (mp:condition-variable-wait *event-signal* *event-lock*))
    (finger-tree:popq *event-queue*)))

(defun run ()
  (when (sys:compare-and-swap *run-thread* nil t)
    (setf *run-thread*
          (mp:process-run-function
           "MicroActor Run"
           ()
           (lambda ()
             (unwind-protect
                 (loop
                  (destructuring-bind (*self* . message)
                      (get-next-event)
                    (with-simple-restart (abort "Process next event")
                      (apply #'dispatch-message self message))
                    ))
               (setf *run-thread* nil)))
           ))))
(run)

(define-condition no-immediate-answer ()
  ())

;; -----------------------------------------------
;; Since these methods are called against self they can
;; only be called from within a currently active Actor.

(defun become (new-fn)
  ;; change behavior, returning old. If an Actor didn't call this, an
  ;; error will result.
  (shiftf self-beh new-fn))

(defun self-call (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply self-beh msg))

(defun self-dispatch (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply #'dispatch-message self msg))

;; ---------------------------------------------------------
;; ASK Infrastructure...

(defvar *whole-message* nil)

(declaim (inline whole-message))

(defun whole-message ()
  *whole-message*)

(defun repeat-send (dest)
  (apply #'send dest *whole-message*))

;; ---------------------------------------------------------
;; Central Actor message handling

(defun dispatch-message (actor &rest *whole-message*)
  (with-trampoline
    (if-let (handler (apply #'get-message-dispatch-handler actor *whole-message*))
        (funcall handler)
      ;; else - anything else is up to the programmer who constructed
      ;; this Actor, and possibly redirected by BECOME
      (apply #'self-call *whole-message*))
    ))

(defmethod get-message-dispatch-handler ((actor actor) &rest msg)
  ;; Compute a pre-user handler thunk based on the incoming message
  ;; pattern.  Make it into a METHOD so we can augment by Actor class
  ;; with :AROUND methods.  These methods will always be invoked if
  ;; necessary, despite user function replacement by BECOME.
  (um:tcase msg
    (actors/internal-message:continuation (fn &rest args)
       ;; Used for callbacks into the Actor
       (apply fn args))    
    ))

;; ---------------------------------------------
;; SPAWN a new Actor using a function with args
;;
;; This is a less common way of starting an Actor. Most often we
;; MAKE-ACTOR, then SEND messages to it. But this is a shortcut for
;; that combo on the first message send.
;;
;; More commonly we only SPAWN-WORKER.

(defun spawn (fn &rest args)
  (let ((actor (make-actor fn)))
    (apply 'send actor args)
    actor))

;; ----------------------------------------------
;; WORKER is just a function that executes on an Executive pool
;; thread.
;;
;; WORKER is intended as a lightweight vehicle to perform non
;; Actor-centric duties, e.g., blocking I/O. It will see a null value
;; from self, just like non-Actor code running on any other
;; thread. It takes advantage of the Executive thread pool to launch
;; faster than fully constructing a thread to run them.

(defun spawn-worker (fn &rest args)
  (apply #'send (make-instance 'actor) fn args))

(defmacro with-worker (bindings &body body)
  (let ((args (mapcar #'first bindings))
        (vals (mapcar #'second bindings)))
    `(spawn-worker (lambda ,args
                     ,@body) ,@vals)
    ))

#+:LISPWORKS
(editor:setup-indent "with-worker" 1)

;; ------------------------------------------
;; Sends directed to mailboxes, functions, etc.

(defmethod send ((actor actor) &rest msg)
  (mp:with-lock (*event-lock*)
    (finger-tree:addq *event-queue* (cons actor msg))
    (mp:condition-variable-signal *event-signal*)))
  
(defmethod send ((mbox mp:mailbox) &rest message)
  (mp:mailbox-send mbox message))

(defmethod send ((fn function) &rest message)
  (apply fn message))

(defmethod send ((sym symbol) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor sym))
      (apply 'send actor message))
     
     ((fboundp sym)
      (apply sym message))
     
     (t
      (call-next-method))
     )))

(defmethod send (other-obj &rest message)
  ;; E.g., Smalltalk'ish (send 7 'truncate 4)
  (let ((mfn (car message)))
    (if (funcallable-p mfn)
        (apply mfn other-obj (cdr message))
      ;; else
      (error 'invalid-send-target :target other-obj))
    ))

(defun funcallable-p (obj)
  (or (functionp obj)
      (and (symbolp obj)
           (fboundp obj))))
 
(define-condition invalid-send-target (simple-error)
  ((target :initarg :target :initform nil :accessor target))
  (:documentation "An error indicating a target of SEND that cannot be resolved into something valid.")
  (:report (lambda (condition stream)
	     (format stream "~%Invalid SEND target: ~&  ~S" (target condition)))))

