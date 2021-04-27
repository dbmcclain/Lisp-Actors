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


(in-package #:actors/base)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 2) #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------------------------------

(defgeneric send (obj &rest msg))
(defgeneric ask  (obj &rest msg))
(defgeneric get-property (obj key &optional default))
(defgeneric set-property (obj key value))
(defgeneric remove-property (obj key))

(defsetf get-property set-property)

;; ------------------------------------------------------

(declaim (inline current-actor))

(defvar *current-actor* nil)

(defun current-actor ()
  ;; each running thread will have its own version of this global
  ;; value. But, if non-nil, it points to the currently active Actor
  ;; running in that thread
  *current-actor*)

(define-symbol-macro self (current-actor))

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

(define-actor-class <runnable> (t)
  ;; Parent class of all Actors and Workers
  ((busy
    ;; when non-nil this Actor is either already enqueued for running,
    ;; or is running. We use a CONS cell for the flag for SMP CAS
    ;; This needs to be here, for both WORKER and ACTOR, since watchdog looks at
    ;; insertion time for ready queueu. (= (CDR (ACTOR-BUSY obj)))
    :reader    actor-busy
    :initarg   :busy))
  (:default-initargs
   :busy  (list nil)
   ))

(define-actor-class worker (<runnable>)
  ;; A Worker is simply a function that runs on an independent thread
  ;; in the thread pool. It does not have a message queue. It is not
  ;; an Actor, and cannot respond to external message SENDs.
  ((wrapper
    ;; contains a list whose CAR is a function and whose CDR is a list
    ;; of args, or contains NIL when nullified by terminate-actor
    :accessor worker-dispatch-wrapper
    :initarg :wrapper))
  (:default-initargs
   :wrapper (list #'lw:do-nothing)
   ))

(define-actor-class actor (<runnable>)
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
   (mbox
    ;; the Actor's message queue. SMP-safe
    :reader    actor-mailbox
    :initarg   :mailbox)
   (lock
    :reader actor-lock
    :initform (mp:make-lock))
   (behavior
    ;; points to the user code describing the behavior of this Actor.
    ;; This pointer is changed when the Actor performs a BECOME. Only
    ;; the Actor queries this slot so SMP safety not a concern.
    :accessor  actor-beh
    :initarg   :beh))
  (:default-initargs
   :beh        #'funcall
   :mailbox    (finger-tree:make-unshared-queue)
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

#|
(defun make-limited-actor (&key (mailbox-size 1)
                                behavior
                                properties)
  (make-instance 'actor
                 :mailbox    (make-instance 'limited-actor-mailbox
                                            :queue  (mp:make-mailbox
                                                     :size mailbox-size))
                 :beh        (or behavior #'funcall)
                 :properties properties))
|#

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

;; --------------------------------------------------------
;; Core SEND to Actors

;; Caution!! It is not safe to enable an Actor if its message queue
;; appears empty. It might still be runinng the last message
;; presented. We need to ensure single-thread semantics for all
;; Actors. That's why there is a busy-bit.
;;
;; That busy bit gets unset only after the Actor has completed message
;; processing and no more messages remain in its message queue.

(defun was-retired? (actor)
  ;; if was retired, also mark now active
  (sys:compare-and-swap (car (actor-busy actor)) nil t))

(defun retire (actor)
  ;; if not terminated, mark us retired
  (sys:compare-and-swap (car (actor-busy actor)) t nil))

;; ----------------------------------------------------------------
;; Toplevel Actor / Worker behavior

(defgeneric %run-actor (actor)
  #-:USING-MAC-GCD
  (:method ((worker worker))
   ;; Just run the form with which it was consructed
   (let ((form (worker-dispatch-wrapper worker)))
     (with-simple-restart (abort "Exit worker")
       (apply #'funcall form))))

  (:method ((*current-actor* actor))
   ;; An Actor loops on sent messages until no more are pending for
   ;; it.
   (let ((mbox  (actor-mailbox self))
         (busy  (actor-busy self))
         (lock  (actor-lock self)))
     (symbol-macrolet ((not-terminated? (eq t (car busy))))
       (unwind-protect
           (tagbody
            again
            (when not-terminated?
              (multiple-value-bind (msg ok)
                  (mp:with-lock (lock)
                    (finger-tree:popq mbox))
                (when ok
                  (apply #'dispatch-message *current-actor* msg)
                  (go again))
                )))
         (when not-terminated?
           (mp:with-lock (lock)
             (if (finger-tree:is-empty? mbox)
                 (retire self)
               (add-to-ready-queue self))
             ))
         )))))

;; -----------------------------------------------
;; Since these methods are called against (CURRENT-ACTOR) they can
;; only be called from within a currently active Actor.

(defun current-behavior ()
  (actor-beh (current-actor)))

(defun become (new-fn)
  ;; change behavior, returning old. If an Actor didn't call this, an
  ;; error will result.
  (shiftf (actor-beh (current-actor)) new-fn))

(defun self-call (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply (actor-beh (current-actor)) msg))

(defun self-dispatch (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply #'dispatch-message (current-actor) msg))

;; ---------------------------------------------------------
;; ASK Infrastructure...

(defvar *in-ask*        nil)
(defvar *whole-message* nil)

(declaim (inline in-ask-p whole-message))

(defun in-ask-p ()
  *in-ask*)

(defun whole-message ()
  *whole-message*)

(defun repeat-send (dest)
  (apply #'send dest *whole-message*))

(define-condition no-immediate-answer ()
  ())

(defun assemble-ask-message (reply-to &rest msg)
  (list* 'actors/internal-message:ask reply-to msg))

;; ---------------------------------------------------------
;; Central Actor message handling

(defun dispatch-message (actor &rest *whole-message*)
  (with-simple-restart (abort "Handle next message")
    (with-trampoline
      (if-let (handler (apply #'get-message-dispatch-handler actor *whole-message*))
          (funcall handler)
        ;; else - anything else is up to the programmer who constructed
        ;; this Actor, and possibly redirected by BECOME
        (apply #'self-call *whole-message*))
      )))

(defmethod get-message-dispatch-handler ((actor actor) &rest msg)
  ;; Compute a pre-user handler thunk based on the incoming message
  ;; pattern.  Make it into a METHOD so we can augment by Actor class
  ;; with :AROUND methods.  These methods will always be invoked if
  ;; necessary, despite user function replacement by BECOME.
  (um:tcase msg
    (actors/internal-message:continuation (fn &rest args)
       ;; Used for callbacks into the Actor
       (apply fn args))
    
    (actors/internal-message:send-sync (reply-to &rest sub-message)
       ;; tell sender we got the message before executing it
       (send reply-to t)
       (let ((*whole-message* sub-message))
         (apply #'self-call sub-message)))
      
    (actors/internal-message:ask (reply-to &rest sub-msg)
       ;; Intercept restartable queries to send back a response
       ;; from the following message, reflecting any errors back to
       ;; the caller.
       (let ((original-ask-message *whole-message*))
         (dynamic-wind
           (let ((*whole-message* original-ask-message)
                 (*in-ask*        t))
             (handler-case
                 (send reply-to
                       (capture-ans-or-exn
                         (um:proceed
                          (apply #'self-call sub-msg))))
               
               (no-immediate-answer ())
               )))))
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

#|
(defun spawn-limited (fn &rest args)
  (let ((actor (make-limited-actor :behavior fn)))
    (apply 'send actor args)
    actor))
|#

;; ----------------------------------------------
;; WORKER is just a function that executes on an Executive pool
;; thread.
;;
;; WORKER is intended as a lightweight vehicle to perform non
;; Actor-centric duties, e.g., blocking I/O. It will see a null value
;; from (CURRENT-ACTOR), just like non-Actor code running on any other
;; thread. It takes advantage of the Executive thread pool to launch
;; faster than fully constructing a thread to run them.

#-:USING-MAC-GCD
(defun spawn-worker (fn &rest args)
  (let ((worker (make-instance 'worker
                               :busy    (list t)
                               :wrapper (if (consp fn)
                                            fn
                                          (cons fn args)))))
    (add-to-ready-queue worker)
    worker))

#+:USING-MAC-GCD
(defgeneric spawn-worker (fn &rest args)
  (:method ((fn function) &rest args)
   (apply #'run-worker-direct fn args)
   fn)
  (:method ((fn symbol) &rest args)
   (apply #'spawn-worker (symbol-function fn) args)))

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
  (mp:with-lock ((actor-lock actor))
    (finger-tree:addq (actor-mailbox actor) msg)
    (when (was-retired? actor)
      (add-to-ready-queue actor)))
  t)
  
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

;; ----------------------------------------------
;; ASK - RPC with an Actor. Any errors incurred during the message
;; handling are reflected back to the caller

(defmethod ask ((actor actor) &rest message)
  (if (eq actor (current-actor))
      (apply 'self-call message)
    ;; else - blocking synchronous ASK with mailbox
    (=wait ((ans)
            :timeout *timeout*
            :errorp  t)
        (apply 'send actor (apply 'assemble-ask-message =wait-cont message))
      (recover-ans-or-exn ans))))

(defmethod ask ((fn function) &rest message)
  (apply fn message))

(defmethod ask ((sym symbol) &rest message)
  (let (actor)
    (cond
     ((setf actor (find-actor sym))
      (apply 'ask actor message))
     
     ((fboundp sym)
      (apply sym message))
     
     (t
      (call-next-method))
     )))

(defmethod ask (obj &rest message)
  (let ((mfn (car message)))
    (if (funcallable-p mfn)
        (apply mfn obj (cdr message))
      ;; else
      (error 'invalid-send-target :target obj)
      )))

;; ----------------------------------------------------------------
;; Non-blocking ASK for use in =BIND

(=defgeneric =ask (actor &rest message)

  (:method ((actor actor) &rest message)
   (if (eq actor (current-actor))
     (=values (apply 'self-call message)))
    ;; else - non-blocking asynchronous ASK
    (=bind (ans)
        (apply 'send actor (apply 'assemble-ask-message =bind-cont message))
      (=values (recover-ans-or-exn ans))))

  (:method ((fn function) &rest message)
   (=values (apply fn message)))

  (:method ((sym symbol) &rest message)
   (let (actor)
     (cond
      ((setf actor (find-actor sym))
       (=apply '=ask actor message))
      
      ((fboundp sym)
       (=values (apply sym message)))
      
      (t
       (call-next-method))
      )))

  (:method (obj &rest message)
   (let ((mfn (car message)))
     (if (funcallable-p mfn)
         (=values (apply mfn obj (cdr message)))
       ;; else
       (error 'invalid-send-target :target obj)))))

;; ------------------------------------------------------
;; Synchronous Send?
;;

(defgeneric send-sync (actor &rest message)
  (:method ((actor actor) &rest message)
   (if (eq actor (current-actor))
       (apply 'self-call message)
     (=wait ((&rest _)
             :timeout *timeout*
             :errorp  t)
         (apply 'send actor 'actors/internal-message:send-sync =wait-cont message)
       (declare (ignore _)))
     )))
