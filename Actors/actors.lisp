;; Actors.lisp -- An implementation of Actors - single thread
;; semantics across multithreaded systems
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


(in-package #:actors.base)

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

;; ----------------------------------------------------
;; An Actor mailbox contains a priority queue holding newly delivered
;; messages, plus a list of previously stashed messages in arrival
;; order. Stashed messages will be read before additional enqueued
;; messages. Message may become stashed, e.g., during operation of a
;; selective RECV.

(defclass actor-mailbox ()
  ((lock   :reader   actor-mailbox-lock
           :initform (mp:make-lock))
   (msgs   :reader   actor-msgs
           :initarg  %mailbox)
   (replay :accessor actor-message-replay
           :initform nil))
  (:default-initargs
   %mailbox (hcl:make-unlocked-queue)))

(defclass limited-actor-mailbox (actor-mailbox)
  ()
  (:default-initargs
   %mailbox (mp:make-mailbox)
   ))

;; -----------------------------------------------------

(defmacro with-actor-mailbox-locked (mbox &body body)
  `(mp:with-lock ((actor-mailbox-lock ,mbox))
     ,@body))

(defgeneric next-message (mbox)
  (:method ((mbox actor-mailbox))
   (if (actor-message-replay mbox)
       (values (pop (actor-message-replay mbox)) t)
     ;; else
     (with-actor-mailbox-locked mbox
       (let ((queue (actor-msgs mbox)))
         (when (hcl:unlocked-queue-ready queue)
           (values (hcl:unlocked-queue-read queue) t))
         ))))
  (:method ((mbox limited-actor-mailbox))
   (if (actor-message-replay mbox)
       (values (pop (actor-message-replay mbox)) t)
     ;; else
     (mp:mailbox-read (actor-msgs mbox) nil 0))))
  
(defgeneric unsafe-send-message (mbox msg)
  ;; always called from within a locked condition,
  ;; using that lock, makes this "safe"
  (:method ((mbox actor-mailbox) msg)
   ;; always succeeds
   (hcl:unlocked-queue-send (actor-msgs mbox) msg)
   t)
  (:method ((mbox limited-actor-mailbox) msg)
   ;; might not succeed
   (mp:mailbox-send-limited (actor-msgs mbox) msg 1 *timeout*)))

(defgeneric unsafe-mailbox-not-empty-p (mbox)
  ;; called only under lock by Actor itself
  (:method ((mbox actor-mailbox))
   (or (actor-message-replay mbox)
       (hcl:unlocked-queue-ready (actor-msgs mbox))))
  
  (:method ((mbox limited-actor-mailbox))
   (or (actor-message-replay mbox)
       (mp:mailbox-not-empty-p (actor-msgs mbox)))))

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
     (:metaclass clos:funcallable-standard-class)))

#+:LISPWORKS
(editor:indent-like 'define-actor-class 'defclass)

(define-actor-class <runnable> (t)
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
  ((wrapper
    ;; contains a list whose CAR is a function and whose CDR is a list
    ;; of args, or contains NIL when nullified by terminate-actor
    :accessor worker-dispatch-wrapper
    :initarg :wrapper))
  (:default-initargs
   :wrapper (list 'lw:do-nothing)
   ))

(define-actor-class actor (<runnable>)
  ((properties-ref
    ;; globally visible properties on this Actor. SMP-safe methods are
    ;; provided for getting / setting
    :reader    actor-properties-ref
    :initarg   :properties-ref)
   (mbox
    ;; the Actor's message queue. SMP-safe
    :reader    actor-mailbox
    :initform  (make-instance 'actor-mailbox))
   (user-fn
    ;; points to the user code describing the behavior of this Actor.
    ;; This pointer is changed when the Actor performs a BECOME. Only
    ;; the Actor queries this slot so SMP safety not a concern.
    :accessor  actor-user-fn
    :initarg   :user-fn))
  (:default-initargs
   :properties-ref (ref:ref (maps:empty))
   :user-fn        #'funcall
   ))

(define-actor-class limited-actor (actor)
  ((mbox
    :reader   actor-mailbox
    :initform (make-instance 'limited-actor-mailbox))
   ))

(define-actor-class actor-as-worker (worker actor)
  ())

(define-actor-class limited-actor-as-worker (worker limited-actor)
  ())

;; -----------------------------------------------------
;; These methods can be called from any thread. SMP safe.

(defmethod initialize-instance :after ((actor actor) &key properties &allow-other-keys)
  (um:wr (slot-value actor 'properties-ref)
         (maps:add-plist (maps:empty) properties))
  (clos:set-funcallable-instance-function actor
                                          (lambda (&rest args)
                                            (if (eq actor (current-actor))
                                                (apply #'self-call args)
                                              (apply #'send actor args)))))

(defun make-actor (&optional fn &key properties)
  (make-instance 'actor
                 :user-fn    (or fn #'funcall)
                 :properties properties))

(defun make-limited-actor (&optional fn &key properties)
  (make-instance 'limited-actor
                 :user-fn    (or fn #'funcall)
                 :properties properties))

(defmethod get-property ((actor actor) key &optional default)
  ;; SMP-safe
  (maps:find (um:rd (actor-properties-ref actor)) key default))

(defmethod set-property ((actor actor) key value)
  ;; SMP-safe
  (um:rmw (actor-properties-ref actor) (lambda (map)
                                         (maps:add map key value)))
  value)

(defmethod remove-property ((actor actor) key)
  (um:rmw (actor-properties-ref actor) (lambda (map)
                                         (maps:remove map key))))

;; --------------------------------------------------------

(defmethod send ((actor actor) &rest msg)
  ;; send a message to an Actor and possibly activate it if not
  ;; already running. SMP-safe
  (let ((mbox (actor-mailbox actor)))
    ;; need to lock the mailbox first to avoid race condition with
    ;; execution unwind
    (with-actor-mailbox-locked mbox
      (when (unsafe-send-message mbox msg)
        ;; If succeeded, notify actor of message
        (notify-actor actor)
        t)))) ;; indicate success

(defun notify-actor (actor)
  ;; Mark busy, if not already marked. And if it wasn't already
  ;; marked, place it into the ready queue.
  (when (sys:compare-and-swap (car (actor-busy actor)) nil t)
    ;; The Ready Queue contains Actors awaiting a runtime
    ;; thread. When dequeued, they will be sent to RUN by an
    ;; executive thread.
    (add-to-ready-queue actor)))

;; ----------------------------------------------------------------

(defun %basic-run-worker (worker)
  (let ((form (worker-dispatch-wrapper worker)))
    (apply (car form) (cdr form))))

(defun %basic-run-actor (actor)
  (let ((mbox  (actor-mailbox actor))
        (busy  (actor-busy actor)))
    (unwind-protect
        (tagbody
         again
         (when (eq t (car busy)) ;; not terminated?
           (multiple-value-bind (msg ok)
               (next-message mbox)
             (when ok  ;; until no more messages waiting
               (with-simple-restart (abort "Run same Actor with next message")
                 (apply #'dispatch-message msg))
               (go again)))))
      ;; unwind clause
      (maybe-add-to-ready-queue actor))
    ))

(defun %basic-run-actor-as-worker (actor class)
  (%basic-run-worker actor)
  ;; first call treats as worker, thereafter as actor
  (change-class actor class)
  (%basic-run-actor actor))

(defun maybe-add-to-ready-queue (actor)
  (let ((mbox (actor-mailbox actor))
        (busy (actor-busy actor)))
    (when (eq t (car busy)) ;; not terminated
      ;; <-- a message could have arrived here, but would have
      ;; failed to enqueue the Actor.  So we double check before
      ;; clearing the busy mark.
      (with-actor-mailbox-locked mbox
        (cond ((unsafe-mailbox-not-empty-p mbox)
               ;; leave marked busy and put back on ready queue
               (add-to-ready-queue actor))
              
              (t
               ;; might have been terminated - so conditionally unmark
               (sys:compare-and-swap (car busy) t nil))
              )))))

;; ----------------------------------------------------------------

(defgeneric %run-actor (actor)
  (:method ((worker worker))
   (%basic-run-worker worker))
  
  (:method ((*current-actor* actor))
   (declare (special *current-actor*))
   (%basic-run-actor *current-actor*))

  (:method ((*current-actor* actor-as-worker))
   (declare (special *current-actor*))
   (%basic-run-actor-as-worker *current-actor* 'actor))

  (:method ((*current-actor* limited-actor-as-worker))
   (declare (special *current-actor*))
   (%basic-run-actor-as-worker *current-actor* 'limited-actor)))

;; -----------------------------------------------
;; Since these methods are called against (CURRENT-ACTOR) they can
;; only be called from within a currently active Actor.

(defun become (new-fn)
  ;; change behavior, returning old. If an Actor didn't call this, an
  ;; error will result.
  (shiftf (actor-user-fn (current-actor)) new-fn))

(defun self-call (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply (actor-user-fn (current-actor)) msg))

;; ---------------------------------------------------------
;; ASK Infrastructure...

(defvar *in-ask*        nil)
(defvar *whole-message* nil)

(declaim (inline in-ask-p whole-message))

(defun in-ask-p ()
  *in-ask*)

(defun whole-message ()
  *whole-message*)

(define-condition no-immediate-answer ()
  ())

(defun assemble-ask-message (reply-to &rest msg)
  (list* 'actor-internal-message:ask reply-to msg))

;; ---------------------------------------------------------

(defun dispatch-message (&rest *whole-message*)
  (with-trampoline
    (um:dcase *whole-message*
      (actor-internal-message:continuation (fn &rest args)
                                           ;; Used for callbacks into the Actor
                                           (apply fn args))
      
      (actor-internal-message:send-sync (reply-to &rest sub-message)
                                        (send reply-to t)
                                        (apply #'self-call sub-message))
      
      (actor-internal-message:ask (reply-to &rest sub-msg)
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
      
      (t (&rest msg)
         ;; anything else is up to the programmer who constructed
         ;; this Actor
         (apply #'self-call msg))
      )))

;; ---------------------------------------------
;; SPAWN a new Actor on a function with args
;; This is the way we start all Actors in the system.

(defun spawn (fn &rest args)
  (let ((actor (make-actor fn)))
    (apply 'send actor args)
    actor))

(defun spawn-limited (fn &rest args)
  (let ((actor (make-limited-actor fn)))
    (apply 'send actor args)
    actor))

;; ----------------------------------------------
;; WORKER and ACTOR-AS-WORKER exist as separate classes so that
;; helper functions can be launched, yet disabled if necessary,
;; before the Executive pool runs them.
;;
;; WORKER is just a function that executes on an Executive pool
;; thread. ACTOR-AS-WORKER can accept messages, but performs an
;; initial function just like WORKER, and thereafter becomes a normal
;; ACTOR.
;;
;; WORKER is intended as a lightweight vehicle to perform non
;; Actor-centric duties, e.g., blocking I/O. It will see a null value
;; from (CURRENT-ACTOR), just like non-Actor code running on any other
;; thread. They take advantage of the Executive thread pool to launch
;; faster than constructing a full thread to run them.
;;
;; ACTOR-AS-WORKER will see itself as (CURRENT-ACTOR) and can run full
;; Actor-centric facilities, if desired. In order to respond to
;; anything other than system messages, it either needs to perform a
;; RECV, or else a BECOME to enable message handling. It defaults to
;; DO-NOTHING with messages.
;;
;; When in doubt, lauch as either ACTOR or ACTOR-AS-WORKER.

(defun spawn-actor/worker (class fn &rest args)
  (let ((worker (make-instance class
                               :busy    (list t)
                               :wrapper (if (consp fn)
                                            fn
                                          (cons fn args)))))
    (add-to-ready-queue worker)
    worker))
  
(defun spawn-worker (fn &rest args)
  (apply 'spawn-actor/worker 'worker fn args))

(defun spawn-actor-as-worker (fn &rest args)
  (apply 'spawn-actor/worker 'actor-as-worker fn args))

(defun spawn-limited-actor-as-worker (fn &rest args)
  (apply 'spawn-actor/worker 'limited-actor-as-worker fn args))

;; ------------------------------------------
;; Sends directed to mailboxes, functions, etc.

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

(defun funcallable-p (obj)
  (or (functionp obj)
      (and (symbolp obj)
           (fboundp obj))))
 
(define-condition invalid-send-target (simple-error)
  ((target :initarg :target :initform nil :accessor target))
  (:documentation "An error indicating a target of SEND that cannot be resolved into something valid.")
  (:report (lambda (condition stream)
	     (format stream "~%Invalid SEND target: ~&  ~S" (target condition)))))

(defmethod send (other-obj &rest message)
  ;; E.g., Smalltalk'ish (send 7 'truncate 4)
  (let ((mfn (car message)))
    (if (funcallable-p mfn)
        (apply mfn other-obj (cdr message))
      ;; else
      (error 'invalid-send-target :target other-obj))
    ))

;; ----------------------------------------------
;; ASK - RPC with an Actor. Any errors incurred during the message
;; handling are reflected back to the caller

(defmethod ask ((actor actor) &rest message)
  (if (eq actor (current-actor))
      (apply 'self-call message)
    ;; else - blocking synchronous ASK with mailbox
    (=wait ((ans) :timeout *timeout* :errorp t)
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
     (let ((mb (mp:make-mailbox)))
       (apply 'send actor 'actor-internal-message:send-sync mb message)
       (mp:mailbox-read mb))
     )))
