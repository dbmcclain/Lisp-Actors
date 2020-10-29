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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            actors.executives:add-to-ready-queue

            actors.directory:find-actor

            timeout:*timeout*

            cps:=wait
            cps:=wait-cont
            cps:=defun
            cps:=defgeneric
            cps:=bind
            cps:=bind-cont
            cps:=values
            cps:=apply
            )))

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
   %mailbox (mp:make-mailbox :size 1)
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

(defclass <runnable> ()
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

(defclass worker (<runnable>)
  ((wrapper
    ;; contains a list whose CAR is a function and whose CDR is a list
    ;; of args, or contains NIL when nullified by terminate-actor
    :accessor worker-dispatch-wrapper
    :initarg :wrapper))
  (:default-initargs
   :wrapper (list 'lw:do-nothing)
   ))

(defclass actor (<runnable>)
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

(defclass limited-actor (actor)
  ((mbox
    :reader   actor-mailbox
    :initform (make-instance 'limited-actor-mailbox))
   ))

(defclass actor-as-worker (worker actor)
  ())

(defclass limited-actor-as-worker (worker limited-actor)
  ())

;; -----------------------------------------------------
;; These methods can be called from any thread. SMP safe.

(defmethod initialize-instance :after ((actor actor) &key properties &allow-other-keys)
  (setf (ref:val (slot-value actor 'properties-ref))
        (maps:add-plist (maps:empty) properties)))

(defun make-actor (fn &key
                      properties)
  (make-instance 'actor
                 :user-fn    fn
                 :properties properties))

(defun make-limited-actor (fn &key
                              properties)
  (make-instance 'limited-actor
                 :user-fn    fn
                 :properties properties))

(defmethod get-property ((actor actor) key &optional default)
  ;; SMP-safe
  (maps:find (ref:val (actor-properties-ref actor)) key default))

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
        (loop while (eq t (car busy)) ;; not terminated?
              for (msg ok) = (multiple-value-list (next-message mbox)) ;; until no more messsages waiting
              while ok
              do
              (with-simple-restart (abort "Run same Actor with next message")
                (apply 'dispatch-message actor msg)))
      ;; -- the following is the unwind clause --
      (maybe-add-to-ready-queue actor))
    ))

(defun %basic-run-actor-as-worker (actor)
  (%basic-run-worker actor)
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
   ;; first call treats as worker, thereafter as actor
   (change-class *current-actor* 'actor)
   (%basic-run-actor-as-worker *current-actor*))

  (:method ((*current-actor* limited-actor-as-worker))
   (declare (special *current-actor*))
   ;; first call treats as worker, thereafter as actor
   (change-class *current-actor* 'limited-actor)
   (%basic-run-actor-as-worker *current-actor*))
  )

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

(defun try-asking (fn args whole-msg)
  (handler-case
      ;; must *NOT* use =HANDLER-CASE here
      (let ((*in-ask*  whole-msg))
        (send (cadr whole-msg)
              (mcapture-ans-or-exn
                (apply fn args))))

    (no-immediate-answer ())
    ))

(define-condition try-again ()
  ((retry-fn  :reader retry-fn :initarg :fn)))

(defun #1=from-the-top (fn)
  ;; re-entering from-the-top with a TRY-AGAIN also unwinds all the
  ;; dynamic handlers in effect, forcing their reconstruction beneath
  ;; the call to the retry FN.
  (loop
   (handler-case
       (return-from #1# (funcall fn))
     (try-again (cx)
       (setf fn (retry-fn cx)))
     )))

(defun assemble-ask-message (reply-to &rest msg)
  (list* 'actor-internal-message:ask reply-to msg))

;; ---------------------------------------------------------

(defgeneric dispatch-message (obj &rest msg)
  (:method ((*current-actor* actor) &rest *whole-message*)
   (declare (special *current-actor* *whole-message*))
   ;; recoded dbm 8/20 - use direct cond instead of dlambda, dcase, for speedup
   (let ((sel (car *whole-message*)))
     (cond
      ((eq sel 'actor-internal-message:continuation)
       ;; Used for callbacks into the Actor
       (destructuring-bind (fn . args) (cdr *whole-message*)
         ;; message is (:conintuation fn . args)
         (apply fn args)))
      
      ((eq sel 'actor-internal-message:ask)
       ;; Intercept queries to send back a response from the following
       ;; message, reflecting any errors back to the caller.
       ;;
       ;; Msg format is: ('internal:ask reply-to &rest message)
       (from-the-top
        (lambda ()
          (try-asking 'self-call (cddr *whole-message*) *whole-message*))))
      
      (t
       ;; anything else is up to the programmer who constructed
       ;; this Actor
       (apply 'self-call *whole-message*))
      ))
   ))

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
    (=wait (ans) (:timeout *timeout* :errorp t)
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

