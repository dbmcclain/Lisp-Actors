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


(in-package #:actors)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 3) #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------

(declaim (inline current-actor))

(defvar *current-actor* nil)

(defun current-actor ()
  ;; each running thread will have its own version of this global
  ;; value. But, if non-nil, it points to the currently active Actor
  ;; running in that thread
  *current-actor*)

(defvar *actor-ready-queue*  ;; a queue of pending Actor activations
  (make-prio-mailbox :name "Actor Ready Queue"))

(defun do-nothing (&rest args)
  (declare (ignore args))
  ;; literally, do nothing...
  )
           
;; ----------------------------------------------------
;; An Actor mailbox contains a priority queue holding newly delivered
;; messages, plus a list of previously stashed messages in arrival
;; order. Stashed messages will be read before additional enqueued
;; messages. Message may become stashed, e.g., during operation of a
;; selective RECV.

(defclass actor-mailbox ()
  ((mbox   :reader   actor-message-mbox
           :initform (make-priq))
   (replay :accessor actor-message-replay
           :initform nil)))

;; -----------------------------------------------------

(defmethod next-message ((mbox actor-mailbox))
  (if (actor-message-replay mbox)
      ;; replay stashed messages in order of arrival
      (values (pop (actor-message-replay mbox)) t)
    ;; else - mailbox read with immediate return
    ;; return of (val t/f)
    (popq (actor-message-mbox mbox))))

(defmethod enqueue-replay ((mbox actor-mailbox) lst)
  ;; enqueue our list of messages ahead of pending stashed in mailbox
  (setf (actor-message-replay mbox) (nconc lst (actor-message-replay mbox))))

(defmethod deposit-message ((mbox actor-mailbox) msg &key (priority 0))
  ;; deposit one message into the mailbox
  (addq (actor-message-mbox mbox) msg :prio priority))

(defmethod mailbox-not-empty-p ((mbox actor-mailbox))
  ;; true if either stashed messsages or some in mailbox
  (or (actor-message-replay mbox)
      (not (emptyq-p (actor-message-mbox mbox)))))

;; -----------------------------------------------------
;; Version 3... make the Actor's internal state more readily visible
;; to debuggers. Use a CLOS object instead of closed over lambda vars.

(defclass actor ()
  ((properties
    ;; globally visible properties on this Actor. SMP-safe methods are
    ;; provided for getting / setting
    :reader    actor-properties
    :initarg   :properties
    :initform  (make-instance '<shared-plist>))
   (priority
    ;; we don't use this yet, but maybe someday there will be good
    ;; reason to do so.
    :accessor  actor-priority
    :initarg   :priority
    :initform  0)
   (mbox
    ;; the Actor's message queue. SMP-safe
    :reader    actor-mailbox
    :initform  (make-instance 'actor-mailbox))
   (recv-info
    ;; when non-nil this points to the RECV block in control. Only the
    ;; Actor queries this slot so SMP safety not a concern.
    :accessor  actor-recv-info
    :initform  nil)
   (busy
    ;; when non-nil this Actor is either already enqueued for running,
    ;; or is running. We use a CONS cell for the flag for SMP CAS
    :reader    actor-busy
    :initform  (list nil))
   (user-fn
    ;; points to the user code describing the behavior of this Actor.
    ;; This pointer is changed when the Actor performs a BECOME. Only
    ;; the Actor queries this slot so SMP safety not a concern.
    :accessor  actor-user-fn
    :initarg   :fn
    :initform  'do-nothing)
   (wrapper
    ;; points to context wrapper for dispatch
    ;; Allows for dynamic context establishment before any messages are dispatched.
    :accessor actor-dispatch-wrapper
    :initarg  :wrapper
    :initform 'dispatch-message)
   ))

;; -----------------------------------------------------
;; These methods can be called from any thread. SMP safe.

(defun make-actor (fn &key
                      (priority 0)
                      properties)
  (make-instance 'actor
                 :fn         fn
                 :priority   priority
                 :properties (make-instance '<shared-plist>
                                    :initial properties)
                 ))

(defmethod get-property ((actor actor) key &optional default)
  ;; SMP-safe
  (get-kv key (actor-properties actor) default))

(defmethod set-property ((actor actor) key value)
  ;; SMP-safe
  (setf (get-kv key (actor-properties actor)) value))

(defsetf get-property set-property)

(defmacro wrap-context (actor bindings)
  (let ((g!actor        (gensym))
        (g!prev-wrapper (gensym))
        (g!msg          (gensym)))
    `(let* ((,g!actor        ,actor)
            (,g!prev-wrapper (actor-dispatch-wrapper ,g!actor)))
       (setf (actor-dispatch-wrapper ,g!actor)
             (lambda (&rest ,g!msg)
               (let* ,bindings
                 (apply ,g!prev-wrapper ,g!msg)))))
    ))
;; --------------------------------------------------------

(defun %add-to-ready-queue (self)
  ;; Mark busy, if not already marked. And if it wasn't
  ;; already marked, place it into the ready queue and be
  ;; sure there are Executives running.
  (when (CAS (car (actor-busy self)) nil t)
    ;; The Ready Queue contains Actors awaiting a runtime thread. When
    ;; dequeued, they will be sent to RUN by an executive thread.
    (monitored-add-to-ready-queue self)))


(defmethod send ((actor actor) &rest msg)
  ;; send a message to an Actor and possibly activate it if not
  ;; already running. SMP-safe
  (deposit-message (actor-mailbox actor) msg)
  (%add-to-ready-queue actor))


(defun %run-actor (*current-actor*)
  (let ((mbox  (actor-mailbox *current-actor*))
        (since (car (actor-busy *current-actor*)))) ;; wakeup time
    (#+:LISPWORKS hcl:unwind-protect-blocking-interrupts-in-cleanups
     #+(OR :ALLEGRO :CLOZURE sbcl)  unwind-protect
     (loop for (msg ok) = (multiple-value-list
                           (next-message mbox))
           while ok do
           (apply (actor-dispatch-wrapper *current-actor*) msg))
     ;; -- the following are the unwind clauses --
     ;; <-- a message could have arrived here, but would
     ;; have failed to enqueue the Actor.  So we double
     ;; check after clearing the busy mark.
     ;;
     ;; Note that this had been a simple SETF shown
     ;; commented out and replaced with CAS:
     ;;
     (CAS (car (actor-busy *current-actor*)) since nil) ;; need the wakeup time here...
     ;;
     ;; And while, ostensibly, that nilling SETF
     ;; accomplishes an atomic write just like the CAS
     ;; operation, there is another benefit to the CAS in
     ;; that any and all memory writes will have become
     ;; flushed to memory before CAS. Hence, when we query
     ;; the mailbox-not-empty-p we will see an accurate
     ;; mailbox. Without CAS, some mail could have been
     ;; written but not yet flushed to memory, and the
     ;; mailbox-not-empty-p could indicate incorrectly.
     ;;
     (when (mailbox-not-empty-p mbox)
       (%add-to-ready-queue *current-actor*))
     )))

;; -----------------------------------------------
;; Since these methods are called against (CURRENT-ACTOR) they can
;; only be called from within a currently active Actor.

(defmethod become (new-fn)
  ;; change behavior, returning old. If an Actor didn't call this, an
  ;; error will result.
  (shiftf (actor-user-fn (current-actor)) new-fn))

(defun self-call (&rest msg)
  ;; send a message to myself, immediate execution. If an Actor didn't
  ;; call this, an error will result.
  (apply (actor-user-fn (current-actor)) msg))

;; ------------------------------------------------
;; RECV handling
;;
;; RECV under Actors is asynchronous with callback.  Consecutive RECV
;; forms in the body code enqueue internal messages to ensure
;; sequential performance of the successive RECV clauses. When the
;; first RECV clause finishes its callback or timeout, the next will
;; start.
;;
;; But RECV clauses perform without waiting, just falling through on
;; first encounter. While a RECV clause is active, it modifies the
;; behavior of the Actor to intercept messages selectively, stashing
;; those that don't match one of the RECV clauses, for later
;; execution.
;;
;; When a RECV is in operation, the RECV-INFO slot of the Actor points
;; to one of these control blocks.

(defclass recv-info ()
  ;; unique token used to associate timeouts with corresponding RECV
  ;; sessions
  ((id          :reader   recv-info-id          
                :initarg  :id)
   ;; a list of pending successive RECV's
   ;; no need for SMP safety here - only modified by active Actor
   (recvq       :reader   recv-info-recvq       
                :initform (make-unsafe-fifo))
   ;; a list of incoming messages that didn't match
   ;; no need for SMP safety here - only modified by active Actor   
   (msgq        :reader   recv-info-msgq 
                :initform (make-unsafe-fifo))
   ;; a function to screen messages for match
   (selector-fn :reader   recv-info-selector-fn 
                :initarg  :selector-fn)
   ;; the function to invoke on a timeout
   (timeout-fn  :reader   recv-info-timeout-fn
                :initarg  :timeout-fn)
   ;; currently active timer - when nil => none
   (timer       :accessor recv-info-timer
                :initarg  :timer)))

;; ----------------------------------------
;; RECV handlers...

(defmethod enqueue-replay ((self actor) (info recv-info))
  ;; the RECV has finished... enqueue all the stashed messages in the
  ;; Actor's mailbox, giving priority to internal RECV messages.
  ;;
  ;; This method overrides the one for the Actor's mailbox
  (enqueue-replay (actor-mailbox self)
                  (nconc (contents (recv-info-recvq info))
                         (contents (recv-info-msgq  info)))
                  ))

(defun actor-recv-timeout (timer-id)
  ;; a timeout occurred... is it ours? If not, just ignore.
  (let ((info (actor-recv-info *current-actor*)))
    (when (and info   ;; were we in a RECV?
               (eq (recv-info-id info) timer-id)) ;; was it the same one as for timer?
      (setf (actor-recv-info *current-actor*) nil) ;; terminate RECV
      (enqueue-replay *current-actor* info)        ;; prep for life after RECV
      (if-let (fn (recv-info-timeout-fn info))
          (funcall fn)
        (error "RECV Timeout")))))
         
(defun actor-recv-setup (conds-fn timeout-fn timeout-expr)
  ;; setup a new RECV control block in the current Actor, hence
  ;; activating RECV behavior until we find a message we want, or
  ;; else timeout waiting for one.
  (let ((this-id (gensym))) ;; make a unique id for recv-info
    (setf (actor-recv-info *current-actor*)
          (make-instance 'recv-info
                         :id          this-id
                         :selector-fn conds-fn
                         :timeout-fn  timeout-fn
                         :timer       (make-timeout-timer timeout-expr this-id)
                         ))))

;; -------------------------------------------------------------
;; Timeout Timers...

(defun send-timeout-message (self this-id)
  (send self
        :recv-timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A}
        this-id))

(defun make-timeout-timer (delta this-id)
  "Delta in seconds"
  (when delta
    (let ((timer (make-timer
                  'send-timeout-message *current-actor* this-id)))
      (schedule-timer-relative timer delta)
      timer)))

;; -------------------------------------------------------------

(defun actor-recv-test-message (msg)
  ;; see if the incoming message matches one of our RECV handlers
  (let* ((info   (actor-recv-info *current-actor*))
         (ans-fn (funcall (recv-info-selector-fn info) msg)))
    (cond (ans-fn
           (setf (actor-recv-info *current-actor*) nil) ;; revert to non-RECV behavior
           (when-let (timer (recv-info-timer info))
             (unschedule-timer timer))
           (enqueue-replay *current-actor* info) ;; prep for life after RECV
           (funcall ans-fn))          ;; handle the message

          (t 
           ;; else - not a message we are looking for - stash it
           (addq (recv-info-msgq info) msg))
          )))
            
;; ------------------------------------------------------
;; The main outer dispatch method for all Actors. It is here that we
;; differentiate among messages during active RECV, intercept RPC ASK
;; messages to reflect errors back to the caller, and perform
;; continuation messages resulting from callbacks. Otherwise, we
;; forward the message to the user's Actor code.
;;
;; Dynamic context from possible user specified wrappers are already
;; in effect at this point. There is always at least one dynamic
;; binding in effect: *CURRENT-ACTOR*.

(defvar *is-asking*  nil)

(defun asking-p ()
  ;; is (asking-p) is true, then anything executed in the message
  ;; handling will be wrapped with CAPTURE-ANS-OR-EXN
  *is-asking*)

(defun asking-self-call (&rest msg)
  (let ((*is-asking* t))
    (apply 'capture-ans-or-exn 'self-call msg)))

(defun dispatch-message (&rest msg)
  (dcase msg
    
    (:continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} (fn &rest vals)
     ;; Used for callbacks into the Actor
     (apply fn vals))
    
    (:recv-timeout-{3A95A26E-D84E-11E7-9D93-985AEBDA9C2A} (timer-id)
     ;; an incoming RECV timeout message
     (actor-recv-timeout timer-id))
    
    (:recv-setup-{204E1756-D84E-11E7-9D93-985AEBDA9C2A} (conds-fn timeout-fn timeout-expr)
     ;; another RECV clause. If not already in a RECV clause, activate
     ;; it. Otherwise stash it as an internal RECV message to be run
     ;; after the current RECV clause finishes.
     (if-let (info (actor-recv-info *current-actor*))
         (addq (recv-info-recvq info) msg)
       (actor-recv-setup conds-fn timeout-fn timeout-expr)))
    
    (t (&rest msg)
       (cond ((actor-recv-info *current-actor*)
              ;; we are in an active RECV clause - handle or stash
              ;; this message
              (actor-recv-test-message msg))
             
             (t 
              ;; else -- not currently in a RECV
              (dcase msg
                (:ask-{061B3878-CD81-11E7-9B0D-985AEBDA9C2A} (replyTo &rest msg)
                 ;; Intercept queries to send back a response from the
                 ;; following message, reflecting any errors back to
                 ;; the caller.
                 (send replyTo (apply 'asking-self-call msg)))
                
                (t (&rest args)
                   ;; anything else is up to the programmer who
                   ;; constructed this Actor
                   (apply (actor-user-fn *current-actor*) args))
                ))
             ))
    ))

;; ------------------------------------------
;; Create a callback on the function argument

(defclass callback-function ()
  ()
  (:metaclass #+:LISPWORKS clos:funcallable-standard-class
              #+:ALLEGRO   mop:funcallable-standard-class
              #+:CLOZURE   ccl:funcallable-standard-class
              #+:SBCL      sb-mop:funcallable-standard-class
              ))

(defmethod initialize-instance :after ((obj callback-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   #+:CLOZURE   ccl:set-funcallable-instance-function
   #+:SBCL      sb-mop:set-funcallable-instance-function
   obj behavior))

(defmethod =cont ((contfn callback-function))
  contfn)

(defmethod =cont ((contfn symbol))
  (=cont (symbol-function symbol)))

(defmethod =cont ((contfn function))
  (if-let (self (current-actor))
      ;;
      ;; If the callback originated from inside an Actor, we ensure
      ;; that it will later execute inside that Actor only when that
      ;; Actor is alive.
      ;;
      ;; Code inside an Actor should only be executing on one thread
      ;; at a time, in order to preserve SMP single-thread semantics.
      ;;
      (make-instance 'callback-function
                     :behavior (lambda (&rest args)
                                 (if (eq self (current-actor))
                                     (apply contfn args)
                                   (apply 'send self :continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} contfn args))))
    ;; else - not originating from inside of an Actor, just return the
    ;; function unchanged
    contfn))

;; ------------------------------------------

(defmacro without-actor-status (&body body)
  ;;
  ;; Used to avoid deadlocking an Actor. This should be quite rare.
  ;; (Also found a use for it in interrupt routines - see
  ;; WITH-TIMEOUT-FUTURE)
  ;;
  ;; In general, if you will have the Actor hang waiting on a resource
  ;; (e.g. a local mailbox), and the only way to release that resource
  ;; is to perform a callback function, that callback function would
  ;; ordinarily be redirected as a continuation message to the Actor.
  ;; The Actor would have to respond to the message, and you will have
  ;; induced a classic deadlock.
  ;;
  ;; You need to surround that action with WITHOUT-ACTOR-STATUS so
  ;; that embedded =CONT calls will become identity operations instead
  ;; of setups to send continuation messages back to the Actor.
  ;;
  ;; The Actor will be hung waiting on the resource, so there is no
  ;; danger of multiple thread access to Actor internals, until the
  ;; resource is released, if code in callback functions access Actor
  ;; internals from a foreign thread prior to that release. When in
  ;; doubt, use a lock.
  ;;
  ;; NOTE:  (without-actor-status (=cont (lambda () ...))) = (lambda () ...)
  ;;
  ;;  In other words, (without-actor-status (=cont fn)) is an identity
  ;;  operation on functions.
  ;;
  `(let ((*current-actor* nil))
     ,@body))

;; ------------------------------------------
;; Sends directed to mailboxes, functions, etc.

#+:LISPWORKS
(defmethod send ((mbox mp:mailbox) &rest message)
  (mp:mailbox-send mbox message))

#+:ALLEGRO
(defmethod send ((mbox mp:queue) &rest message)
  (mpcompat:mailbox-send mbox message))

#+:CLOZURE
(defmethod send ((mbox mpcompat::queue) &rest message)
  (mpcompat:mailbox-send mbox message))

(defmethod send ((mbox prio-mailbox) &rest message)
  (mailbox-send mbox message))

(defmethod send ((fn function) &rest message)
  (apply fn message))

(defmethod send ((sym symbol) &rest message)
  (if-let (actor (find-actor sym))
      (apply 'send actor message)
    (if (fboundp sym)
        (apply sym message)
      (call-next-method))))

(defmethod send ((str string) &rest message)
  (if-let (actor (find-actor str))
      (apply 'send actor message)
    (call-next-method)))

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

;; ------------------------------------------
;; A mailbox repository...
;; ... some things just can't be turned into an Actor service...

#+:LISPWORKS
(let ((queue (list nil)))

  (defun get-mailbox ()
    (or (sys:atomic-pop (car queue))
        (mp:make-mailbox)))

  (defun release-mailbox (mbox)
    (sys:atomic-push mbox (car queue)))

  (defun ensure-mbox-empty (mbox)
    (um:nlet-tail iter ()
      (unless (mp:mailbox-empty-p mbox)
        (mp:mailbox-read mbox)
        (iter)))))

#+(or :ALLEGRO :CLOZURE :SBCL)
(let ((queue (list nil))
      (lock  (mpcompat:make-lock)))

  (defun get-mailbox ()
    (mpcompat:with-lock (lock)
       (or (pop (car queue))
           (mpcompat:make-mailbox))))

  (defun release-mailbox (mbox)
    (mpcompat:with-lock (lock)
       (push mbox (car queue))))

  (defun ensure-mbox-empty (mbox)
    (um:nlet-tail iter ()
      (unless (mpcompat:mailbox-empty? mbox)
        (mpcompat:mailbox-read mbox)
        (iter)))))

(defun do-with-borrowed-mailbox (fn)
  (let ((mbox (get-mailbox)))
    (ensure-mbox-empty mbox)
    (unwind-protect
        (funcall fn mbox)
      (release-mailbox mbox))))

(defmacro with-borrowed-mailbox ((mbox) &body body)
  (let ((g!func (gensym)))
    `(flet ((,g!func (,mbox)
              ,@body))
       (declare (dynamic-extent #',g!func))
       (do-with-borrowed-mailbox #',g!func))))

#+:LISPWORKS
(editor:setup-indent "with-borrowed-mailbox" 1)

;; ----------------------------------------------
;; ASK - RPC with an Actor. Any errors incurred during the message
;; handling are reflected back to the caller

(defmethod ask ((actor actor) &rest message)
  ;; Blocking synchronous ASK with mailbox
  (if (eq actor (current-actor))
      (apply (actor-user-fn actor) message)
    ;; else - asking someone else
    (apply 'recover-ans-or-exn
           ;; return through mailbox is via SEND which always produces a
           ;; list. Hence the APPLY in the line above.
           (with-borrowed-mailbox (mbox)
             (apply 'send actor :ask-{061B3878-CD81-11E7-9B0D-985AEBDA9C2A} mbox message)
             (mpcompat:mailbox-read mbox)
             ))))

;; ----------------------------------------
;; ASK RPC directed to functions etc.

(defun huh!? ()
  (error "Huh!?"))

(defmethod ask (obj &rest message)
  (let ((mfn (car message)))
    (if (funcallable-p mfn)
        (apply mfn obj (cdr message))
      (Huh!?))))

(defmethod ask ((fn function) &rest message)
  (apply fn message))

(defmethod ask ((sym symbol) &rest message)
  (if-let (actor (find-actor sym))
      (apply 'ask actor message)
    (if (fboundp sym)
        (apply sym message)
      (call-next-method))))

(defmethod ask ((str string) &rest message)
  (if-let (actor (find-actor str))
      (apply 'ask actor message)
    (call-next-method)))

;; ---------------------------------------------
;; SPAWN a new Actor on a function with args
;; This is the way we start all Actors in the system.

(defun spawn (fn &rest args)
  (let ((actor (make-actor fn)))
    (apply 'send actor args)
    actor))

;; --------------------------------------------------------------------
;; Executive Pool - actual system threads dedicated to running Actor code

(defvar *heartbeat-interval* 1)   ;; how often the watchdog should check for system stall
(defvar *maximum-age*        3)   ;; how long before watchdog should bark, in seconds
(defvar *nbr-execs*               ;; should match the number of CPU Cores but never less than 4
  #+(AND :LISPWORKS :MACOSX)
  (load-time-value
   (with-open-stream (s (sys:open-pipe "sysctl -n hw.logicalcpu"))
     (let ((ans (ignore-errors (parse-integer (read-line s nil nil)))))
       (or (and (integerp ans)
                ans)
           (max 4 ans)))))
  #+:CLOZURE
  (max 4 (ccl:cpu-count))
  #-(or :CLOZURE (AND :LISPWORKS :MACOSX)) 4)

(defun default-watchdog-function (age)
  (restart-case
      (error "Actor Executives are stalled (blocked waiting or compute bound). ~&Last heartbeat was ~A sec ago."
             age)
    (:do-nothing-just-wait ()
      :report "It's okay, just wait"
      (start-watchdog-timer))
    (:spawn-new-executive ()
      :report "Spawn another Executive"
      (push-new-executive))
    (abort ()
      :report "Terminate Actor system"
      (kill-executives))
    ))

(defvar *watchdog-hook* 'default-watchdog-function)

;; ----------------------------------------------------------------
;; Ready Queue

;; ------------------------------------------------------------
;; Executive Actions

#+:LISPWORKS
(defun executive-loop ()
  ;; the main executive loop - more general, in that it will abosrb
  ;; global settings of print-vars from dynamic environment process
  ;; creator
  (restart-case
      (%run-actor (mailbox-read *actor-ready-queue*
                                "Waiting for Actor"))
    (abort ()
      :report "Terminate Actor execution"))
  ;; go around again with initial state
  (mp:process-reset (mp:get-current-process)))

#-:LISPWORKS
(defun executive-loop ()
  ;; the main executive loop
  (loop
   do
   (restart-case
       (with-standard-io-syntax
         (%run-actor (mailbox-read *actor-ready-queue*
                                   "Waiting for Actor")))
     (abort ()
       :report "Terminate actor execution")
     )))

(defun exec-terminate-actor (actor)
  ;; an interrupt handler - if the actor is ours, we terminate it
  (when (eq actor (current-actor))
    (invoke-restart 'abort)))

;; --------------------------------------------------------------

#|
(defun test-stall ()
  (loop repeat (1+ *nbr-execs*) do 
	(spawn (lambda () 
		 (sleep 10) 
		 (pr :hello (current-actor)))
	       )))
|#
;; -------------------------------------------------------------
;; Executive Control

;; these are all defined here as DEFVAR so that they won't be
;; disrupted on reloading of this code
(defvar *executive-processes*  nil)
(defvar *heartbeat-timer*      nil)
(defvar *watchdog-checking*    nil)
(defvar *executive-counter*    0)

(defun make-new-executive ()
  (mpcompat:process-run-function
   (format nil "Actor Executive ~D"
           (incf *executive-counter*))
   '(:internal-server t)
   'executive-loop))

(defun resume-periodic-checking ()
  (setf *watchdog-checking* nil))
    
(defmonitor
    ;; All under a global lock - called infrequently
    ((terminate-actor (actor)
       (dolist (exec *executive-processes*)
         (mpcompat:process-interrupt exec 'exec-terminate-actor actor)))
     
     (check-sufficient-execs ()
       (um:nlet-tail iter ()
         (let ((actor  nil))
           ;; Any Actors waiting?
           (cond ((setf actor (mailbox-peek *actor-ready-queue*))
                  ;; indeterminate delay between finding Actor and
                  ;; reading its timestamp...
                  (let ((timestamp (car (actor-busy actor))))
                    ;; Actor was on the ready queue, if its timestamp is REAL
                    ;; then it is either still on the queue or running. Else
                    ;; now retired.
                    
                    ;; It could also happen that we got a REAL timestamp, but
                    ;; we were delayed in reading it, and we read it after
                    ;; the Actor had already been retired, then prompted by a
                    ;; message send, and placed back on the ready queue with
                    ;; a more recent timestamp.
                    (cond ((and (realp timestamp)
                                (eq actor (mailbox-peek *actor-ready-queue*)))
                           ;; So if timestamp is REAL, and Actor is still at
                           ;; the head of the queue then either we are
                           ;; probably running sufficiently quickly, or else
                           ;; this really is the oldest one still waiting.
                           (let ((age  (- (get-universal-time) timestamp)))
                             (cond ((< age *maximum-age*)
                                    ;; Everything is fine. Put us back
                                    ;; on check duty - timer took us
                                    ;; off to avoid duplicate
                                    ;; notifications in slow runnging
                                    ;; systems
                                    (resume-periodic-checking))
                                   
                                   (t
                                    ;; we are taking too long, someone is possibly blocking?
                                    ;; -------------------------------------------
                                    ;;
                                    ;; Why kill the workhorse?
                                    ;;
                                    ;; For LW, the timer routine triggers in an arbitrary
                                    ;; thread with a retriggering timer. This routine runs as
                                    ;; an interrupt routine and we need to keep it short. We
                                    ;; also need to prevent retriggering of nuisance
                                    ;; notifications while we are busy handling the situation.
                                    ;;
                                    ;; For ACL, the timer runs in its own dedicated thread and
                                    ;; won't retrigger until we return from here. But we also
                                    ;; need to keep this short so that we don't block ongoing
                                    ;; useful activity that may need something inside this
                                    ;; monitor section.
                                    ;;
                                    ;; So in both cases, just kill off the timer and let a new
                                    ;; thread handle the notification with the user.
                                    ;; ----------------------------------------------
                                    (unschedule-timer (shiftf *heartbeat-timer* nil))
                                    ;; --------------------------------------------
                                    
                                    (mpcompat:process-run-function
                                     "Handle Stalling Actors"
                                     '()
                                     *watchdog-hook* age))
                                   )))
                          
                          (t
                           ;; We didn't get a consistent view. Try again.
                           (iter))
                          )))
                 (t
                  ;; No Actors were seen waiting.
                  ;; Put us back on watch duty
                  (resume-periodic-checking))
                 ))))
     
     (set-executive-pool (npool)
       ;; set the Executive pool nbr threads, return previous value
       (check-type npool (integer 1))
       (prog1
           (shiftf *nbr-execs* npool)
         (kill-executives)))
     
     (push-new-executive ()
       (push (make-new-executive) *executive-processes*)
       (setf *nbr-execs* (max *nbr-execs*
                              (length *executive-processes*)))
       (start-watchdog-timer))
     
     (start-watchdog-timer ()
       (unless *heartbeat-timer*
         (setf *watchdog-checking* nil
               *heartbeat-timer*
               (make-timer (lambda ()
                             (when (CAS *watchdog-checking* nil t)
                               (mpcompat:funcall-async 'check-sufficient-execs)))
                           ))
         (schedule-timer-relative
          *heartbeat-timer*
          *maximum-age*
          *heartbeat-interval*)))
     
     (kill-executives ()
       (um:when-let (timer (shiftf *heartbeat-timer* nil))
         (unschedule-timer timer))
       (um:when-let (procs (shiftf *executive-processes* nil))
         (when (consp procs)
           (dolist (proc procs)
             (ignore-errors
               #+:LISPWORKS
               (mp:process-terminate proc
                                     :force-timeout 10)
               #+(OR :ALLEGRO :CLOZURE)
               (mpcompat:process-kill proc))))
         ;; empty the ready queue
         (setf *actor-ready-queue* (make-prio-mailbox))
         ))
     
     (monitored-add-to-ready-queue (actor)
       ;; use the busy cell to hold our wakeup time - for use by watchdog,
       ;; and non-nil for the CAS we just grabbed
       (setf (car (actor-busy actor)) (get-universal-time))
       (mailbox-send *actor-ready-queue* actor
                     :prio (actor-priority actor))
       (unless *executive-processes*
         (setf *executive-counter*   0
               *executive-processes* (loop repeat *nbr-execs* collect
                                           (make-new-executive)))
         (start-watchdog-timer)))
     
     ))

