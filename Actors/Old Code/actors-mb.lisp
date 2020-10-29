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
  (mp:make-mailbox))

(defvar *executive-processes* nil)  ;; the list of Executive threads

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
   ))

;; -----------------------------------------------------
;; These methods can be called from any thread. SMP safe.

(defun make-actor (fn &key (priority 0) properties)
  (make-instance 'actor
                 :fn         fn
                 :priority   priority
                 :properties (make-instance '<shared-plist>
                                    :initial properties)))

(defmethod get-property ((actor actor) key &optional default)
  ;; SMP-safe
  (get-kv key (actor-properties actor) default))

(defmethod set-property ((actor actor) key value)
  ;; SMP-safe
  (setf (get-kv key (actor-properties actor)) value))

(defsetf get-property set-property)

;; --------------------------------------------------------

(defparameter *mbsends* 0)

(defmethod send ((self actor) &rest msg)
  ;; send a message to an Actor and possibly activate it if not
  ;; already running. SMP-safe
  (let ((mbox (actor-mailbox self)))
    (labels ((add-self-to-ready-queue ()
               ;; Mark busy, if not already marked. And if it wasn't
               ;; already marked, place it into the ready queue and be
               ;; sure there are Executives running.
               (when (CAS (car (actor-busy self)) nil t)
                 ;; The Ready Queue just contains function closures to
                 ;; be dequeued and executed by the Executives.
                 (mp:mailbox-send *actor-ready-queue* #'run)
                 (sys:atomic-incf *mbsends*)
                 (unless *executive-processes*
                   (ensure-executives))))
             
             (run ()
               (#+:LISPWORKS hcl:unwind-protect-blocking-interrupts-in-cleanups
                #+(OR :ALLEGRO :CLOZURE)  unwind-protect
                   (let ((*current-actor* self))
                     (loop for (msg ok) = (multiple-value-list
                                           (next-message mbox))
                           while ok do (apply 'dispatch-message self msg)))
                 ;; <-- a message could have arrived here, but would
                 ;; have failed to enqueue the Actor.  So we double
                 ;; check after clearing the busy mark.
                 (setf (car (actor-busy self)) nil)
                 (when (mailbox-not-empty-p mbox)
                   (add-self-to-ready-queue)))))
      (deposit-message mbox msg)
      (add-self-to-ready-queue))
    ))

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
  (apply 'dispatch-message (current-actor) msg))

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

(defmethod actor-recv-timeout ((self actor) timer-id)
  ;; a timeout occurred... is it ours? If not, just ignore.
  (let ((info (actor-recv-info self)))
    (when (and info   ;; were we in a RECV?
               (eq (recv-info-id info) timer-id)) ;; was it the same one as for timer?
      (setf (actor-recv-info self) nil) ;; terminate RECV
      (enqueue-replay self info)        ;; prep for life after RECV
      (if-let (fn (recv-info-timeout-fn info))
          (funcall fn)
        (error "RECV Timeout")))))
         
(defmethod actor-recv-setup ((self actor) conds-fn timeout-fn timeout-expr)
  ;; setup a new RECV control block in the current Actor, hence
  ;; activating RECV behavior until we find a message we want, or
  ;; else timeout waiting for one.
  (let ((this-id (gensym))) ;; make a unique object
    (setf (actor-recv-info self)
          (make-instance 'recv-info
                         :id          this-id
                         :selector-fn conds-fn
                         :timeout-fn  timeout-fn
                         :timer       (make-timeout-timer timeout-expr self this-id)
                         ))))

;; -------------------------------------------------------------
;; Timeout Timers...

(defun send-timeout-message (self this-id)
  (send self
        'actor-internal-message:recv-timeout
        this-id))

(defun make-timeout-timer (delta self this-id)
  (when delta
    (let ((timer (make-timer
                  'send-timeout-message self this-id)))
      (schedule-timer-relative timer delta)
      timer)))

;; -------------------------------------------------------------

(defmethod actor-recv-test-message ((self actor) msg)
  ;; see if the incoming message matches one of our RECV handlers
  (let* ((info   (actor-recv-info self))
         (ans-fn (funcall (recv-info-selector-fn info) msg)))
    (cond (ans-fn
           (setf (actor-recv-info self) nil) ;; revert to non-RECV behavior
           (when-let (timer (recv-info-timer info))
             (unschedule-timer timer))
           (enqueue-replay self info) ;; prep for life after RECV
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

(defmethod dispatch-message ((self actor) &rest msg)
  (dcase msg
    
    (actor-internal-message:continuation (fn &rest vals)
     ;; Used for callbacks into the Actor
     (apply fn vals))
    
    (actor-internal-message:recv-timeout (timer-id)
     ;; an incoming RECV timeout message
     (actor-recv-timeout self timer-id))
    
    (actor-internal-message:recv-setup (conds-fn timeout-fn timeout-expr)
     ;; another RECV clause. If not already in a RECV clause, activate
     ;; it. Otherwise stash it as an internal RECV message to be run
     ;; after the current RECV clause finishes.
     (if-let (info (actor-recv-info self))
         (addq (recv-info-recvq info) msg)
       (actor-recv-setup self conds-fn timeout-fn timeout-expr)))
    
    (t (&rest msg)
       (cond ((actor-recv-info self)
              ;; we are in an active RECV clause - handle or stash
              ;; this message
              (actor-recv-test-message self msg))
             
             (t 
              ;; else -- not currently in a RECV
              (dcase msg
                (actor-internal-message:ask (replyTo &rest msg)
                 ;; Intercept queries to send back a response from the
                 ;; following message, reflecting any errors back to
                 ;; the caller.
                 (send replyTo (apply 'capture-ans-or-exn
                                'self-call msg)))
                     
                (t (&rest args)
                   ;; anything else is up to the programmer who
                   ;; constructed this Actor
                   (apply (actor-user-fn self) args))
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
              ))

(defmethod initialize-instance :after ((obj callback-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   #+:CLOZURE   ccl:set-funcallable-instance-function
   obj behavior))

(defmethod =cont ((contfn callback-function))
  contfn)

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
                                   (apply 'send self 'actor-internal-message:continuation contfn args))))
    ;; else - not originating from inside of an Actor, just return the
    ;; function unchanged
    contfn))

;; ------------------------------------------

(defmacro without-actor-status (&body body)
  ;;
  ;; Used to avoid deadlocking an Actor.
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
  ;; NOTE:  (=cont (without-actor-status (lambda () ...))) = (lambda () ...)
  ;;
  ;;  In other words, (=cont (without-actor-status )) is an identity
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

(defmethod send (other-obj &rest message)
  (let ((mfn (car message)))
    (when (funcallable-p mfn)
      (apply mfn other-obj (cdr message))
      )))

;; ------------------------------------------
;; A mailbox repository...
;; ... some things just can't be turned into an Actor service...

#+:LISPWORKS
(let ((queue (list nil)))

  (defun get-mailbox ()
    (or (sys:atomic-pop (car queue))
        (mp:make-mailbox)))

  (defun release-mailbox (mbox)
    (sys:atomic-push mbox (car queue))))

#+(or :ALLEGRO :CLOZURE)
(let ((queue (list nil))
      (lock  (mpcompat:make-lock)))

  (defun get-mailbox ()
    (mpcompat:with-lock (lock)
       (or (pop (car queue))
           (mpcompat:make-mailbox))))

  (defun release-mailbox (mbox)
    (mpcompat:with-lock (lock)
       (push mbox (car queue)))))
  
(defun do-with-borrowed-mailbox (fn)
  (let ((mbox (get-mailbox)))
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
      (apply 'dispatch-message actor message)
    ;; else - asking someone else
    (apply 'recover-ans-or-exn
           ;; return through mailbox is via SEND which always produces a
           ;; list. Hence the APPLY in the line above.
           (with-borrowed-mailbox (mbox)
             (apply 'send actor 'actor-internal-message:ask mbox message)
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

(defvar *heartbeat-timer*    nil) ;; the system watchdog timer
(defvar *last-heartbeat*     0)   ;; time of last Executive activity
(defvar *executive-counter*  0)   ;; just a serial number on Executive threads
(defvar *heartbeat-interval* 1)   ;; how often the watchdog should check for system stall
(defvar *maximum-age*        3)   ;; how long before watchdog should bark
(defvar *nbr-execs*               ;; should match the number of CPU Cores
  #+(AND :LISPWORKS :MACOSX)
  (load-time-value
   (with-open-stream (s (sys:open-pipe "sysctl -n hw.logicalcpu"))
     (let ((ans (ignore-errors (parse-integer (read-line s nil nil)))))
       (or (and (integerp ans)
                ans)
           4))))
  #+:CLOZURE
  (ccl:cpu-count)
  #-(or :CLOZURE (AND :LISPWORKS :MACOSX)) 4)

;; ----------------------------------------------------------------
;; Ready Queue

;; ------------------------------------------------------------
;; Executive Actions

(defun executive-loop ()
  ;; the main executive loop
  (unwind-protect
      (loop for fn = (mp:mailbox-read *actor-ready-queue*
                                   "Waiting for Actor")
            do
            (setf *last-heartbeat* (get-universal-time))
            (restart-case
                (funcall fn)
              (:terminate-actor ()
                :report "Terminate Actor"
                )))
    (remove-from-pool (mpcompat:current-process))))

(defun exec-terminate-actor (actor)
  ;; an interrupt handler - if the actor is ours, we terminate it
  (when (eq actor (current-actor))
    (throw :terminate-actor nil)))

;; --------------------------------------------------------------

(defun empty-ready-queue ()
  ;; this is a support routine, to be called only from the safe
  ;; Monitor section. So, only one of us is mutating
  ;; *ACTOR-READY-QUEUE*, the pointer.
  (let ((old-mb  (shiftf *actor-ready-queue*
                         (mp:make-mailbox))))
    ;; nudge any Executives waiting on the queue to move over to the
    ;; new one.
    (mapc (lambda (proc)
            (declare (ignore proc))
            (mp:mailbox-send old-mb 'do-nothing))
          *executive-processes*)
    ))

#|
(defun test-stall ()
  (loop repeat (1+ *nbr-execs*) do 
	(spawn (lambda () 
		 (sleep 10) 
		 (pr :hello (current-actor)))
	       )))
|#

(defun set-executive-pool (npool)
  ;; set the Executive pool nbr threads, return previous value
  (check-type npool (integer 1))
  (prog1
      (shiftf *nbr-execs* npool)
    (kill-executives)))
     
(defmonitor
    ;; All under a global lock - called infrequently
    ((terminate-actor (actor)
       (dolist (exec *executive-processes*)
         (mp:process-interrupt exec 'exec-terminate-actor actor)))
     
     (check-sufficient-execs ()
       (let (age)
         (unless (or (mp:mailbox-empty-p *actor-ready-queue*)        ;; nothing to do anyway?
                     (< (setf age (- (get-universal-time)  ;; been stalled long enough?
				     *last-heartbeat*))
			*maximum-age*))
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
            (lambda ()
              (restart-case
                  (error "Actor Executives are stalled (blocked waiting or compute bound). ~&Last heartbeat was ~A sec ago."
                         age)
                (:do-nothing-just-wait ()
                  :report "It's okay, just wait"
                  (start-watchdog-timer))
                (:spawn-new-executive ()
                  :report "Spawn another Executive"
                  (incf *nbr-execs*)
                  (push-new-executive))
                (:stop-actor-system ()
                  :report "Stop Actor system"
                  (kill-executives))
                ))
            ))))

     (remove-from-pool (proc)
       (setf *executive-processes* (delete proc *executive-processes*)))
     
     (push-new-executive ()
       (push (mpcompat:process-run-function
              (format nil "Actor Executive ~D" (incf *executive-counter*))
              '()
              'executive-loop)
             *executive-processes*)
       (start-watchdog-timer))

     (start-watchdog-timer ()
       (unless *heartbeat-timer*
         (setf *heartbeat-timer*
               (make-timer 'check-sufficient-execs))
         (setf *last-heartbeat* (get-universal-time))
         (schedule-timer-relative
          *heartbeat-timer*
          *maximum-age*
          *heartbeat-interval*)))

     (ensure-executives ()
       (unless *executive-processes*
         (dotimes (ix *nbr-execs*)
           (push-new-executive))))
     
     (kill-executives ()
       (let ((timer (shiftf *heartbeat-timer* nil)))
         (when timer
           (unschedule-timer timer)
           (setf *last-heartbeat* 0)))
       (let ((procs (shiftf *executive-processes* nil)))
         (setf *executive-counter* 0)
         (dolist (proc procs)
           (ignore-errors
             #+:LISPWORKS
             (mp:process-terminate proc)
             #+(OR :ALLEGRO :CLOZURE)
             (mp:process-kill proc)))
         (empty-ready-queue)
         ))))

;; -----------------------------------------------------------------------------
;; Not specifically a part of Actors, but might help when describing
;; non-blocking Actor code...
;;
;; Be careful here... the use of parallel execution could lead to
;; shared access to local state, which might violate single-thread
;; semantics inside of Actor bodies.
;;
;; These macros also game the system by intentional capture of free
;; variable %SK (the current continuation). This is a lexical binding,
;; and must not be made a global special symbol.

(defmacro =lambda (parms &body body)
  ;; define an anonymous CPS function
  `#'(lambda (%sk ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  ;; define a named CPS function
  (let* ((f            (symb '= name))
         (has-rest     (position '&rest parms))
         (prefix-parms (subseq parms 0 has-rest))
         (tail-parm    (when has-rest
                         (nthcdr (1+ has-rest) parms))))
    ;; ooftah...
    `(progn
       (defmacro ,name ,parms
         `(,',f %sk ,,@prefix-parms ,@,@tail-parm))
       (defun ,f (%sk ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  ;;
  ;; (let ((f (=LAMBDA ()
  ;;             (=VALUES e1) )))
  ;;
  ;;   (=BIND (v)
  ;;        (=FUNCALL f)
  ;;     e2 ))
  ;;
  ;; equiv to ML:
  ;;
  ;;    let f k = function
  ;;       k e1
  ;;
  ;;    let k v = function
  ;;       e2
  ;;    in
  ;;       f k
  ;;
  `(let ((%sk (=cont #'(lambda ,parms ,@body))))
     ,expr))

(defmacro =values (&rest retvals)
  ;; invoke a continuation. This should generally be in tail position
  `(funcall %sk ,@retvals))

(defmacro =funcall (fn &rest args)
  ;; invoke a CPS function
  `(funcall ,fn %sk ,@args))

(defmacro =apply (fn &rest args)
  ;; invoke a CPS function
  `(apply ,fn %sk ,@args))

;; ---------------------------------------------------

(defmacro with-cont (&body body)
  ;; for REPL toplevel call to function defined with =defun
  `(let ((%sk #'values))
     ,@body))

(defmacro with-future ((ans) form &body body)
  ;; alternative for Actors and Processes alike
  `(=bind (,ans)
       (spawn (lambda ()
                (=values ,form)))
     ,@body))

(define-symbol-macro =bind-callback %sk)

#+:LISPWORKS
(editor:setup-indent "with-future"  2)

;; --------------------------------------------------
;; Async ASK

(defmacro aska (args (obj &rest message) &body body)
  ;; Non-blocking asynchronous ASK with callback
  `(do-aska ,obj ,message (lambda ,args
                            ,@body)))

(defmethod do-aska ((obj actor) message cbfn)
  (if (eq obj (current-actor))
      (funcall cbfn (apply 'dispatch-message obj message))
    ;; else
    (apply 'send obj 'actor-internal-message:ask
           (=cont (lambda (ans)
                    (funcall cbfn (apply 'recover-ans-or-exn ans))))
           message)))

(defmethod do-aska (obj message cbfn)
  (with-future (ans)
      (apply 'ask obj message)
    (funcall cbfn ans)))
  
;; ------------------------------------------------

(defun trn (mat)
  ;; transpose of list-form matrix
  (apply 'mapcar 'list mat))

(=defun pmapcar (fn &rest lists)
  ;;
  ;; Parallel mapcar - returns a result list.
  ;; Use like PAR for indefinite number of parallel forms,
  ;; each of which is the same function applied to different args.
  ;;
  ;; The function fn should be defined with =DEFUN or =LAMBDA, and
  ;; return via =VALUES
  ;;
  ;; PMAPCAR is intended for use within an =BIND
  ;;  (see example below)
  ;;
  (let* ((grps   (trn lists))
         (len    (length grps))
         (count  (list len))
         (ansv   (make-array len)))
    (labels ((done (ix ans)
               (setf (aref ansv ix) ans)
               (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                 (=values (coerce ansv 'list))))
             (callback (ix)
               (lambda (ans)
                 (done ix ans))))
      (if grps
          (loop for grp in grps
                for ix from 0
                do
                (apply 'spawn fn (callback ix) grp))
        ;; else - empty lists, nothing to do
        (=values nil)))
    ))

(defun par-xform (pfn &rest clauses)
  ;; Internal transform function. Converts a list of clauses to a form
  ;; suitable for application by PMAPCAR or PFIRST
  `(,pfn (=lambda (fn)
          (=values (funcall fn)))
         (list ,@(mapcar #`(lambda () ,a1) clauses))))

(defmacro par (&rest clauses)
  ;; PAR - perform clauses in parallel
  ;; This is intended for use within an =BIND clause
  ;;  (example below in WITH-FUTURES)
  (apply 'par-xform 'pmapcar clauses))

(defmacro with-futures (args forms &body body)
  (let ((g!list (gensym)))
    `(=bind (,g!list)
         (par ,@forms)
       (multiple-value-bind ,args (values-list ,g!list)
         ,@body))))

#+:LISPWORKS
(editor:setup-indent "with-futures" 2)

#|
;; examples...
  
(=bind (lst)
    (par
      (sin 1)
      (sin 2)
      (sin 3))
  (pr (reduce '+ lst)))
=> 1.8918884

(with-futures (a b c)
    ((sin 1)
     (sin 2)
     (sin 3))
  (pr (+ a b c)))
=> 1.8918884

(=bind (lst)
    (pmapcar (=lambda (x y)
               (=values (list x y)))
             '(a b c)
             '(1 2 3))
  (print lst))
==> '((a 1) (b 2) (c 3))

(=bind (lst)
    (pmapcar (=lambda (v)
               (=values (sin v)))
             '(1 2 3))
  (pr (reduce '+ lst)))
=> 1.8918884

(=bind (lst)
    (pmapcar (=lambda (fn)
               (=values (funcall fn 1)))
             '(sin cos tan))
  (pr lst))
=> (0.84147096 0.5403023 1.5574077)
|#

;; ---------------------------------------------------------------

(=defun pfirst (fn &rest lists)
  ;;
  ;; Parallel OR - returns the first parallel exec that returns a
  ;; non-null value.  Use like PMAPCAR against parallel execs, each of
  ;; which is the same function applied to different args.
  ;;
  ;; The function fn should be defined with =DEFUN or =LAMBDA, and
  ;; return via =VALUES
  ;;
  ;; PFIRST is intended for use within an =BIND
  ;;  (see example below)
  ;;
  (let* ((grps   (trn lists))
         (ret    (list nil)))
    (labels ((done (ans)
               (when (and ans
                          (mpcompat:CAS (car ret) nil t))
                 (=values ans))))
      (when grps
        (loop for grp in grps
              do
              (apply 'spawn fn #'done grp)))
      )))

(defmacro par-first (&rest clauses)
  (apply 'par-xform 'pfirst clauses))

(defmacro with-first-future ((arg) forms &body body)
  ;; actually like a WHEN-LET on OR of parallel conditionals
  `(=bind (,arg)
       (par-first ,@forms)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-first-future" 2)

#|
;; example
(with-first-future (which)
    ;; setup a race...
    ((progn
       (sleep 1)
       :one)
     (progn
       (sleep 1)
       :two)
     (progn
       (sleep 1)
       :three))
  (pr which))

(=bind (which)
    ;; same race, different syntax...
    (pfirst (=lambda (id)
              (sleep 1)
              (=values id))
            '(:one :two :three))
  (pr which))
|#
