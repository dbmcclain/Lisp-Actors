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


(in-package #:com.ral.actors.base)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 2) #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------

(deflex +timed-out+ (make-condition 'timeout))

(defun sink-beh ()
  #'do-nothing)

(deflex sink
  (create (sink-beh)))

(defgeneric is-pure-sink? (ac)
  ;; used by networking code to avoid sending useless data
  (:method ((ac actor))
   (eq (sink-beh) (actor-beh ac)))
  (:method (ac)
   t))
   

(defun become-sink ()
  (become (sink-beh)))

;; --------------------------------------------------------
;; Core RUN Dispatcher for Actors

;; -------------------------------------------------
;; Message Event Frames - submitted to the event queue. Message frames
;; are simple lists, where a head element specifies the target actor
;; for the rest of the message.
#|
     Message Event Frame: Just a LIST of items.

      Event
        |
     ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐
     │ Link │ │Target│ │ Msg  │ │ ...  │ │ ...  │
     └──────┘ └──────┘ └──────┘ └──────┘ └──────┘
                 |        |----->
                SELF      |
                       SELF-MSG
                       
     When tracing, LINK points to parent message frame. Otherwise, NIL.
     (Potentially very memory and GC costly to keep all parent
      chains, except those needed along an activation chain of interest.)
|#

(defun msg (target args)
  (declare (list args))
  (list* self-msg-parent target args))

;; --------------------------------------------

(defun %send-to-pool (msg)
  (mpc:mailbox-send *central-mail* msg))

(defun send-to-pool (target &rest msg)
  ;; the default SEND for foreign (non-Actor) threads
  (when (actor-p target)
    (%send-to-pool (msg target msg))))

;; -----------------------------------------------
;; SEND/BECOME
;;
;; SEND can only be called on an Actor. BECOME can only be called from
;; within an Actor.
;;
;; SEND and BECOME are transactionally staged, and will commit *ONLY*
;; upon error free completion of the Actor body code.
;;
;; So if you need them to take effect, even as you call potentially
;; unsafe functions, then surround your function calls with
;; HANDLER-CASE, HANDLER-BIND, or IGNORE-ERRORS. Otherwise, an error
;; will make it seem that the message causing the error was never
;; delivered.

(defun get-send-hook ()
  (sys-cached *send-hook*
              (prog1
                  (setf *central-mail* (mpc:make-mailbox :lock-name "Central Mail")
                        *send-hook*    #'%send-to-pool)
                (restart-actors-system *nbr-pool*))
              ))

(defun send (target &rest msg)
  (when (actor-p target)
    (funcall (get-send-hook) (msg target msg))))

(defun send* (&rest args)
  ;; when last arg is a list that you want destructed
  (apply #'send (apply #'list* args)))

(defun repeat-send (target)
  (send* target self-msg))

(defun send-combined-msg (target msg1 msg2)
  (multiple-value-call #'send target (values-list msg1) (values-list msg2)))

;; ---------------------------------------

(defun become (new-beh)
  #F
  (funcall *become-hook* new-beh))

;; -----------------------------------
;; In an Actor, (ABORT-BEH) undoes any BECOME and SENDS to this point,
;; but allows Actor to exit normally. In contrast to ERROR action
;; which aborts all BECOME and SENDs and exits immediately. ABORT-BEH
;; allows subsequent SENDs and BECOME to still take effect.

(defun abort-beh ()
  (funcall *abort-beh-hook*))

;; -----------------------------------------------------------------
;; Generic RUN for all threads
;;
;; Actors are now completely transactional. SENDs and BECOME are
;; staged for commit or rollback. They are committed upon successful
;; completion of the Actor behavior code invocation, and discarded
;; otherwise.
;;
;; All sent messages are collected in the stash, but only the last
;; BECOME will have effect.
;;
;; Actors will run fully in parallel on multiple machine threads.
;; If BECOME cannot commit because it clashes with a parallel BECOME,
;; the Actor is retried after rolling back the BECOME and SENDs. This
;; is maximum optimistic parallelism.
;;
;; Possible retries also means that any behavior code executing a
;; BECOME should be idempotent. Idempotence implies never causing any
;; globally visible effects. No I/O or timer launches should be
;; performed in that branch of the behavior code.
;;
;; If you want to produce some effect while also performing a BECOME,
;; then relegate the effect to a NON-IDEMPOTENT, or ON-COMMIT, clause
;; of the code. That clause will only be performed if the
;; transactional commit succeeds.
;;
;; Actor behaviors should be written as FPL pure to make them thread
;; safe. E.g, banish the use of SETF against Actor state bindings, and
;; use REMOVE, never DELETE, with Actor state sequences. FPL code is
;; the easiest to write correctly and debug.
;;
;; In all cases, Actor state should only ever be changed through the
;; use of BECOME with a mutated copy of state, since Actor state is
;; visible to all parallel threads executing the same Actor behavior
;; code.
;;
;; SEND and BECOME produce the only system-wide acceptable global
;; mutations. SEND modifies the global event queue at commit time, and
;; BECOME alters the behavior slot of a gobally visible Actor wrapper.
;;
;; In this context, GLOBAL means a binding or an object that is
;; potentially visible to more than one thread. When you first CREATE
;; an Actor, it is not visible to anyone else until it is shared
;; through a SEND, or made part of the Actor state with BECOME, or
;; handed to a newly created Actor for its state, where that Actor
;; will be shared.
;;
;; If you live within these constraints, then there is never any need
;; for spawning new machine threads, exerting thread control, nor
;; using locks around global bindings. All global information becomes
;; subsumed by purpose-built Actors who act as custodians of the state
;; bindings as their own state.
;;
;; You never need to think about machine threads, multitasking, locks,
;; semaphoes, etc. Just write FPL Actors code as though you are the
;; sole occupant of the machine, and the code is also naturally
;; parallel in this Actors system. It runs as well on a single thread
;; or on multiple threads, albeit potentially more slowly on a single
;; thread.
;;
;; Since Actors are inert wrapped functional closures, they are not
;; "alive" and cannot be killed. You cannot kill an Actor any more
;; than you can kill the SQRT function. But once running in response
;; to a delivered message, their code can refer to their Actor
;; wrapper as SELF.
;;
;; There is always some machine thread, of completely unknown and
;; irrelevant identity, that is running an Actor's behavior code upon
;; message delivery. In a multiple thread system, each separate
;; message delivery to any one Actor can happen on an aribtrary and
;; possibly different machine thread. And on multi-core CPU's several
;; different messages to the same Actor can be executing
;; simultanesously on different machine threads running in separate
;; CPU cores.
;;
;; Keeping your Actors code FPL-clean means you never have to concern
;; yourself with such low level details. Just be mindful that any code
;; branch with a BECOME needs to be idempotent, or else have its
;; effects wrapped inside an ON-COMMIT clause.
;;
;; NOTE on SEND Ordering: Since all SENDs are staged for commit upon
;; successful return from Actors, there is no logical distinction
;; between when each of them is sent, if there were more than one
;; arising from the Actor execution. They are all sent logically at
;; once - even though there may, or may not, be some underyling
;; ordering in the event queue.
;;
;; You should not depend on any particular ordering of messages,
;; except that no messages sent from an Actor invocation can appear in
;; the event queue before its behavior code exits normally. BECOME is
;; likewise staged.
;;
;; Hence, no executing Actor behavior code can see the results of its
;; own BECOME and SENDs. Nor can anyone else, until the behavior code
;; exits normally.
;;
;; The event queue is a FIFO queue, but message ordering might become
;; jumbled if they are ever delivered and then re-sent.
;;
;; NOTE: You can call SEND from non-Actor code. That SEND happens
;; immediately.  BECOME cannot be called from non-Actor code.  CREATE
;; can be called from non-Actor code, but it won't be visible to
;; anyone else until it is either shared via SEND, or have a message
;; sent to it, or it is given as the value of a global binding.

;; --------------------------------------------
;; H&S Monitoring
;;
;; Because we allow full parallel operation of the Actors, with
;; optimistic commits, there can sometimes be collisions when two or
;; more Actors are executing the same behavior code and they both
;; attempt a BECOME.
;;
;; Initial findings show that collisions increase whenever the Mac
;; system has its attention diverted to other running programs.
;;
;; Despite that, with numerous task switches during the test run, I
;; find that we get about 0.05% collision rate when at least two
;; competing activity tasks are being Forked and must rendezvous.
;; During the rendezvous, is when the collisions are happening.
;;
;; In my test, I have several live telemetry graphs monitoring a
;; remote audio processing system, and those graphics are being
;; forked. They update collectively at 20 Hz, and I get about 30
;; collisions per hour. (approx 0.05% collision rate, or 1 in 2000
;; updates)

(defvar *collision-counter* 0)
(defvar *collision-start*   0)

(defun report-collision ()
  (sys:atomic-incf *collision-counter*))

(defun cph ()
  ;; Report Collisions per Hour
  (let* ((now   (get-universal-time))
         (start (shiftf *collision-start* now))
         (count (sys:atomic-exchange *collision-counter* 0)))
    (format t "~%Collisions: count = ~d, rate = ~,2f/hr"
            count (* 3600 (/ count (- now start))))
    (values)
    ))
         
#|
(cph)
|#
;; --------------------------------------------

(defun run-actors (&optional (actor nil actor-provided-p) &rest message)
  #F
  (let (done timeout sends pend-beh)
    (labels
        ((%send (msg)
           ;; Within one behavior invocation there can be no
           ;; significance to the ordering of sent messages.
           (push msg sends))
         
         (%become (new-beh)
           (setf pend-beh new-beh))
         
         (%abort-beh ()
           (setf pend-beh *self-beh*
                 sends    nil))

         (dispatch-loop ()
           ;; -------------------------------------------------------
           ;; Think of the *current-x* global vars as dedicated registers
           ;; of a special architecture CPU which uses a FIFO queue for its
           ;; instruction stream, instead of linear memory, and which
           ;; executes breadth-first instead of depth-first. This maximizes
           ;; concurrency.
           ;; -------------------------------------------------------
           
           ;; Fetch next event from event queue - ideally, this
           ;; would be just a handful of simple register/memory
           ;; moves and direct jump. No call/return needed, and
           ;; stack useful only for a microcoding assist. Our
           ;; depth is never more than one Actor at a time,
           ;; before trampolining back here.
           
           (loop until done
                 do
                   (when-let (evt (mpc:mailbox-read *central-mail* nil timeout))
                     (setf *self-msg-parent* (and (car (the cons evt)) evt)
                           *self*            (cadr (the cons evt))   ;; self
                           *self-msg*        (cddr (the cons evt)))  ;; self-msg
                     (tagbody
                      RETRY
                      (setf pend-beh   (actor-beh (the actor *self*))
                            sends      nil
                            *self-beh* pend-beh)
                      ;; ---------------------------------
                      ;; Dispatch to Actor behavior with message args
                      (apply (the function pend-beh) (the list *self-msg*))
                      
                      ;; ---------------------------------
                      ;; Commit BECOME and SENDS
                      (unless (or (eq *self-beh* pend-beh)   ;; no BECOME
                                  (mpc:compare-and-swap
                                   (actor-beh (the actor *self*))
                                   *self-beh* pend-beh))     ;; effective BECOME
                        ;; failed on behavior update - try again...
                        (report-collision) ;; for engineering telemetry
                        (go RETRY)))
                     
                     (dolist (msg (the list sends))
                       (mpc:mailbox-send *central-mail* msg))
                     )))
         
         (dispatcher ()
           (loop until done 
                 do
                   (with-simple-restart (abort "Handle next event")
                     (dispatch-loop))
                 )))
      (declare (dynamic-extent #'%send #'%become #'%abort-beh
                               #'dispatch-loop #'dispatcher))
      ;; --------------------------------------------

      (let ((*dyn-specials* (make-dyn-specials
                             :send-hook      #'%send
                             :become-hook    #'%become
                             :abort-beh-hook #'%abort-beh
                             )))
        (declare (dynamic-extent *dyn-specials*))
        ;; --------------------------------------------
        (cond
         (actor-provided-p
          ;; we are an ASK
          (if (is-pure-sink? actor)
              (values nil t)
            ;; else
            (let ((me  (once
                        (create
                         (lambda* msg
                           (setf done (list msg))))
                        )))
              (setf timeout *ASK-TIMEOUT*)  ;; for periodic DONE checking
              (forced-send-after *timeout* me +timed-out+) ;; overall timeout from ASK caller
              
              (apply #'send-to-pool actor me message)
              (with-simple-restart (abort "Terminate ASK")
                (dispatcher))
              (when done
                (values (car done) t)))
            ))
         
         (t  ;; else - we are normal Dispatch thread
             (with-simple-restart (abort "Terminate Actor thread")
               (dispatcher)))
         ))
      )))

;; ----------------------------------------------------------------
;; Error Handling

(define-condition referred-error (error)
  ((from  :reader referred-error-from
          :initarg :from)
   (err   :reader referred-error-err
          :initarg :err))
  (:report (lambda (c stream)
             (format stream
                     "Referred Error from: ~A err: ~A"
                     (referred-error-from c)
                     (referred-error-err c)))
   ))

(defun err-from (e)
  (make-condition 'referred-error
                  :from self
                  :err  e))

(defun* check-for-errors ((&optional x &rest args))
  ;; Error replies for ASK are single element lists containing an
  ;; error Condition object.
  (when (and (null args)
             (typep x 'error))
    (error x)))

(defun err-chk (cust)
  ;; like FWD, but checks for error return
  (create
   (behav (&rest msg)
     (check-for-errors msg)
     (send* cust msg))))

;; ------------------------------------------------
;; ASK - The bridge between imperative code and the Actors world
;;
;; Foreign threads can use ASK to query an Actor that provides a
;; response to a customer. It is superfluous to do so from an Actor.
;;
;; For querying such an Actor, just leave out the customer arg in your
;; message. Your thread joins the dispatcher pool until an answer can
;; be had.
;;
;; In effect, this saves the current continuaition (stack state) and
;; enters into the world of dispatchers until an answer is generated.
;; Then at the eariliest opportunity the (now dispatcher) current
;; thread returns with that answer.
;;
;; If your thread becomes busy performing a behavior, you have to wait
;; until that behavior finishes. If it is idle, waiting for message
;; Events at the Central Mailbox when the answer is generated, then
;; you will return within 1s. If your thread is the one generating the
;; answer, then return is immediate.
;;
;; Meanwhile, sit back and enjoy SMP Parallel Processing on Multi-Core
;; architectures.
;;
;; ASK performs an immediate reified SEND. So, when called from within
;; an Actor behavior, that violates transactional boundaries, and can
;; produce leakage that is best avoided. If you are in an Actor
;; behavior it is better to avoid using ASK, and use β-forms for CPS
;; Actor style.

(define-condition recursive-ask (warning)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream
                     "An Actor calling ASK violates transactional boundaries.
         Try using β-forms instead."))))

(defun do-with-recursive-ask (fn)
  (handler-bind
      ((recursive-ask #'muffle-warning))
    (funcall fn)))

(defmacro with-recursive-ask (&body body)
  ;; Under some special conditions we know it will be okay to allow
  ;; recursive ASKs. This macro muffles the warning.
  ;;
  ;; E.g., an Actor executing a user supplied function on behalf of
  ;; another thread. The function might (unwittingly) call ASK to
  ;; query other portions of the Actor system.
  ;;
  ;; In that case, we are behaving as the client thread, running
  ;; ostensibly single-threaded code, and pretend not to know about
  ;; the Actors system.
  `(do-with-recursive-ask (lambda ()
                            ,@body)))

(define-condition terminated-ask (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Terminated ASK"))
   ))

(defmacro ensure-actors-running ()
  `(get-send-hook)) ;; ensures that the Actors system is running
  
(defgeneric ask (target &rest msg)
  (:method ((target actor) &rest msg)
   ;; Unlike SEND, ASKs are not staged, and perform immediately,
   ;; potentially violating transactional boundaries. From non-Actor
   ;; code, this is normally okay, and expected behavior. ASK behaves
   ;; like a function call.
   ;;
   ;; But if called from within an Actor, the immediacy violates
   ;; transactional boundaries, since SEND is normally staged for
   ;; execution at successful exit, or discarded if errors.
   (if self
       (warn 'recursive-ask)
     ;; else
     (ensure-actors-running))
  
   ;; In normal situation, we get back the result message as a list and
   ;; flag t.  In exceptional situation, from restart "Terminate ASK",
   ;; we get back nil.  If *TIMEOUT* is not-nil, and timeout occurs, we
   ;; get back list (<timeout-condiiton-object>) as ans.
   (multiple-value-bind (ans okay)
       (apply #'run-actors target msg)
     (unless okay
       (error 'terminated-ask))
     (check-for-errors ans)
     (values-list ans)))
  (:method ((target function) &rest msg)
   (apply target msg))
  (:method (target &rest msg)
   (values)))
   
;;
;; ASK can generate errors:
;;
;;    TERMINATED-ASK -- happens if we, while acting as Dispatch and
;;    awaiting our result, receive an error condition that the user
;;    chooses to respond to with restart "Terminate Ask". That error
;;    may arise from some other logical task that has nothing to do
;;    with us, but happened on our watch.
;;
;;    TIMEOUT-ERROR - whatever we ASK'd to happen did not produce
;;    a result for us before expiration of our timeout. Timout
;;    duration is set by wrapping our ASK with WITH-TIMEOUT. By
;;    default, it is nil, meaning no timeout.
;;
;;    REFERRED-ERROR - our request produced an error while being
;;    handled by some Dispatcher. It will have been reported
;;    already, but sent back to our ASK for our use. We have to
;;    decide what to do with it.
;;

;; ----------------------------------------
;; We must defer startup until the MP system has been instantiated.

(defun* lw-start-actors _
  ;; ...the compiler complains that I'm not making use of an
  ;; atomic-exchange result
  (do-nothing (mpc:atomic-exchange *send-hook* nil))
  (init-custodian)
  ;; (princ "Actors are alive!")
  (its-alive!!)
  (values))

(defun* lw-kill-actors _
  (kill-actors-system)
  (mpc:process-wait "Waiting for Actors Shutdown"
                    (lambda ()
                      (null *send-hook*)))
  (princ "Actors have been shut down.")
  (values))

#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor System"
                    'lw-start-actors
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor System"
                    'lw-kill-actors)

  (lw:define-action "Save Session After"
                    "Restart Actor System"
                    'lw-start-actors)
  )

#| ;; for manual loading mode...
(if (mpc:get-current-process)
    (unless (actors-running-p)
      (lw-start-actors))
  ;; else
  (pushnew '("Start Actors" () lw-start-actors) mpc:*initial-processes*
           :key #'third))
|#
