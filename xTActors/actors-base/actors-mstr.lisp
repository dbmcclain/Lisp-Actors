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
  nil)

(deflex sink
  (create))

(defgeneric is-pure-sink? (ac)
  ;; used by networking code to avoid sending useless data
  (:method ((ac actor))
   (not (functionp (resolved-beh (actor-beh ac)))))
  (:method (ac)
   t) )

(defun become-sink ()
  (become (sink-beh)))

;; --------------------------------------------------------
;; Core RUN for Actors

;; -------------------------------------------------
;; Message Frames - submitted to the event queue. These carry their
;; own link pointer to obviate consing on the event queue.

(defstruct (msg
            (:constructor msg (actor args &optional link)))
  link
  id
  (parent              *current-message-frame*)
  (actor (create)      :type actor)
  (args  nil           :type list))

(defun send-to-pool (actor &rest msg-args)
  ;; the default SEND for foreign (non-Actor) threads
  #F
  (mpc:mailbox-send *central-mail* (msg actor msg-args)))

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

(defvar *send*  nil)

(defun get-send-hook ()
  (sys-cached *send*
              (prog1
                  (setf *central-mail* (mpc:make-mailbox :lock-name "Central Mail")
                        *send*         #'send-to-pool)
                (restart-actors-system *nbr-pool*))
              ))

(defun send (target &rest msg)
  (when (actor-p target)
    (apply (get-send-hook) target msg)))

(defun send* (&rest args)
  ;; when last arg is a list that you want destructed
  (apply #'send (apply #'list* args)))

(defun repeat-send (target)
  (send* target self-msg))

(defun send-combined-msg (cust msg1 msg2)
  (multiple-value-call #'send cust (values-list msg1) (values-list msg2)))

;; ---------------------------------------

(defvar *not-actor*  "Not in an Actor")

(defvar *become*
  (lambda (new-beh)
    (declare (ignore new-beh))
    (error *not-actor*)))

(defun become (new-beh)
  #F
  (funcall *become* (screened-beh new-beh)))

;; -----------------------------------
;; In an Actor, (ABORT-BEH) undoes any BECOME and SENDS to this point,
;; but allows Actor to exit normally. In contrast to ERROR action
;; which aborts all BECOME and SENDs and exits immediately. ABORT-BEH
;; allows subsequent SENDs and BECOME to still take effect.

(defvar *abort-beh*
  #'do-nothing)

(defun abort-beh ()
  (funcall *abort-beh*))

;; -----------------------------------------------------------------
;; Generic RUN for all threads
;;
;; Actors are now completely transactional. SENDs and BECOME are
;; staged for commit or rollback. They are committed upon successful
;; completion of the Actor behavior code invocation, and discarded
;; otherwise.
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
;; use REMOVE, never DELETE, with Actor state sequences.
;;
;; In all cases, Actor state should only ever be changed through the
;; use of BECOME, since Actor state is visible to all parallel threads
;; executing the same Actor behavior code.
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

(defun #1=run-actors (&optional (actor nil actor-provided-p) &rest message)
  #F
  (let (sends evt pend-beh done timeout)
    (labels ((%send (actor &rest msg-args)
               ;; Within one behavior invocation there can be no
               ;; significance to the ordering of sent messages.
               (setf sends (msg actor msg-args sends)))

             (%become (new-beh)
               (setf pend-beh new-beh))
             
             (%abort-beh ()
               (setf pend-beh self-beh
                     sends    nil))

             (commit-sends (msg)
               (when msg
                 (let ((next (shiftf (msg-link msg) nil))) ;; for GC
                   (mpc:mailbox-send *central-mail* msg)
                   (commit-sends next)
                   )))
             
             (dispatch-loop ()
               ;; -------------------------------------------------------
               ;; Think of the *current-x* global vars as dedicated registers
               ;; of a special architecture CPU which uses a FIFO queue for its
               ;; instruction stream, instead of linear memory, and which
               ;; executes breadth-first instead of depth-first. This maximizes
               ;; concurrency.
               ;; -------------------------------------------------------
               (tagbody
                AGAIN
                ;; Fetch next event from event queue - ideally, this
                ;; would be just a handful of simple register/memory
                ;; moves and direct jump. No call/return needed, and
                ;; stack useful only for a microcoding assist. Our
                ;; depth is never more than one Actor at a time,
                ;; before trampolining back here.
                (when done
                  (return-from #1# (values (car done) t)))
                (unless (msg-p (setf evt (mpc:mailbox-read *central-mail* nil timeout)))
                  (go AGAIN))
                
                (let ((*current-message-frame*  (and (msg-parent (the msg evt)) evt))
                      (*current-actor*          (msg-actor (the msg evt)))  ;; self
                      (*current-message*        (msg-args  (the msg evt)))) ;; self-msg
                  (unless (actor-p *current-actor*)
                    (go AGAIN))
                  
                  (tagbody
                   RETRY
                   (setf pend-beh (actor-beh (the actor self))
                         sends    nil)
                   (let ((*current-behavior*  pend-beh) ;; self-beh
                         (behfn               (resolved-beh pend-beh)))
                     (unless (functionp behfn)
                       (go AGAIN))
                     
                     ;; ---------------------------------
                     ;; Dispatch to Actor behavior with message args
                     (apply (the function behfn) (the list self-msg))
                     
                     ;; ---------------------------------
                     ;; Commit BECOME and SENDS
                     (unless (or (eq self-beh pend-beh)               ;; no BECOME
                                 (%actor-cas self self-beh pend-beh)) ;; effective BECOME
                       ;; failed on behavior update - try again...
                       (go RETRY))

                     (commit-sends sends)
                     (go AGAIN)
                     ))
                  )))
             
             (dispatcher ()
               (loop
                  (with-simple-restart (abort "Handle next event")
                    (dispatch-loop))
                  )))
      (declare (dynamic-extent #'%send #'%become #'%abort-beh
                               #'dispatch-loop #'dispatcher #'commit-sends))
      (let ((*send*      #'%send)
            (*become*    #'%become)
            (*abort-beh* #'%abort-beh))
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
              (setf timeout +ASK-TIMEOUT+)  ;; for periodic DONE checking
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
;; The bridge between imperative code and the Actors world
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
;; If your thread is busy performing a behavior, you have to wait
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
;; behavior it is still better to avoid using ASK, and use β-forms for
;; CPS Actor style.

(define-condition recursive-ask (warning)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream
                     "Calling ASK from within an Actor has you violating transactional boundaries.
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
   (when self
     (warn 'recursive-ask))

   (get-send-hook) ;; ensures that the Actors system is running
  
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
  (mpc:atomic-exchange *send* nil)
  (princ "Actors are alive!"))

(defun* lw-kill-actors _
  (kill-actors-system)
  (print "Actors have been shut down."))

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
