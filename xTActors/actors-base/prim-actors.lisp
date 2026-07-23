
;; prim-actors.lisp - A collection of useful primitive Actors
;;
;; DM/RAL 05/21
;; ------------------------------------------------------
(in-package :com.ral.actors.base)

#| ------------------------------------------------------
   There are, broadly, some conventions followed for Actor messages:
  
    1. When an Actor expects a customer argument it is, by
    convention, always in first position.
  
    2. Every Actor network living behind a SERIALIZER *must* arrange
    to send a reply to the customer.
  
    3. Sink Actors don't have customers, unless they live behind a
    SERIALIZER gate.
  
    4. Timeout conditions are signaled by sending +TIMED-OUT+ to a
    customer. This is a predefined timeout condition object.
  
    5. When nothing else to send cust, send :OK. That helps when the
    sender wants to sequence its behavior using continuation Actors
    (β). If sender doesn't care, they will send SINK (or NIL) as
    customer.
  
    6. There are no guarantees that a message will be sent to a
    customer. When in doubt you have no other choice but to rely on
    timeout notification.
  
    This is to be contrasted with UNWNID-PROTECT. Such behavior is not
    unique to Actors, and can occur in any system in which
    continuations / coroutines are employed.
  
    7. In a parallel environment, BECOME could fail and cause an
    automatic retry of the message delivery. So you need to ensure
    that any non-idempotent code alongside BECOME is marked as
    NON-IDEMPOTENT, ON-COMMIT, or RESTARTABLE (they all mean the same
    thing), or SEND to a CREATE of the thunk packaging the code. This
    ensures that the body of code will not be executed unless BECOME
    succeeds.
  
    8. Since Actors are transactional, SEND, SEND*, REPEAT-SEND,
    SEND-TO-ALL, SEND-ALL-TO, and SEND-AFTER, are always idempotent.
    Message will only be sent at exit of body code, and only if BECOME
    succeeds. But the message args might have been created using
    non-idempotent code, so see (7).
  
    9. Inside the body of a β-clause, NON-IDEMPOTENT, ON-COMMIT, and
    RESTARTABLE, the SELF object is no longer the containing Actor. So
    BECOME should not be used there. Same with (SEND SELF ...).
  
    For (SEND SELF ...) you could capture the outer SELF into a
    binding and send to it;
  
        (...actor-body code...
         (let ((ME  SELF))
           (β _
               (send some-actor β ...)
             (SEND ME ..))))
  
     But BECOME always pertains to SELF.
  
   ------------------------------------------------------- |#
;; === NOTE: Irreversible Side-Effecting Behaviors ===
;;
;; When Actor bodies must perform irreversible side effects, aka
;; non-idempotency, e.g, Drop the Bomb, which cannot be undone - you
;; must take precautions.  Concurrent access to the same Actor code
;; body is a given. You have to prevent duplication of effort. Ensure
;; that only one logical thread of activity can succeed.
;;
;; You should always surround the code with (NON-IDEMPOTENT ...)
;; which would then be executed in a separate anonymous Actor, but
;; only if the entire rest of the behavior code successfully
;; completes.
;;
;; An unsuccessful execution of the behavior can happen for two
;; possible reasons:
;;
;;    1. Your code triggers an error condition and aborts, and so
;;    nothing will have happened. Your staged SENDs, BECOME, and
;;    NON-IDEMPOTENT clauses will be discarded.
;;
;;    2. You tried to BECOME while another logical thread commited a
;;    BECOME just before your attempt to commit at code exit. And so,
;;    all your staged actions will be discarded, and you will be
;;    automatically retried.
;;
;;    But the non-idempotent action might already have been triggered.
;;    You can't be certain unless there is only one clause in the
;;    behavior that peforms a BECOME.  So a successful BECOME must
;;    necessarily establish a new bahavior that precludes any repeat
;;    of the same non-idempotent action.
;;
;; But this alone, does not ensure non-duplication of effort. Two
;; concurrent logical threads of activity could both succeed in the
;; Actor body, if there were no BECOME. Lacking a BECOME, the only
;; hazard is execution error. And so this could have devastating
;; consequences since both couild have performed the same
;; non-idempotent action.
;;
;; So any non-idempotent clause should always be accompanied by a
;; BECOME to some new behavior that preclude repitition of the
;; non-idempotent action.
;;
;; In concurrent code, which must be assumed as the usual state of
;; affairs for Actors code, you should never perform any
;; non-idempotent action unless one of 3 possible situations:
;;
;;    1. Your Actor residees behind a SERIALIZER gate to ensure single
;;    logical-thread access,
;;
;;    2. Your Actor performs an accompanying BECOME to a new behavior
;;    that precludes repitition of the non-idempotent action.
;;
;;    3. Your Actor is a private Actor and has never been shared, nor
;;    has any duplicate construct doing the same non-idempotent action
;;    ever been created.
;;
;; There is one oft-used non-idempotent action taken in many Actors -
;; BECOME-SINK, which turns the Actor into a one-time-only use. This a
;; brute-force way to enforce single-task access. It just drops all
;; future messages on the floor, after having succeeded just once.
;;
;; Concurrent code is fun, but requires deep, careful, thought. But
;; concurrency has nothing at all to do with whether or not your
;; machine has multiple threads running.
;;
;; Actors carry no sense of threads, locks, mutexes, etc. You can
;; program as though you are single-threaded.  But concurrency spawns
;; the notion of concurrent tasks, or logical-threads, all of which
;; can be running at the same time.
;;
;; Multicore CPU's enable Parallel Concurrency. Lacking that you could
;; still have Serial and/or Time-Sliced, Single- or Multi-Thread,
;; Concurrency, which carries nearly identical hazards to Parallel
;; Concurrency.
;;
;; The only difference between Parallel and non-Parallel is that the
;; machine must perform cache-coherency operations among the CPU Cores
;; for Parallel operation. This has almost nothing to do with the
;; programmer.
;; ------------------------------------------------------------

;; ------------------------------------------------------
;; FN-ACTOR - the most general way of computing something is to send
;; the result of an Actor execution to a customer. That may involve an
;; indefinite number of message sends along with continuation Actors
;; before arriving at the result to send to the customer.
;;
;; But sometimes all we need is a simple direct function call against
;; some args. In order to unify these two situations, we make
;; FN-ACTORs which encapsulate a function call inside of an Actor.
;; 
;; See also: SERVICES.

(defun fn-actor (fn)
  (create
   (λ (cust . args)
     (send* cust (multiple-value-list (apply fn args))))
   ))

;; ------------------------------------------------------
;; Get everyone playing off the same sheet of music...
;;
;; The trouble with printing is that any arbitrary thread can execute
;; a println, writeln, or fmt-println. And where that output goes
;; depends on the thread.
;;
;; Most of the time, it goes to the background print stream. But if
;; you happen to ASK from the Listener REPL, then some output may also
;; go to the Listener window. It just depends on which thread is
;; available for message dispatch.
;;
;; So by ensuring a non-Actor background thread to perform all
;; printing activity, we can be assured that the output will always go
;; to just one destination - the background printer stream.

(defun printing-handler (cust fn)
  ;; Not Actor code - no difference between SEND and SEND-TO-POOL
  (unwind-protect
      (funcall fn)
    (send-to-pool cust)))

;; --------------------------------------------
;; Central Printer
;;   - SERIALIZER ensures that only one request will be in flight at any time.
;;   - FUNCALL-ASYNC ensures that a background thread will be used for consistent printing behavior.

(deflex* printer
  (serializer-sink
   (create
    (lambda (cust fn)
      (mpc:funcall-async #'printing-handler cust fn))
    )))
  
(defmacro with-printer (&body body)
  `(send printer (lambda ()
                   ,@body)))

;; --------------------------------------------

(deflex println
  (create
   (behav msg
     (with-printer
      (format t "~&~{~A~%~}" msg))
     )))

(defun do-with-maximum-io-syntax (fn)
  (with-standard-io-syntax
    (let ((*print-radix*  t)
          (*print-circle* t)
          (*read-default-float-format* 'double-float))
      (handler-case
          (funcall fn)
        (print-not-readable ()
          (let ((*print-readably* nil))
            (funcall fn)))
        ))))
  
(defmacro with-maximum-io-syntax (&body body)
  `(do-with-maximum-io-syntax (lambda () ,@body)))

(deflex writeln
  (create
   (behav msg
     (with-printer
      (with-maximum-io-syntax
        (format t "~&~{~:W~%~}" msg)))
     )))

(deflex fmt-println
  (create
   (behav (fmt-str &rest args)
     (with-printer
        (terpri)
        (apply #'format t fmt-str args))
     )))

(defun its-alive!! ()
  (send println "Actors are alive!"))

;; ---------------------------------

(define ((fwd-beh actor) . msg)
  (send* actor msg))

(defun fwd (actor)
  (create (fwd-beh actor)))

;; ---------------------------------

(defun once (cust)
  "ONCE -- Construct an Actor to behave as a FWD relay to the
customer, just one time."
  (create
   (behav (&rest msg)
     (send* cust msg)
     (become-sink))
   ))

;; -----------------------------------------
;; Delayed Send

(defun forced-send-after (dt actor &rest msg)
  ;; FORCED-SEND-AFTER -- unsafe for use in a BECOME branch of
  ;; behavior code...
  ;;
  ;; NOTE: Actors, except those at the edge, must never do anything
  ;; that has observable effects beyond SEND and BECOME. Starting a
  ;; timer running breaks this. Our caller might have to be retried,
  ;; in which case there will be a spurious timer running from a prior
  ;; attempt.
  ;;
  (when (realp dt)
    (let ((timer (apply #'mpc:make-timer #'send-to-pool actor msg)))
      (mpc:schedule-timer-relative timer (max 0 dt)))
    ))
  
(defun send-after (dt actor &rest msg)
  ;; SEND-AFTER is safe to use in a BECOME branch of behavior code.
  ;; The timer doesn't get launched unless the SENDS and BECOME commit
  ;; at our exit.
  ;;
  ;; We mark the timer launch as non-idempotent so that it happens in
  ;; an edge Actor, and gets launched via message SEND. If we get
  ;; retried, that SEND will have been discarded and possibly tried
  ;; again during message delivery retry.
  ;;
  (when (realp dt)
    (on-commit
      (apply #'forced-send-after dt actor msg))
    ))

;; --------------------------------------------
;; Updatable Timeouts...
;;
;; We often have no idea how much time to allow a process. But we need
;; a timeout to avoid indefinite delay if things might go wrong.
;;
;; So we set up for a renewable timeout mechanism. A ONCE gate is
;; normally passsed along as the customer to send results to. It sets
;; up a race between either a timeout error or a generated result back
;; to the original customer.
;;
;; But a renewable gate also allows you to cancel/renew the timeout to
;; an improved estimate as you pass milestones in the logical task.
;;
;; You do this by sending the message :RENEW-TIMEOUT to your customer,
;; which is the gate.
;;
;; In the event that your customer is not a RENEWABLE-TIMED-GATE, then
;; the message will most likely be ignored.
;;
;; NOTE: Do not mistake a LOGICAL TASK for a MACHINE THREAD. They are
;; completely orthogonal to each other. One logical task may span the
;; execution of any number of machine threads, back and forth between
;; several of them, or not. Whichever thread runs a portion of a
;; logical task is of no consequence.
;;
;; And any one machine thread, running a Dispatcher, can execute
;; portions of any number of different logical tasks.
;;
;; Machine Threads have dynamic context surrounding function
;; execution. The context grows and shrinks with the use of Lisp
;; control structures. Threads have dynamic binding for special vars.
;;
;; Logical Tasks have no notion of dynamic context. No dynamic binding
;; of context between Actors. Each portion of a logical task (an
;; Actor) exists as an island function with dynamic depth 1.

(define-condition no-timeout (warning)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream
                     "Running without a timeout opens your task to indefinite suspension."))
   ))

(defun check-timeout (timeout timeout-present-p)
  ;; By referring to TIMEOUT-PRESENT-P we allow for explicitly
  ;; cancelling a timeout timer by way of using a non-REAL TIMEOUT
  ;; value. There might be a good reason for doing this.
  (unless timeout-present-p
    (unless (realp timeout)
      (warn 'no-timeout))))

(defun renewable-timed-gate-beh (cust tag timer-tag)
  (alambda
   ((acust :renew-timeout &optional (timeout nil timeout-present-p))
    ;; NOTE: We do not reference *TIMEOUT* here. Can you understand why not?
    (check-timeout timeout timeout-present-p)
    (let ((new-tag  (tag self)))
      (become (renewable-timed-gate-beh cust new-tag timer-tag))
      (send-after timeout new-tag timer-tag)
      (send* acust :ok)))
   
   ((atag _) / (eq atag tag)
    ;; The current timeout timer is the only thing that knows TAG.
    (send cust +timed-out+)
    (become-sink))

   ((_ atag) / (eq atag timer-tag)
    ;; Older timeout timers are the only things that know TIMER-TAG.
    ;; So we filter away these messages to avoid triggering the ONCE'ness.
    )

   (msg
    ;; for results sent directly to this gate
    (send* cust msg)
    (become-sink))
   ))

(defun timed-gate (cust &optional (timeout *timeout* timeout-present-p))
  (check-timeout timeout timeout-present-p)
  (actors ((tag       (tag gate))  ;; TAGs are unique and self-identifying
           (timer-tag (tag gate))
           (gate      (create
                       (renewable-timed-gate-beh cust tag timer-tag))))
    (send-after timeout tag timer-tag)
    gate))

(defun timed-service (svc &optional (timeout *timeout* timeout-present-p))
  ;; The clock only starts running when a message is sent to svc.
  (check-timeout timeout timeout-present-p)
  (create
   (behav (cust &rest msg)
     ;; A new message starts a new logical task that can be killed on
     ;; a timeout condition, or as soon as an answer is sent to
     ;; cutomer, whichever occurs first.
     (send* svc (timed-gate cust timeout) msg))
   ))

;; --------------------------------------------

(defun error-reply-checker (cust &optional (error-type 'error))
  (create
   (alambda
    ((ans) / (typep ans error-type)
     (error ans))

    (msg
     (send* cust msg))
    )))

(defun timeout-checked-serivce (svc)
  ;; Make a service wrapper that checks for timeout errors on the way
  ;; to delivering results to your customer.
  (create
   (lambda* (cust . msg)
     (send* svc (error-reply-checker cust 'timeout) msg))
   ))

(defun checked-service (svc)
  ;; Wrap service with an Actor that interposes an error checker
  ;; customer between the service and your actual customer.
  (create
   (lambda* (cust . msg)
     (send* svc (error-reply-checker cust) msg))
   ))

#|
(let ((svc (create
            (lambda (cust msg)
              (sleep 2)
              (send cust msg)))
           ))
  (with-timeout 1
    (send (checked-service
           (timed-service svc)) println :hello)))
|#
;; --------------------------------------------
;; Why not simply combine CHECKED-SERVICE with TIMED-SERVICE?
;;
;; See SERIALIZER.
;;
;; A serialized service must reply to its customer. A SERIALIZER
;; interposes itself between the service and the actual customer, in
;; order to allow the next task to enter the serialized service.
;;
;; We must place any error checking on the aft of the SERIALIZER block
;; to avoid interfering with the SERIALIZER's interposing.
;; --------------------------------------------

(defun send-to-all (actors &rest msg)
  "SEND-TO-ALL -- Send a message to all of the indicated Actors."
  (dolist (actor actors)
    (send* actor msg)))

(defun send-all-to (actor msg-list)
  "SEND-ALL-TO -- Send all of the messages to an Actor."
  (dolist (msg msg-list)
    (send* actor msg)))

;; ---------------------

(defun race (&rest actors)
  ;; Getting an answer from whichever actor responds first to the same
  ;; message
  (create
   (behav (cust &rest msg)
     (let ((gate (once cust)))
       (apply #'send-to-all actors gate msg)))
   ))

(defun running-one-of-beh (&rest active)
  ;; Sends result from successful channel and :NOK to all other
  ;; waiting customers.
  (alambda
   ((atag . ans)
    (let* ((pair (assoc atag active))
           (cust (cdr pair)))
      (send* cust ans)
      (become-sink)))
   ))

(defmacro one-of (&rest msgs)
  ;; Getting an answer from whichever send generates a response first.
  ;; Messages may all be different, and customers may all be
  ;; different. Only one response is accepted.
  ;;
  ;; But whichever message generates a response first, that is the
  ;; response delivered to one of the customers. The other customers
  ;; are left dangling. If that isn't what you want, then see
  ;; ALT-WITH-NAK below.
  ;;
  (um:with-unique-names (gate)
    (let* ((custs (mapcar #'third msgs))
           (cust  (car custs)))
      (if (every (um:curry #'equalp cust) custs)
          `(let ((,gate  (once ,cust)))
             ,@(mapcar #`(,@(um:take 2 a1) ,gate ,@(um:drop 3 a1)) msgs))
        ;; else
        (let ((tags  (mapcar (lambda (x)
                               (declare (ignore x))
                               (gensym (string :tag)))
                             msgs)))
          `(actors (,@(mapcar #`(,a1 (tag ,gate)) tags)
                    (,gate  (create
                             (running-one-of-beh ,@(mapcar #2`(cons ,a1 ,a2) tags custs))
                             )))
             ,@(mapcar #2`(,@(um:take 2 a1) ,a2 ,@(um:drop 3 a1)) msgs tags))
          ))
      )))
#|
Example:
(one-of
 (send chan1 cust :mess1)
 (send chan2 cust :mess13)
 ...)
=>
(LET ((#:GATE14654 (ONCE CUST)))
  (SEND CHAN1 #:GATE14654 :MESS1)
  (SEND CHAN2 #:GATE14654 :MESS13)
  ...)
|#

(defun running-alt-beh (&rest active)
  ;; Sends result from successful channel and :NOK to all other
  ;; waiting customers.
  (alambda
   ((atag . ans)
    (let* ((pair (assoc atag active))
           (cust (cdr pair)))
      (send* cust ans)
      (dolist (ent active)
        (let ((cust′ (cdr ent)))
          (unless (eq cust′ cust)
            (send cust′ :nok))))
      (become-sink)
      ))
   ))

(defmacro alt-with-nak (&rest msgs)
  ;; Allow only the first responder, pass along its response to its customer,
  ;; and pass along :NOK to all other customers in this race.
  (um:with-unique-names (gate)
    (let ((custs (mapcar #'third msgs))
          (tags  (mapcar (lambda (x)
                           (declare (ignore x))
                           (gensym (string :tag)))
                         msgs)))
      `(actors (,@(mapcar #`(,a1 (tag ,gate)) tags)
                (,gate  (create
                         (running-alt-beh ,@(mapcar #2`(cons ,a1 ,a2) tags custs))
                         )))
         ,@(mapcar #2`(,@(um:take 2 a1) ,a2 ,@(um:drop 3 a1)) msgs tags))
      )))
#|
Example
(alt-with-nak
 (send chan1 cust1 :mess1)
 (send chan2 cust2 :mess13)
 ...)
=>
(ACTORS ((#:TAG14722 (TAG #:GATE14721))
         (#:TAG14723 (TAG #:GATE14721))
         ...
         (#:GATE14721
          (CREATE (RUNNING-ALT-BEH (LIST (CONS #:TAG14722 CUST1)
                                         (CONS #:TAG14723 CUST2))))))
  (SEND CHAN1 #:TAG14722 :MESS1)
  (SEND CHAN2 #:TAG14723 :MESS13)
  ...)
|#

;; --------------------------------------------
;; Wrapping a client Actor with a handler. If a response is :NOK or
;; +TIMED-OUT+, then the handler is prodded with a null message.
;; Otherwise the normal response is passed along to the customer. Only
;; one response is permitted.

(defun wrap (client handler)
  (once
   (create
    (alambda
     ((:nok)
      (send handler))
     ((ans) / (eq ans +timed-out+)
      (send handler))
     (ans
      (send* client ans))
     ))))

(defun also (client handler)
  ;; Similar to WRAP, but wraps the client Actor with an Actor that
  ;; will forward messages to the client and also prod the handler.
  (create
   (lambda* msg
     (send* client msg)
     (send handler))))

;; --------------------------------------------

(defun select (cust &rest services)
  ;; Obtain answer from one of the services, or a timeout.
  (let ((gate (timed-gate cust)))
    (send-to-all services gate)))

(defun expect (actor cust &rest msg)
  ;; Expect an answer from actor being sent to cust, or timeout.
  ;; Analog of SEND
  (select cust (apply #'racurry actor msg)))

(defun expect* (actor cust &rest msg)
  ;; Convert actor into a service and expect of it.
  ;; Analog of SEND*
  (apply #'expect actor cust (apply #'list* msg)))

#|
(send-after 1 println (list :timeout +timed-out+))
(β ans
    (with-timeout 1
      (select β))
  (send* println ans))
|#
;; -------------------------------------------------

(defun rate-limited-gate (cust dt)
  ;; Construct an Actor that allows sending a message to cust no
  ;; sooner than dt sec from when we exit the current behavior.
  ;; -- useful for real-time animation of graphs where we want to
  ;; control the animation update rate --
  (actors ((tag-dt   (tag joiner))
           (joiner   (create
                      (labels
                          ((waiting-beh (&rest msgs)
                             (alambda
                               ((atag) / (eq atag tag-dt)
                                (become (fwd-beh cust))
                                (send-all-to cust msgs))
                               
                               (msg
                                (become (apply #'waiting-beh msg msgs)))
                               )))
                        (waiting-beh)))))
    (send-after dt tag-dt) ;; won't fire until we exit beh
    joiner
    ))


;; --------------------------------------

(defun timing (dut)
  (create
   (behav (cust &rest msg)
     (let ((start (get-time-usec)))
       (β _
           (send* dut β msg)
         (send cust (- (get-time-usec) start)))
       ))))

#|
(let* ((dut (create
                (behav (cust nsec)
                  (sleep nsec)
                  (send cust)))
            (timer (timing dut))))
  (send timer println 1))
|#

;; -------------------------------------
;; Systolic Processing Pipelines

(defun acurry (actor &rest largs)
  ;; like Curried functions, but for Actors
  (create
   (behav (&rest rargs)
     (multiple-value-call #'send actor (values-list largs) (values-list rargs)))
   ))
  
(defun racurry (actor &rest rargs)
  (create
   (behav (&rest largs)
     (multiple-value-call #'send actor (values-list largs) (values-list rargs)))
   ))
  
(defun pipe (&rest elts)
  ;; Hmmm... constructs a new pipe every time invoked. But is this any
  ;; worse than a sequence of nested Beta forms? Same effect, just
  ;; performed in advance here.
  (create
   (behav (cust &rest msg)
     (send* (reduce #'acurry elts
                    :from-end t
                    :initial-value cust)
            msg))
   ))

(defun sink-pipe (&rest elts)
  ;; for pipelines whose last block are sinks
  (reduce #'acurry (butlast elts)
          :from-end t
          :initial-value (um:last1 elts)))

(defun tee (&rest sink-blks)
  ;; can be used to convert a sink into a filter component
  ;; A sink-block is one that does not take a cust arg in messages.
  (create (if sink-blks
              (behav (cust &rest msg)
                (apply #'send-to-all sink-blks msg)
                (send* cust msg))
            #'send)))

(defun splay (&rest custs)
  ;; Define a sink block that passes on the message to all custs
  (create
   (behav msg
     (apply #'send-to-all custs msg))
   ))
  
;; --------------------------------------------
;; UNW-PROT -- Unwind-protect for Actors... sort of...
;;
;; The pattern here is just like CHECKED-SERVICE, but with an extra
;; unwind chore to perform on the way out.

(defun unw-prot (svc unw &optional (timeout *timeout* timeout-present-p))
  (check-timeout timeout timeout-present-p)
  (create
   (lambda (cust &rest msg)
     (β ans
         (send* (timed-service svc timeout) β msg)
       (send unw)
       (send* cust ans)
       ))
   ))

;; --------------------------------------------
;; FILE-USER -- a WITH-OPEN-FILE for Actors... sort of...

(defun file-user (svc timeout &rest open-args)
  ;; SVC should know what to do with a (CUST FP ...) message.
  (create
   (lambda (cust &rest msg)
     (with-timeout timeout
       (let ((fp  (apply #'open open-args)))
         (send* (unw-prot svc (create
                               (lambda ()
                                 (close fp))))
                cust fp msg)
         )))
   ))

#|
(let ((counter (create
                (behav (cust fd)
                  ;; (sleep 5)
                  (let ((wds 0))
                    (loop for line = (read-line fd nil fd)
                          for ix from 0
                          until (eql line fd)
                          do
                            (incf wds (length (um:split-string line :delims '(#\space #\tab))))
                          finally (send fmt-println "File has ~D lines, ~D words" ix wds))
                    ;; (error "What!?")
                    ;; (sleep 5)
                    (send cust :ok))
                  ))))
  (β ans
      (send (file-user counter 3
            "/Users/davidmcclain/projects/Lispworks/color-theme.lisp"
            :direction :input)
            β)
    (send fmt-println "I guess we're done: ~A" ans)))
|#

;; ---------------------------------------

(define ((counter-beh &optional (ct 0)) . msg)
  (match msg
    ((:inc)
     (become (counter-beh (1+ ct))))
    ((:inc delta)
     (become (counter-beh (+ ct delta))))
    
    ((:dec)
     (become (counter-beh (1- ct))))
    ((:dec delta)
     (become (counter-beh (- ct delta))))

    ((:reset)
     (become (counter-beh 0)))
    ((:reset init)
     (become (counter-beh init)))

    ((cust :read)
     (send cust ct))
    ((cust :read-reset)
     (send cust ct)
     (become (counter-beh 0)))
    ((cust :read-reset init)
     (send cust ct)
     (become (counter-beh init)))
    ))

(defun counter (&optional (initial-ct 0))
  (create (counter-beh initial-ct)))

    
;; --------------------------------------------

(deflex inspector
  (create (lambda* args
            (inspect args))))

