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

(defun fwd-beh (actor)
  (behav (&rest msg)
    (send* actor msg)))

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

#| ;; Don't use - the clock starts running as soon as this is invoked.
(defun timed-gate (cust timeout)
  (cond ((realp timeout)
         (let ((gate (once cust)))
           (send-after timeout gate +timed-out+)
           gate))
        (t
         cust)))
|#

(defun timed-service (svc &optional (timeout *timeout*))
  ;; Prefer this, so that the clock only starts running when a message
  ;; is sent to svc.
  (create
   (behav (cust &rest msg)
     (let ((gate (once cust)))
       (send-after timeout gate +timed-out+)
       (send* svc gate msg)))
   ))

;; ---------------------

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
  (create
   (behav (cust &rest msg)
     (let ((gate (once cust)))
       (apply #'send-to-all actors gate msg)))
   ))
  
;; ---------------------

(defun once-tag (cust)
  (create
   (behav (&rest msg)
     (send* cust self msg)
     (become-sink))
   ))

;; ---------------------

(defun timed-tag (cust &optional (timeout *timeout*))
  (let ((atag (tag cust)))
    (send-after timeout atag +timed-out+)
    atag))

(defun timed-once-tag (cust &optional (timeout *timeout*))
  (let ((atag (once-tag cust)))
    (send-after timeout atag +timed-out+)
    atag))

;; -------------------------------------------------

(defun rate-limited-customer (cust dt)
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

;; -------------------------------------------------
;; LAZY-FUTURE - A non-macro β replacement?
;;
;;  Instead of:
;;
;;     (β (targ)
;;          (send* targ-generator β generator-args)
;;       (send* targ my-args))
;;
;;  do this:
;;
;;     (send* (lazy-future targ-generator generator-args) my-args)

;; -----------------------------------------------
;; Now two years out, and I still haven't found a use for FUTURE
;; DM/RAL 09/23

(defun future-wait-beh (tag &optional msgs)
  (alambda
   ((atag ans) / (eq tag atag)
    (become (const-beh ans))
    (send-all-to self msgs))
   (msg
    (become (future-wait-beh tag (cons msg msgs))))
   ))

(defun future (actor &rest constr-args)
  ;; Return a value that represents a future Actor target. Send the
  ;; Actor arg its constructor args to produce that future Actor
  ;; target.
  (actors ((fut (create
                 (future-wait-beh tag)))
           (tag (tag fut)))
    (send* actor tag constr-args)
    fut))

(defun lazy-future (actor &rest constr-args)
  (create
   (lambda* msg
     (let ((tag  (tag self)))
       (become (future-wait-beh tag (list msg)))
       (send* actor tag constr-args)))
   ))

#|
;; This peculiar construct is roughly equiv to a beta form, but more
;; general in that many future customers could be sent to the same
;; action

 (send (future ac arg1 arg2 ...) (α (&rest ans)
                                   ... body using ans))

 .EQUIV.

 (β (&rest ans)
     (send ac β arg1 arg2 ...)
   ... body using ans)
 |#

;; -----------------------------------------

(defun future-become-wait-beh (tag &optional msgs)
  ;; There are times when you know you need to BECOME, but the
  ;; parameters aren't yet available. This FUTURE-BECOME-BEH behavior
  ;; function allows you to wait until the behavior can be completely
  ;; defined.
  ;;
  ;; Meanwhile, incoming messges are stashed for transmission to
  ;; yourself, until you become fully defined.
  ;;
  ;; NOTE: See SET-BEH and why it is unsafe to directly mutate an
  ;; Actor's behavior.
  ;;
  ;; Use as:
  ;;
  ;;     (let ((tag   (TAG SELF)))
  ;;       (BECOME (FUTURE-BECOME-BEH tag))
  ;;       ... develop new-beh ...
  ;;       (SEND tag new-beh)) ;; now become new-beh
  ;;
  ;; --------------------------------------------------------
  ;; In the wait function, when a result Actor arrives, we must
  ;; produce a FWD to that Actor, and not simply subsume its behavior
  ;; function. Why?
  ;;
  ;; Desipite the fact that behaviors could be shared (as with macro
  ;; ACTORS), in this case, we are relying on some procesing to
  ;; produce a future Actor to us. And that Actor might well have some
  ;; TAG's and LABEL's pointing to it for its proper functioning.
  ;;
  ;; If we simply borrowed its behavior function for ourselves, we
  ;; would not receive the notifications from those TAG's and LABEL's,
  ;; because those are targeted to an Actor, not a behavior function.
  
  (alambda
   ((atag actor) / (eq atag tag)
    (become (fwd-beh actor))
    (send-all-to actor msgs))
   (msg
    (become (future-become-beh tag (cons msg msgs))))
   ))

(defun future-become (actor &rest constr-args)
  (let ((tag  (tag self)))
    (send* actor tag constr-args)
    (become (future-become-wait-beh tag))
    ))

(defun lazy-future-become (actor &rest constr-args)
  (create
   (lambda* msg
     (let ((tag  (tag self)))
       (become (future-become-wait-beh tag (list msg)))
       (send* actor tag constr-args)))
   ))

;; --------------------------------------
;; SER - make an Actor that evaluates a series of blocks sequentially
;; - i.e., without concurrency between them.  Each block is fed the
;; same initial message, and the results from each block are sent as
;; an ordered collection to cust.

(deflex ser
  (α (cust lst &rest msg)
    (if (null lst)
        (send cust)
      (let ((me self))
        (beta msg-hd
            (send* (car lst) beta msg)
          (beta msg-tl
              (send* me beta (cdr lst) msg)
            (send-combined-msg cust msg-hd msg-tl)))
        ))))

;; ----------------------------------------------

(defun simd (svc)
  ;; process an entire list of args in parallel
  ;; cust should expect a (&rest ans)
  ;; svc expects only a cust arg and a single operand
  ;;
  ;; -- So, why ever do this? --
  ;; You could as well just send individual messages to the svc,
  ;; naming the cust which then has to accept individual results.
  ;;
  ;; Using SIMD just gathers all results into an ordered list for
  ;; delivery to cust.
  (create
   (behav (cust &rest args)
     (send (apply #'fork (mapcar (curry #'racurry svc) args)) cust))))

#|
(let ((svc (create (behav (cust arg)
                     (send cust (1+ arg))))))
  (send (simd svc) println 1 2 3))
|#

(defun mimd (&rest svcs)
  ;; Multiple Instr, Mult Data - process a list of svc against a
  ;; matching list of args. Each svc expects a cust and a single operand.
  ;; cust should expect a (&rest ans)
  ;; Why? -- same comments as for SIMD --
  (create
   (behav (cust &rest args)
     (send (apply #'fork (mapcar #'racurry svcs args)) cust))))

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
  (when (and (actor-p actor)
             (realp dt))
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
  (on-commit
    (apply #'forced-send-after dt actor msg)))

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

;; ---------------------------------------------------------
;; SEQUENCED-DELIVERY
;;   Provider sends :DELIVER with sequence counter and message
;;   Consumer sends :READY with customer and sequence counter of desired messaage

(defun sequenced-delivery ()
  (labels ((sequenced-beh (&optional items)
             (alambda
              ((cust :ready ctr)
               (let ((msg (assoc ctr items)))
                 (cond (msg
                        (send cust (cdr msg))
                        (become (sequenced-beh (remove msg items))))
                       (t
                        (become (pending-sequenced-beh cust ctr items)))
                       )))
              
              ((:deliver ctr . msg)
               (become (sequenced-beh (acons ctr msg items))))
              ))

           (pending-sequenced-beh (cust ctr items)
             (alambda
              ((:deliver in-ctr . msg)
               (cond ((eql in-ctr ctr)
                      (send cust msg)
                      (become (sequenced-beh items)))
                     (t
                      (become (pending-sequenced-beh cust ctr (acons in-ctr msg items))))
                     )))))
    (create (sequenced-beh))
    ))

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
  
;; -------------------------------------------------------
;; Unwind-Protect for Actors...
;;

(defun warn-timeout (timeout timeout-provided-p msg)
  (unless (or timeout-provided-p
              (and (realp timeout)
                   (plusp timeout)))
    (warn "You are taking a risk not using a Timeout for ~A." msg)))


(defun unw-prot-beh (fn-form fn-unw &key (timeout *timeout* timeout-provided-p))
  (warn-timeout timeout timeout-provided-p "UNW-PROT")
  (alambda
   ((cust)
    (β ans
        (send (timed-service (create fn-form) timeout) β)
      (send* cust ans)
      (send (create fn-unw))
      ))
   ))
  
(defmacro unw-prot ((cust) form &rest unw-clauses)
  "UNW-PROT -- Constructs an Actor that expects a message indicating a
customer.

It will peform the first clause and then guarantee that the remaining
unwind clauses get performed, amd provide a response to the customer,
no later than TIMEOUT seconds after initiating the first clause.

If the action of the first clause sends a response to the customer
before the TIMEOUT period expires, that answer is provided to the
customer, as well as performing the unwind clauses at that time.

This is the Actors equivalent of UNWIND-PROTECT."
  (let* ((pos     (position :timeout unw-clauses))
         (unspec  (gensym))
         (timeout (if pos 
                      (nth (1+ pos) unw-clauses)
                    unspec))
         (unwinds (if pos
                      (append (um:take pos unw-clauses)
                              (um:drop (+ 2 pos) unw-clauses))
                    unw-clauses)))
    `(create (unw-prot-beh
              (lambda (,cust)
                ,form)
              (lambda ()
                ,@unwinds)
              ,@(unless (eq timeout unspec)
                  `(:timeout ,timeout))
              ))))
  
#+:LISPWORKS
(editor:indent-like "UNW-PROT" "MULTIPLE-VALUE-BIND")

#|
(send (unw-prot (cust)
          (progn
            (sleep 1)
            ;; (error "What!?")
            (send cust :hello))
        :timeout 1.1
        (send println :unwinding))
      println))
|#
;; -------------------------------------------------------
;; OPEN-FILE for Actors - using UNW-PROT to ensure file closing.

(defun open-file (filename &rest open-args
                           &key (timeout *timeout* timeout-provided-p)
                           &allow-other-keys)
  "OPEN-FILE -- Constructs an Actor that will open the file in the
indicated mode, and ensure that the file gets closed, and issue a
response to the customer, no later than TIMEOUT seconds after opening
the file.

The Actor expects a message with customer and target service, along
with any args needed by the service. After opening the file, it will
forward the customer, open file descr, and args, to the service.

If the service sends a message back to the customer before the TIMEOUT
period expires, that answer will be sent to the customer, and the file
will be closed at that time.

This the Actors equivalent of WITH-OPEN-FILE."
  (warn-timeout timeout timeout-provided-p "OPEN-FILE")
  (create
   (alambda
    ((cust target . args)
     ;; Target should expect a customer and a file-descr
     (let* ((fd   (apply #'open filename (um:remove-prop :timeout open-args)))
            (prot (unw-prot (cust)
                      (send* target cust fd args)
                    
                    (close fd)
                    (send fmt-println "File closed: ~A" filename)
                    :timeout timeout) ))
       (send fmt-println "File opened: ~A" filename)
       (send prot cust)
       )))
   ))

#|
(let ((line-counter (create
                     (behav (cust fd)
                       ;; (sleep 1)
                       (loop for line = (read-line fd nil fd)
                             for ix from 0
                             until (eql line fd)
                             finally (send fmt-println "File has ~D lines" ix))
                       ;; (error "What!?")
                       ;; (sleep 5)
                       (send cust :ok))
                     ))
      (word-counter (create
                     (behav (cust fd)
                       ;; (sleep 2)
                       (let ((sum (loop for line = (read-line fd nil fd)
                                        until (eql line fd)
                                        sum (length (um:split-string line :delims '(\space \tab))))
                                  ))
                         (send fmt-println "File has ~D words" sum)
                         ;; (error "What!?")
                         (send cust :ok))
                       )))
      (filer (open-file
              "/Users/davidmcclain/projects/Lispworks/color-theme.lisp"
              :direction :input
              :timeout   3)
             ))
  (β ans
      (send (fork (racurry filer line-counter)
                  (racurry filer word-counter))
            β)
    (send fmt-println "I guess we're done: ~A" ans)))
|#

;; ---------------------------------------

(defun counter-beh (&optional (ct 0))
  (alambda
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

    