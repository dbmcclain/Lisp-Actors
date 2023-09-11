;; prim-actors.lisp - A collection of useful primitive Actors
;;
;; DM/RAL 05/21
;; ------------------------------------------------------
(in-package :com.ral.actors.base)

;; ------------------------------------------------------
;; There are, broadly, some conventions followed for Actor messages:
;;
;;  1. When an Actor expects a customer argument it is, by
;;  convention, always in first position.
;;
;;  2. Every Actor network living behind a SERIALIZER *must* arrange
;;  to send a reply to the customer.
;;
;;  3. Sink Actors don't have customers, unless they live behind a
;;  SERIALIZER gate.
;;
;;  4. Timeout conditions are signaled by sending TIMED-OUT to a
;;  customer. This is a predefined timeout condition object.
;;
;;  5. When nothing else to send cust, send :OK. That helps when the
;;  sender wants to sequence its behavior using continuation Actors
;;  (β). If sender doesn't care, they will send SINK (or NIL) as
;;  customer.
;;
;;  6. There are no guarantees that a message will be sent to a
;;  customer. When in doubt you have no other choice but to rely on
;;  timeout messaging.
;;
;;  This is to be contrasted with UNWNID-PROTECT. Such behavior is not
;;  unique to Actors, and can occur in any system in which
;;  continuations / coroutines are employed.
;;
;;  7. In a parallel environment, BECOME could fail and cause an
;;  automatic retry of the message delivery. So you need to ensure
;;  that any nono-idempotent code alongside BECOME is marked as
;;  NON-IDEMPOTENT or RESTARTABLE, or package up in a thunk and send
;;  to EXECUTOR. This ensures that the body of code will not be
;;  executed unless BECOME succeeds.
;;
;;  8. Since Actors are transactional, SEND, SEND*, REPEAT-SEND,
;;  SEND-TO-ALL, SEND-ALL-TO, and SEND-AFTER, are always idempotent.
;;  Message will only be sent at exit of body code, and only if BECOME
;;  succeeds. But the message args might have been created using
;;  non-idempotent code, so see (7).
;;
;; -------------------------------------------------------

(deflex executor
  ;; Use for performing non-idempotent actions
  ;; when BECOME is being used.
  (create
   (lambda (cust fn)
     (funcall fn)
     (send cust :ok))))

;; --------------------------------------------------------

(defun once-beh (cust)
  (lambda (&rest msg)
    (send* cust msg)
    (become-sink)))

(defun once (cust)
  (create (once-beh cust)))

#| ;; Don't use - the clock starts running as soon as this is invoked.
(defun timed-gate (cust timeout)
  (cond ((realp timeout)
         (let ((gate (once cust)))
           (send-after timeout gate timed-out)
           gate))
        (t
         cust)))
|#

(defun timed-service (svc &optional (timeout *timeout*))
  ;; Prefer this, so that the clock only starts running when a message
  ;; is sent to svc.
  (cond ((realp timeout)
         (create
          (lambda (cust &rest msg)
            (let ((gate (once cust)))
              (send-after timeout gate timed-out)
              (send* svc gate msg)))
          ))
        (t
         svc)
        ))

;; ---------------------

(defun send-to-all (actors &rest msg)
  (dolist (actor actors)
    (send* actor msg)))

(defun send-all-to (actor msg-list)
  (dolist (msg msg-list)
    (send* actor msg)))

;; ---------------------

(defun race-beh (&rest actors)
  (lambda (cust &rest msg)
    (let ((gate (once cust)))
      (apply #'send-to-all actors gate msg))))

(defun race (&rest actors)
  (create (apply #'race-beh actors)))

;; ---------------------

(defun fwd-beh (actor)
  (lambda (&rest msg)
    (send* actor msg)))

(defun fwd (actor)
  (create (fwd-beh actor)))

;; ---------------------
;; Finds good use when sending messages to a serialized sink

(defun label-beh (cust lbl)
  (lambda (&rest msg)
    (send* cust lbl msg)))

(defun label (cust lbl)
  (create (label-beh cust lbl)))

;; ---------------------

(defun tag-beh (cust)
  (lambda (&rest msg)
    (send* cust self msg)))

(defun tag (cust)
  (create (tag-beh cust)))

;; ---------------------

(defun once-tag-beh (cust)
  (lambda (&rest msg)
    (send* cust self msg)
    (become-sink)))

(defun once-tag (cust)
  (create (once-tag-beh cust)))

;; ---------------------

(defun timed-tag (cust &optional (timeout *timeout*))
  (let ((atag (tag cust)))
    (send-after timeout atag timed-out)
    atag))

(defun timed-once-tag (cust &optional (timeout *timeout*))
  (let ((atag (once-tag cust)))
    (send-after timeout atag timed-out)
    atag))

;; -------------------------------------------------

(defun future-wait-beh (tag &rest custs)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (become (apply #'const-beh msg))
           (apply #'send-to-all custs msg))
          (t
           (become (apply 'future-wait-beh tag cust custs)))
          )))

(defun future (actor &rest msg)
  ;; Return an Actor that represents the future value. Send that value
  ;; (when it arrives) to cust with (SEND (FUTURE actor ...) CUST).
  ;; Read as "send the future result to cust".
  (actors ((fut (future-wait-beh tag))
           (tag (tag-beh fut)))
    (send* actor tag msg)
    fut))

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

;; --------------------------------------------------

(defun future-become-beh (tag &optional msgs)
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
  (alambda
   ((atag abeh) / (eq atag tag)
    (become abeh)
    (send-all-to self msgs))
   (msg
    (become (future-become-beh tag (cons msg msgs))))
   ))

;; -----------------------------------------

(defun lazy-beh (actor &rest msg)
  (lambda (cust)
    (let ((tag (once-tag self)))
      (become (future-wait-beh tag cust))
      (send* actor tag msg)
      )))

(defun lazy (actor &rest msg)
  ;; Like FUTURE, but delays evaluation of the Actor with message
  ;; until someone demands it. (SEND (LAZY actor ... ) CUST)
  (create-service (apply 'lazy-beh actor msg)))

;; --------------------------------------
;; LAZY-FWD -- become a FWD on demand

(defun future-fwd-wait-beh (tag msgs)
  (alambda
   ((atag dest) / (eq atag tag)
    (become (fwd-beh dest))
    (send-all-to dest msgs))
   (msg
    (become (future-fwd-wait-beh tag (cons msg msgs))))
   ))

(defun lazy-fwd (actor &rest init-msg)
  (create
   (lambda (&rest msg)
     (let ((tag (once-tag self)))
       (become (future-fwd-wait-beh tag (list msg)))
       (send* actor tag init-msg))
     )))

;; --------------------------------------
;; SER - make an Actor that evaluates a series of blocks sequentially
;; - i.e., without concurrency between them.  Each block is fed the
;; same initial message, and the results from each block are sent as
;; an ordered collection to cust.

(def-actor ser
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

;; -----------------------------------------------------------
;; The previous FORK/JOIN ran into MULTIPLE-VALUES-LIMIT in a real
;; application.
;;
;; So the alternative must have participant Actors accepting a
;; customer and a single argument. The customers of the FORK should
;; expect any number of result values.

#|
(defun join-beh (cust lbl1)
  ;; Join a pair of two possible messages into one response. One of the
  ;; incoming messages will be labeled lbl1, while the other has
  ;; another label. There are only two possible incoming incoming
  ;; messages, because in use, our Actor is ephemeral and anonymous. So no
  ;; other incoming messages are possible.
  (lambda (lbl &rest msg)
    (cond ((eql lbl lbl1)
           (become (lambda (_ &rest msg2)
                     (declare (ignore _))
                     (send* cust (append msg msg2)))
                   ))
          (t ;; could only be lbl2
             (become (lambda (_ &rest msg1)
                       (declare (ignore _))
                       (send* cust (append msg1 msg)))
                     ))
          )))

(defun fork (left right)
  ;; Accept two message lists, lreq and rreq, sending lreq to left,
  ;; and rreq to right, collecting combined results into one ordered
  ;; response.
  ;;
  ;; Each service, left and right, should expect a customer and a
  ;; single argument for their messages. The outer customer for this
  ;; FORK should expect any number of results, i.e., (&rest ans).
  ;; C.f., JOIN-BEH above. Services, left and right, are free to send
  ;; any number of items in their results.
  (actor (cust lreq rreq)
    (actors ((join  (join-beh cust tag-l))
             (tag-l (tag-beh join))
             (tag-r (tag-beh join)))
      (send left tag-l lreq)
      (send right tag-r rreq))
    ))
|#
;; ----------------------------------------------

(defun simd (svc)
  ;; process an entire list of args in parallel
  ;; cust should expect a (&rest ans)
  (actor (cust args)
    (cond ((null args)
           (send cust))
          ((atom args)
           (send svc cust args))
          ((null (cdr args))
           (send svc cust (car args)))
          (t
           (send (fork svc self) cust (car args) (cdr args)))
          )))

(defun mimd (&rest svcs)
  (actor (cust &rest args)
    (map 'nil (lambda (svc arg)
                (let ((lbl (label cust svc)))
                  (send (simd svc) lbl arg)))
         svcs args)))

#|
(def-actor par
  ;; Send same msg to all actors in the lst, running them
  ;; concurrently, and collect the results into one ordered response.
  (α (cust lst msg)
    ;; cust should expect a (&rest ans)
    (if (null lst)
        (send cust)
      (actors ((join    (join-beh cust tag-car))
               (tag-car (tag-beh join))
               (tag-cdr (tag-beh join)))
        (send (car lst) tag-car msg)
        (send self tag-cdr (cdr lst) msg)))
    ))
|#
#|
(defun and-gate-beh (services &optional (last-ans t))
  (lambda (cust)
    (if (endp services)
        (send cust last-ans)
      (β (ans)
          (send (car services) β)
        (if ans
            (progn
              (become (and-gate-beh (cdr services) ans))
              (send self cust))
          (send cust nil))
        ))))

(defun and-gate (&rest services)
  (create (and-gate-beh services)))

(defun or-gate-beh (services)
  (lambda (cust)
    (if (endp services)
        (send cust nil)
      (β (ans)
          (send (car services) β)
        (if ans
            (send cust ans)
          (progn
            (become (or-gate-beh (cdr services)))
            (send self cust)))
        ))))

(defun or-gate (&rest services)
  (create (or-gate-beh services)))
|#

;; ---------------------------------------------------------
#|
(send ser println
      (list
       (const :blk1)
       (const :blk2)
       (const :blk3)))

(send par println
      (list
       (const :blk1)
       (const :blk2)))

(let* ((actor (create (lambda (cust) (sleep 2) (send cust :ok))))
       (fut   (future actor)))
  (send fut println)
  (send fut println))
 |#
;; -----------------------------------------
;; Delayed Send

(let ((schedule-timer
       (α (dt actor &rest msg)
         ;; No BECOME, so no need to worry about being retried.
         ;; Parallel-safe.
         (flet ((sender ()
                  ;; If we have a binding for SELF, then SEND is also
                  ;; redirected. We need to avoid using that version...
                  ;; But it also means that someone is listening to the
                  ;; Central Mailbox.
                  (let ((fn (if self
                                #'send-to-pool
                              #'send)))
                    (apply fn actor msg)
                    :stop)))
           (let ((timer (mpc:make-timer #'sender)))
             (mpc:schedule-timer-relative timer dt))
           ))))
         
  (defun send-after (dt actor &rest msg)
    ;; NOTE: Actors, except those at the edge, must never do anything
    ;; that has observable effects beyond SEND and BECOME. Starting a
    ;; timer running breaks this. The caller might have to be retried,
    ;; in which case there will be a spurious timer running from a prior
    ;; attempt.
    ;;
    ;; We place the timer launch in an edge Actor here, and SEND a
    ;; message to trigger it. If we need to be retried, that SEND never
    ;; happens.
    ;;
    ;; Edge Actors typically live behind a SERIALIZER wall, to prevent
    ;; the possibility that they will need to be retried. In this case,
    ;; we are the only ones that know about this edge Actor.
    ;;
    (when (and (actor-p actor)
               (realp dt))
      (send* schedule-timer dt actor msg))))

;; --------------------------------------

(defun timing-beh (dut)
  (lambda (cust &rest msg)
    (let ((start (get-time-usec)))
      (beta _
          (send* dut beta msg)
        (send cust (- (get-time-usec) start)))
      )))

(defun timing (dut)
  (create (timing-beh dut)))

#|
(let* ((dut (actor (cust nsec)
             (sleep nsec)
             (send cust)))
      (timer (timing dut)))
  (send timer println 1))
|#

;; ---------------------------------------------------------
;; SEQUENCED-DELIVERY
;;   Provider sends :DELIVER with sequence counter and message
;;   Consumer sends :READY with customer and sequence counter of desired messaage

(def-beh sequenced-beh (&optional items)
  ((cust :ready ctr)
   (let ((msg (assoc ctr items)))
     (cond (msg
            (send cust (cdr msg))
            (become (sequenced-beh (remove msg items))))
           (t
            (become (pending-sequenced-beh cust ctr items)))
           )))

  ((:deliver ctr . msg)
   (become (sequenced-beh (acons ctr msg items)))))

(def-beh pending-sequenced-beh (cust ctr items)
  ((:deliver in-ctr . msg)
   (cond ((eql in-ctr ctr)
          (send cust msg)
          (become (sequenced-beh items)))
         (t
          (become (pending-sequenced-beh cust ctr (acons in-ctr msg items))))
         )))
  
(defun sequenced-delivery ()
  (create (sequenced-beh)))

;; -------------------------------------
;; Systolic Processing Pipelines

(defun acurry-beh (actor &rest largs)
  ;; like Curried functions, but for Actors
  (lambda (&rest rargs)
    (multiple-value-call #'send actor (values-list largs) (values-list rargs))))

(defun acurry (actor &rest largs)
  (create (apply #'acurry-beh actor largs)))

(defun racurry-beh (actor &rest rargs)
  (lambda (&rest largs)
    (multiple-value-call #'send actor (values-list largs) (values-list rargs))))

(defun racurry (actor &rest rargs)
  (create (apply #'racurry-beh actor rargs)))

(defun pipe-beh (&rest elts)
  ;; Hmmm... constructs a new pipe every time invoked. But is this any
  ;; worse than a sequence of nested Beta forms? Same effect, just
  ;; performed in advance here.
  (lambda (cust &rest msg)
    (send* (reduce #'acurry elts
                   :from-end t
                   :initial-value cust)
           msg)))

(defun pipe (&rest elts)
  (create (apply #'pipe-beh elts)))

(defun sink-pipe (&rest elts)
  ;; for pipelines whose last block are sinks
  (reduce #'acurry (butlast elts)
          :from-end t
          :initial-value (um:last1 elts)))

(defun tee (&rest sink-blks)
  ;; can be used to convert a sink into a filter component
  ;; A sink-block is one that does not take a cust arg in messages.
  (create (if sink-blks
              (lambda (cust &rest msg)
                (apply #'send-to-all sink-blks msg)
                (send* cust msg))
            #'send)))

(def-beh splay-beh (&rest custs)
  ;; Define a sink block that passes on the message to all custs
  (msg
   (apply #'send-to-all custs msg)))

(defun splay (&rest custs)
  (create (apply #'splay-beh custs)))

;; ------------------------------------------------

(def-beh watchdog-timer-beh (action timeout on-timeout &optional supv)
  ;; Place WATCHDOG-TIMER after a SERIALIZER gate, to ensure new
  ;; message release on timeout.
  ((cust :become new-beh reply-to) when (and cust
                                             (eql cust supv))
   ;; allow a supervisor to change us out
   (become new-beh)
   (send reply-to :ok))
  
  ((cust . start-msg)
   (cond ((realp timeout)
          (let ((me  self))
            (actors ((tag-ok      (tag-beh gate))
                     (tag-timeout (tag-beh gate))
                     (gate        (once-beh arbiter))
                     (arbiter     (λ (tag . msg)
                                    (if (eql tag tag-ok)
                                        (send* cust msg)
                                      (cond (on-timeout
                                             (send on-timeout me action start-msg timeout supv cust))
                                            (t
                                             (send cust timed-out))
                                            )))
                                  ))
              (send* action tag-ok start-msg)
              (send-after timeout tag-timeout)
              )))
         (t
          (send* action cust start-msg))
     )))

(defun watchdog-timer (action &key (timeout *timeout*) on-timeout supv)
  ;; If timeout happens, then on-timeout is sent a message with the
  ;; watchdog, its action Actor, the message in progress, its timeout
  ;; duration, its supv, and customer, as arguments.
  (cond ((or timeout supv)
         (create (watchdog-timer-beh action timeout on-timeout supv)))
        (t
         action)))

#|
(send (safe-serializer (α (cust . msg)
                         (send writeln msg)
                         (sleep 2)
                         (send cust :ok))
                       :timeout 1)
      println :hello)


(send (safe-serializer sink
                       :timeout 1
                       ;; :on-timeout (label writeln :on-timeout)
                       )
      (label writeln :direct))

 |#
#|
(send (with-watchdog-timer 2.1 (actor (cust)
                        (send-after 2 cust :ok))
                    (actor _
                      (send println :nah)))
      println)
 |#
#|
(defun long-running-beh (action)
  (flet ((doit (cust args)
           (let ((tag  (tag self)))
             (become (busy-running-beh action tag cust nil))
             (send* action tag args))))
    (alambda
     ((cust :run . args)
      (doit cust args))

     ((cust :run-immediately . args)
      (doit cust args))
     )))

(defun busy-running-beh (action tag cust queue)
  ;; action should send back non-nil first arg in reply to indicate
  ;; success..
  (alambda
   ((atag . ans) when (eql atag tag)
    (send* cust ans)
    (if (emptyq? queue)
        (become (long-runinng-beh action))
      (multiple-value-bind (next-up new-queue) (popq queue)
        (destructuring-bind (next-cust . next-args) next-up
          (let ((new-tag  (tag self)))
            (become (busy-running-beh action new-tag next-cust new-queue))
            (send* action new-tag next-args))
          ))))

   ((acust :run . args)
    (become (busy-running-beh action tag cust (addq queue (cons acust args)))))

   ((cust :run-immediately . _)
    (send cust nil))
   ))

(defun make-long-running (action)
  (create (long-running-beh action)))
|#
;; ------------------------------------------------------

;; --------------------------------------------------------
;; Membranes - controlled access to selected services
;;
;; Supv constructs a membrane, can shut down through the ctrl channel.
;; Construct with an AList of (kw . handler)
;; Clients receive a svcs channel through which they can:
;;    * Ask what services are offered (by KWSym?) - a list of designators.
;;    * Request a channel to a service handler, get nil if not offered.
;;    * Send a message to a service handler. Message may incl their cust.
;;
(defun membrane-beh (ctrl svcs alist)
  (alambda
   ((tag cust :close) when (eq tag ctrl)
    ;; from (send ctrl cust :close) -- tag only known by supv
    ;; controller says to shutdown, let him know we have
    (become (membrane-beh ctrl (tag self) nil))  ;; svcs tag no longer useful
    (send cust :ok))

   ((tag cust :req-access svc) when (eq tag svcs)
    ;; from (send svcs cust :req-access svc)
    ;; only clients can (send svcs cust :req-access)
    (let ((handler (assoc svc alist)))
      (if handler
          (send cust (label svcs (cdr handler)))
        (send cust nil))))

   ((tag cust :what-services?) when (eq tag svcs)
    ;; from (send svcs cust :what-services?)
    ;; only clients can ask
    (send cust (mapcar #'car alist)))
   
   ((tag handler . msg) when (eq tag svcs)
    ;; from (send portal . msg)
    ;; msg may include cust reply-to
    ;; send message to handler via the portal
    (send* handler msg))

   ))

(defun membrane (alist)
  ;; typically called by supv to set up a membrane controlled
  ;; collection of services
  (actors ((ctrl  (tag-beh mem))
           (svcs  (tag-beh mem))
           (mem   (membrane-beh ctrl svcs alist)))
    (values svcs    ;; give this out to clients
            ctrl))) ;; for supv control of membrane
   
