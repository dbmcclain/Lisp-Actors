;; prim-actors.lisp - A collection of useful primitive Actors
;;
;; DM/RAL 05/21
;; ------------------------------------------------------
(in-package :com.ral.actors.base)

;; ------------------------------------------------------
;; There are, broadly, two conventions followed for Actor messages:
;;
;;  1. When an Actor expects a customer argument, it is always in
;;  first position.
;;
;;  2. Sink Actors don't have customers, unless they live behind a
;;  SERIALIZER gate. Every Actor living behind a SERIALIZER *must*
;;  eventually send a response to the customer.
;;
;; -------------------------------------------------------

(defun once-beh (cust)
  (lambda (&rest msg)
    (send* cust msg)
    (become (sink-beh))))

(defun once (cust)
  (create (once-beh cust)))

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
    (become (sink-beh))))

(defun once-tag (cust)
  (create (once-tag-beh cust)))

(defun timed-tag (cust timeout cust-id)
  (let ((atag (once-tag cust)))
    (when timeout
      (send-after timeout atag :timeout cust-id))
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
  (actors ((fut (create (future-wait-beh tag)))
           (tag (tag fut)))
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

;; -----------------------------------------

(defun lazy-beh (actor &rest msg)
  (lambda (cust)
    (let ((tag (tag self)))
      (become (future-wait-beh tag cust))
      (send* actor tag msg)
      )))

(defun lazy (actor &rest msg)
  ;; Like FUTURE, but delays evaluation of the Actor with message
  ;; until someone demands it. (SEND (LAZY actor ... ) CUST)
  (create (apply 'lazy-beh actor msg)))

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
    (actors ((join  (create (join-beh cust tag-l)))
             (tag-l (tag join))
             (tag-r (tag join)))
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
      (actors ((join    (create (join-beh cust tag-car)))
               (tag-car (tag join))
               (tag-cdr (tag join)))
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

(def-actor schedule-timer
  (α (dt actor &rest msg)
    ;; No BECOME, so no need to worry about being retried.
    ;; Parallel-safe.
    (labels ((sender ()
               ;; If we have a binding for SELF, then SEND is also
               ;; redirected. We need to avoid using that version...
               ;; But it also means that someone is listening to the
               ;; Central Mailbox.
               (let ((fn (if self
                             #'send-to-pool
                           #'send)))
                 (apply fn actor msg))))
      (let ((timer (mpc:make-timer #'sender)))
        (mpc:schedule-timer-relative timer dt))
      )))

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
  (when (actor-p actor)
    (send* schedule-timer dt actor msg)))

;; -----------------------------------------
;; Serializer Gateway - service must always respond to a customer
;;
#|
(def-beh serializer-beh (service
                         &optional
                         timeout
                         (id  #())) ;; unique to me
  ((cust . msg)
   (let ((tag (timed-tag self timeout id)))
     (send* service tag msg)
     (become (busy-serializer-beh
              service timeout id tag cust +emptyq+))
     )))

(defun busy-serializer-beh (service timeout id tag in-cust queue)
  (labels ((release-one ()
             (if (emptyq? queue)
                 (become (serializer-beh service timeout id))
               (multiple-value-bind (next-req new-queue) (popq queue)
                 (destructuring-bind (next-cust . next-msg) next-req
                   (let ((new-tag (timed-tag self timeout id)))
                     (send* service new-tag next-msg)
                     (become (busy-serializer-beh
                              service timeout id new-tag next-cust new-queue))
                     )))
               )))

    (alambda
     ((atag :TIMEOUT an-id) when (and (eql atag tag)
                                      (eql an-id id)) ;; need to be sure it is *our* timeout
      ;; If we just received a timeout on our tag, then allow the timeout
      ;; to propagate via timeout, i.e., making the customer suffer his
      ;; own timeout, or else let the customer just hang.
      ;;
      ;; In any event, we got a prod to release the next guy...
      (send fmt-println "*** We got a timeout in a serializer: ~S" self)
      (release-one))
     
     ((atag . ans) when (eql atag tag)
      (send* in-cust ans)
      (release-one))
     
     (msg
      (become (busy-serializer-beh
               service timeout id tag in-cust
               (addq queue msg))
              ))
     )))
|#

(def-beh serializer-beh (service)
  ((cust . msg)
   (let ((tag (tag self)))
     (send* service tag msg)
     (become (busy-serializer-beh
              service tag cust +emptyq+))
     )))

(def-beh busy-serializer-beh (service tag in-cust queue)
  ((atag . msg) when (eql atag tag)
   (send* in-cust msg)
   (if (emptyq? queue)
       (become (serializer-beh service))
     (multiple-value-bind (next-req new-queue) (popq queue)
       (destructuring-bind (next-cust . next-msg) next-req
         (let ((new-tag (tag self)))
           (send* service new-tag next-msg)
           (become (busy-serializer-beh
                    service new-tag next-cust new-queue))
           )))
     ))

  (msg
   (become (busy-serializer-beh
            service tag in-cust
            (addq queue msg))
           )))

(defun serializer (service)
  (create (serializer-beh service)))

(defun serializer-sink (service)
  ;; Turn a service into a sink. Service must accept a cust argument,
  ;; and always send a response to cust - even though it appears to be
  ;; a sink from the caller's perspective.
  (label (serializer service) sink))

#|
(defun tst (n)
  (labels ((doit-beh (&optional (n 0))
             (lambda (&rest _)
               (declare (ignore _))
               (let ((newct (1+ n)))
                 (send println newct)
                 (become (doit-beh newct))))))
    (let* ((dst (create (doit-beh)))
           (x (safe-serializer
               (α (cust)
                 (sleep 0.19999)
                 (send cust :ok))
               :timeout 0.2)
              ))
      (dotimes (ix n)
        (send x dst))
      )))
(tst 100)
|#
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
  ((cust :become new-beh reply-to) when (and cust
                                             (eql cust supv))
   ;; allow a supervisor to change us out
   (become new-beh)
   (send reply-to :ok))
  
  ((cust . msg)
   (cond (timeout
          (let ((me  self))
            (actors ((tag-ok      (tag gate))
                     (tag-timeout (tag gate))
                     (gate        (once arbiter))
                     (arbiter     (α (tag . msg)
                                    (if (eql tag tag-ok)
                                        (send* cust msg)
                                      (cond (on-timeout
                                             (send on-timeout me action timeout supv cust msg))
                                            (t
                                             (send cust :timeout))
                                            )))
                                  ))
              (send* action tag-ok msg)
              (send-after timeout tag-timeout)
              )))
         (t
          (send* action cust msg))
     )))

(defun watchdog-timer (action &key timeout on-timeout supv)
  ;; if timeout happens, then on-timeout is sent a message with the
  ;; watchdog, its action, its timeout duration, its supv, customer,
  ;; and message as arguments.
  (cond ((or timeout supv)
         (create (watchdog-timer-beh action timeout on-timeout supv)))
        (t
         action)))

(defun safe-serializer (action &key timeout on-timeout supv)
  ;; If timeout happens, by default the serializer will be unblocked
  ;; with message :TIMEOUT.
  ;;
  ;; If ON-TIMEOUT is specified it will instead be called with the
  ;; customer and message that was attempted. It is up to ON-TIMEOUT
  ;; to clear the way with the SERIALIZER, perform retries, shut down
  ;; the service, or whatever.. Rquests held in the SERIALIZER queue
  ;; will remain blocked until the SERIALIZER hears a response.
  (let ((wd (watchdog-timer action
                              :timeout    timeout
                              :on-timeout on-timeout
                              :supv       supv)))
    (values (serializer wd)
            wd)
    ))

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
             (become (busy-running-beh action tag cust +emptyq+))
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

