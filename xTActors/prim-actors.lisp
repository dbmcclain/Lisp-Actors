; prim-actors.lisp - A collection of useful primitive Actors
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
;;  2. When an Actor uses DCASE, it expects the dispatch token in
;;  second position when a customer arg is present.
;;
;; -------------------------------------------------------

(defactor fmt-println
  (λ (fmt-str &rest args)
    (send println (apply #'format nil fmt-str args))
    ))

;; ---------------------

(defun const-beh (&rest msg)
  (lambda (cust)
    (send* cust msg)))

(defun const (&rest msg)
  (create (apply #'const-beh msg)))

;; ---------------------

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
  ;; (when it arrives) to cust with (SEND (FUTURE actor ...) CUST)
  (actors ((fut (future-wait-beh tag))
           (tag (tag-beh fut)))
    (send* actor tag msg)
    fut))

;; -----------------------------------------

(defun lazy (actor &rest msg)
  ;; Like FUTURE, but delays evaluation of the Actor with message
  ;; until someone demands it. (SEND (LAZY actor ... ) CUST)
  (actor (cust)
    (let ((tag (tag self)))
      (become (future-wait-beh tag cust))
      (send* actor tag msg)
      )))

;; --------------------------------------
;; SER - make an Actor that evaluates a series of blocks sequentially
;; - i.e., without concurrency between them.  Each block is fed the
;; same initial message, and the results from each block are sent as
;; an ordered collection to cust.

(defactor ser
  (λ (cust lst &rest msg)
    (if (null lst)
        (send cust)
      (let ((me self))
        (beta msg-hd
            (send* (car lst) beta msg)
          (beta msg-tl
              (send* me beta (cdr lst) msg)
            (send-combined-msg cust msg-hd msg-tl)))
        ))))

;; -----------------------------------
;; PAR - make an Actor that evaluates a series of blocks concurrently.
;; Each block is fed the same initial message, and the results from
;; each block are sent as an ordered collection to cust.

(defun join-beh (cust lbl1)
  ;; Join a pair of two possible messages into one response. One of the
  ;; incoming messages will be labeled lbl1, while the other has
  ;; another label. There are only two possible incoming incoming
  ;; messages, because in use, our Actor is ephemeral and anonymous. So no
  ;; other incoming messages are possible.
  (lambda (lbl &rest msg)
    (cond ((eq lbl lbl1)
           (become (lambda (_ &rest msg2)
                     (declare (ignore _))
                     (send-combined-msg cust msg msg2))
                   ))
          (t ;; could only be lbl2
             (become (lambda (_ &rest msg1)
                       (declare (ignore _))
                       (send-combined-msg cust msg1 msg))
                     ))
          )))

(defun fork (left right)
  ;; Accept two message lists, lreq and rreq, sending lreq to left,
  ;; and rreq to right, collecting combined results into one ordered
  ;; response.
  (actor (cust lreq rreq)
    (actors ((join   (join-beh cust tag-l))
             (tag-l  (tag-beh join))
             (tag-r  (tag-beh join)))
      (send* left tag-l lreq)
      (send* right tag-r rreq))
    ))

(defactor par
  ;; Send same msg to all actors in the lst, running them
  ;; concurrently, and collect the results into one ordered response.
  (λ (cust lst &rest msg)
    (if (null lst)
        (send cust)
      (actors ((join     (join-beh cust tag-car))
               (tag-car  (tag-beh join))
               (tag-cdr  (tag-beh join)))
        (send* (car lst) tag-car msg)
        (send* self tag-cdr (cdr lst) msg)))
    ))

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
;; Delayed Trigger

(defun scheduled-message-beh (actor dt &rest msg)
  ;; This is an interesting case... recall that *SEND* is dynamically
  ;; bound, and its binding varies between foreign threads, and the
  ;; various Actor dispatch engines.
  ;;
  ;; But timer events are like interrupt routines, and can happen on
  ;; any thread and at any time. The only reasonable alternative here
  ;; is to send the message to the community mailbox and let the
  ;; dispatch routines take it from there.
  ;;
  ;; What to do about the single-thread dispatcher?
  ;;
  (let ((timer (apply #'mp:make-timer #'send-to-pool actor msg)))
    (lambda* _
      (mp:schedule-timer-relative timer dt))
    ))

(defun scheduled-message (actor dt &rest msg)
  (create (apply #'scheduled-message-beh actor dt msg)))

(defun send-after (dt actor &rest msg)
  (send (apply #'scheduled-message actor dt msg)))

;; -------------------------------------------
;; A cheap FP Banker's queue
;; When all you need is ADDQ, PUSHQ, POPQ...

(defconstant +emptyq+ (list nil)) ;; strictly speaking, but NIL is okay in CL too.
(defconstant +doneq+  #())

(defun normq (q)
  (if (car q)
      q
    (list (reverse (cdr q)))))

(defun addq (q item)
  ;; add item to tail, return new queue
  (normq (cons (car q) (cons item (cdr q)))))

(defun pushq (q item)
  ;; add item to head, return new queue
  (cons (cons item (car q)) (cdr q)))

(defun popq (q)
  (if (car q)
      (values (caar q)
              (normq (cons (cdar q) (cdr q))))
    +doneq+))

(defun emptyq? (q)
  (null (car q)))

(defun iterq (q fn)
  (um:nlet iter ((q q))
    (unless (emptyq? q)
      (multiple-value-bind (item new-q)
          (popq q)
        (funcall fn item)
        (go-iter new-q))
      )))

(defmacro do-queue ((item q) &body body)
  `(iterq ,q (lambda (,item) ,@body)))

;; -----------------------------------------
;; Serializer Gateway
;;
;; This kind of Actor widget is not needed in our 1-Core-per-Actor
;; system. Every Actor already has a message queue that serializes
;; requests for service.
;;
;; It would be useful in a single-threaded implementation which must
;; continue to dispatch messages to remain lively.
;;
;; We default to shared par-safe behavior because SERIALIZERs are
;; frequently used for shared access to a resource. And since we use
;; BECOME, we have to make the SERIALIZER have par-safe behavior.
;;
;; As with PAR-SAFE and IO, any cust args should be fully specified
;; sponsored-actors.

#|
  ;; This version takes advantage of the already existing event queue
  ;; in the sponsor. However, it also causes the CPU to spin
  ;; needlessly.
(defun serializer-beh (service)
  ;; initial empty state
  (lambda (cust &rest msg)
    (let ((tag  (tag self)))
      (send* service tag msg)
      (become (enqueued-serializer-beh
               service tag cust))
      )))

(defun enqueued-serializer-beh (service tag in-cust)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (send* in-cust msg)
           (become (serializer-beh service)))
          
          (t
           (repeat-send self))
          )))
|#

#|
;; This version does not cause the CPU to spin
(defun serializer-beh (service)
   ;; initial non-busy state
   (alambda
    ((cust . msg)
     (let ((tag  (tag self)))
       (send* service tag msg)
       (become (busy-serializer-beh
                service tag cust +emptyq+))
       ))))

(defun busy-serializer-beh (service tag in-cust queue)
  (alambda
   ((atag . ans) when (eql atag tag)
    (send* in-cust ans)
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

   ((cust . msg)
    (become (busy-serializer-beh
             service tag in-cust
             (addq queue (cons cust msg)))))
   ))

(defun serializer (service)
  (create (serializer-beh service)))
|#
;; ----------------------------------------------------
;; Safe Serializer - serializer with unblocking channel and timeout

(defun new-pend-serializer-beh (svc ret timeout cust waitq msg)
  (actors ((gate  (once-beh ret))
           (tmout (tag-beh gate))
           (reply (tag-beh gate)))
    (send-after timeout tmout)
    (become (pend-serializer-beh svc ret timeout cust tmout reply waitq))
    (send* svc reply msg)))

(defun no-pend-serializer-beh (svc ret timeout)
  (alambda
   ((tag . _) / (eql tag ret))

   ((cust . msg)
    (new-pend-serializer-beh svc ret timeout cust +emptyq+ msg))
   ))

(defun pend-serializer-beh (svc ret timeout cur-cust tmout reply waitq)
  (alambda
   ((tag type-tag . ans) / (eql tag ret)
    (when (or (and (eql type-tag reply)
                   (or (send* cur-cust ans)
                       t))
              (eql type-tag tmout)
              (eql type-tag self))
      (if (emptyq? waitq)
          (become (no-pend-serializer-beh svc ret timeout))
        (multiple-value-bind (top newq) (popq waitq)
          (destructuring-bind (new-cust . new-msg) top
            (new-pend-serializer-beh svc ret timeout new-cust newq new-msg)
            ))
        )))

   (msg
    (become (pend-serializer-beh svc ret timeout cur-cust tmout reply (addq waitq msg))))
   ))

(defun serializer (svc &optional (timeout 10))
  (actors ((ret  (tag-beh gate))
           (gate (no-pend-serializer-beh svc ret timeout)))
    (values gate ret)))

(defun unblock-serializer (ser ret)
  ;; emergency use back door
  (send ret ser))

;; --------------------------------------

(defun timing-beh (dut)
  (lambda (cust &rest msg)
    (let ((start (usec:get-time-usec)))
      (beta _
          (send* dut beta msg)
        (send cust (- (usec:get-time-usec) start)))
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

;; -----------------------------------------------
;; For sequenced message delivery
;;
;; When a messages arrives out of order, send it with :WAIT to the
;; pending items list. When you are ready for any particular sequence
;; number or label, then send :READY with that seequence number to the
;; pending list. If it had previously arrived, it will be re-sent.
;;
;; The purpose of this Actor is to avoid spinning on messages,
;; needlessly using CPU cycles.

(defun pruned-beh (next cust)
  (alambda
   ((:pruned beh)
    (send cust :ok)
    (become beh))
   
   (msg
    (send* next msg))
   ))

(defmacro prunable-alambda (&rest clauses)
  (lw:with-unique-names (tmp-next tmp-cust)
    `(macrolet ((prune-self (next &optional cust)
                  `(let ((,',tmp-next ,next)
                         (,',tmp-cust ,cust))
                     (become (pruned-beh ,',tmp-next ,',tmp-cust))
                     (send ,',tmp-next self :prune))
                  ))
       (alambda 
        ((cust :prune)
         (send cust :pruned self-beh))
        ,@clauses)
       )))

(defun no-pend-beh ()
  (prunable-alambda
    ((:wait ctr . msg)
     (let ((next (create
                  (no-pend-beh))))
       (become (pend-beh ctr msg next))))
    ))

(defun pend-beh (ctr msg next)
  (prunable-alambda
    ((cust :ready in-ctr) when (eql ctr in-ctr)
     (send* cust ctr msg)
     (prune-self next))
    
    (msg
     (send* next msg))
    ))
    
(defun sequenced-delivery ()
  (create (no-pend-beh)))

;; --------------------------------------------------

(defun suspended-beh (prev-beh tag queue)
  (alambda
   ((atag) when (eq tag atag)
    (become prev-beh)
    (do-queue (item queue)
      (send* self item)))
   
   (msg
    (become (suspended-beh prev-beh tag (addq queue msg))))
   ))
   
(defun suspend ()
  ;; To be used only inside of Actor behavior code.
  ;; Just send to the tag to resume the Actor.
  ;; (Watch out for cross-thread actions...)
  (let ((tag (tag self)))
    (become (suspended-beh self-beh tag +emptyq+))
    tag))

#|
;; Example of using SUSPENDED-BEH to serialize host Actor with
;; embedded Beta forms:

  ... ;; inside host Actor
  (let ((resume (suspend)))
    (beta (ans)
        (send some-actor beta msg))
      .... beta body forms...
      (send resume)
      ))

;; Afer SUSPEND, instead of the beta form operating concurrently with
;; the enclosing host Actor, the host Actor suspends its normal
;; message handling, enqueueing all arriving messages except those
;; that arrive via the tag. Once the send to the resume tag happens,
;; the host Actor resumes its prior behavior, and handles all the
;; enqueued messages.
|#
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

(defun pass-beh (&optional sink-blk)
  ;; can be used to convert a sink into a filter component
  ;; A sink-block is one that does not take a cust arg in messages.
  (if sink-blk
      (lambda (cust &rest msg)
        (send* sink-blk msg)
        (send* cust msg))
    #'send))

(defun pass (&optional sink-blk)
  (create (pass-beh sink-blk)))

;; ---------------------------------------------------------
#|
(defun ticketed-perform-beh ()
  (alambda
   ((cust :req)
    (let ((tag  (tag self)))
      (become (pending-perform-beh tag +emptyq+))
      (send cust tag)
      (send-after 1 tag :done)
      ))
   ))

(defun pending-perform-beh (tag pend)
  (alambda
   ((cust :done) when (eq cust tag)
    (if (emptyq? pend)
        (become (ticketed-perform-beh))
      (multiple-value-bind (next-cust new-queue) (popq pend)
        (let ((new-tag (tag self)))
          (send next-cust new-tag)
          (send-after 1 new-tag :done)
          (become (pending-perform-beh new-tag new-queue)))
        )))
   
   ((cust :req)
    (become (pending-perform-beh tag (addq pend cust))))
   ))

(defun ticketed-perform ()
  (create (ticketed-perform-beh)))

(defmacro with-ticket (ticket-master &body body)
  (lw:with-unique-names (tag)
    `(beta (,tag)
         (send ,ticket-master self :req)
       ,@body
       (send ,tag :done))
    ))

#+:LISPWORKS
(editor:setup-indent "with-ticket" 1)
|#

;; ------------------------------------------------

(defun with-timeout (timeout action on-timeout)
  (actor (cust &rest msg)
    (actors ((tag-ok      (tag-beh gate))
             (tag-timeout (tag-beh gate))
             (arbiter     (alambda
                           ((tag . ans) when (eq tag tag-ok)
                            (send* cust ans))
                           (_
                            (send on-timeout))))
             (gate        (once-beh arbiter)))
      (send* action tag-ok msg)
      (send-after timeout tag-timeout)
      )))

#|
(send (with-timeout 2.1 (actor (cust)
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

