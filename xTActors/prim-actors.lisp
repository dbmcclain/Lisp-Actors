;; prim-actors.lisp - A collection of useful primitive Actors
;;
;; DM/RAL 05/21
;; ------------------------------------------------------
(in-package :actors/base)
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

(defun const-beh (&rest msg)
  (lambda (cust)
    (send* cust msg)))

(defun const (&rest msg)
  (make-actor (apply #'const-beh msg)))

;; ---------------------

(defun once-beh (cust)
  (lambda (&rest msg)
    (send* cust msg)
    (become (sink-beh))))

(defun once (cust)
  (make-actor (once-beh cust)))

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
  (make-actor (apply #'race-beh actors)))

;; ---------------------

(defun fwd-beh (actor)
  (lambda (&rest msg)
    (send* actor msg)))

(defun fwd (actor)
  (make-actor (fwd-beh actor)))

;; ---------------------

(defun label-beh (cust lbl)
  (lambda (&rest msg)
    (send* cust lbl msg)))

(defun label (cust lbl)
  (make-actor (label-beh cust lbl)))

;; ---------------------

(defun tag-beh (cust)
  (lambda (&rest msg)
    (send* cust self msg)))

(defun tag (cust)
  (make-actor (tag-beh cust)))

;; -------------------------------------------------

(defun future-wait-beh (tag custs)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (become (apply #'const-beh msg))
           (apply #'send-to-all custs msg))
          (t
           (become (future-wait-beh tag (cons cust custs))))
          )))

(defun future (actor &rest msg)
  ;; Return an Actor that represents the future value. Send that value
  ;; (when it arrives) to cust with (SEND (FUTURE actor ...) CUST)
  (actors ((fut (future-wait-beh tag nil))
           (tag (tag-beh fut)))
    (send* actor tag msg)
    fut))

;; -----------------------------------------

(defun lazy (actor &rest msg)
  ;; Like FUTURE, but delays evaluation of the Actor with message
  ;; until someone demands it. (SEND (LAZY actor ... ) CUST)
  (actor (cust)
    (let ((tag (tag self)))
      (become (future-wait-beh tag (list cust)))
      (send* actor tag msg))
    ))

;; --------------------------------------
;; SER - make an Actor that evaluates a series of blocks sequentially
;; - i.e., without concurrency between them.  Each block is fed the
;; same initial message, and the results from each block are sent as
;; an ordered collection to cust.

(defvar ser
  (make-actor
   (lambda (cust lst &rest msg)
     (if (null lst)
         (send cust)
       (let ((me self))
         (beta msg-hd (send* (car lst) beta msg)
           (beta msg-tl (send* me beta (cdr lst) msg)
             (send-combined-msg cust msg-hd msg-tl)))
         )))
   ))

(defun send-combined-msg (cust msg1 msg2)
  (multiple-value-call #'send cust (values-list msg1) (values-list msg2)))

;; -----------------------------------
;; PAR - make an Actor that evaluates a series of blocks concurrently.
;; Each block is fed the same initial message, and the results from
;; each block are sent as an ordered collection to cust.

(defun join-beh (cust lbl1 lbl2)
  (declare (ignore lbl2))
  (lambda (lbl &rest msg)
    (cond ((eq lbl lbl1)
           (become (lambda (_ &rest msg2)
                     (declare (ignore _)) ;; _ = lbl arg
                     (send-combined-msg cust msg msg2))
                   ))
          (t ;; (eq lbl lbl2)
           (become (lambda (_ &rest msg1)
                     (declare (ignore _)) ;; _ = lbl arg
                     (send-combined-msg cust msg1 msg))
                   ))
          )))

(defun fork (left right)
  (actor (cust lreq rreq)
    (let ((tag-l (tag self))
          (tag-r (tag self)))
      (become (join-beh cust tag-l tag-r))
      (send* left tag-l lreq)
      (send* right tag-r rreq))))

(defvar par
  (actor (cust lst &rest msg)
    (if (null lst)
        (send cust)
      (actors ((join (join-beh cust lbl1 lbl2))
               (lbl1 (tag-beh join))
               (lbl2 (tag-beh join)))
        (send* (car lst) lbl1 msg)
        (send* self lbl2 (cdr lst) msg)))
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

(let* ((actor (make-actor (lambda (cust) (sleep 2) (send cust :ok))))
       (fut   (future actor)))
  (send fut println)
  (send fut println))
 |#
;; -----------------------------------------
;; Delayed Trigger

(defun scheduled-message-beh (cust dt &rest msg)
  (let ((timer (apply #'mp:make-timer #'send cust msg)))
    (lambda* _
      (mp:schedule-timer-relative timer dt)
      (become (sink-beh)))))

(defun scheduled-message (cust dt &rest msg)
  (make-actor (apply #'scheduled-message-beh cust dt msg)))

(defun send-after (dt &rest msg)
  (let ((timer (apply #'mp:make-timer #'send msg)))
    (mp:schedule-timer-relative timer dt)))

;; -------------------------------------------
;; A cheap FP Banker's queue - empty queue is NIL
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

(defun iterq (q fn)
  (um:nlet iter ((q q))
    (multiple-value-bind (item new-q)
        (popq q)
      (unless (eq item +doneq+)
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
#||#
;; This version does not cause the CPU to spin
(defun serializer-beh (service)
  ;; initial empty state
  (lambda (cust &rest msg)
    (let ((tag  (tag self)))
      (send* service tag msg)
      (become (enqueued-serializer-beh
               service tag cust +emptyq+))
      )))

(defun enqueued-serializer-beh (service tag in-cust queue)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (send* in-cust msg)
           (multiple-value-bind (next-req new-queue)
               (popq queue)
             (if (eq next-req +doneq+)
                 (become (serializer-beh service))
               (destructuring-bind (next-cust . next-msg) next-req
                 (send* service tag next-msg)
                 (become (enqueued-serializer-beh
                          service tag next-cust new-queue))
                 ))))
          (t
           (become (enqueued-serializer-beh
                    service tag in-cust
                    (addq queue
                          (cons cust msg))) ))
          )))
#||#

(defun serializer (service)
  (par-safe (make-actor (serializer-beh service))))

;; --------------------------------------

(defun timing-beh (dut)
  (lambda (cust &rest msg)
    (let ((start (usec:get-time-usec))
          (spon  *current-sponsor*))
      (beta _
            (send* dut beta msg)
        (beta _
            ;; ensure we are back in the start thread to get proper timings
            (send spon beta)
          (send cust (- (usec:get-time-usec) start)))
        ))))

(defun timing (dut)
  (make-actor (timing-beh dut)))

#|
(let* ((dut (actor (cust nsec)
             (sleep nsec)
             (send cust)))
      (timer (timing dut)))
  (send timer println 1))
|#
(defun sponsor-switch (spons)
  ;; Switch to other Sponsor for rest of processing
  (actor msg
    (send* spons msg)))

#|
(defun io (svc)
  ;; svc should be an Actor expecting a customer and args from msg
  (actor (cust &rest msg)
    (let ((spons *current-sponsor*))
      ;; Forward to svc on IO thread
      ;; send result back to customer on current thread
      (send* *slow-sponsor*
             svc
             (actor ans
               ;; forwarding customer for svc
               (send* spons cust ans))
             msg)
      )))
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

(defun pruned-beh (next)
  (alambda
   ((:pruned beh)
    (become beh))

   ( msg
     (send* next msg))
   ))

(defun prune-self (next)
  (become (pruned-beh next))
  (send next self :prune))

(defun no-pend-beh ()
  (alambda
   ((prev :prune)
    (send prev :pruned self-beh))

   ((:wait ctr . msg)
    (let ((next (make-actor
                 (no-pend-beh))))
      (become (pend-beh ctr msg next))))
   ))

(defun pend-beh (ctr msg next)
  (alambda
   ((prev :prune)
    (send prev :pruned self-beh))

   ((cust :ready in-ctr) when (eql ctr in-ctr)
    (send* cust ctr msg)
    (prune-self next))

   ( msg
     (send* next msg))
   ))
    
(defun sequenced-delivery ()
  (make-actor (no-pend-beh)))

;; ----------------------------------------------
;; PIPE - Data processing pipelines
;; (cust . msg) -> {A} -> {B} -> {C} -> {cust}

(defun working-pipe-beh (cust elts)
  (lambda (&rest ans)
    (cond ((cdr elts)
           (send* (car elts) self ans)
           (become (working-pipe-beh cust (cdr elts))))
          (t
           (send* (car elts) cust ans))
          )))

(defun pipe (&rest elts)
  (cond ((cdr elts)
         (actor (cust &rest msg)
           (send* (car elts) self msg)
           (become (working-pipe-beh cust (cdr elts)))
           ))
        (elts  (car elts))
        (t     (actor (cust &rest msg)
                 (send* cust msg)))
        ))

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
  (let ((tag (tag self)))
    (become (suspended-beh self-beh tag nil))
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
