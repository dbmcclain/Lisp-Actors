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

(def-beh const-beh (&rest msg)
  (lambda (cust)
    (send* cust msg)))

(defun const (&rest msg)
  (make-actor (apply #'const-beh msg)))

;; ---------------------

(def-beh once-beh (cust)
  (lambda (&rest msg)
    (send* cust msg)
    (become (sink-beh))))

(defun once (cust)
  (make-actor (once-beh cust)))

;; ---------------------

(def-beh send-to-all (actors &rest msg)
  (dolist (actor actors)
    (send* actor msg)))

;; ---------------------

(def-beh race-beh (&rest actors)
  (lambda (cust &rest msg)
    (let ((gate (once cust)))
      (apply #'send-to-all actors gate msg))))

(defun race (&rest actors)
  (make-actor (apply #'race-beh actors)))

;; ---------------------

(def-beh fwd-beh (actor)
  (lambda (&rest msg)
    (send* actor msg)))

(defun fwd (actor)
  (make-actor (fwd-beh actor)))

;; ---------------------

(def-beh label-beh (cust lbl)
  (lambda (&rest msg)
    (send* cust lbl msg)))

(defun label (cust lbl)
  (make-actor (label-beh cust lbl)))

;; ---------------------

(def-beh tag-beh (cust)
  (lambda (&rest msg)
    (send* cust self msg)))

(defun tag (cust)
  (make-actor (tag-beh cust)))

;; -------------------------------------------------

(def-beh future-wait-beh (tag &rest custs)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (become (apply #'const-beh msg))
           (apply #'send-to-all custs msg))
          (t
           (become (apply 'future-wait-beh tag cust custs)))
          )))

(def-beh future (actor &rest msg)
  ;; Return an Actor that represents the future value. Send that value
  ;; (when it arrives) to cust with (SEND (FUTURE actor ...) CUST)
  (actors ((fut (future-wait-beh tag))
           (tag (tag-beh fut)))
    (send* actor (once tag) msg)
    fut))

;; -----------------------------------------

(defun lazy (actor &rest msg)
  ;; Like FUTURE, but delays evaluation of the Actor with message
  ;; until someone demands it. (SEND (LAZY actor ... ) CUST)
  (actor (cust)
    (actors ((fut  (future-wait-beh tag cust))
             (tag  (tag-beh fut)))
      (send* actor (once tag) msg))
    ))

;; --------------------------------------
;; SER - make an Actor that evaluates a series of blocks sequentially
;; - i.e., without concurrency between them.  Each block is fed the
;; same initial message, and the results from each block are sent as
;; an ordered collection to cust.

(defvar ser
  (actor (cust lst &rest msg)
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

(def-beh join-beh (cust lbl1)
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
      (send* left (once tag-l) lreq)
      (send* right (once tag-r) rreq))
    ))

(defvar par
  ;; Send same msg to all actors in the lst, running them
  ;; concurrently, and collect the results into one ordered response.
  (actor (cust lst &rest msg)
    (if (null lst)
        (send cust)
      (actors ((join     (join-beh cust tag-car))
               (tag-car  (tag-beh join))
               (tag-cdr  (tag-beh join)))
        (send* (car lst) (once tag-car) msg)
        (send* self (once tag-cdr) (cdr lst) msg)))
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
  (let ((timer (apply #'mp:make-timer #'foreign-send cust msg)))
    (lambda* _
      (mp:schedule-timer-relative timer dt))
    ))

(defun scheduled-message (cust dt &rest msg)
  (make-actor (apply #'scheduled-message-beh cust dt msg)))

(defun send-after (dt actor &rest msg)
  (let ((timer (apply #'mp:make-timer #'foreign-send actor msg)))
    (mp:schedule-timer-relative timer dt)))

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
(def-beh serializer-beh (service)
  ;; initial empty state
  (lambda (cust &rest msg)
    (let ((tag  (tag self)))
      (send* service tag msg)
      (become (enqueued-serializer-beh
               service tag cust))
      )))

(def-beh enqueued-serializer-beh (service tag in-cust)
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
(def-beh serializer-beh (service)
  ;; initial empty state
  (lambda (cust &rest msg)
    (let ((tag  (tag self)))
      (send* service (once tag) msg)
      (become (enqueued-serializer-beh
               service tag cust +emptyq+))
      )))

(def-beh enqueued-serializer-beh (service tag in-cust queue)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (send* in-cust msg)
           (multiple-value-bind (next-req new-queue)
               (popq queue)
             (if (eq next-req +doneq+)
                 (become (serializer-beh service))
               (destructuring-bind (next-cust . next-msg) next-req
                 (send* service (once tag) next-msg)
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
  (make-actor (serializer-beh service)))

;; --------------------------------------

(def-beh timing-beh (dut)
  (lambda (cust &rest msg)
    (let ((start (usec:get-time-usec)))
      (beta _
          (send* dut beta msg)
        (send cust (- (usec:get-time-usec) start)))
      )))

(defun timing (dut)
  (make-actor (timing-beh dut)))

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

(def-beh pruned-beh (next)
  (alambda
   ((:pruned beh)
    (become beh))

   (msg
     (send* next msg))
   ))

(def-beh prune-self (next)
  (become (pruned-beh next))
  (send next self :prune))

(def-beh no-pend-beh ()
  (alambda
   ((prev :prune)
    (send prev :pruned self-beh))

   ((:wait ctr . msg)
    (let ((next (make-actor
                 (no-pend-beh))))
      (become (pend-beh ctr msg next))))
   ))

(def-beh pend-beh (ctr msg next)
  (alambda
   ((prev :prune)
    (send prev :pruned self-beh))

   ((cust :ready in-ctr) when (eql ctr in-ctr)
    (send* cust ctr msg)
    (prune-self next))

   (msg
     (send* next msg))
   ))
    
(defun sequenced-delivery ()
  (make-actor (no-pend-beh)))

;; ----------------------------------------------
;; PIPE - Data processing pipelines
;; (cust . msg) -> {A} -> {B} -> {C} -> {cust}

(def-beh working-pipe-beh (cust elts)
  (lambda (&rest ans)
    (let ((rest (cdr elts)))
      (cond (rest
             (send* (car elts) (once self) ans)
             (become (working-pipe-beh cust rest)))
            (t
             (send* (car elts) cust ans))
            ))))

(defun pipe (&rest elts)
  (cond ((cdr elts)
         (actor (cust &rest msg)
           (send* (make-actor (working-pipe-beh cust elts)) msg)))
        (elts  (car elts))
        (t     (actor (cust &rest msg)
                 (send* cust msg)))
        ))

;; --------------------------------------------------

(def-beh suspended-beh (prev-beh tag queue)
  (alambda
   ((atag) when (eq tag atag)
    (become prev-beh)
    (do-queue (item queue)
      (send* self item)))

   (msg
    (become (suspended-beh prev-beh tag (addq queue msg))))
   ))
   
(def-beh suspend ()
  ;; To be used only inside of Actor behavior code.
  ;; Just send to the tag to resume the Actor.
  (let ((tag (tag (in-this-sponsor self))))
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
;; ------------------------------------------

(def-beh ret-beh (spon cust)
  (lambda* ans
    (with-sponsor (spon)
      (send* cust ans))
    ))

(defun make-ret (cust)
  (make-actor (ret-beh self-sponsor cust)))

(def-beh ioreq-beh (actor)
  ;; send to actor, return its reply to cust in its original sponsor.
  ;; typically, actor with be (IO actor)
  (lambda (cust &rest msg)
    (send* actor (once (make-ret cust)) msg)))

(defun ioreq (actor)
  (make-actor (ioreq-beh actor)))
