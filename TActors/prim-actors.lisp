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
;; --------------------------------------
;; Sink Behaviors

(defun make-sink-beh ()
  #'lw:do-nothing)

(defvar sink
  (make-actor (make-sink-beh)))

;; --------------------------------------

(defvar println
  (make-actor
   (ensure-par-safe-behavior
    ;; because we are managing an output stream
    (lambda* msg
      (format t "~&~{~A~^ ~}~%" msg))
    )))

;; -------------------------------------
;; Non-Sink Behaviors

(defun make-const-beh (&rest msg)
  (lambda (cust)
    (send* cust msg)))

(defun const (&rest msg)
  (make-actor (apply #'make-const-beh msg)))

;; ---------------------

(defun make-once-beh (cust)
  (lambda (&rest msg)
    (send* cust msg)
    (become (make-sink-beh))))

(defun once (cust)
  (make-actor (make-once-beh cust)))

;; ---------------------

(defun send-to-all (actors &rest msg)
  (dolist (actor actors)
    (send* actor msg)))

;; ---------------------

(defun make-race-beh (&rest actors)
  (lambda (cust &rest msg)
    (let ((gate (once cust)))
      (apply #'send-to-all actors gate msg))))

(defun race (&rest actors)
  (make-actor (apply #'make-race-beh actors)))

;; ---------------------

(defun make-fwd-beh (actor)
  (lambda (&rest msg)
    (send* actor msg)))

(defun fwd (actor)
  (make-actor (make-fwd-beh actor)))

;; ---------------------

(defun make-label-beh (cust lbl)
  (lambda (&rest msg)
    (send* cust lbl msg)))

(defun label (cust lbl)
  (make-actor (make-label-beh cust lbl)))

;; ---------------------

(defun make-tag-beh (cust)
  (lambda (&rest msg)
    (send* cust self msg)))

(defun tag (cust)
  (make-actor (make-tag-beh cust)))

;; -------------------------------------------------

(defun make-future-wait-beh (tag custs)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (become (apply #'make-const-beh msg))
           (apply #'send-to-all custs msg))
          (t
           (become (make-future-wait-beh tag (cons cust custs))))
          )))

(defun future (actor &rest msg)
  ;; Return an Actor that represents the future value. Send that value
  ;; (when it arrives) to cust with (SEND (FUTURE actor ...) CUST)
  (actors ((fut (make-future-wait-beh tag nil))
           (tag (make-tag-beh fut)))
    (send* actor tag msg)
    fut))

;; -----------------------------------------

(defun lazy (actor &rest msg)
  ;; Like FUTURE, but delays evaluation of the Actor with message
  ;; until someone demands it. (SEND (LAZY actor ... ) CUST)
  (actor (cust)
    (let ((tag (tag self)))
      (become (make-future-wait-beh tag (list cust)))
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

(defun make-join-beh (cust lbl1 lbl2)
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
      (become (make-join-beh cust tag-l tag-r))
      (send* left tag-l lreq)
      (send* right tag-r rreq))))

(defvar par
  (actor (cust lst &rest msg)
    (if (null lst)
        (send cust)
      (actors ((join (make-join-beh cust lbl1 lbl2))
               (lbl1 (make-tag-beh join))
               (lbl2 (make-tag-beh join)))
        (send* (car lst) lbl1 msg)
        (send* self lbl2 (cdr lst) msg)))
    ))

;; ---------------------------------------------------------
#|
(send ser println
      (list
       (blk ()
         :blk1)
       (blk ()
         :blk2)
       (blk ()
         :blk3)))
               
(send par println
      (list
       (blk ()
         :blk1)
       (blk ()
         :blk2)))

(let* ((actor (make-actor (lambda (cust) (sleep 2) (send cust :ok))))
       (fut   (future actor)))
  (send fut println)
  (send fut println))
 |#
;; -----------------------------------------
;; Delayed Trigger

(defun make-scheduled-message-beh (cust dt &rest msg)
  (let ((timer (apply #'mp:make-timer #'send cust msg)))
    (lambda* _
      (mp:schedule-timer-relative timer dt)
      (become (make-sink-beh)))))

(defun scheduled-message (cust dt &rest msg)
  (make-actor (apply #'make-scheduled-message-beh cust dt msg)))

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

(defun make-serializer-beh (service)
  ;; initial empty state
  (ensure-par-safe-behavior
   (lambda (cust &rest msg)
     (let ((tag  (tag self)))
       (send* service tag msg)
       (become (make-enqueued-serializer-beh
                service tag cust nil))
       ))))

(defun make-enqueued-serializer-beh (service tag in-cust queue)
  (ensure-par-safe-behavior
   (lambda (cust &rest msg)
     (cond ((eq cust tag)
            (send* in-cust msg)
            (if queue
                (multiple-value-bind (next-req new-queue)
                    (finger-tree:popq queue)
                  (destructuring-bind (spon next-cust . next-msg) next-req
                    (sendx* spon service tag next-msg)
                    (become (make-enqueued-serializer-beh
                             service tag next-cust new-queue))
                    ))
              ;; else
              (become (make-serializer-beh service))))
           (t
            (become (make-enqueued-serializer-beh
                     service tag in-cust
                     (finger-tree:addq queue
                                       (list* *current-sponsor* cust msg)))))
           ))))
  
(defun serializer (service)
  (make-actor (make-serializer-beh service)))

;; --------------------------------------

(defun make-timing-beh (dut)
  (lambda (cust &rest msg)
    (let ((start (usec:get-time-usec)))
      (beta _
            (send* dut beta msg)
        (send cust (- (usec:get-time-usec) start)))
      )))

(defun timing (dut)
  (make-actor (make-timing-beh dut)))

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
    (sendx* spons msg)))

(defun io (svc)
  ;; svc should be an Actor expecting a customer and args from msg
  (actor (cust &rest msg)
    (let ((spons *current-sponsor*))
      (if (eq spons *slow-sponsor*)
          (send* svc cust msg)
        
        ;; Else - forward to svc on IO thread
        ;; send result back to customer on current thread
        (sendx* *slow-sponsor*
                svc
                (actor ans
                  ;; forwarding customer for svc
                  (sendx* spons cust ans))
                msg)
        ))))
      
;; -----------------------------------------------
;; For sequenced message delivery
;;
;; When a messages arrives out of order, send it with :WAIT to the
;; pending items list. When you are ready for any particular sequence
;; number or label, then send :START with that seequence number to the
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

