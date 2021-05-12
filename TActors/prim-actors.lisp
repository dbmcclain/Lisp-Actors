;; prim-actors.lisp - A collection of useful primitive Actors
;;
;; DM/RAL 05/21
;; ------------------------------------------------------
(in-package :actors/base)
;; ------------------------------------------------------
;; Sink Behaviors

(defun make-sink-beh ()
  (make-par-safe-behavior
   #'lw:do-nothing))

(defvar sink
  (α (make-sink-beh)))

;; --------------------------------------

(defvar println
  (actor (&rest msg)
    (format t "~&~{~A~^ ~}~%" msg)
    (values)))

;; -------------------------------------
;; Non-Sink Behaviors

(defun make-const-beh (&rest msg)
  (make-par-safe-behavior
   (lambda (cust)
     (send* cust msg))))

(defun const (&rest msg)
  (make-actor (apply #'make-const-beh msg)))

;; ---------------------

(defun make-one-shot-beh (cust)
  (lambda (&rest msg)
    (send* cust msg)
    (become (make-sink-beh))))

(defun one-shot (cust)
  (make-actor (make-one-shot-beh cust)))

;; ---------------------

(defun make-send-to-all-beh (&rest actors)
  (make-par-safe-behavior
   (lambda (&rest msg)
     (dolist (cust actors)
       (send* cust msg)))))

(defun send-to-all (&rest actors)
  (make-actor (apply #'make-send-to-all-beh actors)))

;; ---------------------

(defun make-race-beh (&rest actors)
  (make-par-safe-behavior
   (lambda (cust &rest msg)
     (let ((gate (one-shot cust)))
       (dolist (actor actors)
         (send* actor gate msg))
       ))))

(defun race (&rest actors)
  (make-actor (apply #'make-race-beh actors)))

;; ---------------------

(defun make-fwd-beh (actor)
  (make-par-safe-behavior
   (lambda (&rest msg)
     (send* actor msg))))

(defun fwd (actor)
  (make-actor (make-fwd-beh actor)))

;; ---------------------

(defun make-label-beh (cust lbl)
  (make-par-safe-behavior
   (lambda (&rest msg)
     (send* cust lbl msg))))

(defun label (cust lbl)
  (make-actor (make-label-beh cust lbl)))

;; ---------------------

(defun make-tag-beh (cust)
  (make-par-safe-behavior
   (lambda (&rest msg)
     (send* cust self msg))))

(defun tag (cust)
  (make-actor (make-tag-beh cust)))

;; -------------------------------------------------

(defun make-future-wait-beh (tag custs)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (become (apply #'make-const-beh msg))
           (dolist (cust custs)
             (send self cust)))
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

;; ----------------------------------------

(defmacro blk (args &rest clauses)
  ;; Makes an Actor from a PROGN block of Lisp
  (lw:with-unique-names (cust)
    `(actor ,(if (listp args)
                 `(,cust ,@args)
               `(,cust &rest ,args)) ;; handle (BLK _ ...)
       (send ,cust
             (progn
               ,@clauses)))
    ))

#+:LISPWORKS
(editor:setup-indent "blk" 1)

;; --------------------------------------
;; SER - make an Actor that evaluates a series of blocks sequentially
;; - i.e., without concurrency between them.  Each block is fed the
;; same initial message, and the results from each block are sent as
;; an ordered collection to cust.

(defun make-ser-beh ()
  (make-par-safe-behavior
   (lambda (cust lst &rest msg)
     (if (null lst)
         (send cust)
       (destructuring-bind (hd . tl) lst
         (let ((me self))
           (@bind (&rest msg-hd)
               (send* hd @bind msg)
             (@bind (&rest msg-tl)
                 (send* me @bind tl msg)
               (multiple-value-call #'send cust (values-list msg-hd) (values-list msg-tl))))
           ))))))

(defvar ser
  ;; since SER is par-behavior safe, and is not parameterized we only
  ;; need one
  (α (make-ser-beh)))
  
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
                     (multiple-value-call #'send cust (values-list msg) (values-list msg2)))
                   ))
          (t ;; (eq lbl lbl2)
           (become (lambda (_ &rest msg1)
                     (declare (ignore _)) ;; _ = lbl arg
                     (multiple-value-call #'send cust (values-list msg1) (values-list msg)))
                   ))
          )))

(defun make-par-beh ()
  (make-par-safe-behavior
   (lambda (cust lst &rest msg)
     (if (null lst)
         (send cust)
       (destructuring-bind (hd . tl) lst
         (actors ((join (make-join-beh cust lbl1 lbl2))
                  (lbl1 (make-tag-beh join))
                  (lbl2 (make-tag-beh join)))
           (send* hd lbl1 msg)
           (send* self lbl2 tl msg)))
       ))))

(defvar par
  ;; since PAR is par-behavior safe, and is not parameterized, we only
  ;; need one
  (α (make-par-beh)))

;; ---------------------------------------------------------
#|
(send par println
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

(defun make-serializer-beh (service)
  ;; initial empty state
  (lambda (cust &rest msg)
    (let ((tag  (tag self)))
      (send* service tag msg)
      (become (make-enqueued-serializer-beh
               service tag cust nil))
      )))

(defun make-enqueued-serializer-beh (service tag in-cust queue)
  (lambda (cust &rest msg)
    (cond ((eq cust tag)
           (send* in-cust msg)
           (if queue
               (multiple-value-bind (next-req new-queue)
                   (finger-tree:popq queue)
                 (destructuring-bind (next-cust . next-msg) next-req
                   (send* service tag next-msg)
                   (become (make-enqueued-serializer-beh
                            service tag next-cust new-queue))
                   ))
               ;; else
               (become (make-serializer-beh service))))
          (t
           (become (make-enqueued-serializer-beh
                    service tag in-cust (finger-tree:addq queue (cons cust msg)))))
          )))

(defun serializer (service)
  (make-actor (make-serializer-beh service)))

;; --------------------------------------

(defun make-timing-beh (dut)
  (lambda (cust &rest msg)
    (let ((start (usec:get-time-usec)))
      (@bind _
          (send* dut @bind msg)
        (send cust (- (usec:get-time-usec) start)))
      )))

(defun timing (dut)
  (α (make-timing-beh dut)))

#|
(let* ((dut (actor (cust nsec)
             (sleep nsec)
             (send cust)))
      (timer (timing dut)))
  (send timer println 1))
|#

