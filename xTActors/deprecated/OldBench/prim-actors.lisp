;; prim-actors.lisp - A collection of useful primitive Actors
;;
;; DM/RAL 05/21
;; ------------------------------------------------------
(in-package :actors/base)
;; ------------------------------------------------------
;; Sink Behaviors

(defun make-sink-beh ()
  (make-par-behavior
   #'lw:do-nothing))

(defvar *bitpit*
  (make-actor (make-sink-beh)))

(defun sink ()
  *bitpit*)

;; --------------------------------------

(defvar *println*
  (actor (&rest msg)
    (format t "~&~{~A~^ ~}~%" msg)
    (values)))

(defun println ()
  *println*)

;; -------------------------------------
;; Non-Sink Behaviors

(defun make-const-beh (&rest msg)
  (make-par-behavior
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
  (make-par-behavior
   (lambda (&rest msg)
     (dolist (cust actors)
       (send* cust msg)))))

(defun send-to-all (&rest actors)
  (make-actor (apply #'make-send-to-all-beh actors)))

;; ---------------------

(defun make-race-beh (&rest actors)
  (make-par-behavior
   (lambda (cust &rest msg)
     (let ((gate (one-shot cust)))
       (dolist (actor actors)
         (send* actor gate msg))
       ))))

(defun race (&rest actors)
  (make-actor (apply #'make-race-beh actors)))

;; ---------------------

(defun make-fwd-beh (actor)
  (make-par-behavior
   (lambda (&rest msg)
     (send* actor msg))))

(defun fwd (actor)
  (make-actor (make-fwd-beh actor)))

;; ---------------------

(defun make-label-beh (cust lbl)
  (make-par-behavior
   (lambda (&rest msg)
     (send* cust lbl msg))))

(defun label (cust lbl)
  (make-actor (make-label-beh cust lbl)))

;; ---------------------

(defun make-tag-beh (cust)
  (make-par-behavior
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
  (actors ((fut (make-future-wait-beh tag nil))
           (tag (make-tag-beh fut)))
    (send* actor tag msg)
    fut))

;; -----------------------------------------

(defun lazy (actor &rest msg)
  (actor (cust)
    (let ((tag (tag self)))
      (become (make-future-wait-beh tag (list cust)))
      (send* actor tag msg))
    ))

;; ----------------------------------------

(defmacro blk (args &rest clauses)
  (lw:with-unique-names (cust)
    `(actor ,(if (consp args)
                 `(,cust ,@args)
               `(,cust &rest ,args)) ;; handle (BLK _ ...)
       (send ,cust
             (progn
               ,@clauses)))
    ))

#+:LISPWORKS
(editor:setup-indent "blk" 1)

;; --------------------------------------

(defun ser-pair (blk1 blk2)
  (actor (cust &rest msg)
    (send* blk1
           (actor (&rest msg1)
             (send* blk2 cust msg1))
           msg)))

(defmacro ser (blk &rest blks)
  (if blks
      `(ser-pair ,blk (ser ,@blks))
    blk))

;; -----------------------------------

(defun make-join-beh (cust lbl1 lbl2)
  (declare (ignore lbl2))
  (lambda (lbl &rest msg)
    (cond ((eq lbl lbl1)
           (become (lambda (_ &rest msg2)
                     (declare (ignore _))
                     (multiple-value-call #'send cust (values-list msg) (values-list msg2)))
                   ))
          (t ;; (eq lbl lbl2)
           (become (lambda (_ &rest msg1)
                     (declare (ignore _))
                     (multiple-value-call #'send cust (values-list msg1) (values-list msg)))
                   ))
          )))

(defun par-pair (blk1 blk2)
  (actor (cust)
    (actors ((join (make-join-beh cust lbl1 lbl2))
             (lbl1 (make-tag-beh join))
             (lbl2 (make-tag-beh join)))
      (send blk1 lbl1)
      (send blk2 lbl2))
    ))

(defmacro par (blk &rest blks)
  (if blks
    `(par-pair ,blk (par ,@blks))
    blk))

#|
(ser blk1 blk2 blk3)
(send (par
       (blk ()
         :blk1)
       (blk ()
         :blk2)
       (blk ()
         :blk3))
      (println))
               
(send (par
       (blk ()
         :blk1)
       (blk ()
         :blk2))
      (println))

(let* ((actor (make-actor (lambda (cust) (sleep 2) (send cust :ok))))
       (fut   (future actor)))
  (send fut (println))
  (send fut (println)))
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
#|
(defun make-timer-beh ()
  ;; On :START it records the start time and awaits a :STOP command.
  ;; On :STOP it sends the elapsed time in microsec to cust.
  (um:dlambda
    (:start ()
     (let* ((k-st  (actor ()
                     (let ((start (usec:get-time-usec)))
                       (become (lambda (cust)
                                 (let ((stop (usec:get-time-usec)))
                                   (send cust (- stop start)))))
                       ))))
       (become (um:dlambda
                 (:stop (cust)
                  (send k-st cust))))
       (send k-st))
     )))
|#
(defun make-timer-beh ()
  ;; On :START it records the start time and awaits a :STOP command.
  ;; On :STOP it sends the elapsed time in microsec to cust.
  (um:dlambda
    (:start ()
     (let* ((k-st  (actor ()
                     (let ((start (usec:get-time-usec)))
                       (become (lambda (spon cust)
                                 (let ((stop (usec:get-time-usec)))
                                   (sendx spon cust (- stop start)))))
                       ))))
       (become (um:dlambda
                 (:stop (cust)
                  (sendx nil k-st *current-sponsor* cust))))
       (sendx nil k-st))
     )))

(defun timer ()
  (α (make-timer-beh)))

#|
(let ((timer (α (make-timer-beh))))
  (sendx nil timer :start)
  (sleep 1)
  (sendx nil timer :stop (println)))
  |#
