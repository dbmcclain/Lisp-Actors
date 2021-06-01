;; Actors.lisp -- An implementation of Actors
;;
;; Single thread semantics across multithreaded and SMP systems
;;
;; DM/RAL  12/17
;; -----------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#


(in-package #:actors/base)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 2) #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------------------------------
(defgeneric send (obj &rest msg))
;; ------------------------------------------------------

;; -----------------------------------------------------
;; Actors are simply indirect refs to a beh closure (= function + state).
;;
;; Actor behavior/state can change without affecting the identity of
;; the Actor.
;;               +------+-----+
;;  Actor Ref -->| Type | Beh |
;;               +------+-----+
;;                  |      |
;;                  |      v      Closure
;;                  |    +----+-------+
;;                  v    | Fn | State |
;;             T(Actor)  +----+-------+     Bindings
;;                         |      |      +------+-----+-----+---
;;                         |      +----->| Data | ... | ... |
;;                         |             +------+-----+-----|---
;;                         |    +------+-----+-----+---
;;                         +--->| Code | ... | ... |
;;                              +------+-----+-----+---

(defstruct (actor
               (:constructor %make-actor))
  beh)

(defun make-actor (&optional (beh #'lw:do-nothing))
  (check-type beh function)
  (%make-actor :beh beh))

;; ----------------------------------
;; SPONSORS -- offer event queues and have associated runtime threads
;; to perform RUN dispatching of Actor events.
;;
;; We have two main Sponsors - a single-threaded one (denoted as :ST
;; or NIL), and a multi-threaded one (denoted as :MT or T). We default
;; to the :MT Sponsor event queue for maximum parallelism.

(defstruct (sponsor
            (:constructor %make-sponsor))
  (mbox   (mp:make-mailbox))
  thread
  msg-send)

(defvar *sponsor*         nil) ;; Single-Threaded
(defvar *slow-sponsor*    nil)
(defvar *current-sponsor* nil)

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *whole-message*    nil) ;; Current Event Message
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior
(defvar *evt-queue*        nil) ;; Current Event Queue

(define-symbol-macro self     *current-actor*)
(define-symbol-macro self-beh *current-behavior*)

;; -----------------------------------------------------------------
;; Fast Imperative Queue
;; Simple Direct Queue ~54ns SEND/dispatch
;; Simple CONS cell for queue: CAR = head, CDR = last

(declaim (inline make-evqueue empty-evq?))

(defun make-evqueue ()
  #F
  (list nil))

(defun empty-evq? (queue)
  #F
  (declare (cons queue))
  (null (car queue)))

(defun pop-evq (queue)
  #F
  (declare (cons queue))
  (let ((cell (car queue)))
    (when cell
      (unless (setf (car queue) (cdr (the cons cell)))
        (setf (cdr queue) nil))
      (car (the cons cell)))))

(defun add-evq (queue elt)
  #F
  (declare (cons queue))
  (let ((cell (list elt)))
    (if (cdr queue)
        (setf (cdr queue)
              (setf (cddr queue) cell))
      (setf (car queue)
            (setf (cdr queue) cell)))))

;; -----------------------------------------------------------------
;; Generic RUN for all threads, across all Sponsors
;;
;; SENDs and BECOME are optimistically committed.  In more uncommon
;; case of error, the tail of the event queue is rolled back, and the
;; Actor behavior of the current Actor is restored.
;;
(defun run-actors (*current-sponsor*)
  #F
  (let ((mbox         (sponsor-mbox *current-sponsor*))
        (*evt-queue*  (list nil))
        (qsave        nil)) ;; rollback copy
    (declare (dynamic-extent *evt-queue* qsave))
    (loop
     (with-simple-restart (abort "Handle next event")
       (handler-bind
           ((error (lambda (c)
                     (declare (ignore c))
                     ;; unroll the committed SENDS and BECOME
                     (if (setf (cdr *evt-queue*) qsave)
                         (setf (cdr qsave) nil)
                       (setf (car *evt-queue*) nil))
                     (setf (actor-beh self)  self-beh))
                   ))
           (loop
            ;; Get a Foreign SEND event if any
            (when (mp:mailbox-not-empty-p mbox)
              (add-evq *evt-queue*
                       (mp:mailbox-read mbox)))
            
            ;; Fetch next event from event queue
            (let ((evt (or (pop-evq *evt-queue*)
                           (mp:mailbox-read mbox))))
              (declare (cons evt)
                       (dynamic-extent evt))
              (setf qsave (cdr *evt-queue*))  ;; grab for possible rollback
              ;; Setup Actor context
              (let* ((*current-actor*    (car evt))
                     (*current-behavior* (actor-beh self))
                     (*whole-message*    (cdr evt)))
                (declare (actor    *current-actor*)
                         (function *current-behavior*)
                         (list     *whole-message*)
                         (dynamic-extent *current-actor* *current-behavior* *whole-message*))
                ;; ---------------------------------
                ;; Dispatch to Actor behavior with message args
                (apply self-beh *whole-message*)
                ))
            
            ;; Fetch next event from event queue
            (let ((evt (or (pop-evq *evt-queue*)
                           (mp:mailbox-read mbox))))
              (declare (cons evt)
                       (dynamic-extent evt))
              (setf qsave (cdr *evt-queue*))  ;; grab for possible rollback
              ;; Setup Actor context
              (let* ((*current-actor*    (car evt))
                     (*current-behavior* (actor-beh self))
                     (*whole-message*    (cdr evt)))
                (declare (actor    *current-actor*)
                         (function *current-behavior*)
                         (list     *whole-message*)
                         (dynamic-extent *current-actor* *current-behavior* *whole-message*))
                ;; ---------------------------------
                ;; Dispatch to Actor behavior with message args
                (apply self-beh *whole-message*)
                ))
            ))))
    ))

#|
(kill-executives)
(start-actors-system)
 |#

;; ----------------------------------------------------------

(defun start-actors-system ()
  (unless *sponsor*
    (setf *sponsor*      (make-sponsor "Actor Thread")
          *slow-sponsor* (make-sponsor "Actor IO Thread"))))

(defun kill-executives ()
  (when *sponsor*
    (kill-sponsor (shiftf *sponsor* nil))
    (kill-sponsor (shiftf *slow-sponsor* nil))))

;; ----------------------------------------------------------

(defun msg-send-beh (mbox)
  #F
  (lambda (&rest msg)
    (mp:mailbox-send mbox msg)))

(defun make-sponsor (title)
  (let ((new-sponsor (%make-sponsor)))
    (setf (sponsor-msg-send new-sponsor)
          (make-actor (msg-send-beh (sponsor-mbox new-sponsor)))

          (sponsor-thread new-sponsor) 
          (mp:process-run-function title
                                   ()
                                   #'run-actors new-sponsor))
    new-sponsor))

(defun kill-sponsor (sponsor)
  (mp:process-terminate (sponsor-thread sponsor)))

;; -----------------------------------------------
;; SEND/BECOME - Since these methods are called against SELF they can
;; only be called from within a currently active Actor.
;;
;; SEND and BECOME are transactionally staged, and will commit *ONLY*
;; upon error free completion of the Actor body code. So if you need
;; them to take effect, even as you call potentially unsafe functions,
;; then surround your function calls with HANDLER-CASE, HANDLER-BIND,
;; or IGNORE-ERRORS.

(defmethod become ((new-fn function))
  ;; Change behavior/state. Only meaningful if an Actor calls
  ;; this.
  #F
  (when self
    (setf (actor-beh self) new-fn)))

(defmacro send* (&rest msg)
  ;; to be used when final arg is a list
  ;; saves typing APPLY #'SEND, analogous to LIST*
  `(apply #'send ,@msg))

(defmethod send ((actor actor) &rest msg)
  #F
  (declare (dynamic-extent msg))
  (cond (self
         (add-evq *evt-queue*
                  (cons actor (the list msg))))
        (t
         (apply #'sendx *sponsor* actor (the list msg)))
        ))

(defmacro sendx* (&rest msg)
  `(apply #'sendx ,@msg))

(defmethod sendx ((spon sponsor) (actor actor) &rest msg)
  ;; cross-sponsor sends
  #F
  (declare (dynamic-extent msg))
  (cond (self
         (if (eq spon *current-sponsor*)
             (send* actor (the list msg))
           (send* (sponsor-msg-send spon) actor (the list msg))
           ))

        (t
         (mp:mailbox-send (sponsor-mbox spon)
                          (cons actor (the list msg))))
        ))

(defmethod repeat-send ((dest actor))
  ;; Send the current event message to another Actor
  #F
  (when self
    (send* dest (the list *whole-message*))))

;; ----------------------------------------------------------------
;; Using Sponsors

(defmacro with-sponsor (sponsor &body body)
  `(let ((*current-sponsor* ,sponsor))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-sponsor" 1)

(defmacro with-worker (&body body)
  `(beta _
       (send beta)
     ,@body))

;; --------------------------------------
;; A Par-Safe-Behavior is guaranteed safe for sharing of single
;; instances across multiple SMP threads. Only one thread at a time is
;; permitted to execute the behavior code.
;;
;; This becomes important when a single instance of an Actor is shared
;; among multiple event handlers (multiple threads) and the Actor
;; exercises BECOME, or otherwise mutates its internal state. BECOME
;; mutates internal state.

(defun ensure-par-safe-behavior (beh)
  (check-type beh function)
  (locally
    #F
    (declare (function beh))
    (let ((lock  (mp:make-lock))
	  this-beh)
      (setf this-beh
	    #'(lambda (&rest msg)
		(let ((wait-dur (if (and (empty-evq? *evt-queue*)
					 (mp:mailbox-empty-p
                                          (sponsor-mbox *current-sponsor*)))
				    0.1  ;; no pending work, so just wait
                                  0)))   ;; other work to do, so go around again if can't lock immed
		  (unless (mp:with-lock (lock nil wait-dur)
			    (when (eq (actor-beh self) this-beh) ;; behavior changed while waiting?
			      (apply beh msg)
			      t))
		    (send* self msg))) ;; go around again, or perform other work
		))
      )))
