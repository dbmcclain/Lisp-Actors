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
  thread)

(defvar *sponsor*         nil) ;; Single-Threaded
(defvar *slow-sponsor*    nil)
(defvar *current-sponsor* nil)

(defun current-sponsor ()
  (or *current-sponsor*
      *sponsor*))

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *new-beh*       nil)   ;; Staging for BECOME
(defvar *sendx-evts*    nil)   ;; Staging for SENDX
(defvar *whole-message* nil)   ;; Current Event Message
(defvar *current-actor* nil)   ;; Current Actor
(defvar *evt-queue*     nil)   ;; Current Event Queue

(define-symbol-macro self     *current-actor*)
(define-symbol-macro self-beh (actor-beh self))

;; -----------------------------------------------------------------
;; Fast Imperative Queue
;; Simple Direct Queue ~74ns SEND/dispatch

(declaim (inline make-queue emptyq?))

(defun make-queue ()
  #F
  (list nil))

(defun emptyq? (queue)
  #F
  (declare (cons queue))
  (null (car queue)))

(defun popq (queue)
  #F
  (declare (cons queue))
  (let ((cell (car queue)))
    (when cell
      (unless (setf (car queue) (cdr (the cons cell)))
        (setf (cdr queue) nil))
      (car (the cons cell)))))

(defun addq (queue elt)
  #F
  (declare (cons queue))
  (let ((cell (list elt)))
    (if (cdr queue)
        (setf (cdr queue)
              (setf (cddr queue) cell))
      (setf (car queue)
            (setf (cdr queue) cell)))))

(defun appendq (qhd qtl)
  #F
  (declare (cons qhd qtl))
  (when (car qtl)
    (if (car qhd)
        (setf (cddr qhd) (car qtl))
      (setf (car qhd) (car qtl)))
    (setf (cdr qhd) (cdr qtl))
    ))

;; -----------------------------------------------------------------
;; Generic RUN for all threads, across all Sponsors
;;
;; SENDs are optimistically committed to the event queue. In case of
;; error, the tail of the queue is rolled back.
;;
(defun run-actors (*current-sponsor*)
  #F
  (let ((mbox         (sponsor-mbox *current-sponsor*))
        (*evt-queue*  (make-queue))
        (current-tail nil)) ;; rollback pointer
    (loop
     (when (setf (cdr *evt-queue*) current-tail)
       (setf (cdr current-tail) nil))
     (with-simple-restart (abort "Handle next event")
       (loop
        ;; Get a Foreign SEND event if any
        (when (mp:mailbox-not-empty-p mbox)
          (addq *evt-queue*
                (mp:mailbox-read mbox)))
        ;; Fetch next event from event queue
        (let ((evt (or (popq *evt-queue*)
                       (mp:mailbox-read mbox))))
          (declare (cons evt))
          (setf current-tail (cdr *evt-queue*)) ;; grab queue tail for possible rollback
          ;; Setup Actor context
          (let* ((*current-actor* (car evt))
                 (*whole-message* (cdr evt))
                 (*new-beh*       nil)
                 ;; (*new-beh*       self-beh)
                 (*sendx-evts*    nil))
            (declare (actor    *current-actor*)
                     (list     *whole-message* *sendx-evts*))
            ;; ---------------------------------
            ;; Dispatch to Actor behavior with message args
            (apply #|*new-beh*|# self-beh *whole-message*)

            (when *sendx-evts*
              ;; Handle cross-Sponsor SENDs
              (dolist (evt *sendx-evts*)
                (apply #'mp:mailbox-send evt)))
            ;; Apply staged BECOME
            (when *new-beh*
              (setf self-beh *new-beh*)))
          ))))))

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

(defun make-sponsor (title)
  (let ((new-sponsor (%make-sponsor)))
    (setf (sponsor-thread new-sponsor) 
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

(defun become (new-fn)
  ;; Change behavior/state. Only meaningful if an Actor calls
  ;; this.
  #F
  (check-type new-fn function)
  ;; BECOME is staged.
  (when self
    (setf *new-beh* new-fn)))

(defmacro send* (&rest msg)
  ;; to be used when final arg is a list
  ;; saves typing APPLY #'SEND, analogous to LIST*
  `(apply #'send ,@msg))

(defmethod send ((actor actor) &rest msg)
  #F
  (cond (self
         ;; Actor SENDs are staged.
         (addq *evt-queue*
               (cons actor msg)))
        (t
         ;; Non-Actor SENDs take effect immediately.
         (apply #'sendx (current-sponsor) actor msg))
        ))

(defmacro sendx* (&rest msg)
  `(apply #'sendx ,@msg))

(defmethod sendx ((spon sponsor) (actor actor) &rest msg)
  ;; cross-sponsor sends
  #F
  (cond (self
         (if (eq spon *current-sponsor*)
             (send* actor msg)
           (push (list (sponsor-mbox spon)
                       (cons actor msg))
                 (the list *sendx-evts*))
           ))

        (t
         (mp:mailbox-send (sponsor-mbox spon)
                          (cons actor msg)))
        ))

(defun repeat-send (dest)
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

#|
(defun ensure-par-safe-behavior (beh)
  (check-type beh function)
  (locally
    #F
    (declare (function beh))
    ;; Take care to avoid race conditition as a result of BECOME
    (labels ((go-around-beh (&rest msg)
               (send* self msg))
             (restore-behavior (cx)
               (declare (ignore cx))
               (setf (actor-beh self) #'swap-out-beh))
             (swap-out-beh (&rest msg)
               (if (sys:compare-and-swap (actor-beh self)
                                         #'swap-out-beh
                                         #'go-around-beh)
                   (handler-bind ((error #'restore-behavior))
                     (progn
                       (apply beh msg)
                       (setf (actor-beh self) (or (shiftf *new-beh* nil)
                                                  #'swap-out-beh))))
                 ;; else -- something changed beh behind our backs...
                 (send* self msg))))
      #'swap-out-beh)))
|#
#||#
(defun ensure-par-safe-behavior (beh)
  (check-type beh function)
  (locally
    #F
    (declare (function beh))
    (let ((lock  (mp:make-lock))
	  this-beh)
      (setf this-beh
	    #'(lambda (&rest msg)
		(let ((wait-dur (if (and (emptyq? *evt-queue*)
					 (mp:mailbox-empty-p
                                          (sponsor-mbox *current-sponsor*)))
				    nil  ;; no pending work, so just wait
                                  0)))   ;; other work to do, so go around again if can't lock immed
		  (unless (mp:with-lock (lock nil wait-dur)
			    (when (eq self-beh this-beh) ;; behavior changed while waiting?
			      (apply beh msg)
			      (when *new-beh* ;; must perform staged BECOME before releasing lock
				(setf self-beh (shiftf *new-beh* nil)))
			      t))
		    (send* self msg))) ;; go around again, or perform other work
		))
      )))
#||#
