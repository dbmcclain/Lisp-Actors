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
(defgeneric become (new-beh))
(defgeneric send (obj &rest msg))
(defgeneric repeat-send (dest))
(defgeneric ensure-par-safe-behavior (beh))
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

; -----------------------------------------------------------------
;; ACTOR-TRAIT distinguishes objects that can behave like Actors. This
;; includes Actors, SPONSORED-ACTORS, and HOSTED-ACTORS.
(defstruct actor-trait)

;; LOCAL-ACTOR-TRAIT distinguishes objects that behave as Actors on
;; the current host machine. This includes Actors and
;; SPONSORED-ACTORS, but not HOSTED-ACTORS.
(defstruct (local-actor-trait
            (:include actor-trait)))

;; ------------------------------------------------------------------

(defstruct (actor
               (:include local-actor-trait)
               (:constructor %make-actor (beh)))
  beh)

(defun make-actor (&optional (beh #'lw:do-nothing))
  (check-type beh function)
  (%make-actor beh))

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

(defstruct (sponsored-actor
            (:include local-actor-trait)
            (:constructor sponsored-actor (spon act)))
  spon
  act)

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *whole-message*    nil) ;; Current Event Message
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior
(defvar *evt-queue*        nil) ;; Current Event Queue

(define-symbol-macro self         *current-actor*)
(define-symbol-macro self-beh     *current-behavior*)
(define-symbol-macro self-sponsor *current-sponsor*)

;; -----------------------------------------------------------------
;; Fast Imperative Queue
;; Simple Direct Queue ~54ns SEND/dispatch
;; Simple CONS cell for queue: CAR = head, CDR = last

(declaim (inline make-evq empty-evq?))

(defun make-evq ()
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
  (let ((cell (list elt))
        (tail (cdr queue)))
    (setf (cdr queue)
          (if tail
              (setf (cdr (the cons tail)) cell)
            (setf (car queue) cell)))
    ))

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
        (*evt-queue*  (make-evq))
        (qsave        nil)) ;; rollback copy
    (declare (dynamic-extent mbox *evt-queue* qsave))
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

(defun mbox-sender-beh (mbox)
  (lambda (&rest ans)
    (mp:mailbox-send mbox ans)))

(defun mbox-sender (mbox)
  (make-actor (mbox-sender-beh mbox)))

(defun make-sponsor (title)
  (let ((new-sponsor (%make-sponsor)))
    (setf (sponsor-msg-send new-sponsor)
          (mbox-sender (sponsor-mbox new-sponsor))

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
  (setf (actor-beh self) new-fn))

(defmacro send* (&rest msg)
  ;; to be used when final arg is a list
  ;; saves typing APPLY #'SEND, analogous to LIST*
  `(apply #'send ,@msg))

(defmethod send ((actor actor) &rest msg)
  #F
  (if self
      (add-evq *evt-queue*
               (cons actor (the list msg)))
    (send* *sponsor* actor msg)
    ))

(defvar *sponsor-send-gateway*
  ;; allow us to re-interpret SEND target in *current-sponsor*
  (make-actor #'send))

(defmethod send ((spon sponsor) &rest msg)
  ;; Cross-Sponsor SEND
  ;; First msg component is typically an Actor...
  #F
  (if self
      (if (eq spon *current-sponsor*)
          (send* (the list msg))
        (send* (sponsor-msg-send spon) *sponsor-send-gateway* (the list msg)))
    (mp:mailbox-send (sponsor-mbox spon)
                     (cons *sponsor-send-gateway* (the list msg)))
    ))

(defmethod send ((dest sponsored-actor) &rest msg)
  (send* (sponsored-actor-spon dest) (sponsored-actor-act dest) msg))

(defmethod send ((mbox mp:mailbox) &rest msg)
  (mp:mailbox-send mbox msg))

;; ------------------------------------------------

(defmethod repeat-send ((dest actor-trait))
  ;; Send the current event message to another Actor
  #F
  (send* dest (the list *whole-message*)))

;; ------------------------------------------------

(defmethod send-now ((spon sponsor) (actor actor) &rest msg)
  ;; bypass the delayed SEND mechanism to force an immediate SEND
  (mp:mailbox-send (sponsor-mbox spon) (cons actor msg)))

(defmethod ask ((spon sponsor) (actor actor) &rest msg)
  ;; The bridge between Actors and imperative style code...
  ;;
  ;; Actor cannot call upon anything in the *current-sponsor* for
  ;; itself - that will cauase a hang-waiting lockup of the current
  ;; sponsor while it awaits its answer in the local mailbox.
  (assert (not (eq spon *current-sponsor*)))
  (let ((mbox (mp:make-mailbox)))
    (apply #'send-now spon actor mbox msg)
    (values-list (mp:mailbox-read mbox))))

;; --------------------------------------
;; A Par-Safe-Behavior is guaranteed safe for sharing of single
;; instances across multiple SMP threads. Only one thread at a time is
;; permitted to execute the behavior code.
;;
;; This becomes important when a single instance of an Actor is shared
;; among multiple event handlers (multiple threads) and the Actor
;; exercises BECOME, or otherwise mutates its internal state. BECOME
;; mutates internal state.

(defmethod ensure-par-safe-behavior ((beh function))
  ;; no need for locks (implicit in cross-thread mailbox sends)
  (let ((act (make-actor beh)))
    (lambda (&rest msg)
      (send* *sponsor* act msg))
    ))
