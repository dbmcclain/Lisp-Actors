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
;; ------------------------------------------------------------------

(defstruct (actor
               (:constructor %make-actor (beh)))
  beh)

(defun make-actor (&optional (beh #'lw:do-nothing))
  (check-type beh function)
  (%make-actor beh))

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *whole-message*    nil) ;; Current Event Message
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior
(defvar *evt-queue*        nil) ;; Current Event Queue
(defvar *current-sponsor*  nil) ;; Current Sponsor active during Actor exec

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
(defun run-actors (*current-sponsor* mbox)
  #F
  (let ((*evt-queue*  (make-evq))
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

;; ----------------------------------------------------------
;; SPONSORS -- offer an event queue and have an associated runtime
;; thread performing RUN dispatching of Actor events.
;;

(defun primordial-sponsor-beh ()
  (lambda (mbox thread)
    (become (sponsor-beh mbox thread))))

(defun sponsor-beh (mbox thread)
  (alambda
   ((:shutdown)
    (become #'lw:do-nothing)
    (mp:process-terminate thread))

   (msg
    (mp:mailbox-send mbox msg))
   ))

(defun make-sponsor (title)
  (let ((spon (make-actor)))
    (restart-sponsor spon title)))

(defun kill-sponsor (sponsor)
  (foreign-send sponsor :shutdown))

(defun restart-sponsor (sponsor title)
  (setf (actor-beh sponsor) (primordial-sponsor-beh))
  (let* ((mbox   (mp:make-mailbox))
         (thread (mp:process-run-function title () #'run-actors sponsor mbox)))
    (mp:mailbox-send mbox (list sponsor mbox thread))
    sponsor))

;; ----------------------------------------------------------------

(defvar base-sponsor (make-actor))
(defvar slow-sponsor (make-actor))

(defun restart-actors-system ()
  (restart-sponsor base-sponsor "Actor Thread")
  (restart-sponsor slow-sponsor "Actor I/O Thread"))

(defun kill-actors-system ()
  (kill-sponsor base-sponsor)
  (kill-sponsor slow-sponsor))

#|
(kill-actors-system)
(restart-actors-system)
 |#

;; -----------------------------------------------
;; SEND/BECOME
;;
;; SEND & BECOME can only be called from within an Actor.
;;
;; SEND and BECOME are transactionally staged, and will commit *ONLY*
;; upon error free completion of the Actor body code.
;;
;; So if you need them to take effect, even as you call potentially
;; unsafe functions, then surround your function calls with
;; HANDLER-CASE, HANDLER-BIND, or IGNORE-ERRORS. Otherwise, an error
;; will make it seem that the message causing the error was never
;; delivered.

(defun become (new-beh)
  ;; Change behavior/state. Only meaningful if an Actor calls
  ;; this.
  #F
  (setf (actor-beh self) new-beh))

(defmacro send* (&rest msg)
  ;; to be used when final arg is a list
  ;; saves typing APPLY #'SEND, analogous to LIST*
  `(apply #'send ,@msg))

(defun send (&rest msg)
  ;; msg should always start with an Actor
  #F
  (add-evq *evt-queue* msg))

(defun repeat-send (dest)
  ;; Send the current event message to another Actor
  #F
  (send* dest (the list *whole-message*)))

;; -------------------------------------------------------
;; Cross-sponsor sends

(defun in-sponsor-beh (sponsor actor)
  (lambda* msg
    (if (eq sponsor self-sponsor)
        (send* actor msg)
      (send* sponsor actor msg))
    ))

(defun in-sponsor (sponsor actor)
  (make-actor (in-sponsor-beh sponsor actor)))

(defun par-safe (actor)
  (in-sponsor base-sponsor actor))

(defun io (actor)
  (in-sponsor slow-sponsor actor))

;; --------------------------------------

(defun sink-beh ()
  #'lw:do-nothing)

(defvar sink
  (make-actor (sink-beh)))

;; --------------------------------------

(defvar println
  (io
    ;; because we are managing an output stream
    (make-actor
     (lambda* msg
       (format t "~&~{~A~^ ~}~%" msg))
     )))

;; ------------------------------------------------
;; The bridge between imperative code and the Actors world

(defun foreign-send (&rest msg)
  ;; msg should always start with an Actor
  (if self
      (send* base-sponsor msg)
    ;; this only works because we know how simple the sponsor code is.
    (apply (actor-beh base-sponsor) msg))) 

(defun mbox-sender-beh (mbox)
  (lambda (&rest ans)
    (mp:mailbox-send mbox ans)))

(defun mbox-sender (mbox)
  (make-actor (mbox-sender-beh mbox)))

(defun foreign-ask (actor &rest msg)
  ;; Actor should expect a cust arg in first position. Here, the
  ;; mailbox.
  (if self
      ;; Counterproductive when called from an Actor, except for
      ;; possible side effects
      (mp:funcall-async (lambda ()
                          (apply 'foreign-send actor sink msg)))
    (let ((mbox (mp:make-mailbox)))
      (apply 'foreign-send actor (mbox-sender mbox) msg)
      (values-list (mp:mailbox-read mbox)))
    ))

;; ----------------------------------------

(defun lw-start-actors (&rest _)
  (declare (ignore _))
  (restart-actors-system))

(defun lw-kill-actors (&rest _)
  (declare (ignore _))
  (kill-actors-system))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor Server"
                    'lw-start-actors
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Reset Actors"
                    'lw-kill-actors)

  (lw:define-action "Save Session After"
                    "Restart Actor System"
                    'lw-start-actors)
  )

