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

(defmacro α (args &body body)
  `(make-actor
    (lambda* ,args
      ,@body)))

#+:LISPWORKS
(editor:indent-like "α" 'lambda)

;; --------------------------------------
;; ACTOR in function position acts like a higher level LAMBDA expression

(defmacro actor (args &body body)
  `(make-actor
    (lambda* ,args
      ,@body)))

#+:LISPWORKS
(editor:setup-indent "actor" 1)

;; ------------------------------------
;; ACTORS macro allows for defining new Actors which recursively
;; reference each other in their initial state. Like LETREC, but one
;; more layer of indirection here.

(defmacro actors (bindings &body body)
  ;; Binding values should be behavior closures
  `(let ,(mapcar #`(,(car a1) (make-actor)) bindings)
     ,@(mapcar #`(setf (actor-beh ,(car a1)) ,(cadr a1)) bindings)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "actors" 1)

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
(defvar *send-evts*     nil)   ;; Staging for SEND
(defvar *sendx-evts*    nil)   ;; Staging for SENDX
(defvar *whole-message* nil)   ;; Current Event Message
(defvar *current-actor* nil)   ;; Current Actor
;; (defvar *current-beh*   nil)   ;; Current Behavior

(define-symbol-macro self     *current-actor*)
;; (define-symbol-macro self-beh *current-beh*)
(define-symbol-macro self-beh (actor-beh self))

;; Simple Direct Queue ~140ns

;; -----------------------------------------------------------------

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

(defun run-actors (*current-sponsor*)
  #F
  (let ((mbox    (sponsor-mbox *current-sponsor*))
        (queue   (make-queue)))
    (loop
     (with-simple-restart (abort "Handle next event")
       (loop
        ;; Get a Foreign SEND event if any
        (when (mp:mailbox-not-empty-p mbox)
          (addq queue
                (mp:mailbox-read mbox)))
        ;; Fetch next event from event queue
        (let ((evt (or (popq queue)
                       (mp:mailbox-read mbox))))
          (declare (cons evt))
          ;; Setup Actor context
          (let* ((*current-actor* (car evt))
                 (*whole-message* (cdr evt))
                 (*new-beh*       self-beh)
                 (*send-evts*     nil)
                 (*sendx-evts*    nil))
            (declare (actor    *current-actor*)
                     (function *new-beh*)
                     (list     *whole-message* *sendx-evts*))
            ;; ---------------------------------
            ;; Dispatch to Actor behavior with message args
            (apply *new-beh* *whole-message*)
            
            ;; Commit SEND / BECOME effects
            (when *send-evts*
              ;; Handle local SENDs
              (appendq queue *send-evts*))
            (when *sendx-evts*
              ;; Handle cross-Sponsor SENDs
              (dolist (evt *sendx-evts*)
                (apply #'mp:mailbox-send evt)))
            ;; Apply staged BECOME
            (setf self-beh *new-beh*))
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
         (addq (or *send-evts*
                   (setf *send-evts* (make-queue)))
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
         (push (list (sponsor-mbox spon)
                     (cons actor msg))
               (the list *sendx-evts*)))
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

;; ----------------------------------------------
;; WORKER is just a function that executes on an external thread.
;;
;; WORKER is intended as a lightweight vehicle to perform non
;; Actor-centric duties, e.g., blocking I/O. It will see a null value
;; from SELF, just like non-Actor code running on any other
;; thread.

(defun spawn-worker (fn &rest args)
  (apply #'mp:funcall-async fn args))

(defmacro with-worker (bindings &body body)
  (let ((args (mapcar #'first bindings))
        (vals (mapcar #'second bindings)))
    `(spawn-worker (lambda* ,args
                     ,@body)
                   ,@vals)
    ))

#+:LISPWORKS
(editor:setup-indent "with-worker" 1)

;; ----------------------------------------------

(defmacro @bind (args form &body body)
  `(let ((@bind  (α ,args ,@body)))
     ,form))

(defmacro β (args form &body body)
  `(let ((β  (α ,args ,@body)))
     ,form))

#+:LISPWORKS
(editor:indent-like "@bind" 'destructuring-bind)

(defmacro @values (&rest retvals)
  `(send @bind ,@retvals))
     
;; --------------------------------------
;; A Par-Safe-Behavior is guaranteed safe for sharing of single
;; instances across multiple SMP threads. Only one thread at a time is
;; permitted to execute the behavior code.

(defun ensure-par-safe-behavior (beh)
  (check-type beh function)
  (locally
    #F
    (declare (function beh))
    (let ((ref (list t)))
      (declare (cons ref))
      (lambda* msg
        (if (sys:compare-and-swap (car ref) t nil)
            (unwind-protect
                (apply beh msg)
              (setf (car ref) t))
          (repeat-send self))) ;; try again later
      )))
       
