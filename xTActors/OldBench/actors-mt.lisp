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

(defun α (&optional (beh #'lw:do-nothing))
  (make-actor beh))
  
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

;; --------------------------------------
;; A Par-Behavior is one that does not invoke BECOME, i.e., no state
;; changes in the Actor.  And which causes no damaging side effects.
;; Examples are LABEL-BEH, TAG-BEH, CONST-BEH, FWD-BEH.  Actors with
;; such behavior can support simultaneous parallel execution.

(defclass par-safe-behavior ()  ;; A Typed-Function
  ()
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((obj par-safe-behavior) &key fn &allow-other-keys)
  (clos:set-funcallable-instance-function obj fn))

(defun make-par-safe-behavior (fn)
  (check-type fn function)
  (make-instance 'par-safe-behavior
                 :fn fn))

;; ----------------------------------
;; Events - a reference to an Actor and a reference to message data.

(defstruct evt
  actor
  msg
  next)

;; ---------------------------------------------

(defstruct queue
  hd tl)

(defvar *current-queue*   nil) ;; Current Event Queue

(defun addq (evt)
  (with-accessors ((qhd  queue-hd)
                   (qtl  queue-tl)) (the queue *current-queue*)
    (progn ;; mp:with-interrupts-blocked
      (if qtl
          (setf qtl (setf (evt-next (the evt qtl)) evt))
        (setf qhd (setf qtl evt)))
      )))

#|
(defun addqn (queue evts)
  (with-accessors ((qhd  queue-hd)
                   (qtl  queue-tl)) queue
    (let ((last (um:nlet iter ((hd evts)
                               (tl evts))
                  (if hd
                      (go-iter (evt-next hd) hd)
                    tl))))
    (mp:with-interrupts-blocked
      (if qtl
          (setf (evt-next qtl) evts)
        (setf qhd evts))
      (setf qtl last))
    )))
|#

(defun #1=popq ()
  (with-accessors ((qhd  queue-hd)
                   (qtl  queue-tl)) (the queue *current-queue*)
    (loop
     (progn ;; mp:with-interrupts-blocked
       (let ((evt qhd))
         (when evt
           (unless (setf qhd (evt-next (the evt qhd)))
             (setf qtl nil))
           (return-from #1# evt))))
     (mp:process-wait-local "Waiting for event"
                            (lambda ()
                              qhd))
     )))

;; ----------------------------------
;; SPONSORS -- offer event queues and have associated runtime threads
;; to perform RUN dispatching of Actor events.
;;
;; We have two main Sponsors - a single-threaded one (denoted as :ST
;; or NIL), and a multi-threaded one (denoted as :MT or T). We default
;; to the :MT Sponsor event queue for maximum parallelism.

(defconstant +nbr-threads+  8)

(defstruct (sponsor
            (:constructor %make-sponsor))
  ;; (mbox   (mp:make-mailbox))
  (queue   (make-queue))
  evt-freel
  thread)

(defvar *sponsor*         nil) ;; Single-Threaded
(defvar *current-sponsor* nil)

(defun current-sponsor ()
  (or *current-sponsor*
      *sponsor*))

(defun recycled-evt ()
  (with-accessors ((freel sponsor-evt-freel)) (the sponsor *current-sponsor*)
    (progn ;; mp:with-interrupts-blocked
      (let ((evt freel))
        (when evt
          (setf freel (evt-next (the evt evt))))
        evt))
    ))

(defun discard-evt (evt)
  (with-accessors ((freel sponsor-evt-freel)) (the sponsor *current-sponsor*)
    (progn ;; mp:with-interrupts-blocked
      (setf (evt-next (the evt evt)) freel
            freel          evt))
    ))

(defun create-evt (actor msg)
  #||#
  (let ((evt (recycled-evt)))
    (cond (evt
           (locally (declare (evt evt))
             (setf (evt-actor evt) actor
                   (evt-msg   evt) msg
                   (evt-next  evt) nil)
             evt))
          (t
           (make-evt
            :actor actor
            :msg   msg))
          ))
  #||#
  #|
  (make-evt
   :actor actor
   :msg   msg)
  |#
  )

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *new-beh*       nil)   ;; Staging for BECOME
(defvar *send-evts*     nil)   ;; Staging for SEND
(defvar *whole-message* nil)   ;; Current Event Message
(defvar *current-actor* nil)   ;; Current Actor
(defvar *current-beh*   nil)   ;; Current Behavior

(define-symbol-macro self     *current-actor*)
(define-symbol-macro self-beh *current-beh*)

;; Generic RUN for all threads, across all Sponsors
(defun run-actors (*current-sponsor*)
  (let ((*current-queue* (sponsor-queue *current-sponsor*)))
    (loop
     (let* ((evt (popq))
            (*current-actor* (evt-actor evt))
            (*whole-message* (evt-msg evt))
            (*current-beh*   (actor-beh self)))
       (cond ((and self-beh
                   (or (typep self-beh 'par-safe-behavior)
                       (sys:compare-and-swap (actor-beh self) self-beh nil)))
              ;; (discard-evt evt)
              ;; NIL Beh slot indicates busy Actor
              (let ((*new-beh*     self-beh)
                    (*send-evts*   nil))
                ;; ---------------------------------
                (with-simple-restart (abort "Handle next event")
                  (apply self-beh *whole-message*)
                  
                  ;; Effects Commit...
                  (when *send-evts*
                    (dolist (evt *send-evts*)     ;; staged SENDs
                      (apply #'actor-send evt)))
                  (setf *current-beh* *new-beh*)) ;; staged BECOME
                ;; ---------------------------------                    
                (setf (actor-beh self) *current-beh*))) ;; restore Not-Busy
             
             (t
              ;; else - actor was busy
              (addq evt))
             )))))

#|
;; Generic RUN for all threads, across all Sponsors
(defun run-actors (*current-sponsor*)
  (let (;; (mbox (sponsor-mbox *current-sponsor*))
        (queue (sponsor-queue *current-sponsor*))
        )
    (loop
     (let (;; (evt (mp:mailbox-read mbox))
           (evt (popq queue)))
       (destructuring-bind (*current-actor* . *whole-message*) evt
         (let ((*current-beh* (actor-beh self)))
           (cond ((and self-beh
                       (or (typep self-beh 'par-safe-behavior)
                           (sys:compare-and-swap (actor-beh self) self-beh nil)))
                  
                  ;; NIL Beh slot indicates busy Actor
                  (let ((*new-beh*     self-beh)
                        (*send-evts*   nil))
                    ;; ---------------------------------
                    (with-simple-restart (abort "Handle next event")
                      (apply self-beh *whole-message*)
                      
                      ;; Effects Commit...
                      (when *send-evts*
                        (dolist (evt *send-evts*)     ;; staged SENDs
                          (apply #'mp:mailbox-send evt)))
                      (setf *current-beh* *new-beh*)) ;; staged BECOME
                    ;; ---------------------------------                    
                    (setf (actor-beh self) *current-beh*))) ;; restore Not-Busy
                 
                 (t
                  ;; else - actor was busy
                  (mp:mailbox-send mbox evt)
                  ))))))))
|#

;; ----------------------------------------------------------

(defun start-actors-system ()
  (unless *sponsor*
    (setf *sponsor* (make-sponsor "Actor Thread"))))

(defun kill-executives ()
  (when *sponsor*
    (kill-sponsor (shiftf *sponsor* nil))))

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

#|
(kill-executives)
(start-actors-system)
 |#

;; -----------------------------------------------
;; SEND/BECOME - Since these methods are called against SELF they can
;; only be called from within a currently active Actor.
;;
;; SEND and BECOME are transactionally staged, and will commit *ONLY*
;; upon error free completion of the Actor body code. So if you need
;; them to take effect, even as you call potentially unsafe functions,
;; then surround your function calls with HANDLER-CASE, HANDLER-BIND,
;; or IGNORE-ERRORS.

#|
(defun evt-mbox ()
  (sponsor-mbox *current-sponsor*))
|#

(defun become (new-fn)
  ;; Change behavior/state. Only meaningful if an Actor calls
  ;; this.
  (check-type new-fn function)
  ;; BECOME is staged.
  (when self
    (setf *new-beh* new-fn)))

(defmacro send* (&rest msg)
  ;; to be used when final arg is a list
  ;; saves typing APPLY #'SEND, analogous to LIST*
  `(apply #'send ,@msg))

(defmethod send ((actor actor) &rest msg)
  (if self
      (push (list *current-sponsor*
                  (create-evt actor msg))
            *send-evts*)
    ;; else
    (let* ((*current-sponsor*  (or *current-sponsor*
                                   *sponsor*))
           (thread (sponsor-thread *current-sponsor*)))
      (mp:process-interrupt thread
                            (lambda ()
                              (addq (create-evt actor msg))))
      (mp:process-poke thread))
    ))

(defun actor-send (spon evt)
  (declare (sponsor spon))
  (if (eq spon *current-sponsor*)
      (addq evt)
    (let ((thread (sponsor-thread spon)))
      (mp:process-interrupt thread #'addq evt)
      (mp:process-poke thread))
    ))

#|
(defmethod send ((actor actor) &rest msg)
  (cond (self
         ;; Actor SENDs are staged.
         (push (list (evt-mbox) (cons actor msg)) *send-evts*))
        (t
         ;; Non-Actor SENDs take effect immediately.
         (let ((*current-sponsor* (current-sponsor)))
           (mp:mailbox-send (evt-mbox) (cons actor msg))))
        ))
|#

(defun repeat-send (dest)
  ;; Send the current event message to another Actor
  (when self
    (send* dest *whole-message*)))

;; ----------------------------------------------------------------
;; Using Sponsors

(defmacro with-sponsor (sponsor &body body)
  `(let ((*current-sponsor* ,sponsor))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-sponsor" 1)

(defun sendx (sponsor actor &rest msg)
  ;; Cross-sponsor SEND
  (with-sponsor sponsor
    (send* actor msg)))

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
  `(let ((@bind  (α (lambda* ,args ,@body))))
     ,form))

#+:LISPWORKS
(editor:indent-like "@bind" 'destructuring-bind)

(defmacro @values (&rest retvals)
  `(send @bind ,@retvals))
     
;; ----------------------------------------------

