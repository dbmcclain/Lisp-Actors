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

(defun make-actor (&optional (beh #'funcall))
  (check-type beh function)
  (%make-actor :beh beh))

(defun α (&optional (beh #'funcall))
  (make-actor beh))
  
;; --------------------------------------
;; A Safe-Beh, as a behavior, is one that does not invoke BECOME,
;; i.e., no state changes in the Actor.  And which causes no damaging
;; side effects.  Examples are LABEL-BEH, TAG-BEH, CONST-BEH, FWD-BEH.
;; Actors with such behavior can support simultaneous parallel
;; execution.

(defclass safe-beh ()  ;; A Typed-Function
  ()
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance ((obj safe-beh) &key fn &allow-other-keys)
  (clos:set-funcallable-instance-function obj fn))

(defun make-safe-beh (fn)
  (check-type fn function)
  (make-instance 'safe-beh
                 :fn fn))

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
;; references each other in their initial state

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

(defconstant +nbr-threads+  8)

(defstruct (sponsor
            (:constructor %make-sponsor))
  mboxes
  threads
  ctr)

(defvar *sponsor-st* nil) ;; Single-Threaded
(defvar *sponsor-mt* nil) ;; Multiple-Threaded

;; --------------------------------------------------------
;; Core RUN for Actors

(defvar *current-sponsor* nil)

(defun evt-mbox ()
  (let ((mboxes (sponsor-mboxes *current-sponsor*)))
    (with-accessors ((ctr  sponsor-ctr)) *current-sponsor*
      (let ((ix (logand (the fixnum (1- (the fixnum (length mboxes))))
                        (the fixnum (sys:atomic-fixnum-incf (the fixnum ctr))))))
        (declare (fixnum ix))
        (svref mboxes ix)))))
        
;; Per-Thread for Activated Actor
(defvar *new-beh*       nil)   ;; Staging for BECOME
(defvar *send-evts*     nil)   ;; Staging for SEND
(defvar *whole-message* nil)   ;; Current Event Message
(defvar *current-actor* nil)   ;; Current Actor
(defvar *current-beh*   nil)   ;; Current Behavior

(define-symbol-macro self     *current-actor*)
(define-symbol-macro self-beh *current-beh*)

;; Generic RUN for all threads, across all Sponsors
(defun run-actors (*current-sponsor* mbox)
  (loop
   (let ((evt (mp:mailbox-read mbox)))
     (destructuring-bind (*current-actor* . *whole-message*) evt
       (let ((*current-beh* (actor-beh self)))
         (cond ((and self-beh
                     (or (typep self-beh 'safe-beh)
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
                (mp:mailbox-send mbox evt))
               ))))))

;; ----------------------------------------------------------

(defun start-actors-system ()
  (unless *sponsor-mt*
    (setf *sponsor-mt* (make-sponsor +nbr-threads+ "ActorMT Thread ~D")
          *sponsor-st* (make-sponsor 1             "ActorST Thread"))))

(defun kill-executives ()
  (when *sponsor-mt*
    (kill-sponsor (shiftf *sponsor-mt* nil))
    (kill-sponsor (shiftf *sponsor-st* nil))))

;; ----------------------------------------------------------

(defun make-sponsor (nbr-threads title)
  (let ((new-sponsor (%make-sponsor
                      :ctr     0
                      :threads (make-array nbr-threads)
                      :mboxes  (make-array nbr-threads))))
    (dotimes (ix nbr-threads)
      (let ((mbox (mp:make-mailbox)))
        (setf (aref (sponsor-mboxes  new-sponsor) ix) mbox
              (aref (sponsor-threads new-sponsor) ix)
              (mp:process-run-function (if (> nbr-threads 1)
                                           (format nil title (1+ ix))
                                         title)
                                       ()
                                       #'run-actors new-sponsor mbox)
              )))
    new-sponsor))

(defun kill-sponsor (sponsor)
  (loop for thread across (sponsor-threads sponsor) do
        (mp:process-terminate thread)))

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
  (cond (self
         ;; Actor SENDs are staged.
         (push (list (evt-mbox) (cons actor msg)) *send-evts*))
        (t
         ;; Non-Actor SENDs take effect immediately.
         (let ((*current-sponsor* (or *current-sponsor*
                                      *sponsor-mt*)))
           (mp:mailbox-send (evt-mbox) (cons actor msg))))
        ))

(defun repeat-send (dest)
  ;; Send the current event message to another Actor
  (when self
    (send* dest *whole-message*)))

;; ----------------------------------------------------------------
;; Using Sponsors

(defun effective-sponsor (spon)
  (cond ((sponsor-p spon)
         spon)
        
        ((or (eq t spon)
             (eq :mt spon))
         *sponsor-mt*)

        ((or (null spon)
             (eq :st spon))
         *sponsor-st*)

        (t
         (error "Not a Sponsor: ~S" spon))
        ))

(defmacro with-sponsor (sponsor &body body)
  `(let ((*current-sponsor* (effective-sponsor ,sponsor)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-sponsor" 1)

(defun sendx (sponsor actor &rest msg)
  ;; Cross-sponsor SEND
  (with-sponsor sponsor
    (send* actor msg)))

(defmacro with-single-thread (&body body)
  (lw:with-unique-names (k-cont)
    `(let ((,k-cont (actor ()
                      ,@body)))
       (sendx nil ,k-cont))
    ))

(defmacro with-multiple-threads (&body body)
  (lw:with-unique-names (k-cont)
    `(let ((,k-cont (actor ()
                      ,@body)))
       (sendx t ,k-cont))
    ))

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
