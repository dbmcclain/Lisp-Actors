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

(defvar *current-actor* nil)

(defstruct (actor
               (:constructor %make-actor))
  beh)

(defun make-actor (&optional (beh #'funcall))
  (%make-actor :beh beh))

(define-symbol-macro self     *current-actor*)
(define-symbol-macro self-beh (actor-beh self))

;; --------------------------------------
;; A Safe-Beh, as a behavior, is one that does not invoke BECOME,
;; i.e., no state changes in the Actor.  And which causes no damaging
;; side effects.  Examples are LABEL-BEH, TAG-BEH, CONST-BEH, FWD-BEH.
;; Actors with such behavior can support simultaneous parallel
;; execution.

(defclass safe-beh ()
  ()
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance ((obj safe-beh) &key fn &allow-other-keys)
  (clos:set-funcallable-instance-function obj fn))

(defun make-safe-beh (fn)
  (make-instance 'safe-beh
                 :fn fn))

;; --------------------------------------

(defmacro actor (args &body body)
  `(make-actor
    (lambda* ,args
      ,@body)))

#+:LISPWORKS
(editor:setup-indent "actor" 1)

(defmacro actors (bindings &body body)
  `(let ,(mapcar #`(,(car a1) (make-actor)) bindings)
     ,@(mapcar #`(setf (actor-beh ,(car a1)) ,(cadr a1)) bindings)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "actors" 1)

;; --------------------------------------------------------
;; Core SEND to Actors

;; Caution!! It is not safe to enable an Actor if its message queue
;; appears empty. It might still be runinng the last message
;; presented. We need to ensure single-thread semantics for all
;; Actors. That's why there is a busy-bit.
;;
;; That busy bit gets unset only after the Actor has completed message
;; processing and no more messages remain in its message queue.

;; ----------------------------------------------------------------
;; Toplevel Actor / Worker behavior

(defvar *run-thread*    nil)
(defvar *evt-cx*        (mp:make-condition-variable))
(defvar *evt-lock*      (mp:make-lock))
(defvar *evt-queue*     nil)
(defvar *new-beh*       nil)
(defvar *send-evts*     nil)
(defvar *whole-message* nil)

(declaim (inline whole-message))

(defun whole-message ()
  *whole-message*)

(defun run-actors ()
  (loop
   (let ((evt (prog ()
                again
                (mp:with-lock (*evt-lock*)
                  (cond (*evt-queue*
                         (multiple-value-bind (evt new-queue)
                             (finger-tree:popq *evt-queue*)
                           (setf *evt-queue* new-queue)
                           (return evt)))
                        (t
                         (mp:condition-variable-wait *evt-cx* *evt-lock*)
                         (go again))
                        )))
              ))
     (destructuring-bind (*current-actor* . *whole-message*) evt
       (let ((*new-beh*   self-beh)
             (*send-evts* nil))
         (with-simple-restart (abort "Process next event")
           (apply *new-beh* *whole-message*)
           (effect-changes))
         ))
     )))

(defun irq (actor &rest msg)
  (mp:with-lock (*evt-lock*)
    (setf *evt-queue*
          (finger-tree:pushq *evt-queue* (cons actor msg)))
    (mp:condition-variable-signal *evt-cx*)))
             
(defun start-actors-system ()
  (unless *run-thread*
    (setf *run-thread*
          (mp:process-run-function "Actor Thread"
                                   ()
                                   #'(lambda ()
                                       (unwind-protect
                                           (run-actors)
                                         (setf *run-thread* nil)))
                                   ))))
(defun kill-executives ()
  (when *run-thread*
    (mp:process-terminate *run-thread* nil)))

#|
(kill-executives)
(start-actors-system)
 |#

;; -----------------------------------------------
;; Since these methods are called against (CURRENT-ACTOR) they can
;; only be called from within a currently active Actor.

(defun become (new-fn)
  ;; change behavior, returning old. Only meaningful if an Actor calls
  ;; this.
  (when self
    (setf *new-beh* new-fn)))

(defmacro send* (&rest msg)
  ;; to be used when final arg is a list
  `(apply #'send ,@msg))

(defmethod send ((actor actor) &rest msg)
  ;; (prin1 (cons actor msg))
  ;; (terpri)
  (if self
      (setf *send-evts*
            (finger-tree:addq *send-evts* (cons actor msg)))
    ;; else
    (mp:with-lock (*evt-lock*)
      (setf *evt-queue*
            (finger-tree:addq *evt-queue* (cons actor msg)))
      (mp:condition-variable-signal *evt-cx*))
    ))

(defun repeat-send (dest)
  (when self
    (send* dest *whole-message*)))

(defun effect-changes ()
  (setf self-beh *new-beh*)
  (mp:with-lock (*evt-lock*)
    (setf *evt-queue*
          (finger-tree:join *evt-queue* *send-evts*))
    ))

;; ----------------------------------------------
;; WORKER is just a function that executes on an Executive pool
;; thread.
;;
;; WORKER is intended as a lightweight vehicle to perform non
;; Actor-centric duties, e.g., blocking I/O. It will see a null value
;; from (CURRENT-ACTOR), just like non-Actor code running on any other
;; thread. It takes advantage of the Executive thread pool to launch
;; faster than fully constructing a thread to run them.

(defun spawn-worker (fn &rest args)
  (apply #'mp:funcall-async fn args))

(defmacro with-worker (bindings &body body)
  (let ((args (mapcar #'first bindings))
        (vals (mapcar #'second bindings)))
    `(spawn-worker (lambda ,args
                     ,@body) ,@vals)
    ))

#+:LISPWORKS
(editor:setup-indent "with-worker" 1)

;; ------------------------------------------
