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
  busy
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

(defstruct sponsor
  (mbox  (mp:make-mailbox))
  threads)

(defvar *sponsor-st*
  (make-sponsor))

(defvar *sponsor-mt*
  (make-sponsor))

(defvar *evt-mbox*      (sponsor-mbox *sponsor-mt*))
(defvar *new-beh*       nil)
(defvar *send-evts*     nil)
(defvar *whole-message* nil)

(declaim (inline whole-message))

(defun whole-message ()
  *whole-message*)

(defun send-msgs (msgs)
  (dolist (evt msgs)
    (mp:mailbox-send *evt-mbox* evt)))

(defun run-actors (sponsor)
  (let ((*evt-mbox*  (sponsor-mbox sponsor)))
    (loop
     (let ((evt (mp:mailbox-read *evt-mbox*)))
       (destructuring-bind (*current-actor* . *whole-message*) evt
         (let ((*new-beh*  self-beh))
           (cond ((or (typep *new-beh* 'safe-beh)
                      (sys:compare-and-swap (actor-busy *current-actor*) nil t))
                  (let ((*send-evts*   nil)
                      ;; (*pref-msgs*   nil)
                      )
                    (with-simple-restart (abort "Handle next event")
                      (apply *new-beh* *whole-message*)
                      ;; effects commit...
                      (when *send-evts*
                        (send-msgs *send-evts*))
                      (setf self-beh *new-beh*))
                    ;; ----------------------------
                    ;; for all executing Actors, failed or not
                    (setf (actor-busy self) nil)
                    ;; (when *pref-msgs*
                    ;;  (send-msgs *pref-msgs*))
                    ))
                 (t
                  ;; else - actor was busy
                  (mp:mailbox-send *evt-mbox* evt))
                 )))))))

(defconstant +nbr-threads+  8)

(defun start-actors-system ()
  (unless (sponsor-threads *sponsor-mt*)
    (dotimes (ix +nbr-threads+)
      (push (mp:process-run-function (format nil "Actor Thread ~D" ix)
                                     ()
                                     #'run-actors *sponsor-mt*)
            (sponsor-threads *sponsor-mt*)))
    ;; A single thread for code that must be run that way
    (push (mp:process-run-function "ActorST Thread"
                                   ()
                                   #'run-actors *sponsor-st*)
          (sponsor-threads *sponsor-st*))
    ))
#|
(start-actors-system)
(kill-executives)
 |#

(defun kill-executives ()
  (dolist (thr (shiftf (sponsor-threads *sponsor-mt*) nil))
    (mp:process-terminate thr))
  (dolist (thr (shiftf (sponsor-threads *sponsor-st*) nil))
    (mp:process-terminate thr)))

;; -----------------------------------------------
;; Since these methods are called against SELF they can
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
  (cond (self
         ;; (setf *send-evts* (finger-tree:addq *send-evts* (cons actor msg)))
         (push (cons actor msg) *send-evts*)
         )
        (t
         (mp:mailbox-send *evt-mbox* (cons actor msg)))
        ))

(defun repeat-send (dest)
  (when self
    (send* dest *whole-message*)))

;; ----------------------------------------------------------------
;; Using Sponsors

(defun effective-sponsor (spon)
  (cond ((sponsor-p spon) spon)
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
  `(let ((*evt-mbox* (sponsor-mbox (effective-sponsor ,sponsor))))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-sponsor" 1)

(defun sendx (sponsor actor &rest msg)
  ;; cross-sponsor SEND
  (with-sponsor sponsor
    (send* actor msg)))

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
