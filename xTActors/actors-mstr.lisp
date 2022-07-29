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


(in-package #:com.ral.actors.base)

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
               (:constructor %create (beh)))
  (beh #'lw:do-nothing :type function))

(defun need-type (x tp)
  (unless (typep x tp)
    (error "type ~A expected" tp)))

(defun create (&optional (fn #'lw:do-nothing))
  (need-type fn 'function)
  (%create fn))

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *whole-message*    nil) ;; Current Event Message
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior

(define-symbol-macro self         *current-actor*)
(define-symbol-macro self-beh     *current-behavior*)
(define-symbol-macro self-msg     *whole-message*)

;; -------------------------------------------------
;; Message Frames - submitted to the event queue. These carry their
;; own link pointer to obviate consing on the event queue.
;;
;; Minimal garbage generation since most Actors send at least one
;; message. We re-use the last message frame received. If no messages
;; are sent by the Actor, then the message frame becomes garbage.

(defstruct (msg
            (:constructor msg (actor args &optional link)))
  link
  (actor (create) :type actor)
  (args  nil      :type list))

(hcl:defglobal-variable *central-mail*  (mp:make-mailbox))

;; -----------------------------------------------
;; SEND/BECOME
;;
;; SEND can only be called on an Actor. BECOME can only be called from
;; within an Actor.
;;
;; SEND and BECOME are transactionally staged, and will commit *ONLY*
;; upon error free completion of the Actor body code.
;;
;; So if you need them to take effect, even as you call potentially
;; unsafe functions, then surround your function calls with
;; HANDLER-CASE, HANDLER-BIND, or IGNORE-ERRORS. Otherwise, an error
;; will make it seem that the message causing the error was never
;; delivered.

(hcl:defglobal-variable *nbr-pool*    8)
(hcl:defglobal-variable *evt-threads* nil)

(defvar *send* nil)

;; ---------------------------------

(defun send (actor &rest msg)
  #F
  (when (actor-p actor)
    (apply *send* actor msg)))
  
(defmacro send* (actor &rest msg)
  `(apply #'send ,actor ,@msg))

(defun repeat-send (actor)
  (send* actor self-msg))

(defun send-combined-msg (cust msg1 msg2)
  (multiple-value-call #'send cust (values-list msg1) (values-list msg2)))

;; ---------------------------------

(defun send-to-pool (actor &rest msg)
  ;; the default SEND for foreign threads
  #F
  (mp:mailbox-send *central-mail* (msg (the actor actor) msg)))

(defun startup-send (actor &rest msg)
  ;; the boot version of SEND
  (unless *evt-threads*
    (restart-actors-system))
  (setf *send* #'send-to-pool)
  (send* actor msg))

(unless *send*
  (setf *send* #'startup-send))

;; ---------------------------------------

(defvar *become*
  (lambda (new-beh)
    (declare (ignore new-beh))
    (error "not in an Actor")))

(defun become (new-beh)
  #F
  (need-type new-beh 'function)
  (funcall *become* new-beh))

;; -----------------------------------------------------------------
;; Generic RUN for all threads
;;
;; SENDs and BECOME are staged for commit.
;;
;; Actors are now completely thread-safe, FPL pure, SENDs and BECOMEs
;; are staged for commit or rollback. Actors can run completely in
;; parallel among different threads. If BECOME cannot commit, the
;; Actor is retried after rolling back the BECOME and SENDs. This is
;; maximum parallelism.
;;
;; NOTE on SEND Ordering: Since all SENDs are staged for commit upon
;; successful return from Actors, there is no logical distinction
;; between when each of them is sent, when there were more than one
;; arising from the Actor execution. They are all sent logically at
;; once - even though there may be some underyling ordering in the
;; event queue.
;;
;; You should not depend on any particular ordering of messages,
;; except that message sent from an earlier Actor activation will
;; appear in the event queue in front of messages sent by a later
;; Actor activation. The event queue is a FIFO queue.

(defun run-actors ()
  #F
  (let (sends evt pend-beh)
    (flet ((%send (actor &rest msg)
             (if evt
                 (setf (msg-link  (the msg evt)) sends
                       (msg-actor (the msg evt)) (the actor actor)
                       (msg-args  (the msg evt)) msg
                       sends      evt
                       evt        nil)
               ;; else
               (setf sends (msg (the actor actor) msg sends))))

           (%become (new-beh)
             (setf pend-beh new-beh)))

      (declare (dynamic-extent #'%send #'%become))
      
      ;; -------------------------------------------------------
      ;; Think of these global vars as dedicated registers of a
      ;; special architecture CPU which uses a FIFO queue for its
      ;; instruction stream, instead of linear memory, and which
      ;; executes breadth-first instead of depth-first. This maximizes
      ;; concurrency.
      (let* ((*current-actor*    nil)
             (*whole-message*    nil)
             (*current-behavior* nil)
             (*send*             #'%send)
             (*become*           #'%become))
        
        (loop
           (with-simple-restart (abort "Handle next event")
             (loop
                ;; Fetch next event from event queue - ideally, this
                ;; would be just a handful of simple register/memory
                ;; moves and direct jump. No call/return needed, and
                ;; stack useful only for a microcoding assist. Our
                ;; depth is never more than one Actor at a time,
                ;; before trampolining back here.
                (setf evt (mp:mailbox-read *central-mail*))
                (tagbody
                 next
                 (um:when-let (next-msgs (msg-link (the msg evt)))
                   (mp:mailbox-send *central-mail* next-msgs))
                 
                 (setf self     (msg-actor (the msg evt))
                       self-msg (msg-args (the msg evt)))
                 
                 retry
                 (setf pend-beh (actor-beh (the actor self))
                       self-beh pend-beh
                       sends    nil)
                 ;; ---------------------------------
                 ;; Dispatch to Actor behavior with message args
                 (apply (the function pend-beh) self-msg)
                 (cond ((or (eq self-beh pend-beh)
                            (sys:compare-and-swap (actor-beh (the actor self)) self-beh pend-beh))
                        (when sends
                          (cond ((mp:mailbox-empty-p *central-mail*)
                                 ;; No messages await, we are front of queue,
                                 ;; so grab first message for ourself.
                                 ;; This is the most common case at runtime,
                                 ;; giving us a dispatch timing of only 46ns on i9 processor.
                                 (setf evt sends)
                                 (go next))
                                
                                (t
                                 ;; else - we are not front of queue
                                 ;; enqueue new messages and repeat loop
                                 (mp:mailbox-send *central-mail* sends))
                                )))
                       (t
                        ;; failed on behavior update - try again...
                        (setf evt (or evt sends)) ;; prep for next SEND, reuse existing msg block
                        (go retry))
                       )))
             ))
        ))))

;; ---------------------------------------------------

(defun is-pure-sink? (actor)
  ;; used by networking code to avoid sending useless data
  (or (null actor)
      (eq (actor-beh actor) #'lw:do-nothing)))

;; ----------------------------------------------------------------

(defun gen-dispatch (n)
  (push (mp:process-run-function (format nil "Actor Thread #~D" n)
                                 ()
                                 'run-actors)
        *evt-threads*))
  
(defun restart-actors-system ()
  (loop for ix from 1 to *nbr-pool* do
          (gen-dispatch ix)))

(defun kill-actors-system ()
  (map nil #'mp:process-terminate (shiftf *evt-threads* nil)))

(defun add-executives (n)
  (let ((ctr (length *evt-threads*)))
    (loop repeat n do
            (gen-dispatch (incf ctr)))
    ))
#|
(kill-actors-system)
(restart-actors-system)
 |#

;; --------------------------------------

(defun sink-beh ()
  #'lw:do-nothing)

(defactor sink
  (sink-beh))

;; --------------------------------------

;; alas, with MPX we still need locks sometimes
(defmacro with-printer ((var stream) &body body)
  `(stream:apply-with-output-lock
    (lambda (,var)
      ,@body)
    ,stream))

(defactor println
  (λ msg
    (with-printer (s *standard-output*)
      (format s "~&~{~A~%~^~}" msg))))

(defactor writeln
  (λ msg
    (with-printer (s *standard-output*)
      (format s "~&~{~S~%~^~}" msg))))

;; ------------------------------------------------
;; The bridge between imperative code and the Actors world

(defun mbox-sender-beh (mbox)
  (need-type mbox 'mp:mailbox)
  (lambda (&rest ans)
    (mp:mailbox-send mbox ans)))

(defun mbox-sender (mbox)
  (create (mbox-sender-beh mbox)))

(defun ask (actor &rest msg)
  ;; Actor should expect a cust arg in first position. Here, the
  ;; mailbox.
  (need-type actor 'actor)
  (unless self
      ;; Counterproductive when called from an Actor, except for
      ;; possible side effects. Should use BETA forms if you want the
      ;; answer.
      (let ((mbox (mp:make-mailbox)))
        (send* actor (mbox-sender mbox) msg)
        (values-list (mp:mailbox-read mbox)))
      ))

;; ------------------------------------------------------
;; FN-ACTOR - the most general way of computing something is to send
;; the result of an Actor execution to a customer. That may involve an
;; indefinite number of message sends along with continuation Actors
;; before arriving at the result to send to the customer.
;;
;; But sometimes all we need is a simple direct function call against
;; some args. In order to unify these two situations, we make
;; FN-ACTORs which encapsulate a function call inside of an Actor.

(defun fn-actor-beh (fn)
  (λ (cust . args)
    (send* cust (multiple-value-list (apply fn args)))))

(defun fn-actor (fn)
  (create (fn-actor-beh fn)))

;; ----------------------------------------
;; We must defer startup until the MP system has been instantiated.

(defun lw-start-actors (&rest _)
  (declare (ignore _))
  (restart-actors-system)
  (princ "Actors are alive!"))

(defun lw-kill-actors (&rest _)
  (declare (ignore _))
  (kill-actors-system)
  (print "Actors have been shut down."))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor System"
                    'lw-start-actors
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor System"
                    'lw-kill-actors)

  (lw:define-action "Save Session After"
                    "Restart Actor System"
                    'lw-start-actors)
  )

#| ;; for manual loading mode...
(unless *evt-threads*
  (if (mp:get-current-process)
      (lw-start-actors)
    ;; else
    (pushnew '("Start Actors" () lw-start-actors) mp:*initial-processes*
             :key #'third)))

|#