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

(defun create (&optional (fn #'do-nothing))
  (%create (screened-beh fn)))

;; --------------------------------------

(defun sink-beh ()
  #'do-nothing)

(def-actor sink)

(defun is-pure-sink? (actor)
  ;; used by networking code to avoid sending useless data
  (or (not (actor-p actor))
      (eql (actor-beh actor) #'do-nothing)))

(defun become-sink ()
  (become (sink-beh)))

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior
(defvar *current-message*  nil) ;; Current Event Message

(define-symbol-macro self         *current-actor*)
(define-symbol-macro self-beh     *current-behavior*)
(define-symbol-macro self-msg     *current-message*)

;; -------------------------------------------------
;; Message Frames - submitted to the event queue. These carry their
;; own link pointer to obviate consing on the event queue.
;;
;; Minimal garbage generation since most Actors send at least one
;; message. We re-use the last message frame received. If no messages
;; are sent by the Actor, then the message frame becomes garbage.

(defstruct (msg
            (:constructor msg (actor args &key link)))
  link
  (actor (create)      :type actor)
  (args  nil           :type list))

(mpc:defglobal *central-mail*  (mpc:make-mailbox :lock-name "Central Mail"))

(defun send-to-pool (actor &rest msg)
  ;; the default SEND for foreign threads
  #F
  (mpc:mailbox-send *central-mail* (msg (the actor actor) msg)))

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

(mpc:defglobal *nbr-pool*  8 )

(defvar *send*
  (progn
    (defun startup-send (actor &rest msg)
      ;; the boot version of SEND
      (setf *central-mail* (mpc:make-mailbox :lock-name "Central Mail")
            *send*         #'send-to-pool)
      (restart-actors-system *nbr-pool*)
      (send* actor msg))
    #'startup-send))

(defun send (actor &rest msg)
  #F
  (when (actor-p actor)
    (apply *send* actor msg)))
  
(defun repeat-send (actor)
  (send* actor self-msg))

(defun send-combined-msg (cust msg1 msg2)
  (multiple-value-call #'send cust (values-list msg1) (values-list msg2)))

;; ---------------------------------------

(defvar *not-actor*  "Not in an Actor")

(defvar *become*
  (lambda (new-beh)
    (declare (ignore new-beh))
    (error *not-actor*)))

(defun become (new-beh)
  #F
  (check-type new-beh function)
  (funcall *become* new-beh))


(defvar *abort-beh*
  #'do-nothing)

(defun abort-beh ()
  ;; In an Actor, unodes any BECOME and SENDS to this point, but
  ;; allows Actor to exit normally. In contrast to ERROR action which
  ;; aborts all BECOME and SENDs and exits immediately. ABORT-BEH
  ;; allows subsequent SENDs and BECOME to still take effect.
  (funcall *abort-beh*))

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

(defun #1=run-actors (&optional actor &rest message)
  #F
  (let (sends evt pend-beh done period)
    (flet ((%send (actor &rest msg)
             (if evt
                 (setf (msg-link  (the msg evt)) sends
                       (msg-actor (the msg evt)) (the actor actor)
                       (msg-args  (the msg evt)) msg
                       sends      evt
                       evt        nil)
               ;; else
               (setf sends (msg (the actor actor) msg :link sends))))

           (%become (new-beh)
             (setf pend-beh new-beh))

           (%abort-beh ()
             (setf pend-beh self-beh
                   evt      (or evt sends)
                   sends    nil)))

      (declare (dynamic-extent #'%send #'%become #'%abort-beh))

      (when (actor-p actor)
        (let ((me  (create
                    (lambda (&rest msg)
                      (setf done (list msg))
                      (become (sink-beh))))))
          (setf period 1)
          (mpc:mailbox-send *central-mail* (msg actor (cons me message)))
          ))
      
      ;; -------------------------------------------------------
      ;; Think of the *current-x* global vars as dedicated registers
      ;; of a special architecture CPU which uses a FIFO queue for its
      ;; instruction stream, instead of linear memory, and which
      ;; executes breadth-first instead of depth-first. This maximizes
      ;; concurrency.
      ;; -------------------------------------------------------
      (let ((*send*      #'%send)
            (*become*    #'%become)
            (*abort-beh* #'%abort-beh))
        
        (with-simple-restart (abort "Terminate Actor thread")
          (loop
             (with-simple-restart (abort "Handle next event")
               (loop
                  ;; Fetch next event from event queue - ideally, this
                  ;; would be just a handful of simple register/memory
                  ;; moves and direct jump. No call/return needed, and
                  ;; stack useful only for a microcoding assist. Our
                  ;; depth is never more than one Actor at a time,
                  ;; before trampolining back here.
                  (when done
                    (return-from #1# (values-list (car done))))
                  (when (setf evt (mpc:mailbox-read *central-mail* nil period))
                    (tagbody
                     next
                     (um:when-let (next-msgs (msg-link (the msg evt)))
                       (mpc:mailbox-send *central-mail* next-msgs))
                     
                     (let* ((*current-actor*   (msg-actor (the msg evt)))  ;; self
                            (*current-message* (msg-args  (the msg evt)))) ;; self-msg
                       (declare (actor *current-actor*)
                                (list  *current-message*))
                       
                       (tagbody                   
                        retry
                        (setf pend-beh (actor-beh (the actor self))
                              sends    nil)
                        (let ((*current-behavior* pend-beh))  ;; self-beh
                          (declare (function *current-behavior*))
                          ;; ---------------------------------
                          ;; Dispatch to Actor behavior with message args
                          (apply (the function pend-beh) self-msg)
                          (cond ((or (eq self-beh pend-beh)
                                     (%actor-cas self self-beh pend-beh))
                                 (when sends
                                   (cond (done
                                          (mpc:mailbox-send *central-mail* sends))
                                         
                                         ((mpc:mailbox-empty-p *central-mail*)
                                          ;; No messages await, we are front of queue,
                                          ;; so grab first message for ourself.
                                          ;; This is the most common case at runtime,
                                          ;; giving us a dispatch timing of only 46ns on i9 processor.
                                          (setf evt sends)
                                          (go next))
                                         
                                         (t
                                          ;; else - we are not front of queue
                                          ;; enqueue new messages and repeat loop
                                          (mpc:mailbox-send *central-mail* sends))
                                         )))
                                (t
                                 ;; failed on behavior update - try again...
                                 (setf evt (or evt sends)) ;; prep for next SEND, reuse existing msg block
                                 (go retry))
                                )))
                       ))
                    ))
               ))
          )))))

;; ----------------------------------------------------------------
;; Error Handling

(defun do-with-error-response (cust fn fn-err)
  ;; Defined such that we don't lose any debugging context on errors.
  ;;
  ;; Function fn-err takes an error condition as argument and returns
  ;; the message to be sent back in event of error abort.
  ;;
  ;; Function fn is a thunk.
  ;;
  (let (err)
    (restart-case
        (handler-bind ((error (lambda (e)
                                (setf err e))))
          (funcall fn))
      (abort ()
        :report "Handle next event, reporting"
        ;; generalized for use by ERL
        (abort-beh) ;; NOP unless within an Actor
        (apply #'send-to-all (um:mklist cust) (um:mklist (funcall fn-err err)))
        ))
    ))

(defun err-from (e)
  `(:error-from ,self ,e))

(defmacro with-error-response ((cust &optional (fn-err '#'err-from)) &body body)
  ;; Handler-wrapper that guarantees a customized message sent back to
  ;; cust in event of error.
  ;;
  ;; Function fn-err should accept an error condition arg and compute
  ;; a response message.
  ;;
  ;; Cust can be a single customer or a list of customers.
  ;;
  ;; The default fn-err is most useful for Actors guarded by a
  ;; SERIALIZER. Failing to respond to the SERIALIZER-generated
  ;; customer will block all future uses of the SERIALIZER.
  ;;
  ;; A customized (user specified fn-err) version finds most utility
  ;; with services invoked by FORK, where you need to send only a
  ;; single response item to the customer. Failing to respond to the
  ;; FORK-generated customer will tie up the JOIN response forever.
  ;; (In some cases, maybe that is what you want.)
  ;;
  ;; A links Actor, or list of Actors, used as the cust here, allows
  ;; for Erlang-like supervisory Actors to be notified of unexpected
  ;; conditions.
  ;;
  `(do-with-error-response ,cust (lambda () ,@body) ,fn-err))

#+:LISPWORKS
(editor:setup-indent "with-error-response" 1)

;; ---------------------------------------------------

(defmacro def-ser-beh (name args &rest clauses)
  ;; For Actors behind a SERIALIZER, define their behaviors so that,
  ;; in any event, a response is sent to cust. The cust must be the
  ;; first arg of any message. Use ALAMBDA-style handler clauses.
  ;;
  ;; It becomes *Your* responsibilty to eventually respond to cust
  ;; from each of your handler clauses.
  ;;
  (um:with-unique-names (cust msg)
    `(defun ,name ,args
       (lambda (,cust &rest ,msg)
         (with-error-response (,cust)
           (match (cons ,cust ,msg)
             ,@clauses
             (_
              (send ,cust :unhandled-message ,msg))
             ))
         ))
    ))

(defmacro def-beh (name args &rest clauses)
  `(defun ,name ,args
     (alambda
      ,@clauses)))

#+:LISPWORKS
(progn
  (editor:setup-indent "def-ser-beh" 2)
  (editor:setup-indent "def-beh" 2))

;; --------------------------------------
;; Alas, with MPX we still need locks sometimes. You might think that
;; we could use a SERIALIZER here. But that would cover only
;; participating Actors, not foreign threads trying to use the same
;; printer stream...

(defmacro with-printer ((var stream) &body body)
  #+:LISPWORKS
  `(stream:apply-with-output-lock
    (lambda (,var)
      ,@body)
    ,stream)
  #+:SBCL
  `(let ((,var ,stream))
     ,@body))

(def-actor println
  (α msg
    (with-printer (s *standard-output*)
      (format s "~&~{~A~%~^~}" msg))))

(defun do-with-maximum-io-syntax (fn)
  (with-standard-io-syntax
    (let ((*print-radix* t)
          (*print-circle* t)
          (*read-default-float-format* 'double-float))
      (handler-case
          (funcall fn)
        (print-not-readable ()
          (let ((*print-readably* nil))
            (funcall fn)))
        ))))
  
(defmacro with-maximum-io-syntax (&body body)
  `(do-with-maximum-io-syntax (lambda () ,@body)))

(def-actor writeln
  (α msg
    (with-printer (s *standard-output*)
      (with-maximum-io-syntax
       (format s "~&~{~S~%~^~}" msg)))))

(def-actor fmt-println
  (α (fmt-str &rest args)
    (with-printer (s *standard-output*)
      (format s "~&")
      (apply #'format s fmt-str args))
    ))

;; ------------------------------------------------
;; The bridge between imperative code and the Actors world
;;
;; Foretign threads can use ASK to query an Actor that provides a
;; response to a customer. It is superfluous to do so from an Actor.
;;
;; For querying such an Actor, just leave out the customer arg in your
;; message. A local mailbox interposes as the customer. This blocks
;; until a response is received.
#|
(defun mbox-sender-beh (mbox)
  (check-type mbox mpc:mailbox)
  (lambda (&rest ans)
    (mpc:mailbox-send mbox ans)))

(defun mbox-sender (mbox)
  (create (mbox-sender-beh mbox)))

(defun ask (actor &rest msg)
  ;; Actor should expect a cust arg in first position. Here, the
  ;; mailbox.
  (check-type actor actor)
  (unless self
      ;; Counterproductive when called from an Actor, except for
      ;; possible side effects. Should use BETA forms if you want the
      ;; answer.
      (let ((mbox (mpc:make-mailbox)))
        (send* actor (mbox-sender mbox) msg)
        (values-list (mpc:mailbox-read mbox)))
      ))
|#

(defun ask (actor &rest msg)
  (check-type actor actor)
  (apply #'run-actors actor msg))

;; ------------------------------------------------------
;; FN-ACTOR - the most general way of computing something is to send
;; the result of an Actor execution to a customer. That may involve an
;; indefinite number of message sends along with continuation Actors
;; before arriving at the result to send to the customer.
;;
;; But sometimes all we need is a simple direct function call against
;; some args. In order to unify these two situations, we make
;; FN-ACTORs which encapsulate a function call inside of an Actor.
;; 
;; See also: SERVICES.

(defun fn-actor-beh (fn)
  (λ (cust . args)
    (send* cust (multiple-value-list (apply fn args)))))

(defun fn-actor (fn)
  (create (fn-actor-beh fn)))

;; ----------------------------------------
;; We must defer startup until the MP system has been instantiated.

(defun lw-start-actors (&rest _)
  (declare (ignore _))
  (setf *send* #'startup-send)
  (princ "Actors are alive!"))

(defun lw-kill-actors (&rest _)
  (declare (ignore _))
  (kill-actors-system)
  (print "Actors have been shut down."))

#+:LISPWORKS
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
(if (mpc:get-current-process)
    (unless (actors-running-p)
      (lw-start-actors))
  ;; else
  (pushnew '("Start Actors" () lw-start-actors) mpc:*initial-processes*
           :key #'third))
|#
