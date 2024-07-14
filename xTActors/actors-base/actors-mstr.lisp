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

;; --------------------------------------

(deflex +timed-out+ (make-condition 'timeout))

(defun sink-beh ()
  #'do-nothing)

(deflex sink
  (create (sink-beh)))

(defgeneric is-pure-sink? (ac)
  ;; used by networking code to avoid sending useless data
  (:method ((ac actor))
   (eql (actor-beh ac) #'do-nothing))
  (:method ((ac function))
   (eql ac #'do-nothing))
  (:method (ac)
   t) )

(defun become-sink ()
  (become (sink-beh)))

;; --------------------------------------------------------
;; Core RUN for Actors

;; -------------------------------------------------
;; Message Frames - submitted to the event queue. These carry their
;; own link pointer to obviate consing on the event queue.

(defstruct (msg
            (:constructor msg (actor args &key link)))
  link
  id
  (parent              *current-message-frame*)
  (actor (create)      :type actor)
  (args  nil           :type list))

(defun send-to-pool (actor &rest msg)
  ;; the default SEND for foreign (non-Actor) threads
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

(defvar *send-lock*  (mpc:make-lock))
(defvar *send*       nil)

(defun get-send-hook ()
  (sys-unique *send* *send-lock*
              (prog1
                  (setf *central-mail* (mpc:make-mailbox :lock-name "Central Mail")
                        *send*         #'send-to-pool)
                (restart-actors-system *nbr-pool*))
              ))

(defmethod send ((target actor) &rest msg)
  (apply (get-send-hook) target msg))

(defmethod send ((target function) &rest msg) ;; is this useful?
  ;; execution staged for commit
  (apply #'send (create target) msg))

(defmethod send (target &rest msg)) ;; NOP
   
(defun send* (&rest args)
  ;; when last arg is a list that you want destructed
  (apply #'send (apply #'list* args)))

(defun repeat-send (target)
  (send* target self-msg))

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
  (funcall *become* (screened-beh new-beh)))

;; -----------------------------------
;; In an Actor, (ABORT-BEH) undoes any BECOME and SENDS to this point,
;; but allows Actor to exit normally. In contrast to ERROR action
;; which aborts all BECOME and SENDs and exits immediately. ABORT-BEH
;; allows subsequent SENDs and BECOME to still take effect.

(defvar *abort-beh*
  #'do-nothing)

(defun abort-beh ()
  (funcall *abort-beh*))

;; -----------------------------------------------------------------
;; Generic RUN for all threads
;;
;; SENDs and BECOME are staged for commit.
;;
;; Actors are now completely thread-safe, FPL pure, SENDs and BECOMEs
;; are staged for commit or rollback. Actors can run completely in
;; parallel among different matchine threads. If BECOME cannot commit,
;; the Actor is retried after rolling back the BECOME and SENDs. This
;; is maximum parallelism.
;;
;; NOTE on SEND Ordering: Since all SENDs are staged for commit upon
;; successful return from Actors, there is no logical distinction
;; between when each of them is sent, if there were more than one
;; arising from the Actor execution. They are all sent logically at
;; once - even though there may, or may not. be some underyling
;; ordering in the event queue.
;;
;; You should not depend on any particular ordering of messages,
;; except that message sent from an earlier Actor activation will
;; appear in the event queue in front of messages sent by a later
;; Actor activation. The event queue is a FIFO queue.

(defun #1=run-actors (&optional (actor nil actor-provided-p) &rest message)
  #F
  (let (sends evt pend-beh done timeout)
    (flet ((%send (actor &rest msg)
             (setf sends (msg (the actor actor) msg :link sends)))

           #| ;; this re-use interferes with message auditing
           (%send (actor &rest msg)
             (if evt
                 (setf (msg-link  (the msg evt)) sends
                       (msg-actor (the msg evt)) (the actor actor)
                       (msg-args  (the msg evt)) msg
                       sends      evt
                       evt        nil)
               ;; else
               (setf sends (msg (the actor actor) msg :link sends))))
           |#
           
           (%become (new-beh)
             (setf pend-beh new-beh))

           (%abort-beh ()
             (setf pend-beh self-beh
                   ;; evt      (or evt sends)  ;; interferes with msg auditing
                   sends    nil))

           (dispatch ()
             ;; -------------------------------------------------------
             ;; Think of the *current-x* global vars as dedicated registers
             ;; of a special architecture CPU which uses a FIFO queue for its
             ;; instruction stream, instead of linear memory, and which
             ;; executes breadth-first instead of depth-first. This maximizes
             ;; concurrency.
             ;; -------------------------------------------------------
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
                       (return-from #1# (values (car done) t)))
                     (when (setf evt (mpc:mailbox-read *central-mail* nil timeout))
                       (tagbody
                        next
                        (um:when-let (next-msgs (msg-link (the msg evt)))
                          (mpc:mailbox-send *central-mail* next-msgs))
                        
                        (let* ((*current-message-frame*      (and (msg-parent (the msg evt)) evt))
                               (*current-actor*   (msg-actor (the msg evt)))  ;; self
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
                             (cond ((or (eq self-beh pend-beh) ;; no BECOME
                                        (%actor-cas self self-beh pend-beh)) ;; effective BECOME
                                    (when sends
                                      (cond ((or (mpc:mailbox-not-empty-p *central-mail*)
                                                 done)
                                             ;; messages await or we are finished
                                             ;; so just add our sends to the queue
                                             ;; and repeat loop
                                             (mpc:mailbox-send *central-mail* sends))
                                            
                                            (t
                                             ;; No messages await, we are front of queue,
                                             ;; so grab first message for ourself.
                                             ;; This is the most common case at runtime,
                                             ;; giving us a dispatch timing of only 46ns on i9 processor.
                                             (setf evt sends)
                                             (go next))
                                            )))
                                   (t
                                    ;; failed on behavior update - try again...
                                    
                                    ;; -- iterferes with msg auditing --
                                    ;; (setf evt (or evt sends)) ;; prep for next SEND, reuse existing msg block

                                    (go retry))
                                   )))
                          ))
                       )))
                )))
      (declare (dynamic-extent #'%send #'%become #'%abort-beh #'dispatch))
      (cond
       (actor-provided-p
        ;; are we an ASK?
        (when (actor-p actor)
          (let ((me  (once
                      (create
                       (lambda* msg
                         (setf done (list msg))))
                      )))
            (setf timeout +ASK-TIMEOUT+)
            (send-after *timeout* me +timed-out+)
            (apply #'send-to-pool actor me message)
            (let ((*send*      #'%send)
                  (*become*    #'%become)
                  (*abort-beh* #'%abort-beh))
              (with-simple-restart (abort "Terminate ASK")
                (dispatch))
              (when done
                (values (car done) t))))
          ))
       
       (t  ;; else - we are normal Dispatch thread
           (let ((*send*      #'%send)
                 (*become*    #'%become)
                 (*abort-beh* #'%abort-beh))
             (with-simple-restart (abort "Terminate Actor thread")
               (dispatch))
             ))
       ))))

;; ----------------------------------------------------------------
;; Error Handling

(define-condition referred-error (error)
  ((from  :reader referred-error-from
          :initarg :from)
   (err   :reader referred-error-err
          :initarg :err))
  (:report (lambda (c stream)
             (format stream
                     "Referred Error from: ~A err: ~A"
                     (referred-error-from c)
                     (referred-error-err c)))
   ))

(defun err-from (e)
  (make-condition 'referred-error
                  :from self
                  :err  e))

(defun* check-for-errors ((&optional x &rest args))
  ;; Error replies for ASK are single element lists containing an
  ;; error Condition object.
  (when (and (null args)
             (typep x 'error))
    (error x)))

(defun err-chk (cust)
  ;; like FWD, but checks for error return
  (create
   (lambda (&rest msg)
     (check-for-errors msg)
     (send* cust msg))))

;; --------------------------------------
;; Alas, with MPX we still need locks sometimes. You might think that
;; we could use a SERIALIZER here. But that would cover only
;; participating Actors, not foreign threads trying to use the same
;; printer stream...

#+:SBCL
(defvar *out-stream-locks-lock*  (mpc:make-lock))

#+:SBCL
(defvar *out-stream-locks*
  #+:LISPWORKS
  (make-hash-table :weak-kind :key)
  #+:SBCL
  (make-hash-table :weakness :key))

#+:SBCL
(defun do-with-printer (stream fn)
  (let ((lock  (sys-unique (gethash stream *out-stream-locks*)
                           *out-stream-locks-lock*
                           (mpc:make-lock))
               ))
    (mpc:with-lock (lock)
      (funcall fn))))

(defmacro with-printer ((var stream) &body body)
  #+:LISPWORKS
  `(stream:apply-with-output-lock
    (lambda (,var)
      ,@body)
    ,stream)
  #+:SBCL
  `(let ((,var ,stream))
     (do-with-printer ,var (lambda ()
                             ,@body)))
  #+:ALLEGRO
  `(let ((,var ,stream))
     ,@body))

(deflex println
  (create
   (lambda* msg
     (with-printer (s *standard-output*)
       (format s "~&~{~A~%~}" msg)))))
  
(defun do-with-maximum-io-syntax (fn)
  (with-standard-io-syntax
    (let ((*print-radix*  t)
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

(deflex writeln
  (create
   (lambda* msg
     (with-printer (s *standard-output*)
       (with-maximum-io-syntax
         (format s "~&~{~:W~%~}" msg))))))

(deflex fmt-println
  (create
   (lambda (fmt-str &rest args)
     (with-printer (s *standard-output*)
       (format s "~&")
       (apply #'format s fmt-str args))
     )))

;; ------------------------------------------------
;; The bridge between imperative code and the Actors world
;;
;; Foreign threads can use ASK to query an Actor that provides a
;; response to a customer. It is superfluous to do so from an Actor.
;;
;; For querying such an Actor, just leave out the customer arg in your
;; message. Your thread joins the dispatcher pool until an answer can
;; be had.
;;
;; In effect, this saves the current continuaition (stack state) and
;; enters into the world of dispatchers until an answer is generated.
;; Then at the eariliest opportunity the (now dispatcher) current
;; thread returns with that answer.
;;
;; If your thread is busy performing a behavior, you have to wait
;; until that behavior finishes. If it is idle, waiting for message
;; Events at the Central Mailbox when the answer is generated, then
;; you will return within 1s. If your thread is the one generating the
;; answer, then return is immediate.
;;
;; Meanwhile, sit back and enjoy SMP Parallel Processing on Multi-Core
;; architectures.
;;
;; ASK performs an immediate reified SEND. So, when called from within
;; an Actor behavior, that violates transactional boundaries, and can
;; produce leakage that is best avoided. If you are in an Actor
;; behavior it is still better to avoid using ASK, and use β-forms for
;; CPS Actor style.

(define-condition recursive-ask (warning)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream
                     "Calling ASK from within an Actor has you violating transactional boundaries.
         Try using β-forms instead."))))

(defun do-with-recursive-ask (fn)
  (handler-bind
      ((recursive-ask #'muffle-warning))
    (funcall fn)))

(defmacro with-recursive-ask (&body body)
  ;; Under some special conditions we know it will be okay to allow
  ;; recursive ASKs. This macro muffles the warning.
  ;;
  ;; E.g., an Actor executing a user supplied function on behalf of
  ;; another thread. The function might (unwittingly) call ASK to
  ;; query other portions of the Actor system.
  ;;
  ;; In that case, we are behaving as the client thread, running
  ;; ostensibly single-threaded code, and pretend not to know about
  ;; the Actors system.
  `(do-with-recursive-ask (lambda ()
                            ,@body)))

(define-condition terminated-ask (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Terminated ASK"))
   ))

(defgeneric ask (target &rest msg)
  (:method ((target actor) &rest msg)
   ;; Unlike SEND, ASKs are not staged, and perform immediately,
   ;; potentially violating transactional boundaries. From non-Actor
   ;; code, this is normally okay, and expected behavior. ASK behaves
   ;; like a function call.
   ;;
   ;; But if called from within an Actor, the immediacy violates
   ;; transactional boundaries, since SEND is normally staged for
   ;; execution at successful exit, or discarded if errors.
   (when self
     (warn 'recursive-ask))

   (get-send-hook) ;; ensures that the Actors system is running
  
   ;; In normal situation, we get back the result message as a list and
   ;; flag t.  In exceptional situation, from restart "Terminate ASK",
   ;; we get back nil.  If *TIMEOUT* is not-nil, and timeout occurs, we
   ;; get back list (<timeout-condiiton-object>) as ans.
   (multiple-value-bind (ans okay)
       (apply #'run-actors target msg)
     (unless okay
       (error 'terminated-ask))
     (check-for-errors ans)
     (values-list ans)))
  (:method ((target function) &rest msg)
   (apply target msg)) )
   
;;
;; ASK can generate errors:
;;
;;    TERMINATED-ASK -- happens if we, while acting as Dispatch and
;;    awaiting our result, receive an error condition that the user
;;    chooses to respond to with restart "Terminate Ask". That error
;;    may arise from some other task that has nothing to do with us,
;;    but happened on our watch.
;;
;;    TIMEOUT-ERROR - whatever we ASK'd to happen did not produce
;;    a result for us before expiration of our timeout. Timout
;;    duration is set by wrapping our ASK with WITH-TIMEOUT. By
;;    default, it is nil, meaning no timeout.
;;
;;    REFERRED-ERROR - our request produced an error while being
;;    handled by some Dispatcher. It will have been reported
;;    already, but sent back to our ASK for our use. We have to
;;    decide what to do with it.
;;

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

(defun fn-actor (fn)
  (create
   (λ (cust . args)
     (send* cust (multiple-value-list (apply fn args))))
   ))

;; ----------------------------------------
;; We must defer startup until the MP system has been instantiated.

(defun* lw-start-actors _
  (mpc:atomic-exchange *send* nil)
  (princ "Actors are alive!"))

(defun* lw-kill-actors _
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
