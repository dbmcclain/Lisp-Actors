

(in-package #:com.ral.actors.base)

;; ----------------------------------------------------------------
;; System start-up and shut-down.
;;
;; A tour de force in SMP programming, the old way...
;;
;; --------------------------------------------
;;
;; Must keep in mind that Actors split the concept of Logical Tasks
;; from Machine Threads. The two are orthogonal concepts. We have
;; become accustomed to thinking of them as the same thing. And in
;; non-Actor systems they are the same. But not here.
;;
;; One logical task can be multiplexed across any number of different
;; machine threads during the course of its execution. A logical task
;; is a series of messages handled by various Actors, whose collective
;; intention is to accomplish some goal or sub-goal of a job to be
;; done. It makes no difference to the Actors which machine thread
;; executes them.
;;
;; SMP deals with machine threads, which may be multiplexing portions
;; of any number of logical tasks. A machine thread has no notion of
;; logical task progression. It is merely called upon to execute some
;; Actors which may be participating in any number of different
;; logical tasks. A machine thread is simply a workhorse used to
;; exercise a CPU on behalf of logical tasks.
;;
;; There is no direct connection between a running Actor and the
;; machine thread on which it is currently running. It could even be
;; running in parallel on more than one machine thread at the same
;; time. And every time you look, the machine thread running an Actor
;; could be different from previous executions.
;;
;; All you can know is that once execution of an Actor has begun on
;; one machine thread, that thread will carry on until Actor exit. As
;; long as there is at least one machine thread executing the
;; Dispatcher code, then an Actors system is running. It makes no
;; logical difference how many Dispatcher threads are running. Only
;; the runtime performance varies.
;;
;; --------------------------------------------
;;
;; Here we have the full blending of Actors with SMP Threading.  Apart
;; from a small bit in RUN-ACTORS, here we have to wrestle with the
;; fact that we have no direct control over which threads are running
;; the Custodian code. But we want each of the dispatch pool threads
;; to run it during :KILL-EXECUTIVES.
;;
;; We want each of the dispatch pool threads to perform its own
;; suicide from within the Custodian. That way we know that the thread
;; will be exiting cleanly.
;;
;; We can invent one non-pool thread, calling ASK from outside of the
;; Actors system, to serve as a surrogate dispatcher while all the
;; pool threads are shutting down.  But we cannot force that surrogate
;; thread to run anything at all while performing the ASK.
;;
;; Just know that as pool threads die off, we will inevitably find our
;; ASK thread running the Custodian code.  This might happen very
;; quickly, or the ASK may get delayed indefinitely by another
;; delivery task in the event queue.
;;
;; But that ASK thread won't be killed. It just needs to find its way
;; into Custodian so we can have it sleep for bit to stay out of the
;; way, while pool threads are exiting. The ASK thread will resume
;; later and perform its duties as a dispatcher. It won't be satisfied
;; until the :KILL-EXECUTIVES completes.
;;
;; But too, it is unknown if any dispatch pool threads are tied up
;; with long-running tasks, or blocked waiting on I/O. And it may
;; happen that one or more of those dispatch pool threads will never
;; see the Custodian code during the shutdown grace period (currently
;; 5 sec). In that case, we may have to forcibly terminate the
;; threads.
;;
;; --------------------------------------------
;;
;; It is important to realize that going through a SERIALIZER
;; *DOESN'T* cause any machine thread to block waiting. It merely
;; prevents the delivery of additional messages to an Actor. The
;; dispatch threads remain unblocked and ready to handle any other
;; messages.
;;
;; Message SENDS are *NOT* talking to other threads. They are simply
;; stating that, if a dispatch thread is available, it should pick up
;; and deliver the message to an Actor. The Actor system is thread
;; agnostic. But it needs at least one Dispatch thread running.
;;
;; Any of the Dispatch threads can enter the CUSTODIAN, including all
;; the Dispatch Pool threads and any additional extant ASK threads.
;; But due to the SERIALIZER, only one of them can enter the Custodian
;; at a time.
;;
;; For graceful Shutdown, we have each of the Pool threads terminate
;; themself. That way we aren't clobbering some activity in its midst.
;;
;; But there is no guarantee that a Pool thread is running the
;; Custodian. It might be an ASK thread helping out.
;;
;; If an ASK thread is the one running the Custodian for Shutdown,
;; then it must re-broadcast the :KILL-EXECUTIVES message using
;; immediate SEND-TO-POOL, then stand down for a while to allow some
;; Pool thread to handle the message. But this time, the message
;; avoids going through the SERIALIZER.
;;
;; As a pool thread finds itself handling the Kill message, it removes
;; itself from the ledger of pool threads and, if any remain, it
;; re-broadcasts the Kill messasge to nudge another pool thread to
;; have a go, just prior to self immolation.
;;
;; The SERIALIZER gets unblocked after the last Pool Thread has killed
;; itself. For message delivery, a running ASK handler has to do the
;; deed. And we have to use a direct SEND-TO-POOL instead of a SEND,
;; to avoid restarting the whole pool again.
;;
;; Additionally, when the Pool Threads have shut down, the *SEND* hook
;; must be nulled, so that future SENDS will restart the thread Pool.
;; But this hook cannot be nulled from within a running Actor. It must
;; be nulled out by a non-Actor thread.
;;
;; --------------------------------------------

(defun custodian-beh (&optional threads)
  ;; Custodian holds the list of parallel Actor dispatcher threads
  (alambda

   ;; --------------------------------------------
   ((cust 'add-executive id) ;; internal routine
    (if (assoc id threads)
        (send cust :ok)
      (β-become
          (let ((new-thread (mpc:process-run-function
                             (format nil "Actor Thread #~D" id)
                             ()
                             #'custodian-aware-dispatcher
                             )))
            (send cust :ok)
            (custodian-beh (acons id new-thread threads))
            ))
      ))
    
   ;; --------------------------------------------
   ((cust 'ensure-executives n)
    (let* ((outer-me  self)
           (rep       (create
                       (lambda (ix)
                         (cond ((> ix n)
                                (send cust :ok))
                               (t
                                (let ((inner-me self))
                                  (β _
                                      (send outer-me β 'add-executive ix)
                                    (send inner-me (1+ ix)))
                                  ))
                               )))))
      (send rep 1)
      ))

   ;; --------------------------------------------
   ((cust 'add-executives n)
    (send self cust 'ensure-executives (+ (length threads) n)))
   
   ;; --------------------------------------------
   ((cust 'remove-executive proc)
    ;; Sent after one of our Custodian-Aware Dispatchers is killed-off.
    (send cust :ok)
    (let ((pair  (rassoc proc threads)))
      (when pair
        (let ((new-threads (remove pair threads)))
          (become (custodian-beh new-threads)))
        )))

   ;; --------------------------------------------
   (('poison-pill)
    ;; Try to kill off one of our Custodian-Aware Dispatchers
    (let* ((my-proc (mpc:get-current-process))
           (pair    (rassoc my-proc threads)))
      (when pair
        ;; Found it, die.
        (mpc:current-process-kill))
      ))

   ;; --------------------------------------------
   ((cust :get-threads)
    (send cust threads))
   ))

(deflex* custodian
  (create (custodian-beh)))

(defun init-custodian ()
  (setf custodian (create (custodian-beh))))

;; --------------------------------------------

(defun custodian-aware-dispatcher ()
  ;; A type of dispatcher that will remove itself from the Custodian's
  ;; dispatch pool on abnormal exit.
  (unwind-protect
      (run-actors)
    ;; Can't just SEND here, might be nobody left to handle the
    ;; message...
    (ask custodian 'remove-executive (mpc:get-current-process))
    ))

;; --------------------------------------------------------------
;; User-level Functions

(defun actors-running-p ()
  *central-mail*)

(defun get-dispatch-threads ()
  (mapcar #'cdr (ask custodian :get-threads)))

(defun add-executives (n)
  (check-type n (integer 0 *))
  (if self
      (send custodian sink 'add-executives n)
    (ask custodian 'add-executives n)))

;; --------------------------------------------
;; The following are made into generic functions to support later
;; extensions involving the async socket system.

(defgeneric restart-actors-system (&optional nbr-execs)
  (:method (&optional (nbr-execs *nbr-pool*))
   ;; Users don't normally need to call this function. It is
   ;; automatically called on the first message SEND.
   (check-type nbr-execs (integer 1 *))
   (if self
       (send custodian sink 'ensure-executives nbr-execs)
     (ask custodian 'ensure-executives nbr-execs))
   ))

(defgeneric kill-actors-system ()
  (:method :around ()
   (when (actors-running-p)
     (call-next-method)))
  (:method ()
   ;; The FUNCALL-ASYNC assures that this will work, even if called
   ;; from an Actor thread. Of course, that will also cause the Actor
   ;; (and all others) to be killed.
   (mpc:funcall-async
    (lambda ()
      ;; We are now running in a known non-Actor thread
      (mpc:with-lock (*central-mail-lock*)
        (when (actors-running-p)
          (when-let (threads (get-dispatch-threads))
            (setup-dead-man-switch threads)
            (tagbody
             again
             (dotimes (ix (length threads))
               (send-to-pool custodian 'poison-pill))
             (sleep 1)
             (when (setf threads (get-dispatch-threads))
               (go again))
             ))
          (setf *central-mail* nil))
        )))
   ))

;; --------------------------------------------
;; In case of long-running Actor behaviors...
;;
;; If any Dispatch threads remain alive after the grace period,
;; following a shutdown request, then we have to forcibly terminate
;; the threads.

(defun setup-dead-man-switch (procs)
  (let (timer)
    (labels
        ((kill-with-prejudice ()
           (when-let (alive (remove-if (complement #'mpc:process-alive-p) procs))
             (cond
              ((y-or-n-p "Some dispatch threads are still running.
Terminate them?")
               (dolist (proc alive)
                 (mpc:process-terminate proc))
               (terpri)
               (princ "Dispatch threads were forcibly terminated."))
              
              (t
               (launch-timer))
              )))
         
         (launch-timer ()
           (mpc:schedule-timer-relative timer *actors-grace-period*)
           ))
      
      (setf timer (mpc:make-timer #'mpc:funcall-async #'kill-with-prejudice))
      (launch-timer)
      )))

#|
(progn
  (with-actors
    (send (create (lambda ()
                    (sleep 10)))))
  (sleep 1)
  (lw-kill-actors))
|#
;; --------------------------------------------
#|
(kill-actors-system)
(restart-actors-system)

(mpc:atomic-exchange *central-mail* nil)
(print *central-mail*)
(inspect *central-mail*)
(ask custodian :get-threads)
(get-dispatch-threads)
(setf custodian (create (custodian-beh (ask custodian :get-threads))))
(send println :hello)
|#
