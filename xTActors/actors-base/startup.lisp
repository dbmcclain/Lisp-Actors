

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
;; Message SENDS are *NOT* talking to other threads. They are simply
;; stating that, if a dispatch thread is available, it should pick up
;; and deliver the message to an Actor. The Actor system is thread
;; agnostic. But it needs at least one Dispatch thread running.
;;
;; Any of the Dispatch threads can enter the CUSTODIAN, including all
;; the Dispatch Pool threads and any additional extant ASK threads.
;;
;; For graceful Shutdown, we have each of the Pool threads terminate
;; themself. That way we aren't clobbering some activity in its midst.
;;
;; --------------------------------------------

(defstruct dispatcher
  id proc done)

(defun custodian-beh (&optional dispatchers)
  ;; Custodian holds the list of parallel Actor dispatcher threads
  (with-contention-free-semantics
   (alambda
    
    ;; --------------------------------------------
    ((cust 'add-executive id) ;; internal routine
     (check-type id (integer 1 *))
     (send cust :ok)
     (unless (find id dispatchers :key #'dispatcher-id)
       (without-contention
        (let ((entry  (make-dispatcher
                       :id  id)))
          (setf (dispatcher-proc entry)
                (mpc:process-run-function
                 (format nil "Actor Thread #~D" id)
                 ()
                 #'launch-dispatcher
                 (um:pointer-& (dispatcher-done entry))
                 ))
          (become (custodian-beh (cons entry dispatchers)))
          ))))
    
    ;; --------------------------------------------
    ((cust 'ensure-executives n)
     (if (zerop n)
         (send cust :ok)
       (progn
         (check-type n (integer 1 *))
         (let ((me self))
           (β _
               (send self β 'add-executive n)
             (send me cust 'ensure-executives (1- n)))
           ))
       ))

    ;; --------------------------------------------
    ((cust 'add-executives n)
     (check-type n (integer 0 *))
     (send self cust 'ensure-executives (+ (length dispatchers) n)))

    ;; --------------------------------------------
    (('remove-dispatcher thread)
     (without-contention
      (let ((entry (find thread dispatchers :key #'dispatcher-proc)))
        (when entry
          (become (custodian-beh (remove entry dispatchers)))
          ))))
    
    ;; --------------------------------------------
    ((cust 'poison-pill)
     ;; Try to kill off one of our Dispatchers
     (cond (dispatchers
            (send-to-pool self cust 'poison-pill)
            (unless (find (mpc:get-current-process) dispatchers
                          :key #'dispatcher-proc)
              (sleep 0.1))) ;; get out of way...
           (t
            ;; No dispatch threads remain, we are done.
            (send cust :ok))
           ))

    (('reset-custodian)
     ;; Sent by the dead-man switch after forcibly killing off
     ;; Dispatch threads.
     (without-contention
      (become (custodian-beh))
      ))
     
    ;; --------------------------------------------
    ((cust :get-dispatchers)
     (send cust dispatchers))
    )))

(deflex* custodian
  (create (custodian-beh)))

(defun init-custodian ()
  (setf custodian (create (custodian-beh))))

(defun launch-dispatcher (done-ptr)
  ;; Notify Custodian on death of dispatcher thread.
  (unwind-protect
      (run-actor-dispatch-loop done-ptr)
    (send custodian 'remove-dispatcher (mpc:get-current-process))))

;; --------------------------------------------------------------
;; User-level Functions

(defun actors-running-p ()
  *central-mail*)

(defun get-dispatch-threads ()
  (mapcar #'dispatcher-proc
          (with-default-timeout 1
            (ask custodian :get-dispatchers))
          ))

(defun add-executives (n)
  (check-type n (integer 0 *))
  (if self
      (send custodian sink 'add-executives n)
    (with-default-timeout 1
      (ask custodian 'add-executives n))
    ))

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
     (with-default-timeout 1
       (ask custodian 'ensure-executives nbr-execs)))
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
      (when (actors-running-p)
        (mpc:with-lock (*central-mail-lock*)
          (when (actors-running-p)
            (let ((entries (with-timeout 1
                              (ask custodian :get-dispatchers))))
              (when entries
                (dolist (entry entries)
                  (setf (dispatcher-done entry) t))
                (setup-dead-man-switch (mapcar #'dispatcher-proc entries))
                (with-timeout (* 2 *ACTORS-GRACE-PERIOD*)
                  (ask custodian 'poison-pill))))
            (setf *central-mail* nil))
          )))
    )))

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
           (when (setf procs (delete-if (complement #'mpc:process-alive-p) procs))
             (cond
              ((y-or-n-p "Some dispatch threads are still running.
Terminate them?")
               (dolist (proc procs)
                 (mpc:process-terminate proc))
               (send-to-pool custodian 'reset-custodian)
               (terpri)
               (princ "Dispatch threads were forcibly terminated."))
              
              (t
               (launch-timer))
              )))
         
         (launch-timer ()
           (mpc:schedule-timer-relative timer *ACTORS-GRACE-PERIOD*)
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
(ask custodian :get-dispatchers)
(get-dispatch-threads)
(setf custodian (create (custodian-beh (ask custodian :get-dispatchers))))
(send println :hello)
|#
