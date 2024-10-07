

(in-package #:com.ral.actors.base)

;; ----------------------------------------------------------------
;; System start-up and shut-down.
;;
;; A tour de force in SMP programming, the old way...
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
;; We can invent one non-pool thread, using ASK, to serve as a
;; surrogate dispatcher while all the pool threads are shutting down.
;; But we cannot force that surrogate thread to run anything at all
;; while performing the ASK.
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
;; It is important to realize that going through a SERIALIZER
;; *DOESN'T* block a machine thread. It merely prevents the delivery
;; of additional messages to an Actor. The dispatch threads remain
;; unblocked and ready to handle any other messages.
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
;; have a go.
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

(defun custodian-beh (&optional threads)
  ;; Custodian holds the list of parallel Actor dispatcher threads
  (alambda
   ;; --------------------------------------------
   ((cust 'add-executive id) ;; internal routine
    (unless (assoc id threads)
      (let ((new-thread (mpc:process-run-function
                         (format nil "Actor Thread #~D" id)
                         ()
                         #'custodian-aware-dispatcher
                         )))
        (become (custodian-beh (acons id new-thread threads)))
        ))
    (send cust :ok))
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
    (send cust :ok)
    (when threads
      (let* ((pair        (rassoc proc threads))
             (new-threads (remove pair threads)))
        (become (custodian-beh new-threads))
        (unless new-threads
          (%kill-send-hook))
        )))
   ;; --------------------------------------------
   ((cust 'kill-executives)
    ;; Users should not send this message directly -- use function
    ;; KILL-ACTORS-SYSTEM. It sends the :KILL-EXECUTIVES message from
    ;; a non-Actor thread. This code only works properly if there is a
    ;; non-pool thread using a direct dispatcher, as with ASK.
    ;;
    (let ((gate (once
                 ;; We are in a race between self-exits and a
                 ;; dead-man-switch, which forcibly terminates them.
                 ;;
                 ;; Some dispatch pool threads may be indefinitely
                 ;; delayed due to I/O blocking or long-running tasks.
                 ;;
                 ;; The dead-man-switch has a generous delay. And the
                 ;; user is given the final choice to terminate them.
                 (create
                  (lambda (ans)
                    ;; By the time we reach here, no pool threads
                    ;; should be running, but THREADS may still have
                    ;; them in the list. It is safe to zap the list
                    ;; here. We are running in an ASK thread.
                    (setf threads nil)
                    (%kill-send-hook)
                    (send-to-pool cust ans))
                  ))))
      (%setup-dead-man-switch gate (mapcar #'cdr threads))
      (with-actors
        ;; SELF is now an anonymous sub-Actor
        (let* ((my-proc (mpc:get-current-process))
               (pair    (rassoc my-proc threads)))
          (cond
           (pair
            ;; Found my proc in the pool.  Direct mutation okay here,
            ;; since we are behind a SERIALIZER, *and* during shutdown,
            ;; only one KILL message is present at a time - even though
            ;; rebroadcasts avoid the SERIALIZER.
            (setf threads (remove pair threads))
            
            (cond
             (threads
              ;; more to go - bypass SERIALIZER
              (send-to-pool self))
             
             (t
              ;; Else - we were the last one.
              ;; Unblock the SERIALIZER, and kill the send-hook.
              (send-to-pool gate :ok)) )
            
            (mpc:current-process-kill)) ;; Adios!
           
           (t
            ;; We must be in a non-pool ASK thread.
            (cond
             (threads
              ;; Still are some active pool threads,
              ;; so try again and get out of the way for a moment.
              (send-to-pool self)
              (sleep 1))
             
             (t
              ;; Someone else must have done this before we did.
              ;; We have to unblock the SERIALIZER.
              (send-to-pool gate :ok))))
           )))))
    ;; --------------------------------------------
    ((cust :get-threads)
     (send cust threads))
    ))

(deflex* custodian
  (serializer (create (custodian-beh))))

(defun init-custodian ()
  (setf custodian (serializer (create (custodian-beh)))))

;; --------------------------------------------
;; Resetting the *SEND* hook after shutdown

(defun %kill-send-hook ()
  (mpc:funcall-async
   (lambda ()
     (let (threads)
       (tagbody
        again
        (sleep 1)  ;; allow things to settle
        ;; our ASK helps clear the message pipeline
        (setf threads (ask custodian :get-threads))
        (unless (mpc:mailbox-empty-p *central-mail*)
          (go again)
          ))
       (unless threads
         (mpc:atomic-exchange *send* nil))
       ))
   ))

;; --------------------------------------------
;; In case of long-running Actor behaviors...
;;
;; If any Dispatch threads remain alive after the grace period,
;; following a shutdown request, then we have to forcibly terminate
;; the threads.

(defparameter *actors-grace-period*  5f0)

(defun %setup-dead-man-switch (cust procs)
  (let (timer)
    (flet
        ((kill-with-prejudice ()
           (when-let (alive  (remove-if (complement #'mpc:process-alive-p) procs))
             (cond
              ((y-or-n-p "Some dispatch threads are still running.
Terminate them?")
               (dolist (proc alive)
                 (mpc:process-terminate proc))
               (send-to-pool cust :ok)
               (terpri)
               (princ "Dispatch threads were forcefully terminated."))
              
              (t
               (mpc:schedule-timer-relative timer *actors-grace-period*))
              ))))
      (setf timer (mpc:make-timer #'mpc:funcall-async #'kill-with-prejudice))
      (mpc:schedule-timer-relative timer *actors-grace-period*)
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

(defun custodian-aware-dispatcher ()
  ;; A type of dispatcher that will remove itself from the Custodian's
  ;; dispatch pool on abnormal exit.
  (unwind-protect
      (run-actors)
    (let ((proc  (mpc:get-current-process)))
      (send-to-pool custodian sink 'remove-executive proc))
    ))

;; --------------------------------------------------------------
;; User-level Functions

(defun actors-running-p ()
  *send*)

(defun add-executives (n)
  (check-type n (integer 0 *))
  (if self
      (send custodian sink 'add-executives n)
    (ask custodian 'add-executives n)))

(defun restart-actors-system (&optional (nbr-execs *nbr-pool*))
  ;; Users don't normally need to call this function. It is
  ;; automatically called on the first message SEND.
  (check-type nbr-execs (integer 1 *))
  (if self
      (send custodian sink 'ensure-executives nbr-execs)
    (ask custodian 'ensure-executives nbr-execs)))

(defun kill-actors-system ()
  ;; The FUNCALL-ASYNC assures that this will work, even if called
  ;; from an Actor thread. Of course, that will also cause the Actor
  ;; (and all others) to be killed.
  (when (actors-running-p)
    (mpc:funcall-async
     (lambda ()
       ;; We are now running in a known non-Actor thread
       (ask custodian 'kill-executives)
       ))))

#|
(kill-actors-system)
(restart-actors-system)

(mpc:atomic-exchange *send* nil)
(print *send*)
(send println :hello)
|#
