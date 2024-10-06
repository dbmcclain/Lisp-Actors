
(in-package #:com.ral.actors.base)

;; ----------------------------------------------------------------
;; System start-up and shut-down.
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
;; Additionally, when the Pool Threads have shut down, the *SEND*
;; hook must be nulled, so that future SENDS will restart the thread
;; Pool. But this cannot be nulled from within a running Actor. It
;; must be nulled out by a non-Actor thread.


(defvar *exit-lock* (mpc:make-lock))

(defun custodian-beh (&optional threads)
  ;; Custodian holds the list of parallel Actor dispatcher threads
  (alambda
   ((cust 'add-executive id)
    (unless (assoc id threads)
      (let ((new-thread (mpc:process-run-function
                         (format nil "Actor Thread #~D" id)
                         ()
                         #'run-actors
                         )))
        (become (custodian-beh (acons id new-thread threads)))
        ))
    (send cust :ok))

   ((cust :ensure-executives n)
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
     
   ((cust :add-executives n)
    (send self cust :ensure-executives (+ (length threads) n)))
     
   ((cust :kill-executives)
    ;; Users should not send this message directly -- use function
    ;; KILL-ACTORS-SYSTEM from a non-Actor thread. Only works properly
    ;; when called by a non-Actor thread using a direct dispatcher, as
    ;; with ASK.
    ;;
    (let* ((my-proc (mpc:get-current-process))
           (pair    (rassoc my-proc threads)))
      (cond
       (pair
        ;; Found my proc in the pool.
        (mpc:with-lock (*exit-lock*)
          (setf threads (remove pair threads))
          (if threads
              ;; more to go
              (send-to-pool self cust :kill-executives)
            ;; else - we were the last one.
            ;; Unblock the SERIALIZER.
            (send-to-pool cust :ok)))
        (mpc:process-terminate my-proc)) ;; Adios!
       
       (t
        ;; We must be in a non-pool ASK thread.
        (cond
         (threads
          ;; Still are some active pool threads,
          ;; so try again and get out of the way for a moment.
          (send-to-pool self cust :kill-executives)
          (sleep 1))
         
         (t
          ;; Someone else must have done this before we did.
          ;; We have to unblock the SERIALIZER.
          (send-to-pool cust :ok))))
       )))

   ((cust :get-threads)
    (send cust threads))
   ))

(deflex* custodian
  (serializer (create (custodian-beh))))

;; --------------------------------------------------------------
;; User-level Functions

(defun actors-running-p ()
  *send*)

(defun add-executives (n)
  (if self
      (send custodian sink :add-executives n)
    (ask custodian :add-executives n)))

(defun restart-actors-system (&optional (nbr-execs *nbr-pool*))
  ;; Users don't normally need to call this function. It is
  ;; automatically called on the first message SEND.
  (if self
      (send custodian sink :ensure-executives nbr-execs)
    (ask custodian :ensure-executives nbr-execs)))

(defun kill-actors-system ()
  ;; The FUNCALL-ASYNC assures that this will work, even if called
  ;; from an Actor thread. Of course, that will also cause the Actor
  ;; (and all others) to be killed.
  (when (actors-running-p)
    (mpc:funcall-async
     (lambda ()
       ;; We are now running in a known non-Actor thread
       (ask custodian :kill-executives)
       (mpc:atomic-exchange *send* nil))
     )))

#|
(kill-actors-system)
(restart-actors-system)

(mpc:atomic-exchange *send* nil)
(identity *send*)
|#
