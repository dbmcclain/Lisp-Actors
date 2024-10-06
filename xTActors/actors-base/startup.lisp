
(in-package #:com.ral.actors.base)

;; ----------------------------------------------------------------
;; System start-up and shut-down.
;;

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
          (send cust :ok))))
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
