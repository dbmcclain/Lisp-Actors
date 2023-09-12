
(in-package #:com.ral.actors.base)

;; ----------------------------------------------------------------
;; System start-up and shut-down.
;;

(def-ser-beh custodian-beh (&optional threads)
  ;; Custodian holds the list of parallel Actor dispatcher threads
  ((cust :add-executive id)
   (unless (assoc id threads)
     (let ((new-thread (mpc:process-run-function
                        (format nil "Actor Thread #~D" id)
                        ()
                        #'run-actors)
                       ))
       (become (custodian-beh (acons id new-thread threads)))
       ))
   (send cust :ok))

  ((cust :ensuring n ix)
   (cond ((>= ix n)
          (send cust :ok))
         (t
          (let ((me self))
            (β _
                (send self β :add-executive ix)
              (send me cust :ensuring n (1+ ix)))
            ))
         ))
  
  ((cust :ensure-executives n)
   (send self cust :ensuring n 0))
     
  ((cust :add-executives n)
   (send self cust :ensure-executives (+ (length threads) n)))
     
  ((cust :kill-executives)
   ;; Users should not send this message directly -- use function
   ;; KILL-ACTORS-SYSTEM from a non-Actor thread. Only works properly
   ;; when called by a non-Actor thread using a single-thread direct
   ;; dispatcher, as with CALL-ACTOR.
   (become (custodian-beh nil))
   (send cust :ok)
   (let* ((my-thread     (mpc:get-current-process))
          (other-threads (remove my-thread threads)))
     (map nil #'mpc:process-terminate other-threads)
     (when (find my-thread threads)
       ;; this will cancel pending SEND/BECOME...
       (mpc:current-process-kill))
     ))
     
  ((cust :get-threads)
   (send cust threads)))

(def-actor custodian
  (serializer (create (custodian-beh))))

;; --------------------------------------------------------------
;; User-level Functions

(defun actors-running-p ()
  (or self
      (ask custodian :get-threads)))

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
  (non-idempotent
    (mpc:funcall-async
     (lambda ()
       ;; we are now running in a known non-Actor thread
       (ask custodian :kill-executives)
       (setf *send* #'startup-send))
     )))

#|
(kill-actors-system)
(restart-actors-system)
 |#
