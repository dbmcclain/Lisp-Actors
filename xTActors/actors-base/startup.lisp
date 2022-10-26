
(in-package #:com.ral.actors.base)

;; ----------------------------------------------------------------
;; System start-up and shut-down.
;;

(def-ser-beh custodian-beh (&optional (count 0) threads)
  ;; Custodian holds the list of parallel Actor dispatcher threads
  ((cust :add-executive id)
   (send cust :ok)
   (when (< count id)
     ;; Not Idempotent - so we need to be behind a SERIALIZER.
     (let ((new-thread (mpc:process-run-function
                        (format nil "Actor Thread #~D" id)
                        ()
                        #'run-actors)
                       ))
       (become (custodian-beh id (cons new-thread threads)))
       )))
   
  ((cust :ensure-executives n)
   (if (< (length threads) n)
       (let ((me  self)
             (msg self-msg))
         (β _
             (send self β :add-executive (1+ count))
           (send* me msg)))
     ;; else
     (send cust :ok)))
     
  ((cust :add-executives n)
   (send self cust :ensure-executives (+ (length threads) n)))
     
  ((cust :kill-executives)
   ;; Users should not send this message directly -- use function
   ;; KILL-ACTORS-SYSTEM from a non-Actor thread. Only works properly
   ;; when called by a non-Actor thread using a single-thread direct
   ;; dispatcher, as with CALL-ACTOR.
   (become (custodian-beh 0 nil))
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

#|
(defun blocking-serializer-beh (service)
  (let ((lock (mpc:make-lock)))
    (lambda (cust &rest msg)
      (mpc:with-lock (lock)
        (send* cust (multiple-value-list (apply #'call-actor service msg))))
      )))

(defun blocking-serializer (service)
  ;; To be used on Actor services that live at the edge, managing a
  ;; shared resource, and that may need to run even if the Actors
  ;; system is not currently running.
  ;;
  ;; ... the only way to reach us, in that case, is via CALL-ACTOR.
  ;;
  ;; Be cautious never to use this from a single-thread context, on a
  ;; service that might send back to itself, either directly or
  ;; indirectly. That will produce a deadlock. That's why I stated to
  ;; be used on edge Actors!
  ;;
  (create (blocking-serializer-beh service)))
|#

(def-actor custodian
  (serializer (create (custodian-beh))))

;; --------------------------------------------------------------
;; User-level Functions

(defun actors-running-p ()
  (or self
      (call-actor custodian :get-threads)))

(defun add-executives (n)
  (call-actor custodian :add-executives n))

(defun restart-actors-system (&optional (nbr-execs *nbr-pool*))
  ;; Users don't normally need to call this function. It is
  ;; automatically called on the first message SEND.
  (call-actor custodian :ensure-executives nbr-execs))

(defun kill-actors-system ()
  ;; The FUNCALL-ASYNC assures that this will work, even if called
  ;; from an Actor thread. Of course, that will also cause the Actor
  ;; (and all others) to be killed.
  (mpc:funcall-async
   (lambda ()
     ;; we are now running in a known non-Actor thread
     (call-actor custodian :kill-executives)
     (setf *send* #'startup-send))))

#|
(kill-actors-system)
(restart-actors-system)
 |#
