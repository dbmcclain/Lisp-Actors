;; executives.lisp -- Actors Executive Pool
;; The actual runtime guts of the Actors system.
;;
;; DM/RAL  08/20 - latest update using the upgraded Hoare Monitors
;; ---------------------------------------------------------------

(in-package :actors.executives)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(actors.base:%run-actor

            mp:make-timer
            mp:schedule-timer-relative
            mp:unschedule-timer
            mp:funcall-async
            mp:process-run-function
            mp:process-interrupt
            mp:process-terminate
            mp:process-kill
            mp:make-mailbox
            mp:mailbox-read
            mp:mailbox-send
            mp:mailbox-peek
            
            um:defmonitor
            um:critical-section
            um:critical-or
            )))

;; --------------------------------------------------------------------
;; Executive Pool - actual system threads dedicated to running Actor code

(defun default-watchdog-function (age)
  (restart-case
      (error "Actor Executives are stalled (blocked waiting or compute bound). ~&Last heartbeat was ~A sec ago."
             age)
    (:do-nothing-just-wait ()
      :report "It's okay, just wait"
      (start-watchdog-timer))
    (:spawn-new-executive ()
      :report "Spawn another Executive"
      (push-new-executive))
    (abort ()
      :report "Terminate Actor system"
      (kill-executives))
    ))

(defvar *watchdog-hook* 'default-watchdog-function)

;; ------------------------------------------------------------
;; Executive Actions

(defvar *exec-actor*  nil)

(defun executive-loop ()
  ;; the main executive loop - more general, in that it will abosrb
  ;; global settings of print-vars from dynamic environment process
  ;; creator
  (loop
   (with-simple-restart (abort "Run next ready Actor")
     (let ((*exec-actor* (next-actor)))
       (%run-actor *exec-actor*)))
   ))

(defun exec-terminate-actor (actor)
  ;; an interrupt handler - if the actor is ours, we terminate it
  (when (eq actor *exec-actor*)
    (abort)))

(defun exec-terminate-actors (actors)
  ;; an interrupt handler - if we are running one of the actors,
  ;; terminate it
  (when (member *exec-actor* actors)
    (abort)))

;; --------------------------------------------------------------

#|
(defun test-stall ()
  (loop repeat (1+ (get-nbr-execs)) do 
	(spawn (lambda () 
		 (sleep 10) 
		 (pr :hello (current-actor)))
	       )))
|#
;; -------------------------------------------------------------
;; Executive Control
;; ------------------------------------

(defgeneric nullify (actor)
  (:method ((actor <runnable>))
   ;; (sys:atomic-exchange (car (actor-busy actor)) :exit)
   (setf (sys:globally-accessible (car (actor-busy actor))) :exit))

  (:method :before ((actor actor))
   ;; prevent it from doing anything more, even if already running
   (setf (sys:globally-accessible (actor-user-fn actor)) 'lw:do-nothing))

  (:method :before ((worker worker))
   ;; prevent it from doing its job if not already running
   (setf (sys:globally-accessible (worker-dispatch-wrapper worker)) (list 'lw:do-nothing)))
  )

;; ------------------------------------

(defmonitor executives
    ((watchdog-inhibit    (ref:ref 0))     ;; >0 averts watchdog checking
     (watchdog-checking   (ref:ref nil))   ;; t when checking for system stalls
     (heartbeat-timer     nil)             ;; current heartbeat timer
     (heartbeat-interval  1)               ;; how often (s) the watchdog should check for system stall
     (maximum-age         3)               ;; how long (s) before watchdog should bark, in seconds
     (executive-counter   0)               ;; for creating printable exec names
     (executive-processes nil)             ;; list of active exec procs
     (actor-ready-queue                    ;; a queue of pending Actor activations
        (mp:make-mailbox :name "Actor Ready Queue"))
     (nbr-execs                            ;; should match the number of CPU Cores but never less than 4
        (max 4
             #+(AND :LISPWORKS :MACOSX)
             (um:get-number-of-processors)
             #|
             (load-time-value
              (with-open-stream (s (sys:open-pipe "sysctl -n hw.logicalcpu"))
                (let ((ans (ignore-errors (parse-integer (read-line s nil nil)))))
                  (or (and (integerp ans)
                           ans)
                      1))))
             |#
             #+:CLOZURE
             (ccl:cpu-count)
             #-(or :CLOZURE (AND :LISPWORKS :MACOSX))
             1)))

  (labels
      ((resume-periodic-checking ()
         (ref:basic-atomic-exch watchdog-checking nil))
         
       (check-sufficient-execs ()
         (critical-section
           (um:if-let (actor (mp:mailbox-peek actor-ready-queue))
               ;; Any Actors waiting?
               ;; indeterminate delay between finding Actor and
               ;; reading its timestamp...
               (let* ((timestamp (cdr (actor-busy actor)))
                      (age       (- (get-universal-time) timestamp)))
                 (if (< age maximum-age)
                     ;; Everything is fine. Put us back on check
                     ;; duty - timer took us off to avoid
                     ;; duplicate notifications in slow runnging
                     ;; systems
                     (resume-periodic-checking)

                   ;; else
                   ;; we are taking too long, someone is possibly blocking?
                   ;; -------------------------------------------
                   ;;
                   ;; Why kill the workhorse?
                   ;;
                   ;; For LW, the timer routine triggers in an arbitrary
                   ;; thread with a retriggering timer. This routine runs as
                   ;; an interrupt routine and we need to keep it short. We
                   ;; also need to prevent retriggering of nuisance
                   ;; notifications while we are busy handling the situation.
                   ;;
                   ;; For ACL, the timer runs in its own dedicated thread and
                   ;; won't retrigger until we return from here. But we also
                   ;; need to keep this short so that we don't block ongoing
                   ;; useful activity that may need something inside this
                   ;; monitor section.
                   ;;
                   ;; So in both cases, just kill off the timer and let a new
                   ;; thread handle the notification with the user.
                   ;; ----------------------------------------------
                   (progn
                     (unschedule-timer (shiftf heartbeat-timer nil))
                     (mp:funcall-async *watchdog-hook* age))
                   ;; --------------------------------------------
                   ))
             ;; else 
             ;; No Actors were seen waiting.
             ;; Put us back on watch duty
             (resume-periodic-checking))
           ))
        
       (make-new-executive ()
         (mp:process-run-function
          (format nil "Actor Executive ~D"
                  (incf executive-counter))
          '(:internal-server t)
          'executive-loop)))
         
      
    ;; All under a Hoare Monitor lock - called infrequently.
    ;; Things dealing with mutating the Actor ready queue
    ;; and Executive thread pool.

    (defun terminate-actor (actor)
      (critical-section
        (nullify actor)
        (dolist (exec executive-processes)
          (mp:process-interrupt exec 'exec-terminate-actor actor))))

    (defun terminate-actors (actors)
      ;; removes the need to loop with TERMINATE-ACTOR on a collection
      ;; of actors to be terminated, becoming O(M) instead of O(N*M).
      ;; Requires only one interrupt to each executive thread, instead
      ;; of N of them.
      (critical-section
        (map nil 'nullify actors)
        (dolist (exec executive-processes)
          (mp:process-interrupt exec 'exec-terminate-actors actors))))
      
    (defun start-watchdog-timer ()
      (critical-section
        (resume-periodic-checking)
        (unless heartbeat-timer
          (setf heartbeat-timer
                (make-timer (lambda ()
                              (when (and (zerop (ref:basic-val watchdog-inhibit))
                                         (ref:basic-cas watchdog-checking nil t))
                                (mp:funcall-async #'check-sufficient-execs)))
                            ))
          (schedule-timer-relative
           heartbeat-timer
           maximum-age
           heartbeat-interval))))

    (defun set-heartbeat-interval (secs)
      (check-type secs (real 1))
      (critical-section
        (prog1
            (shiftf heartbeat-interval secs)
          (when heartbeat-timer
            (schedule-timer-relative
             heartbeat-timer
             nil
             secs)))
        ))

    (defun get-heartbeat-interval ()
      heartbeat-interval)

    (defun set-maximum-age (secs)
      (check-type secs (real 1))
      (critical-section
        (shiftf maximum-age secs)))

    (defun get-maximum-age ()
      maximum-age)
      
    (defun set-executive-pool (npool)
      ;; set the Executive pool nbr threads, return previous value
      (check-type npool (integer 4))
      (critical-section
        (prog1
            (shiftf nbr-execs npool)
          (kill-executives))))

    (defun get-nbr-execs ()
      nbr-execs)
      
    (defun push-new-executive ()
      (critical-section
        (push (make-new-executive) executive-processes)
        (setf nbr-execs (max nbr-execs
                             (length executive-processes)))
        (start-watchdog-timer)))
      
    (defun kill-executives ()
      (critical-section
        (um:when-let (timer (shiftf heartbeat-timer nil))
          (unschedule-timer timer))
        (um:when-let (procs (shiftf executive-processes nil))
          (when (consp procs)
            (dolist (proc procs)
              (ignore-errors
                (mp:process-terminate proc
                                      :force-timeout 10))))
          ;; empty the ready queue
          (setf actor-ready-queue
                (mp:make-mailbox :name "Actor Ready Queue"))
          )))
      
    (defun do-without-watchdog (fn)
      (ref:atomic-incf watchdog-inhibit)
      (unwind-protect
          (funcall fn)
        (ref:atomic-decf watchdog-inhibit)))

    (defun add-to-ready-queue (actor)
      ;; use the busy cell to hold our wakeup time - for use by watchdog,
      (setf (cdr (actor-busy actor)) (get-universal-time))
      (mp:mailbox-send actor-ready-queue actor)
      ;; ensure executives are live
      (critical-or executive-processes
                   (setf executive-counter    0
                         executive-processes  (loop repeat nbr-execs collect
                                                    (make-new-executive)))
                   (start-watchdog-timer)))
      

    (defun next-actor ()
      (mp:mailbox-read actor-ready-queue "Waiting for Actor"))
    ))

(defmacro without-watchdog (&body body)
  `(do-without-watchdog (lambda ()
                          ,@body)))

;; ----------------------------------------------------------------------------------
