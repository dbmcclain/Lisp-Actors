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

;; ------------------------------------
;; Borrowed shamelessly from LW example/grand-central-dispatch.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; code ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The argument type of the dispatch_* functions that take
;;; a block. 

(fli:define-c-typedef dispatch-block-t fli:foreign-block-pointer)

;;; The callable type for blocks by dispatch_* functions. 
(fli:define-foreign-block-callable-type dispatch-block-callable
                                        :void ())

;;; A dummy C structure. Pointers to this structure can be passed to
;;; DISPATCH-RELEASE and DISPATCH-RETAIN. We define all the dispatch
;;; object types as pointers to it. 

(fli:define-c-struct (dispatch-object-dummy-structure (:forward-reference-p t)))

(fli:define-foreign-function dispatch-retain ((dop (:pointer dispatch-object-dummy-structure))))
(fli:define-foreign-function dispatch-release ((dop (:pointer dispatch-object-dummy-structure))))

;;; The timeout type
(fli:define-c-typedef dispatch-time-t :uint64)
(defconstant DISPATCH_TIME_NOW (fli:cast-integer  0 :uint64))
(defconstant DISPATCH_TIME_FOREVER (fli:cast-integer (lognot 0) :uint64))

  
;;; The dispatch object types that we are going to use. 
(fli:define-c-typedef dispatch-queue-t (:pointer dispatch-object-dummy-structure)) 
(fli:define-c-typedef dispatch-group-t (:pointer dispatch-object-dummy-structure)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting a global queue ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant DISPATCH_QUEUE_PRIORITY_HIGH 2)
(defconstant DISPATCH_QUEUE_PRIORITY_DEFAULT 0)
(defconstant DISPATCH_QUEUE_PRIORITY_LOW -2)

(fli:define-foreign-function dispatch-get-global-queue
    ((priority :long) ;DISPATCH_QUEUE_PRIORITY_*
     (flags (:unsigned :long))) ;;; reserved, currently 0
  :result-type dispatch-queue-t)

(defun call-dispatch-get-global-queue (&optional priority)
  (let ((dp (case priority
             (:high DISPATCH_QUEUE_PRIORITY_HIGH)
             ((:default nil) DISPATCH_QUEUE_PRIORITY_DEFAULT)
             (:low DISPATCH_QUEUE_PRIORITY_LOW)
             (t (error "CALL-DISPATCH-GET-GLOBAL-QUEUE: invaid priority : ~s" priority)))))
    (dispatch-get-global-queue dp 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dispatch functions that we are going to use ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(fli:define-foreign-function dispatch-async
    ((queue dispatch-queue-t)
     (block dispatch-block-t)))


(fli:define-foreign-function dispatch-group-create
    ()
  :result-type dispatch-group-t)

(fli:define-foreign-function dispatch-group-async 
    ((group dispatch-group-t)
     (queue dispatch-queue-t)
     (block dispatch-block-t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Waiting for a group to finish ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The documentation of the timeout in dispatch-group-wait
;;; is not clear. By stepping through it we found that it uses
;;; mach absolute time (on Mac OS X).
;;; Since we want the timeout to be relative in seconds
;;; like all other waiting functions, we need to do
;;; some computations. 

(fli:define-c-struct mach-timebase-info-data-t
  (numer (:unsigned :int))
  (denom (:unsigned :int)))

(fli:define-foreign-function mach-timebase-info ((p :pointer)))

;;; This is fixed on any given machine, but can vary in principle if
;;; the image is saved and restarted on another machine. If you use
;;; this code in an application, make sure it is NIL on restart.

(defparameter *seconds-to-mach-absolute-time-ratio* nil)

;;; The info is how to convert absolute to nanos, we
;;; go from seconds to absolute so invert the denum/numer
;;; and multiply by billion. 
(defun seconds-to-mach-absolute-time-ratio()
  (or *seconds-to-mach-absolute-time-ratio*
      (setq *seconds-to-mach-absolute-time-ratio* 
            (* (expt 10 9)
               (fli:with-dynamic-foreign-objects
                   ((s mach-timebase-info-data-t))
                 (mach-timebase-info s)
                 (/ (fli:foreign-slot-value s 'denom)
                    (fli:foreign-slot-value s 'numer)))))))
  

(fli:define-foreign-function mach-absolute-time ()
  :result-type :uint64)

(fli:define-foreign-function dispatch-group-wait 
    ((group dispatch-group-t)
     (time dispatch-time-t)) 
  :result-type :long ; 0 on success, non-zero timeout 
  )

;;; This is the "proper" interface.
(defun call-dispatch-group-wait (dg timeout)
  (let ((ti (case timeout
              (0 DISPATCH_TIME_NOW)
              ((nil) DISPATCH_TIME_FOREVER)
              (t (+ (truncate (* timeout (seconds-to-mach-absolute-time-ratio)))
                    (mach-absolute-time))))))
    (zerop (dispatch-group-wait dg ti))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  LISP side interface ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Actually doing the dispatch of FUNC and ARGS, using the queue
;;; specified by the priority, which must be one of
;;; :HIGH,:DEFAULT,:LOW or NIL. If the group is non-nil, it must be a
;;; dispatch-group-t object, and we dispatch with it.

(defun apply-with-gcd-and-group (priority group func &rest args)
  (unless (mp:get-current-process)
    (error "Trying to use GCD without multiprocessing"))
  (let ((queue (call-dispatch-get-global-queue priority))
        (block (apply 'fli:allocate-foreign-block 'dispatch-block-callable func args)))
    (prog1
        (if group
            (dispatch-group-async group queue block)
          (dispatch-async queue block))
      (fli:free-foreign-block block))))

;; ------------------------------------
#|
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

    (defun next-actor ()
      (mp:mailbox-read actor-ready-queue "Waiting for Actor"))
    ))

(defmacro without-watchdog (&body body)
  `(do-without-watchdog (lambda ()
                          ,@body)))
|#

;; -------------------------------------------------------------
;; Executive Control
;; ------------------------------------

(defgeneric nullify (actor)
  (:method ((actor <runnable>))
   (sys:atomic-exchange (car (actor-busy actor)) :exit))

  (:method :before ((actor actor))
   ;; prevent it from doing anything more, even if already running
   (setf (actor-user-fn actor) 'lw:do-nothing))

  (:method :before ((worker worker))
   ;; prevent it from doing its job if not already running
   (setf (worker-dispatch-wrapper worker)  (list 'lw:do-nothing)))
  )

(defun terminate-actor (actor)
  ;; if they will listen...
  (nullify actor))

(defun terminate-actors (actors)
  ;; removes the need to loop with TERMINATE-ACTOR on a collection
  ;; of actors to be terminated.
  (map nil 'nullify actors))
      
(defun add-to-ready-queue (actor)
  ;; use the busy cell to hold our wakeup time - for use by watchdog,
  (setf (cdr (actor-busy actor)) (get-universal-time))
  (apply-with-gcd-and-group :DEFAULT NIL #'%run-actor actor))

;; ----------------------------------------------------------------------------------
