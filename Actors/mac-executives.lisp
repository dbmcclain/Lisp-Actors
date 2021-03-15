;; executives.lisp -- Actors Executive Pool
;; The actual runtime guts of the Actors system.
;;
;; DM/RAL  08/20 - latest update using the upgraded Hoare Monitors
;; ---------------------------------------------------------------

(in-package :actors.executives)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(actors.base:%run-actor
            actors.base:%basic-run-actor
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

  (:method ((fn function))
   ;; for direct-run workers
   nil)
  )

(defvar *current-runnable* nil)

(defun kill-actor (actor)
  (when (eq *current-runnable* actor)
    (abort)))

(defun kill-actors (actors)
  (when (member *current-runnable* actors)
    (abort)))

(defun terminate-actor (actor)
  (nullify actor)
  (mp:map-processes (lambda (proc)
                      (mp:process-interrupt proc 'kill-actor actor))
                    ))

(defun terminate-actors (actors)
  ;; Removes the need to loop with TERMINATE-ACTOR on a collection
  ;; of actors to be terminated.
  (map nil 'nullify actors)
  (mp:map-processes (lambda (proc)
                      (mp:process-interrupt proc 'kill-actors actors))))

(defun %pre-run-actor (*current-runnable*)
  ;; gives us a handle on the current runnable for use by
  ;; TERMINATE-ACTOR
  (%run-actor *current-runnable*))

(defun add-to-ready-queue (actor)
  (apply-with-gcd-and-group :DEFAULT NIL #'%pre-run-actor actor))

(defmacro without-watchdog (&body body)
  `(progn
     ,@body))

(defun kill-executives ()
  nil)


(defun run-actor-direct (actor &rest msg)
  (apply #'apply-with-gcd-and-group :DEFAULT NIL #'%pre-run-actor-direct actor msg))

(defun %pre-run-actor-direct (*current-runnable* &rest msg)
  (with-as-current-actor *current-runnable*
    (%basic-run-actor *current-runnable* :initial-message msg)))

(defun run-worker-direct (fn &rest msg)
  (apply #'apply-with-gcd-and-group :DEFAULT NIL #'%pre-run-worker-direct fn msg))

(defun %pre-run-worker-direct (*current-runnable* &rest msg)
  (apply *current-runnable* msg))

;; ----------------------------------------------------------------------------------
