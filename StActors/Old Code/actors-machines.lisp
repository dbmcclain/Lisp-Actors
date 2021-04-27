
(in-package #:actors-machines)

;; ----------------------------------------------------------------

(defun find-kw-assoc (kw lst)
  (let ((test (curry #'eq kw)))
    (multiple-value-bind (pre post)
        (um:split-if test lst)
      (if post
          (values (cadr post) t
                  (nlet-tail flush ((pre  pre)
                                       (tl   (cddr post)))
                    (multiple-value-bind (hd new-tl) (um:split-if test tl)
                      (let ((new-pre (nconc pre hd)))
                        (if new-tl
                            (flush new-pre (cddr new-tl))
                          new-pre)
                        ))) )
        (values nil nil lst))
      )))

(defun parse-clauses (clauses)
  (multiple-value-bind (timeout timeout-present-p clauses)
      (find-kw-assoc :TIMEOUT clauses)
    (multiple-value-bind (on-timeout on-timeout-present-p clauses)
        (find-kw-assoc :ON-TIMEOUT clauses)
      (values clauses
              timeout timeout-present-p
              on-timeout on-timeout-present-p)
      )))

(defun parse-pattern-clauses (clauses)
  `(lambda-match  ;; returns NIL on no matching message, as we need
     ,@(mapcar (lambda (clause)
                 (ematch clause
                   ((list* pat (and which
                                    (or 'when
                                        'unless))
                           pred body)
                    `(,pat ,which ,pred (lambda () ,@body)))

                   ((list* pat body)
                    `(,pat (lambda () ,@body)))
                   ))
               clauses)) )

(defun parse-recv-clauses (clauses)
  (multiple-value-bind (new-clauses
                        timeout-expr      timeout-present-p
                        on-timeout-clause on-timeout-present-p)
      (parse-clauses clauses)
    (declare (ignore timeout-present-p))
    (let* ((conds-fn       (parse-pattern-clauses new-clauses))
           (timeout-fn     (if on-timeout-present-p
                               `(lambda ()
                                  ,on-timeout-clause)
                             (let ((a!self (anaphor 'self)))
                               `(lambda ()
                                  (timed-out ,a!self))))
                           ))
      (values conds-fn timeout-fn timeout-expr))))

;; ----------------------------------------------------------------

(define-condition actors-exn (error)
  ((err-reason :reader err-reason :initarg :reason :initform :abnormal
               :documentation "The reason for the exit.")
   (err-arg    :reader err-arg    :initarg :arg  :initform nil
               :documentation "Any additional arguments for this exit.")
   (err-from   :reader err-from   :initarg :from :initform (current-actor)
               :documentation "The PID of the process that produced this exit condition."))
  (:report report-actors-exn)
  (:documentation "An Actors system error condition."))

(define-condition actors-exn-timeout (actors-exn)
  ()
  (:documentation "A subclass of actors-exn for timeouts"))

(defun report-actors-exn (err stream)
  "Report the exit condition as a printable item.
Only works for PRINC, and FORMAT ~A not FORMAT ~S or ~W"
  (if-let (arg (err-arg err))
      (format stream "Exit: ~A ~A~%From: ~A"
              (err-reason err) arg (err-from err))
    (format stream "Exit: ~A~%From: ~A"
            (err-reason err) (err-from err))
    ))

(defconstant +timeout-msg+  (gensym))

(defun timed-out (self)
  "Internal routine to generate the timeout exception to the current
process."
  (error (make-instance 'actors-exn-timeout
                        :from   self
                        :reason :ABNORMAL
                        :arg    :TIMEOUT)))

(defun actor-timeout-timer (actor)
  (get-actor-property actor 'timeout-timer))

(defsetf actor-timeout-timer (actor) (timer)
  `(setf (get-actor-property ,actor 'timeout-timer) ,timer))
         
(defun unschedule-timeout (actor)
  (let ((timer (actor-timeout-timer actor)))
    (when timer
      (mp:unschedule-timer timer))
    ))

(defun schedule-timeout (actor duration)
  ;; Calling SCHEDULE-TIMEOUT on an Actor that already has a pending
  ;; timeout, cancels the pending timeout and initiates with the
  ;; current duration.
  (labels ((tell-him ()
             (send actor +timeout-msg+)))
    (with-locked-actor (actor)
      (unschedule-timeout actor)
      (with-accessors ((its-timer  actor-timeout-timer)) actor
        (cond ((null duration) )
              
              ((not (realp duration))
               (error "Timeout duration must be a real number"))
              
              ((plusp duration)
               (let ((timer (or its-timer
                                (setf its-timer (mp:make-timer #'tell-him)))))
                 (mp:schedule-timer-relative timer duration)))
              
              (t  ;; negative or zero timeout duration
                  (tell-him))
              )))
    actor)) ;; might be helpful to act like SETF and return the actor

;; ----------------------------------------------------------------
;; Scheduled Actors - Actors with an executed RECV form. Scheduled
;; only in the sense that they may receive a timeout message before
;; any recognizable messages arrive. Can only happen if someone calls
;; SCHEDULE-TIMEOUT on the Actor, or when the RECV form specifies a
;; TIMEOUT expression.

;; -----------------------------------------------------------
;; RECV -- actively listen for a message (blocking wait)

(defpan handle-active-recv (conds-fn timeout-fn timeout-expr) ()
  ;; Until we either get a timeout or a recognizable message, the
  ;; Actor becomes a blocking-wait agent under the Executive. Once
  ;; either of those events occur, the Actor reverts back to passive
  ;; mode.
  (when timeout-expr
    (schedule-timeout self timeout-expr))
  ;; Go ahead and do the unthinkable...
  ;; ... become a blocking-wait Actor
  (block :wait-loop
    (macrolet ((ret (val)
                 `(return-from :wait-loop ,val)))
      (loop
       (match (mailbox-read (actor-messages self))
         ((list (eq +timeout-msg+))
          (ret (funcall timeout-fn)))
         
         (msg
          (when-let (fn (funcall conds-fn msg))
            (unschedule-timeout self)
            (ret (funcall fn))))
         ))
      )))

(defmacro! recv (&rest clauses)
  ;;
  ;; a RECV uses Optima:MATCH style patterns and clauses.
  ;;
  ;; RECV receives and processes one qualifying message or gets timed
  ;; out. Any messages arriving at the Actor's mailbox which do not
  ;; qualify for any of the RECV clauses will be discarded during the
  ;; waiting period. After RECV either times out or receives a
  ;; qualifying message, the body forms of the Actor that follow the
  ;; RECV form will be executed and the Actor will be using its
  ;; original behavior on all future messages.
  ;;
  ;; If there is a TIMEOUT expression inside the RECV form, the Actor
  ;; will setup a timeout timer on that expression, and go into a
  ;; blocking-wait loop of reading its own message mailbox until
  ;; either a timeout occurs or a qualifying message arrives.
  ;; Thereafter it will revert to its original passive Actor mode, and
  ;; continue executing the remaining forms in the body of the Actor.
  ;;
  ;; If there isn't a TIMEOUT expression inside the RECV form, the
  ;; Actor could become blocked-waiting for an indefinite period,
  ;; tying up an Executive thread.
  ;;
  ;; We re-parse the handler body to create a function which takes a
  ;; message and returns a fully deconstructed pattern match closure,
  ;; or nil. This allows us to cancel any pending timeout if a message
  ;; will be handled.
  ;;
  ;; An Actor containing a RECV form will not execute that form until
  ;; it receives some message that causes it to execute the branch of
  ;; code containing the RECV form.
  ;;
  (multiple-value-bind (conds-fn timeout-fn timeout-expr)
      (parse-recv-clauses clauses)
    `(handle-active-recv ,a!self ,conds-fn ,timeout-fn ,timeout-expr)
    ))

;; -------------------------------------------------------------------
;; BECOME-RECV -- a passive listening mode

(defpan handle-passive-recv (conds-fn timeout-fn timeout-expr) ()
  ;; redefine the Actor's behavior to be a passive message handler
  (prog1
      (lambda (&rest msg)
        (match msg
          ((list (eq +timeout-msg+))
           (funcall timeout-fn))
          
          (msg
           (when-let (fn (funcall conds-fn msg))
             (unschedule-timeout self)
             (funcall fn)))
          ))
    (when timeout-expr
      (schedule-timeout self timeout-expr))
    ))

(defmacro! become-recv (lexref state-ref &rest clauses)
  ;;
  ;; This version converts the behavior of the Actor into a message
  ;; handler described by the RECV clauses. It does not perform an
  ;; immediate RECV operation. It simply repurposes the Actor's
  ;; behavior and returns immediately to the body of the Actor to
  ;; execute any remaining forms.
  ;;
  ;; if SELF is not visible when this macro is used, one will be
  ;; provided as a local binding against (CURRENT-ACTOR).
  ;;
  ;; This macro does not establish a new local state, but can make use
  ;; of the existing one through bargs. If you want to make those
  ;; bargs also visible to the pandoric-get/set then include them in
  ;; the pargs list.
  ;;
  (multiple-value-bind (conds-fn timeout-fn timeout-expr)
      (parse-recv-clauses clauses)
    `(become ,lexref ,state-ref
       (handle-passive-recv ,a!self ,conds-fn ,timeout-fn ,timeout-expr))
    ))

(editor:setup-indent "become-recv" 2)

;; -----------------------------------------------------------------------
;; State Machine Actors...

#||#
(defpan handle-state-machine-message (&rest msg) (state backlog state-handlers)
  (labels
      ((handle-message (msg)
         ;; GETF uses EQ comparison -- identical
         (if-let (fn (getf state-handlers state)) ;; get state handlers
             ;; we got the handler function. It takes a message and
             ;; returns a closure to call for that message, or nil
             ;; on no handler.
             (let ((old-state state))
               (funcall fn msg) ;; this might change state...
               ;;
               ;; it is responsibility of the programmer to decide
               ;; when/if to stash messages it doesn't want to handle.
               ;;
               ;; But at every change of state, we retry the stashed
               ;; messages in case the new state can handle them
               ;;
               (unless (eq state old-state)
                 (mapc #'handle-message 
                       (priq:contents backlog))) )  ;; this clears the backlog
           ;; else
           (error "Invalid state"))))
    (handle-message msg)))

(defmacro! make-state-machine (lex-refs state-bindings initial-state &body body)
  ;; Every Actor has a name, possibly some non-empty initial
  ;; internal-state bindings and a body.  A state-machine also takes
  ;; the name used to refer to messages in the body, an initial state
  ;; value for machine state, and a collection of handlers for each
  ;; state. The body will be synthesized for use by the state machine
  ;; grinder.
  `(make-actor ,lex-refs
       (,@state-bindings
        (state          ,initial-state)
        (backlog        (priq:make-unsafe-fifo))
        (state-handlers ,@(last body)))
     ,@(butlast body)
     (curry #'handle-state-machine-message ,a!self)))

(defmacro! defstates (&rest clauses)
  `(macrolet ((,a!next-state (state)
                `(setf state ,state))
              (,a!save-message (msg)
                `(priq:addq backlog ,msg)))
     (list ,@(mapcan #`(,(car a1) (lambda-match
                                    ,@(cdr a1)))
                     clauses))))

(editor:setup-indent "make-state-machine" 3)

#|
(let ((x  (make-state-machine () (val) :initial
            (defstates
             (:initial
              ((list :echo x) (pr x))
              ((list :who)    (pr self))
              ((list :test x)
               (setf val x)
               (next-state :one)))
             
             (:one
              ((list :try x)
               (pr (+ x val))
               (next-state :initial))
              (msg
               (save-message msg)))
             ))))
  (send x :echo :This)
  (send x :who)
  (send x :test 15)
  (send x :who)
  (send x :echo :That)
  (send x :try 32)
  (send x :quit))
|#
#||#

#|
(def-factory make-state-machine (state state-handlers)
    ((backlog (make-unsafe-fifo)))
  (labels
      ((handle-message (msg)
         ;; GETF uses EQ comparison -- identical
         (um:if-let (fn (getf state-handlers state)) ;; get state handlers
             ;; we got the handler function. It takes a message and
             ;; returns a closure to call for that message, or nil
             ;; on no handler.
             (let ((old-state state))
               (funcall fn msg) ;; this might change state...
               ;;
               ;; it is responsibility of the programmer to decide
               ;; when/if to stash messages it doesn't want to handle.
               ;;
               ;; But at every change of state, we retry the stashed
               ;; messages in case the new state can handle them
               ;;
               (unless (eq state old-state)
                 (mapc #'handle-message 
                       (priq:contents backlog))) )  ;; this clears the backlog
           ;; else
           (error "Invalid state"))))
    #'handle-message))

(defmacro defstates (&rest clauses)
  (let ((a!next-state   (anaphor 'next-state))
        (a!save-message (anaphor 'save-messge)))
    `(macrolet ((,a!next-state (state)
                  `(setf state ,state))
                (,a!save-message (msg)
                  `(priq:addq backlog ,msg)))
       (list ,@(mapcan #`(,(car a1) (optima.extra:lambda-match
                                      ,@(cdr a1)))
                       clauses)))
    ))

(let ((x  (make-state-machine
              :state :initial
              :state-handlers
            (defstates
             (:initial
              ((list :echo x) (pr x))
              ((list :who)    (pr self))
              ((list :test x)
               (setf val x)
               (next-state :one)))
             
             (:one
              ((list :try x)
               (pr (+ x val))
               (next-state :initial))
              (msg
               (save-message msg)))
             ))))
  (send x :echo :This)
  (send x :who)
  (send x :test 15)
  (send x :who)
  (send x :echo :That)
  (send x :try 32)
  (send x :quit))
|#

;; ---------------------------------------------------

#|
(progn
  (defvar *ct* 0)
  (def-factory make-stupid (&rest msg)
    ()
    (um:dlambda
      (:quit ()
       (sys:atomic-incf *ct*)
       #|
       (when (= *ct* 1000000)
         (print "You hit the jackpot!"))
       |#
       )))
  
  (defun tst (n)
    (setf *ct* 0)
    (let ((all (loop repeat n collect
                     (make-stupid))))
      #|
       (time
        (map nil (lambda (actor)
                   (send actor :nope))
             all))
       |#
      ;; (inspect *actor-dict*)
      (time
       (progn
         (map nil (lambda (actor)
                    (send actor :quit))
              all)
         (mp:process-wait "Waiting for TST finish"
                          (lambda ()
                            (= n *ct*)))))
      )) )

(tst #N|1_000_000|) ;; about 10 sec/1M Actors elapsed time
(make-actor () ()
  (format t "~&*MUFFLE-EXITS* = ~A" *muffle-exits*))
(kill-executives)
;; ---------------------------------------------------
|#
