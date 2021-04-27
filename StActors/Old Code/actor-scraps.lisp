#|
(defmacro with-future ((&rest args) form &body body)
  ;; should always be in tail position
  (let ((g!block (gensym)))
    `(do-with-future
      (lambda ()
        (block ,g!block
          (flet ((=values (&rest args)
                   (return-from ,g!block (values-list args))))
            ,form)))
      ,(if (member '&rest args)
           `(lambda (&optional ,@args)
              ,@body)
         ;; else
         `(lambda (&optional ,@args &rest #1=ignored)
            (declare (ignore #1#))
            ,@body)))))

(defun do-with-future (exec-fn cont-fn)
  (if-let (me (current-actor))
      (let ((xid        (uuid:make-v1-uuid))
            (my-timeout *timeout*))
        (spawn-worker
         (lambda ()
           (let ((*current-actor*  me)
                 (*timeout*        my-timeout))
             (send me :cont-future-{7c434b50-1a12-11ea-96cb-787b8acbe32e}
                   xid
                   (capture-ans-or-exn exec-fn)))))
        (recv
          ((list ':cont-future-{7c434b50-1a12-11ea-96cb-787b8acbe32e} id ans)
           when (uuid:uuid= id xid)
           (multiple-value-call cont-fn (recover-ans-or-exn ans)))

          :timeout *timeout*
          ))
    ;; else
    (multiple-value-call cont-fn (funcall exec-fn))))
|#

;; --------------------------------------------------------------------
#|

  Now... if your Actor code is reentrant, meaning no (or protected)
mutable state, then you can safely launch futures activity to another
thread, and remain non-blocking for parallel processing of incoming
messages. All state is passed in as arguments to the Actor message
handlers, or protected against SMP multiple access.

  There seems no way to enforce reentrancy in the Actor function, so
buyer beware...

  - Don't mutate local or global bindings without first protecting with a lock.

But if the Actor code is truly reentrant, then there is no need to
enforce single-thread semantics, and the continuation code could be
performed as well in the spawned worker thread. There is no need to
send a message containing the continuation back to the original Actor
for execution.

|#

#|
(defclass callback-function ()
  ()
  (:metaclass #+:LISPWORKS clos:funcallable-standard-class
              #+:ALLEGRO   mop:funcallable-standard-class
              #+:CLOZURE   ccl:funcallable-standard-class
              #+:SBCL      sb-mop:funcallable-standard-class
              ))

(defmethod initialize-instance :after ((obj callback-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   #+:CLOZURE   ccl:set-funcallable-instance-function
   #+:SBCL      sb-mop:set-funcallable-instance-function
   obj behavior))

(defmethod =cont ((contfn callback-function))
  contfn)

(defmethod =cont ((contfn symbol))
  (=cont (symbol-function contfn)))

(defmethod =cont ((contfn function))
  (if-let (self (current-actor))
      ;;
      ;; If the callback originated from inside an Actor, we ensure
      ;; that it will later execute inside that Actor only when that
      ;; Actor is alive.
      ;;
      ;; Code inside an Actor should only be executing on one thread
      ;; at a time, in order to preserve SMP single-thread semantics.
      ;;
      (make-instance 'callback-function
                     :behavior (lambda (&rest args)
                                 (if (eq self (current-actor))
                                     (apply contfn args)
                                   (apply 'send self 'actor-internal-message:continuation contfn args))))
                     
    ;; else - not originating from inside of an Actor, just return the
    ;; function unchanged
    contfn))
|#

;; --------------------------------------------------------
#|
(defmacro with-future ((&rest args) form &body body)
  ;; Should always be in tail position
  ;;
  ;; Result of exec-form indicated by calling =VALUES, or else result
  ;; of exec form.
  ;;
  `(do-with-future
    (=lambda ()
      ,form)
    ,(if (member '&rest args)
         `(lambda (&optional ,@args)
            ,@body)
       ;; else
       `(lambda (&optional ,@args &rest #1=ignored)
          (declare (ignore #1#))
          ,@body))
    ))
|#
#|
(defun do-with-future (exec-fn cont-fn)
  (flet ((doit ()
           (multiple-value-call cont-fn
             (funcall exec-fn))))
    (if-let (me (current-actor))
        (let ((my-timeout *timeout*))
          (spawn-worker
           (lambda ()
             (let ((*current-actor* me)
                   (*timeout*       my-timeout))
               (doit)))
           ))
      ;; else
      (doit))))
|#

#|
(defun do-with-future (exec-fn cont-fn)
  ;; perform exec-fn in another thread,
  ;; have continuation performed back in caller thread
  (flet ((kexec-fn ()
           (with-cont
             (=funcall exec-fn))))
    (cond ((current-actor)
           (let ((kcont-fn (=cont (lambda (arg)
                                    (multiple-value-call cont-fn
                                      (recover-ans-or-exn arg)))
                                  )))
             ;; by using spawn-actor-as-worker here, instead of
             ;; spawn-worker, we can accommodate exec-fn that invokes
             ;; RECV, or futures of its own.
             (spawn-actor-as-worker
              (lambda ()
                (funcall kcont-fn (mcapture-ans-or-exn
                                    (kexec-fn)))))
             ))
          (t 
           (multiple-value-call cont-fn (kexec-fn)))
          )))
|#

#|
(defmacro with-future ((&rest args) form &body body)
  ;; like =bind, but suitable for Actors where continuation may be
  ;; called from foreign thread
  (let ((=bind-cont (=cont ,(cont-fn args body))))
    ;; form should return via =values
    ,form))
|#

#|
(spawn
 (lambda ()
   (with-future (&rest args)
       (progn
         (sleep 1)
         (=values 15 16))
     (pr args))))

(with-future (&rest args)
    (progn
      (sleep 1)
      (=values 15 16))
  (pr args))

(defvar *x*  15)

(spawn
 (lambda ()
   (let* ((*x* 32)
          (xx  *x*))
     (with-future (x)
         (progn
           (sleep 1)
           (=values xx))
       (pr `(,x ,*x*))))))
 |#

(defun do-schedule-timeout-action (timeout fn)
  (spawn (lambda ()
           (recv
             :TIMEOUT    timeout
             :ON-TIMEOUT (funcall fn)))))

(defmacro schedule-timeout-action (timeout &body body)
  ;; a macro to schedule an action after timeout.
  `(do-schedule-timeout-action ,timeout (=cont (lambda ()
                                                 ,@body))))


(defmacro schedule-after (timeout &body body)
  ;; a macro to schedule Actor body code to be performed after timeout.
  `(schedule-timeout-action ,timeout ,@body))

;; ---------------------------------------------------------------

;; ------------------------------------------------
;; RECV handling
;;
;; RECV under Actors is asynchronous with callback.  Consecutive RECV
;; forms in the body code enqueue internal messages to ensure
;; sequential performance of the successive RECV clauses. When the
;; first RECV clause finishes its callback or timeout, the next will
;; start.
;;
;; But RECV clauses perform without waiting, just falling through on
;; first encounter. While a RECV clause is active, it modifies the
;; behavior of the Actor to intercept messages selectively, stashing
;; those that don't match one of the RECV clauses, for later
;; execution.
;;
;; When a RECV is in operation, the RECV-INFO slot of the Actor points
;; to one of these control blocks.

(defclass recv-info ()
  ;; unique token used to associate timeouts with corresponding RECV
  ;; sessions
  ((id          :reader   recv-info-id          
                :initarg  :id)
   ;; a list of pending successive RECV's
   ;; no need for SMP safety here - only modified by active Actor
   (recvq       :reader   recv-info-recvq       
                :initform (make-unsafe-fifo))
   ;; a list of incoming messages that didn't match
   ;; no need for SMP safety here - only modified by active Actor   
   (msgq        :reader   recv-info-msgq 
                :initform (make-unsafe-fifo))
   ;; a function to screen messages for match
   (selector-fn :reader   recv-info-selector-fn 
                :initarg  :selector-fn)
   ;; the function to invoke on a timeout
   (timeout-fn  :reader   recv-info-timeout-fn
                :initarg  :timeout-fn)
   ;; currently active timer - when nil => none
   (timer       :accessor recv-info-timer
                :initarg  :timer)))

;; ----------------------------------------
;; RECV handlers...

(defgeneric enqueue-replay (obj replay)
  (:method ((mbox actor-mailbox) lst)
   ;; enqueue our list of messages ahead of pending stashed in mailbox
   ;; called solely within the active Actor. No need for locks.
   (setf (actor-message-replay mbox) (nconc lst (actor-message-replay mbox))))

  (:method ((self actor) (info recv-info))
   ;; the RECV has finished... enqueue all the stashed messages in the
   ;; Actor's mailbox, giving priority to internal RECV messages.
   ;;
   ;; This method overrides the one for the Actor's mailbox
   (enqueue-replay (actor-mailbox self)
                   (nconc (contents (recv-info-recvq info))
                          (contents (recv-info-msgq  info)))
                   ))
  )
  
(defun actor-recv-timeout (timer-id)
  ;; a timeout occurred... is it ours? If not, just ignore.
  (let ((info (actor-recv-info *current-actor*)))
    (when (and info   ;; were we in a RECV?
               (eq (recv-info-id info) timer-id)) ;; was it the same one as for timer?
      (setf (actor-recv-info *current-actor*) nil) ;; terminate RECV
      (enqueue-replay *current-actor* info)        ;; prep for life after RECV
      (if-let (fn (recv-info-timeout-fn info))
          (funcall fn)
        (error 'timeout)))))
         
(defun actor-recv-setup (conds-fn timeout-fn timeout-expr)
  ;; setup a new RECV control block in the current Actor, hence
  ;; activating RECV behavior until we find a message we want, or
  ;; else timeout waiting for one.
  (let ((this-id (gensym))) ;; make a unique id for recv-info
    (setf (actor-recv-info *current-actor*)
          (make-instance 'recv-info
                         :id          this-id
                         :selector-fn conds-fn
                         :timeout-fn  timeout-fn
                         :timer       (make-timeout-timer timeout-expr this-id)
                         ))))

;; -------------------------------------------------------------
;; Timeout Timers...

(defun send-timeout-message (self this-id)
  (send self
        'actor-internal-message:recv-timeout
        this-id))

(defun make-timeout-timer (delta this-id)
  "Delta in seconds"
  (when delta
    (let ((timer (make-timer
                  'mp:funcall-async 'send-timeout-message *current-actor* this-id)))
      (schedule-timer-relative timer delta)
      timer)))

;; -------------------------------------------------------------

(defun actor-recv-test-message (msg)
  ;; see if the incoming message matches one of our RECV handlers
  (let* ((info   (actor-recv-info *current-actor*))
         (ans-fn (funcall (recv-info-selector-fn info) msg)))
    (cond (ans-fn
           (setf (actor-recv-info *current-actor*) nil) ;; revert to non-RECV behavior
           (when-let (timer (recv-info-timer info))
             (unschedule-timer timer))
           (enqueue-replay *current-actor* info) ;; prep for life after RECV
           (funcall ans-fn))          ;; handle the message

          (t 
           ;; else - not a message we are looking for - stash it
           (addq (recv-info-msgq info) msg))
          )))
            
;; ------------------------------------------------------
;; The main outer dispatch method for all Actors. It is here that we
;; differentiate among messages during active RECV, intercept RPC ASK
;; messages to reflect errors back to the caller, and perform
;; continuation messages resulting from callbacks. Otherwise, we
;; forward the message to the user's Actor code.
;;
;; Dynamic context from possible user specified wrappers are already
;; in effect at this point. There is always at least one dynamic
;; binding in effect: *CURRENT-ACTOR*.

(defgeneric dispatch-message (obj &rest msg)
  (:method ((*current-actor* actor) &rest msg)
   (declare (special *current-actor*))
   ;; recoded dbm 8/20 - use direct cond instead of dlambda, dcase, for speedup
   (let ((sel (car msg)))
     (cond ((eql sel 'actor-internal-message:continuation)
            ;; Used for callbacks into the Actor
            (destructuring-bind (fn . vals) (cdr msg)
              (apply fn vals)))
     
           ((eql sel 'actor-internal-message:recv-timeout)
            ;; an incoming RECV timeout message
            (let ((timer-id (cadr msg)))
              (actor-recv-timeout timer-id)))
     
           ((eql sel 'actor-internal-message:recv-setup)
            ;; Another RECV clause. If not already in a RECV clause,
            ;; activate it. Otherwise stash it as an internal RECV
            ;; message to be run after the current RECV clause
            ;; finishes.
            (destructuring-bind (conds-fn timeout-fn timeout-expr) (cdr msg)
              (if-let (info (actor-recv-info *current-actor*))
                  (addq (recv-info-recvq info) msg)
                (actor-recv-setup conds-fn timeout-fn timeout-expr))))

           #| ----------------------------------------------------
           Handlers above this point will be responded to always.
           ------------------------------------------------------ |# 

           ((actor-recv-info *current-actor*)
            ;; we are in an active RECV clause - handle or stash this
            ;; message
            (actor-recv-test-message msg))
              
           #| ----------------------------------------------------
           Handlers below this point will be responded to only
           if they arrive outside of the context of an active RECV.
           Non-RECV messages will be enqueued for delivery after the
           RECV completes.
           ------------------------------------------------------ |# 

           ;; else -- not currently in a RECV
           ((eql sel 'actor-internal-message:ask)
            ;; Intercept queries to send back a response from the
            ;; following message, reflecting any errors back to the
            ;; caller.
            (destructuring-bind (replyTo &rest msg) (cdr msg)
              (send replyTo (mcapture-ans-or-exn
                              (apply 'self-call msg)))))

           (t
            ;; anything else is up to the programmer who constructed
            ;; this Actor
            (apply 'self-call msg))
           ))))

(defmacro recv-match (&rest clauses)
  ;;
  ;; a RECV uses Optima:MATCH style patterns and clauses.
  ;;
  ;; RECV receives and processes one qualifying message, or gets timed
  ;; out. Messages arriving at the Actor's mailbox which do not
  ;; qualify for any of the RECV clauses will be stashed during the
  ;; waiting period. RECV operates asynchronously and does not block
  ;; waiting.
  ;;
  ;; After RECV either times out or receives a qualifying message, the
  ;; body forms of the Actor that follow the RECV form will be in
  ;; effect, the stashed messages will be replayed, and the Actor will
  ;; be using its original behavior on those and all future messages.
  ;;
  ;; If there is a :TIMEOUT expression inside the RECV form, the Actor
  ;; will setup a timeout timer on that expression. If a qualifying
  ;; message does not arrive before the timer expires, a timeout will
  ;; occur. That timeout will execute the form listed after
  ;; :ON-TIMEOUT if given, or else a timeout error will be generated.
  ;;
  ;; In the macro, we parse the handler body to create a function
  ;; which takes a message and returns a fully deconstructed pattern
  ;; match closure, or nil. This allows us to cancel any pending
  ;; timeout if a message qualifies.
  ;;
  ;; An Actor containing a RECV form will not execute that form until
  ;; it receives some message that causes it to execute a branch of
  ;; code contained in the RECV form.
  ;;
  (multiple-value-bind (conds-fn timeout-fn timeout-expr)
      (parse-recv-clauses clauses)
    `(labels
         ((retry-recv ()
            (dispatch-message
             (current-actor)
             'actor-internal-message:recv-setup
             ,conds-fn ,timeout-fn ,timeout-expr)))
       (retry-recv))
    ))


;; -------------------------------------------------------------------------------------------

;; -------------------------------------------------------------------

(defclass <actor-state-mixin> ()
  ((controlling-actor
    :reader   controlling-actor
    :initform (make-actor
               (lambda (k &rest args)
                 ;; an executor of closures
                 (apply k args))))
   ))

;; -------------------------------------------------------------------

(defmethod send ((obj <actor-state-mixin>) &rest message)
  ;; If the message is a method invocation then result is the same as
  ;; simply executing the method. If a method were named DOIT then:
  ;;
  ;;   (SEND <state> 'DOIT <state> ...other args...)
  ;;
  ;; So no need to do this. But there may be other reasons to SEND to
  ;; an Actor which might be in the midst of a RECV.
  (apply 'send (controlling-actor obj) message))

(defmethod ask ((obj <actor-state-mixin>) &rest message)
  ;; The result of continuation execution can be returned to caller by
  ;; asking the method. E.g., if DOIT is the name of one of the
  ;; methods:
  ;;
  ;;    (ASK <state> 'doit <state> ...args...)
  ;;
  (apply 'ask (controlling-actor obj) message))


(defmethod send-to-controlling-actor ((obj <actor-state-mixin>) closure &rest args)
  (let ((actor (controlling-actor obj)))
    (if (eql actor (current-actor))
        (apply closure obj args) ;; Actor is already running, so act like a closure
      (apply 'send actor closure obj args)) ;; Act like normal message delivery
    ))

(defmethod inject (actor &rest message)
  ;; Used to inject a message as a continuation, ahead of any possible
  ;; filtering used by the user-function of the Actor (e.g., during a
  ;; RECV) - special purpose, as in crypto handshakes over the network
  (apply 'send actor 'actor-internal-message:continuation message))

;; -------------------------------------------------------------------

(defun kw-pos (args)
  (position-if (um:rcurry 'member lambda-list-keywords) args))

(defun get-labels-args (args)
  ;; strip out any class spec from required args
  (let ((pos (kw-pos args)))
    (append (mapcar (lambda (arg)
                      (if (consp arg)
                          (first arg)
                        arg))
                    (subseq args 0 pos))
            (when pos
              (subseq args pos)))
    ))

(defun get-def-args (args)
  (let ((pos (kw-pos args)))
    (subseq args 0 pos)))

(defun get-call-args (args)
  (Let ((pos (kw-pos args)))
    (mapcar (lambda (arg)
              (if (consp arg)
                  (car arg)
                arg))
            (subseq args 0 pos))))

(defun method-signature (def)
  (multiple-value-bind (name args)
      (values-list (cdr def))
    (list name (mapcar (lambda (arg)
                         ;; get the class of the args to form a selector list
                         (if (consp arg)
                             ;; for class-typed selector args
                             (second arg)
                           t)) ;; for un-typed args
                       (subseq args 0
                               ;; all the required args
                               (position-if (um:rcurry 'member lambda-list-keywords)
                                            args))))
    ))

(defvar *recorded-actor-methods* (make-hash-table))

(defun add-actor-methods (class-name sigs)
  ;; The main purpose here is to remove any old methods that may
  ;; become elided in the current redefinition of the actor-class
  (um:when-let (existing-sigs (gethash class-name *recorded-actor-methods*))
    (dolist (sig existing-sigs)
      (destructuring-bind (name specs) sig
        (let* ((gf     (fdefinition name))
               (meth   (find-method gf nil (mapcar 'find-class specs))))
          (when meth
            (remove-method gf meth)))
        )))
  (setf (gethash class-name *recorded-actor-methods*) sigs))

;; -------------------------------------------------------------------

#+:lispworks
(progn
  (editor:setup-indent "def-actor-class" 3)
  (editor:setup-indent "define-actor" 2 2 10)
  (editor:setup-indent "define-actor-class" 2 2 10)
  (editor:setup-indent "define-actor-methods" 2 2 2 'flet))

(defmacro %define-actor-class (class-name superclasses slots &rest class-options)
  "Define a class of state objects with associated Actor methods for
single thread access semantics. While executing one the the methods,
neighboring methods are available by direct call, as with SELF-CALL.
External clients trigger the execution of continuations into the
controlling Actor.

  Syntax is like DEFCLASS:

     (DEF-ACTOR-CLASS <class-name> (superclasses...)
         (slots...)
       (:default ... other class options...)
       (:METHODS
        ((<method-name-1> ((<obj> <classname>) <other-args>)
             <method-body>)
         ..other methods...)
        ))

     Each contained method must have a typed argument of the
<classname> as its first argument.

Rather than sending messages to an Actor, client code can simply call
these methods directly with an instance of the class as its first
argument."
  `(defclass ,class-name (,@superclasses <actor-state-mixin>)
     ,slots ,@class-options))

(defmacro %define-actor-methods (class-name methods)
  "Define a class of state objects with associated Actor methods for
single thread access semantics. While executing one the the methods,
neighboring methods are available by direct call, as with SELF-CALL.
External clients trigger the execution of continuations into the
controlling Actor.

  Syntax is like DEFCLASS:

     (DEF-ACTOR-CLASS <class-name> (superclasses...)
         (slots...)
       (:default ... other class options...)
       (:METHODS
        ((<method-name-1> ((<obj> <classname>) <other-args>)
             <method-body>)
         ..other methods...)
        ))

     Each contained method must have a typed argument of the
<classname> as its first argument.

Rather than sending messages to an Actor, client code can simply call
these methods directly with an instance of the class as its first
argument."
  (let ((g!rest  (gensym))
        (g!slots (mapcar 'clos:slot-definition-name
                         (clos:class-slots (find-class class-name)))))
    (multiple-value-bind (lbl-funcs sym-pairs meth-defs)
        (um:nlet-tail iter ((methods  methods)
                            (funcs    nil)
                            (syms     nil)
                            (defs     nil))
                 (if (endp methods)
                     (values (nreverse funcs)
                             (nreverse syms)
                             (nreverse defs))
                   ;; else
                   (destructuring-bind (meth-name (self &rest args) &rest body) (car methods)
                     (let* ((closure-sym  (gensym))
                            (closure-args (get-labels-args args))
                            (def-args     (get-def-args args))
                            (call-args    (get-call-args args))
                            (label-fn     `(,meth-name (,self &rest ,g!rest)
                                                       (with-slots ,g!slots ,self
                                                         (declare (ignorable ,@g!slots))
                                                         (apply (lambda ,closure-args
                                                                  ,@body)
                                                                ,g!rest)))))
                       (iter (cdr methods)
                             (cons  label-fn funcs)
                             (cons  `(,closure-sym #',meth-name) syms)
                             (cons  `(defmethod ,meth-name ((,self ,class-name) ,@def-args &rest ,g!rest)
                                       (apply 'send-to-controlling-actor ,self ,closure-sym ,@call-args ,g!rest))
                                    defs))
                       ))
                   ))
      `(let ,(mapcar 'first sym-pairs)
         (labels
             ,lbl-funcs
           (setf ,@(apply 'nconc sym-pairs)))
         (add-actor-methods ',class-name ',(mapcar 'method-signature meth-defs))
         ,@meth-defs)
      )))

(defmacro define-actor (class-name superclasses slots &rest class-options)
  (let* ((methods    (assoc :methods class-options))
         (meth-list  (cadr methods)))
    (unless methods
      (error "Section (:METHODS (....)) is missing"))
    `(progn
       (%define-actor-class ,class-name ,superclasses ,slots ,@(remove methods class-options))
       (%define-actor-methods ,class-name ,meth-list))
    ))

;; -------------------------------------------------------------------

#|
(define-actor-class diddly ()
  (a b c))

(define-actor-methods diddly
  ((do-k1 (me x &key (y 15))
          (1+ x))
   (do-k2 (me x)
          (1- x))))

(let ((x (make-instance 'diddly)))
  (do-k1 x 15)
  (do-k2 x 16)
  (ask x 'do-k1 x 20))

|#

;; -----------------------------------------
#|
(defun rewrite-monitor-clauses (actor-name body)
  ;; collect embedded DEFUNs and continuation names
  ;; rewriting the containing body to elide these defuns,
  ;; and collect them, rewritten to call continuations.
  (um:nlet-tail iter ((body      body)
                      (handlers  nil)
                      (assigns   nil)
                      (c-names   nil)
                      (defs      nil)
                      (new-body  nil))
    (if (endp body)
        (values
         c-names
         defs
         (if handlers
             `(labels
                  ,handlers
                (let ((*current-actor* ,actor-name))
                  (setf ,@assigns))
                ,@new-body)
           ;; else
           new-body))
      ;; else
      (destructuring-bind (hd . tl) body
        (if (consp hd)
            (case (car hd)
              (( let flet labels locally )
               (destructuring-bind (def bindings &rest body) hd
                 (multiple-value-bind (sub-c-names sub-defs sub-body)
                     (rewrite-monitor-clauses actor-name body)
                   (iter tl
                         handlers
                         assigns
                         (nconc sub-c-names c-names)
                         (nconc sub-defs defs)
                         `(,@new-body
                           (,def ,bindings
                                 ,sub-body))))))

              (( progn )
               (destructuring-bind (def &rest body) hd
                 (multiple-value-bind (sub-c-names sub-defs sub-body)
                     (rewrite-monitor-clauses actor-name body)
                   (iter tl
                         handlers
                         assigns
                         (nconc sub-c-names c-names)
                         (nconc sub-defs defs)
                         `(,@new-body
                           (,def
                            ,sub-body))))))

              (( defun )
               (destructuring-bind (def name args &rest body) hd
                 (let ((c-name (make-symbol (concatenate 'string
                                                         (string :c-)
                                                         (string name)))))
                   (iter tl
                         (cons `(,name ,args ,@body) handlers)
                         (list* c-name `(=cont #',name) assigns)
                         (cons c-name c-names)
                         (cons `(,def ,name ,args
                                      ,(apply 'assemble-funcall c-name args))
                               defs)
                         new-body))))
              
              (otherwise
               (iter tl
                     handlers
                     assigns
                     c-names
                     defs
                     `(,@new-body ,hd)))
              )
          ;; else
          (iter tl
                handlers
                assigns
                c-names
                defs
                `(,@new-body ,hd))))
      )))

(defmacro hoare-monitor (name &body body)
  ;; Encapsulate a collection of DEFUNS which have strictly serialized
  ;; access to shared state. Shared state would be defined around the
  ;; monitor, as with surrounding LET bindings.
  ;;
  ;; E.g., If DOIT is the name of one of the defuns, it operates
  ;; asynchronously and silently. If you need a response from it you
  ;; can ASK the monitor with the function and args. E.g.,
  ;;
  ;;   (hoare-monitor diddly
  ;;     (defun doit (x)
  ;;       (1+ x))
  ;;
  ;;  (ask diddly 'doit 15)  ;; => 16
  ;;
  ;; This is, in essence, an Actor. But rather than SEND messages to
  ;; it for its actions, we just call normal functions whieh vector
  ;; through a collection of continuations for that Actor.
  (multiple-value-bind (c-names defs new-body)
      (rewrite-monitor-clauses name body)
    `(let (,@c-names)
       (defvar ,name (make-actor
                      (lambda (k &rest args)
                        (apply k args))))
       ,@new-body
       ,@defs)
    ))

#+:LISPWORKS
(editor:setup-indent "hoare-monitor" 1)

#|
 So a Hoare Monitor (Actor) written as

 (hoare-monitor diddly
   (let ...bindings...
     (labels ...functions...
       (defun doit (...args...)
         ....)
       (defun didit (...args...)
         ...)
       ...
       )))

 will be rewritten as

 (let (#:c-doit #:c-didit)
   (defvar diddly (make-actor
                   (lambda (k &rest args)
                     (apply k args))))
   (let ...bindings...
     (labels ...functions...
       (labels
           ((doit (...args...)
              ....)
            (didit (...args...)
              ...))
         (let ((*current-actor* diddly))
           (setf #:c-doit (=cont #'doit)
                 #:c-didit (=cont #'didit)))
         )))
   (defun doit (...args...)
     (funcall/apply #:c-doit args...))
   (defun didit (...args...)
     (funcall/apply #:c-didit args...)) )

 But, !!WARNING!!, do not attempt to use any defining macro other than
DEFUN to describe Actor handlers. They will not be properly detected
by the current implementation.

Similarly, using any other local definer macros beyond LOCALLY, LET,
FLET, LABELS, and PROGN, will subverst our detection of Actor handler
definitions.

While this form of Actor construction seems convenient for client code
(i.e., simple direct calls instead of message sends), it can only be
used for single instance Actors, and it is difficult to serialize
access to the Actor across a neetwork.
 |#

;; ----------------------------------------------------------
;;
;; There seem to be amazingly few places where a simple Hoare Monitor
;; is warranted. You need a single shared resource where functions
;; apply only to that instance.
;;
;; But most often the exlusion applies to state data, not the
;; functions themselves. And there can be many different instances of
;; similar state data. The functions need to apply separately to each
;; instance.
;;
;; Hence we try to find a way to express a similar form but based on a
;; state data structure...

|#


(defmacro define-actor-class (class-name superclasses slots &rest class-options)
  "Define a class of Actors:

     (DEFINE-ACTOR-CLASS <class-name> (<superclasses>*)
       (<slot-definition>*)
       (:DEFAULT-INITARGS ...)
       (:INITARGS (<keyword-arg>*))
       (:BINDINGS (<let-binding>*))
       (:INITIAL  (<form>*))
       (:METHODS  ((<name> (<arg>*) <body-form>*)*))
       (:DOCUMENTATION ...)
       (:METACLASS ...))

The INITARGS are additional initargs that are in scope during INITIALIZE-INSTANCE :AFTER.
The BINDINGS are LET* formed, and are performed in scope of the INITARGS.
The INITIAL and METHODS are in the scope of the INITARGS and BINDINGS.
The Actor instance is (CURRENT-ACTOR) while performing the BINDINGS and INITAL forms.
The INITARGS and BINDINGS are encapsulated by the Actor closure, and not directly visible to
introspection, unlike the class slots. Only the Actor can modify.

Within the running Actor, function SUPER can be used to dispatch messages to superclass handlers.
Any unhandled messages are passed along to superclass handlers.
"
  (flet ((get-group (key)
           (let ((cell (assoc key class-options)))
             (setf class-options (remove cell class-options))
             (cadr cell))))
    (let* ((g!actor         (make-symbol (string 'actor)))
           (g!rest          (make-symbol (string 'rest)))
           (g!super         (make-symbol (string 'super-handler)))
           (methods         (get-group :methods))
           (entry-fns       (reduce (lambda (method defs)
                                      (let ((method-name (car method)))
                                        (if (or (not (symbolp method-name))
                                                (member method-name '(nil t)))
                                            defs
                                          ;; else
                                          (cons `(defmethod ,method-name ((,g!actor ,class-name) &rest ,g!rest)
                                                   (apply 'send ,g!actor ',method-name ,g!rest))
                                                defs))))
                                    methods
                                    :from-end t
                                    :initial-value nil))
           (needs-default   (not (find 't methods :key 'first)))
           (methods         `(,@methods
                              ,@(when needs-default
                                  `((t (&rest ,g!rest)
                                       (apply #'super ,g!rest))))))
           (base-class-name (um:symb "%" class-name :-base))
           (initargs        (get-group :initargs))
           (initarg-names   (mapcar (lambda (arg)
                                      ;; account for defaulted and/or
                                      ;; aliased keyword args
                                      (if (consp arg)
                                          (let ((arg (car arg)))
                                            (if (consp arg)
                                                (second arg)
                                              arg))
                                        arg))
                                    initargs))
           (bindings        (get-group :bindings))
           (initial         (get-group :initial))
           (documentation   (list (get-group :documentation))))
      `(progn
         (defclass ,base-class-name (,@superclasses actor)
           ,slots ,@class-options)
         (defmethod initialize-instance :after ((,g!actor ,base-class-name) &key ,@initargs &allow-other-keys)
           (declare (ignorable ,@initarg-names))
           (let* ((*current-actor* ,g!actor) ;; for benefit of BECOME
                  ,g!super
                  ,@bindings)
             (labels ((super (&rest msg)
                      (apply ,g!super msg))
                      ,@methods)
               (setf ,g!super (become
                               ,(dlam::dlambda*-actors-helper methods)))
               ;; now everything is set up and ready
               ;; initial can do anything it needs
               ,@initial)))
         (defclass ,class-name (,base-class-name)
           ()
           ,@documentation)
         ,@entry-fns)
      )))

(defclass actor-must-handle (actor)
  ())

(defmethod initialize-instance :after ((actor actor-must-handle) &key &allow-other-keys)
  (setf (actor-user-fn actor) (lambda (&rest msg)
                                (error "Unhandled message: ~A ~A" actor msg))
        ))

;; ----------------------------------------------------------------------------
#|
(define-actor-class diddly ()
  ()
  (:initargs  (a (b 3) c))
  (:bindings  ((x 1)
               (y 2)))
  (:methods
   ((doit (x)
          (print x))
    (didit (x)
           (doit (1+ x)))
    )))
 |#

;; --------------------------------------------------------
;; Non-blocking =WAIT*
#|
(defun do-wait* (timeout errorp on-timeout fn cont)
  (if (current-actor)
      (spawn-worker
       (let ((cont       (=cont cont))
             (on-timeout (and on-timeout
                              (=cont on-timeout)))
             (errfn      (and errorp
                              (=cont #'error))))
         (lambda ()
           (let ((mbox (mp:make-mailbox)))
             (trampoline fn (lambda (&rest args)
                              (mp:mailbox-send mbox args)))                   
             (multiple-value-bind (ans ok)
                 (mp:mailbox-read mbox "In =WAIT*" timeout)
               (if ok
                   (apply cont ans)
                 (if on-timeout
                     (funcall on-timeout)
                   (if errorp
                       (funcall errfn (make-condition 'timeout)))
                   )))))))
    ;; else
    (cps:do-wait timeout errorp on-timeout fn cont)))

(defmacro =wait* (args (&key (timeout 60) (errorp t) on-timeout) expr &body body)
  ;; like =WAIT but leaves Actor open to continuations and other
  ;; messages (non-blocking)
  (cps:prep-wait 'do-wait* args timeout errorp on-timeout expr body))


#+:LISPWORKS
(progn
  (editor:setup-indent "=bind*" 2 2 4)
  (editor:setup-indent "=wait*" 3 2 4))
|#
#|
(=wait* (x) ()
    (doit)
  (print x))
|#



