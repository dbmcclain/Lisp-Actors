
(defpackage #:reppy-channels
  (:nicknames #:rch)
  (:use :common-lisp :timeout)
  (:import-from #:useful-macros
   #:when-let
   #:dynamic-wind
   #:proceed
   #:capture-dynamic-environment
   #:with-dynamic-environment
   #:call-with-dynamic-environment
   #:foreach
  )
  (:export
   #:make-channel
   
   #:send
   #:recv
   #:peek
   #:poke
   #:select
   #:select*
   
   #:sync
   #:choose
   #:choose*
   #:on-sync
   #:wrap
   #:on-abort
   #:wrap-handler
   #:wrap-timeout
   
   #:sendEvt
   #:recvEvt
   #:alwaysEvt
   #:neverEvt
   #:timerEvt
   #:failEvt
   #:timeoutEvt
   #:execEvt
   
   #:success
   #:failure
   ))

(in-package :rch)

;; ---------------------------------------------------------------------

(defun join-thread (thread)
  #+:LISPWORKS (mp:process-join thread)
  #+:SBCL      (sb-thread:join-thread thread))

;; ---------------------------------------------
;; Timer Support

(defun make-timer (fn &rest args)
  #+:LISPWORKS (apply #'mp:make-timer fn args)
  #+:SBCL      (sb-ext:make-timer (lambda ()
                                    (apply fn args))))

(defun schedule-timer (timer dt &key absolute)
  #+:LISPWORKS
  (if absolute
      (mp:schedule-timer timer dt)
    (mp:schedule-timer-relative timer dt))

  #+:SBCL
  (sb-ext:schedule-timer timer dt :absolute absolute)) ;; fix me!!

(defun unschedule-timer (timer)
  #+:LISPWORKS (mp:unschedule-timer timer)
  #+:SBCL      (sb-ext:unschedule-timer timer))

;; -------------------------------------------------------
;; Reppy CML Style Channels and Events with Combinators

(defun spawn-process (fn &key args name)
  ;; For when you really want to spawn another thread
  (apply #'mp:process-run-function (symbol-name (gensym (or name "RCH:spawn-"))) nil
         fn args))

;; ----------------------------------------------------------------------
;; CHANNEL-QUEUE -- lock free queues

(defstruct (channel-queue
            (:include ref:ref)))

(defmethod enqueue-bev ((chq channel-queue) bev)
  (um:rmw chq
          (lambda (bevs)
            ;; this function can be called repeatedly, hence must
            ;; appear idempotent - no lasting side effects
            (cons bev
                  ;; we need to trim away the already rendezvous
                  ;; tuples
                  (remove-if (lambda (bev)
                               (marked? (bev-comm bev)))
                             bevs)))
          ))

(defmethod get-bevs ((chq channel-queue))
  ;; return list in FIFO order
  (reverse (um:rd chq)))

(defmethod reset-queue ((chq channel-queue))
  (um:wr chq nil))

;; ----------------------------------------------------------------------
;; CHANNEL -- the object of a rendezvous between threads. Channel
;; objects are shared between threads. We use lock-free queues for
;; this.

(defstruct channel
  (readers (make-channel-queue))
  (writers (make-channel-queue)))

;; -------------------------------

(defstruct evt
  (init #'lw:do-nothing) ;; init allows for reussable event trees
  poll
  success
  failure
  async  ;; async polling when true
  data)  ;; sendEvt has this data

(defun init (evt)
  (funcall (evt-init evt)))

(defun poll (comm evt)
  (funcall (evt-poll evt) comm))

(defun success (comm evt)
  (funcall (evt-success evt) comm))

(defun failure (evt)
  (funcall (evt-failure evt)))

;; -----------------------------------------

(define-condition success ()
  ((val  :accessor success-val :initarg :val)))

(define-condition failure ()
  ())

;; -----------------------------------------------------------------------
;; COMM-CELL -- one of these belongs to each thread attempting a
;; rendezvous.  These cells may become enqueued on pending
;; reader/writer queues in channels.  And with composable events the
;; cell may become enqueued on more than one channel, which any one of
;; them might successfully rendezvous.
;;
;; The first successful rendezvous claims the day. So on an SMP
;; processor we need to grab claims using atomic operators. We could
;; also use locks, but that seems too heavy handed.

(defstruct (comm
            (:include mcas:mcas-ref))
  ;; NIL if not yet performed, will contain the leaf BEV which fired
  ;; against this cell. I.e., in which leaf did the rendevouz occur?
  ;; Note: nowhere in this object is there any indication of what
  ;; counterparty was involved in the rendezvous. (Good for security.
  ;; Necessary?)
  
  ;; needs-wait will be true if we are ever placed on a R/W queue and
  ;; need to wait for a rendezvous.
  (needs-wait t)
  
  ;; calling proc, some way to reach the thread that owns this object.
  (owner  (mp:make-mailbox))
  
  ;; the comm data value
  data)

;; ------------------

(defun mark-async (comm async)
  ;; an async event means we don't need to wait
  (when async
    (setf (comm-needs-wait comm) nil)))

;; ====================================================================

(defun mark (comm evt)
  ;; the comm might also be on other channels and might have been already marked
  ;; returns t if successfully marked, nil otherwise
  (declare (comm comm))
  (ref:cas comm nil evt))

(defun marked? (comm)
  ;; return non-nil if already marked
  (declare (comm comm))
  (ref:val comm))

(defun mark-pair (comm1 evt1 comm2 evt2)
  ;; return true if both comm cells could be marked
  (mcas:mcas comm1 nil evt1
             comm2 nil evt2))

;; -----------------------------------------

(defstruct bev
  ;; joins together an event with a thread comm object
  evt comm)

(defun prod-owner (comm evt)
  (mp:mailbox-send (comm-owner comm) evt))

(defun do-polling (rendezvous-fn queue my-bev)
  (declare
   (function      rendezvous-fn)
   (channel-queue queue)
   (bev           my-bev))
  ;; Scan a queue for an eligbible tuple and discard marked tuples
  ;; from the queue. An eligible tuple may become marked from
  ;; eligible-p. This version avoids consing.
  (let ((my-evt  (bev-evt  my-bev))
        (my-comm (bev-comm my-bev)))
    (flet
        ((try-rendezvous (other-bev)
           (declare (bev other-bev))
           (with-accessors ((other-comm  bev-comm)
                            (other-evt   bev-evt)) other-bev
             (unless (eq my-comm other-comm) ;; can't rendezvous with ourself
               (when (mark-pair my-comm    my-evt
                                other-comm other-evt)
                 (funcall rendezvous-fn other-bev)
                 (unless (evt-async other-evt)
                   (prod-owner other-comm other-evt))
                 ))
             (marked? my-comm))))
      (declare (dynamic-extent #'try-rendezvous))
      (some #'try-rendezvous (get-bevs queue))
      )))

;; -------------------------------------------------------

(defun recvEvt (ch &key async)
  (let ((env (capture-dynamic-environment))
        this-evt)
    (setf this-evt (make-evt
                    :async   async
                    :poll    (lambda (comm)
                               (mark-async comm async)
                               (let ((my-bev (make-bev
                                              :comm comm
                                              :evt  this-evt)))
                                 (enqueue-bev (channel-readers ch) my-bev)
                                 (labels
                                     ((rendezvous (bev)
                                        (setf (comm-data comm) (evt-data (bev-evt bev)))))
                                   (do-polling #'rendezvous (channel-writers ch) my-bev)
                                   )))
                    :success (lambda (comm)
                               (let ((val (comm-data comm)))
                                 (with-dynamic-environment (env)
                                   (signal 'success :val val)
                                   val)))
                    :failure (lambda ()
                               (with-dynamic-environment (env)
                                 (signal 'failure)))
                    ))))

(defun sendEvt (ch val &key async)
  (let ((env (capture-dynamic-environment))
        this-evt)
    (setf this-evt (make-evt
                    :data    val
                    :async   async
                    :poll    (lambda (comm)
                               (mark-async comm async)
                               (let ((my-bev (make-bev
                                              :comm comm
                                              :evt  this-evt)))
                                 (enqueue-bev (channel-writers ch) my-bev)
                                 (labels
                                     ((rendezvous (bev)
                                        (setf (comm-data (bev-comm bev)) val)))
                                   (do-polling #'rendezvous (channel-readers ch) my-bev)
                                   )))
                    :success (lambda (comm)
                               (declare (ignore comm))
                               (with-dynamic-environment (env)
                                 (signal 'success :val val)
                                 val))
                    :failure (lambda ()
                               (with-dynamic-environment (env)
                                 (signal 'failure)))
                    ))))

(defun alwaysEvt (val)
  (let ((env (capture-dynamic-environment))
        this-evt)
    (setf this-evt (make-evt
                    :poll    (lambda (comm)
                               ;; might already be marked
                               (mark comm this-evt))
                    :success (lambda (comm)
                               (declare (ignore comm))
                               (with-dynamic-environment (env)
                                 (signal 'success :val val)
                                 val))
                    :failure (lambda ()
                               (with-dynamic-environment (env)
                                 (signal 'failure)))
                    ))))

(defun neverEvt ()
  ;; success will never be called on neverEvt because these can never
  ;; be emplaced into the comm-marker.
  (let ((env (capture-dynamic-environment)))
    (make-evt
     :poll    #'lw:do-nothing
     :failure (lambda ()
                (with-dynamic-environment (env)
                  (signal 'failure)))
     )))

;; -----------------------------------------

(defun shuffle-evts (&rest evts)
  #F
  (declare (list evts))
  (let ((rnd  (random (ash 1 (length evts))))
        hd tl
        (ix 0))
    (declare (integer rnd)
             (list hd tl)
             (fixnum ix))
    (dolist (evt evts (nconc hd tl))
      (if (logbitp ix rnd)
          (push evt hd)
        (push evt tl))
      (incf ix))
    ))

(defun no-reorder (&rest evts)
  evts)

;; -----------------------------------------------------

(defun choiceEvt (order-fn &rest evts)
  ;; choose evts are never emplaced into the comm-marker
  ;; so success will never be called on them. But failure will be called.
  (make-evt
   :init    (lambda ()
              (foreach #'init evts))
   :poll    (lambda (comm)
              (some (lambda (evt)
                      (poll comm evt)
                      (marked? comm))
                    (apply order-fn evts)))
   :failure (lambda ()
              ;; provide breadth-first failure handling
              (foreach #'failure evts))
   ))

(um:eval-always
  (defun compile-chooser (order-fn &rest evts)
    `(choiceEvt #',order-fn
                ,@(mapcar (lambda (evt)
                            ;; this stops the upward propagation of
                            ;; failure on each individual evt
                            `(dynamic-wind
                               (handler-case
                                   (proceed ,evt)
                                 (failure ())
                                 )))
                          evts)
                ;; this neverEvt propagates failure to higher levels
                (neverEvt))))

(defmacro choose (&rest evts)
  (apply #'compile-chooser 'shuffle-evts evts))

(defmacro choose* (&rest evts)
  (apply #'compile-chooser 'no-reorder evts))

;; -------------------------------------------------------
;; ON-SYNC -- formerly called GUARD A function that is invoked at SYNC
;; time to produce an EVENT object, but only if polled during SYNC.

(defun do-on-sync (fn)
  ;; guard events are never emplaced into the comm-marker
  ;; so success will never be called on them. But failure will be called.
  (let ((env (capture-dynamic-environment))
        evt)
    (make-evt
     :init    (lambda ()
                (setf evt nil))
     :poll    (lambda (comm)
                ;; in a choose event, this polling might never be
                ;; called.
                (setf evt (call-with-dynamic-environment env fn))
                (init evt)
                (poll comm evt))
     :failure (lambda ()
                (when evt
                  (failure evt)))
     )))

(defmacro on-sync (&body body)
  `(do-on-sync (lambda ()
                 ,@body)))

;; ----------------------------------------------------------

(defmacro wrap ((ans evt) &body body)
  ;; A more Lisp-centric approach.
  ;;   (WRAP (var evt) &body clauses* {:on-abort abort-clause} {:handlers handler-clauses*})

  ;; Main body clauses are performed only after the event rendezvous.
  ;; The abort-clause is performed only if no rendezvous on the event.

  ;; Handlers (if any) are active during the rendezvous of the event -
  ;; i.e., during inner wraps as well as during the body clauses of
  ;; the this wrap. They are also active during the abort-clause and
  ;; any inner abort clauses
  ;;
  ;; So the overall effect of WRAP is:
  ;;   (cml:wrap-handlers
  ;;      (cml:wrap
  ;;        (cml:wrap-abort evt
  
  (lw:with-unique-names (cx)
    (let (rendezvous-clauses
          abort-clauses
          handlers)
      (um:nlet-tail iter ((clauses body))
        (unless (endp clauses)
          (destructuring-bind (hd . tl) clauses
            (cond ((eq hd :on-abort)
                   (push (car tl) abort-clauses)
                   (iter (cdr tl)))
                  ((eq hd :handlers)
                   (setf handlers tl))
                  (t
                   (push hd rendezvous-clauses)
                   (iter tl))
                  ))))
      (labels
          ((success-body ()
             (lw:with-unique-names (res)
               `(let ((,res (funcall (lambda (,ans)
                                       ,@(nreverse rendezvous-clauses))
                                     (success-val ,cx))))
                  (signal 'success :val ,res) ;; propagate success upward
                  ,res) ;; if no success handlers
               ))
           
           (wrap-body ()
             (cond
              (abort-clauses
               (lw:with-unique-names (trap-abort event comm)
                 `(let (,trap-abort)
                    (dynamic-wind
                      (handler-case
                          (proceed
                           (let ((,event ,evt))
                             (make-evt
                              :init    (lambda ()
                                         (init ,event)
                                         (setf ,trap-abort t))
                              :poll    (lambda (,comm)
                                         (poll ,comm ,event))
                              :failure (lambda ()
                                         (failure ,event))
                              )))
                        
                        (success (,cx)
                          (setf ,trap-abort nil) ;; neutrailize abort-clause
                          ,@(if rendezvous-clauses
                                `(,(success-body))
                              ;; else
                              `((signal ,cx) ;; propagate success upward
                                (success-val ,cx)) ;; if no success handlers
                              ))
                        
                        (failure (,cx)
                          (when (shiftf ,trap-abort nil) ;; once-only execution
                            ,@(nreverse abort-clauses)
                            (signal ,cx))) ;; propagate failure upward
                        )))
                 ))
                   
              (rendezvous-clauses
               `(dynamic-wind
                  (handler-case
                      (proceed ,evt)
                    
                    (success (,cx)
                      ,(success-body))
                    )))
              
              (t
               evt)
              )))
        
        (if handlers
            `(dynamic-wind
               (handler-case
                   (proceed ,(wrap-body))
                 ,@handlers))
          ;; else
          (wrap-body))
        ))))

(defmacro on-abort (evt &body body)
  ;; ON-ABORT -- a Lisp-centric version of WRAP-ABORT
  `(wrap (_ ,evt)
     :on-abort (progn
                 ,@body)))

(defmacro wrap-handler (evt &rest handlers)
  `(wrap (_ ,evt)
     :handlers ,@handlers))

(defmacro failEvt (evt)
  `(dynamic-wind
     (handler-case
         (proceed ,evt)
       
       (success ()
         (signal 'failure))
       )))

(defun execEvt (fn &rest args)
  ;; an execEvt always succeeds, but might not get called in a choice
  (wrap (_ (alwaysEvt t))
    (declare (ignore _))
    (apply fn args)))

;; -----------------------------------------

(defun sync (evt)
  (let ((comm (make-comm)))
    (init evt)
    (poll comm evt)
    (unwind-protect
        (let ((an-evt (or (marked? comm)
                          (when (comm-needs-wait comm)
                            (handler-case
                                (um:read-mailbox-with-timeout (comm-owner comm)
                                                              :timeout *timeout*
                                                              :errorp  t)
                              (timeout (c)
                                (or (marked? comm)
                                    (error c)))
                              )))))
          (when (evt-p an-evt)
            (success comm an-evt)))
      ;; unwind clause
      (failure evt))
    ))

;; ---------------------------------------------------------

(defmacro select (&rest evts)
  `(sync (choose ,@evts)))

(defmacro select* (&rest evts)
  `(sync (choose* ,@evts)))
        
;; ---------------------------------------------------------

(defun send (ch item)
  (sync (sendEvt ch item)))

(defun recv (ch)
  (sync (recvEvt ch)))

;; -------------------------------------------

(defun poke (ch item)
  ;; send the item, but don't wait for a matching recv
  (sync (sendEvt ch item :async t)))

(defun peek (ch)
  (sync (recvEvt ch :async t)))

;; ---------------------------------------------------------

(defmethod ac:send ((ch channel) &rest message)
  ;; non-blocking semantics for Actor send
  (poke ch message))

;; --------------------------------------------------

(defun timerEvt (dt &key absolute)
  ;; an event that rendezvous with a timer
  ;; dt will be a relative time unless :absolute is T
  (check-type dt (or null
                     (real 0.0)))
  (cond ((null dt) ;; no time indicated -- just form a nop event
         (neverEvt))
        
        ((not (plusp dt))
         (alwaysEvt t)) ;; should already have fired
        
        (t ;; anything else is a real timer rendezvous
           (on-sync
            (let* ((ch     (make-channel))
                   (timer  (make-timer #'mp:funcall-async #'poke ch t)))
              (schedule-timer timer dt :absolute absolute)
              (on-abort (recvEvt ch)
                (unschedule-timer timer))
              )))
        ))

;; ----------------------------------------------------------------------------------
;; Events that could generate errors if chosen

#| ;; say What?!
(defun wrap-error (ev errfn)
  ;; An event that fires an error if successful rendezvos. It also
  ;; acts like a failed rendezvous. Errfn should be prepared to accept
  ;; one argument, which is the result of the ev rendezvous.
  (failEvt
   (wrap ev
         (lambda (v)
           (error (funcall errfn v)))
         )))

(defun errorEvt (errfn)
  ;; An event that always fails with an error. Doing it this way,
  ;; instead of just presenting an immediate error allows the error to
  ;; be bypassed if this event is never chosen. The error in the
  ;; wrapped function only occurs on successful rendezvous with this
  ;; event.
  (wrap-error (alwaysEvt nil)
              errfn))
|#

(defun timeoutEvt (dt)
  ;; an event that produces a timeout error
  (wrap (_ (timerEvt dt))
    (declare (ignore _))
    (error 'timeout)))

(defun wrap-timeout (ev dt)
  (choose* ev
           (timeoutEvt dt)))


;; -------------------------------------------------------------------------------------
#|
(defun tst ()
  ;; randomly rendezvous with one of two threads, sending an abort to
  ;; the other thread
  (let* ((ch12  (make-channel))
         (ch12a (make-channel)) ;; abort chan
         (ch13  (make-channel))
         (ch13a (make-channel))) ;; abort chan
    (ac:pr "----------------------------------")
    (ac:spawn-worker
     (lambda ()
       (select (wrap (ans (recvEvt ch12))
                 (ac:pr (format nil "t2 got ~S" ans)))
               (wrap (_ (recvEvt ch12a))
                 (declare (ignore _))
                 (ac:pr :t2-fail)))))
    (ac:spawn-worker
     (lambda ()
       (select (wrap (ans (recvEvt ch13))
                 (ac:pr (format nil "t3 got ~S" ans)))
               (wrap (_ (recvEvt ch13a))
                 (declare (ignore _))
                 (ac:pr :t3-fail)))))
    (sleep 0.5)
    (select
     (wrap (_ (sendEvt ch12 :one-two))
       (declare (ignore _))
       (ac:pr :sent12)
       
       :on-abort (poke ch12a t))
     
     (wrap (_ (sendEvt ch13 :one-three))
       (declare (ignore _))
       (ac:pr :sent13)

       :on-abort (poke ch13a t)))
    (values)
    ))

(tst)

(let ((ch (make-channel)))
  (ac:spawn-worker
   (lambda ()
     (ac:pr (list :worker (sync (recvEvt ch))))))
  (sync (wrap (val
               (wrap (val
                      (wrap (val (sendEvt ch 15))
                        (ac:pr (list :thread val))) )
                 (ac:pr (list :outer val))) )
          (ac:pr (list :outer-outer val)))
        ))

(let ((ch (make-channel)))
  (ac:pr (list "top" 
               (sync (wrap (x
                            (on-abort
                                (on-abort (choose*
                                           (alwaysEvt 32)
                                           (on-abort (recvEvt ch)
                                             (ac:pr "inner recvEvt abort"))
                                           (on-abort (failEvt (alwaysEvt 15))
                                             (ac:pr "inner failEvt abort")))
                                  (ac:pr "inner-outer choose abort"))
                              (ac:pr "outer-outer choose abort")))
                       (+ x 1))
                     ))))

(ac:pr (list "top"
             (sync (wrap (x (failEvt (alwaysEvt 15)))
                     (1+ x)
                     :on-abort (ac:pr "fail"))
                   )))

(ac:pr (list "top"
             (sync (wrap (x (wrap-timeout (neverEvt)
                                          2))
                     (1+ x)
                     :on-abort (ac:pr "fail"))
                   )))

|#
