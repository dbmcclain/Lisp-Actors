;; actors-futures.lisp -- WITH-FUTURE for Actors
;;
;; DM/RAL 12/19
;; ----------------------------------------------------------

(in-package :actors.base)

#|
 === Be careful! ===

Spinning off another thread to perform blocking actions with =BIND
leaves the Actor ready to handle new messages while awaiting the
continuation. That might mess up the single-thread semantics of the
Actor by potentially changing the internal state to something
incompatible with what the continuation expects.

In contrast, the use of RECV blocks all further message handling until
a proper response arrives and the matching RECV clause continues to
completion. All incoming messages that don't satisfy the RECV get
stashed for later processing after the RECV completes.

On the one hand, =BIND leaves us open to state misconfiguration due to
subseqent message handling during the continuation wait, which might
never arrive. On the other hand, RECV has the potential to block
further message processing forever, if a qualifying message never
arrives.
 |#

;; ----------------------------------------------------------
;; =CONT - turn a function into a continuation closure
;; ------------------------------------------
;; Create a callback on the function argument

(defun wrap-injector (fn)
  (if-let (actor (current-actor))
      (lambda (&rest args)
        (apply 'inject actor fn args))
    (lambda (&rest args)
      (with-trampoline
        (apply fn args)))
    ))

(lw:defadvice (=cont =cont-for-actors :around)
    (fn)
  ;;
  ;; If the callback originated from inside an Actor, we ensure
  ;; that it will later execute inside that Actor only when that
  ;; Actor is alive.
  ;;
  ;; Code inside an Actor should only be executing on one thread
  ;; at a time, in order to preserve SMP single-thread semantics.
  ;;
  (wrap-injector (lw:call-next-advice fn)))

;; ------------------------------------------------------------------
;;
;; If =BIND called within an Actor, and the binding form does not
;; spawn anything, then then continuation will be peformed directly by
;; the calling Actor, using =VALUES within the binding form.
;;
;; If the =BIND occured in an ASK, then the continuation will
;; re-invoke the ASK from the top and also re-instantiate all handlers
;; before performing the continuation body code. The subsequent
;; (SIGNAL 'NO-IMMEDIATE-ANSWER), which follows the binding form, will
;; never be performed due to the restart of the ASK.
;;
(lw:defadvice (=bind =bind-for-asking-actors :around)
    (call-form env)
  (destructuring-bind (_ args expr . body) call-form
    (declare (ignore _))
    (lw:call-next-advice `(=bind ,args
                              (prog1
                                  ,expr
                                (signal 'no-immediate-answer))
                            ,@body)
                         env)
    ))

(defmacro =async (&body body)
  `(spawn-worker (=lambda ()
                   ,@body)
                 =bind-cont))

(defmacro =async/err (&body body)
  ;; umm... not quite... what recovers the answer?
  `(=async (=values (capture-ans-or-exn
                      ,@body))))

;; -------------------------------------------------------
;; Using formal FUTURES

(lw:defadvice (=fut =fut-for-actors :around)
    (fn)
  ;;
  ;; If the callback originated from inside an Actor, we ensure
  ;; that it will later execute inside that Actor only when that
  ;; Actor is alive.
  ;;
  ;; Code inside an Actor should only be executing on one thread
  ;; at a time, in order to preserve SMP single-thread semantics.
  ;;
  (wrap-injector (lw:call-next-advice fn)))

(defstruct future
  forced
  val)

(defun call-future (fn)
  (let ((mb (mp:make-mailbox :size 1)))
    (spawn-worker (lambda ()
                    (mp:mailbox-send mb
                                     (um:call-capturing-ans-or-exn fn))))
    (make-future
     :val mb)))

(defmacro future (&body body)
  `(call-future (lambda ()
                  ,@body)))

(defun %forced-future-val (mb &key (timeout *timeout*) (errorp t) on-timeout)
  (multiple-value-bind (ans ok)
      (mp:mailbox-read mb "Waiting on FUTURE" timeout)
    (if ok
        ans
      (if errorp
          (error 'timeout)
        (when on-timeout
          (funcall on-timeout))))
    ))

(defgeneric force (fut &key &allow-other-keys)
  (:method (obj &key &allow-other-keys)
   obj)
  (:method ((fut future) &rest args &key (timeout *timeout*) (errorp t) on-timeout &allow-other-keys)
   (declare (ignore timeout errorp on-timeout))
   (with-slots (forced val) fut
     (recover-ans-or-exn
      (if forced
          val
        (setf forced t
              val    (apply #'%forced-future-val val args))))
     )))
  
(defun call-forcing (fn fut &rest args &key (timeout *timeout*) (errorp t) on-timeout)
  (declare (ignore timeout errorp on-timeout))
  (multiple-value-call fn (apply #'force fut args)))

(defmacro =fwait ((args &key (timeout *timeout*) (errorp t) on-timeout) fut &body body)
  `(call-forcing (lambda ,args
                   ,@body)
                 ,fut
                 :timeout    ,timeout
                 :errorp     ,errorp
                 :on-timeout ,on-timeout))
  
