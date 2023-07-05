;; serializer.lisp
;;
;; DM/RAL  2022/10/26 05:33:47
;; ----------------------------------

(defpackage #:com.ral.actors.serializer
  (:use #:common-lisp #:ac))

(in-package #:com.ral.actors.serializer)

;; ----------------------------------
;; Serializers are important enough to have their own source file.
;; Serializers in Actors-land are the logical equivalent of
;; (Non-Recursive) Locks in MPX land.
;;
;; --------------------------------------------------------------
;; *** WHAT IS A SERIALIZER? ***
;;
;; Serializers are gateways that allow you to ensure that only a
;; single logical thread of activity can be operating past the
;; Serializer gate.
;;
;; That single logical thread may (and probably does) represent the
;; actions of any number of different machine threads. But all of them
;; are working on behalf of the single logical thread of activity.
;;
;; Hence it is impossible for simultaneous parallel execution inside
;; of any guarded Actors, unless the actions of that Actor produce
;; independent sub-chains of activity that also happen to use the
;; guarded Actor, in which case they should also be entering via the
;; SERIALIZER gate.
;;
;; There are no "threads" at the Actor level. Yes, beneath the
;; surface, all Actors run on machine threads. And the logical
;; sequence of activity starting from one source Actor will find
;; itself switching across many different machine threads along the
;; way. But none of the Actors are aware of these machine threads.
;;
;; Instead, we speak of a logical thread of activity emanating from a
;; source Actor.  Any Actor that SENDs more than one message is
;; spawning new logical threads of activity. Each of those messages
;; can be simultaneously processed by machine threads running on
;; separate CPU cores.
;;
;; ----------------------------------------------------------------
;; *** WHEN TO USE A SERIALIZER? ***
;;
;; Actors live by the discipline of lock-free, purely functional code.
;; The only global mutation permitted in this inherently parallel
;; operating environment is the Behavior slot of an Actor, using
;; BECOME. And that is carefully coordinated by the Message
;; Dispatchers running in every Actor machine thread.
;;
;; And as long as you live by FPL you can freely have multiple
;; simultaneous threads all operating in the same Actor body, even
;; when the Actor performs a BECOME to change shared closure
;; parameters.
;;
;; And that is why you see us using Lisp REMOVE on Actor parameter
;; lists, instead of the mutating imperative DELETE. REMOVE does not
;; damage the shared list, so multiple threads can happily access it
;; in parallel.  It can only be changed for all to see, as a result of
;; a successful BECOME.
;;
;; However, sometimes you just can't be FPL pure, and must make
;; mutations that could be visible outside the running Actor instance.
;; A Hashtable in an Actor closure parameter list is a good example.
;;
;; All Actor instances runnning inside the body of the Actor view the
;; same closure parameters. Only one machine thread at a time can be
;; allowed to mutate the Hashtable, and it must happen while no other
;; thread is attempting to read the Hashtable. In MPX land, you would
;; use a LOCK. But in Actor land, we use a SERIALZER Gate.
;;
;; Once running inside an Actor body that is guarded by a SERIALIZER,
;; you can freely mutate globally visible items that are in the
;; guarded Actor - like directly mutating the Actor closure
;; parameters, e.g., a Hashtable. You are the only one running inside
;; that Actor body.
;;
;; But!! Bear in mind that if there is any possibilty for errors
;; arising, then all mutations must become reversed. The Actor state
;; after an error is expected to be the same as it was on entry.
;;
;; And you still cannot safely mutate anything outside of the Actor
;; body. That is why we have a preference for Actors containing useful
;; shared information, instead of using global vars. If something
;; shared needs occasional mutation, it is safer to make it happen
;; within an Actor body, with or without SERIALIZERs. We only need
;; SERIALIZERS when the mutation cannot happen solely via BECOME.
;;
;; Overall we recommend developing the habit of writing FPL pure code.
;; That way no irreversible changes will be made until BECOME commits
;; at exit of the Actor code. If retry becomes necessary, no state
;; will have been changed.
;;
;; When will retries happen? Whenver two parallel threads are running
;; in the same Actor behavior, and they both execute BECOME, then at
;; exit, only one of them can succeed in committing the BECOME. The
;; other will detect that the system has changed beneath itself, and
;; its message will be automatically retried. BECOME Contention.
;;
;; ----------------------------------------------------------------
;; *** ACTORS ARE LOCK-FREE.  SO, CAN DEADLOCKS BE ELIMINATED BY USING
;; ACTORS? ***
;;
;; Not quite: Full-out deadlocks are impossible in an Actors system.
;; But it is certainly possible to develop a logical livelock between
;; two or more logical threads of Actor activity.
;;
;; Suppose one Actor needs to use SERIALIZED resources A and B. And
;; another Actor neeeds to use SERIALIZED resources B and A.
;;
;; If they both enter their respective first serializers, then Actor 1
;; holds permissions for using resource A, while Actor 2 has
;; permission to use resource B.
;;
;; Now Actor 1 needs to gain resource B, but since it is in use by
;; another Actor, he gets enqueued in Serializer-B until the other
;; Actor finishes and sends a message back to Serializer-B.
;;
;; But that other Actor now tries to use resource A, and finds it in
;; use by another Actor, and so he gets enqueued until the first Actor
;; sends a message back to Serializer-A.
;;
;; And now both Actors are queued up, waiting for each other to send a
;; release message to the serializer where they are waiting. That will
;; never happen, and so the two logical threads of activity come to a
;; halt.
;;
;; And, further, resources A and B are now permanently off-limits to
;; all other Actors. Any other Actors wanting to use either resource
;; will find themselves enqueued, waiting for an event that will never
;; happen.
;;
;; Other, unrelated activities will still be running - hence LiveLock,
;; not DeadLock - but the section of the Actors population needing
;; resource A or B has now become logically blocked.
;;
;; So you need the same kind of discipline that you use with MPX LOCKS
;; - ordered acquisition of resources. Always first acquire resource
;; A, then resource B. As long as all Actors use the same ordered
;; discipline, they will not become logically livelocked.
;;
;; ---------------------------------------------------
;; ### VERY IMPORTANT! ###
;;
;; As just mentioned, once an Actor gets past a SERIALIZER gate, it is
;; obligated to send a message to its customer. That customer happens
;; to be through the SERIALZER gate, which interposes between the
;; running Actor and its actual customer. In that way, the SERIALIZER
;; can detect when it is safe to release another Actor in waiting.
;;
;; You should also realize, that once you send a message to the
;; customer, you are no longer the sole instance running in Actor
;; bodies.
;;
;; (Keep in mind: SENDs and BECOMEs never actually occur until the
;; successful exit of the Actor body. Until then, they are privately
;; staged for eventual commit, or discarded on unsuccessful exit.
;;
;; So the changes don't happen immediately, and they logically occur
;; at the same instant when you exit. You can't see them happen, and
;; neither can anyone else running in parallel with you.)
;;
;; The all important message SEND back to the customer might not
;; happen until many Actor blocks beyond the SERIALIZER. But somewhere
;; along that logical thread, a message must be sent to the customer.
;; Failing to do so results in a permanent logical blocking for any
;; other chains of activity that need to use the same resource.
;;
;; Unlike in CALL/RETURN architectures, we don't have an
;; UNWIND-PROTECT on which to rely. There is no scoping with Actor
;; executions. All Actors execute in the same toplevel environment. We
;; must exercise manual discipline.
;;
;; It is difficult to defensively program against all possible future
;; abuses of your Actor system. In most cases you will need to rely on
;; timeout mechanisms to help out. Just like in the real world...
;; -----------------------------------------------------------------

(defun serializer-beh (svc &key (timeout *timeout*))
  ;; Quiescent state - nobody in waiting, just flag him through, but
  ;; enter the busy state.
  (alambda
   ((cust . msg)
    (let* ((tag   (tag self))
           (lbl   (label tag cust))
           (svct  (timed-service svc timeout)))
      (send* svct lbl msg)
      (become (busy-serializer-beh svc timeout tag nil))
      ))
   ))

(defun busy-serializer-beh (svc timeout tag queue)
  ;; Busy state - new arriving messages get enqueued until we receive
  ;; a message through our interposed customer TAG.
  (alambda
   ((atag . reply) / (eql atag tag)
    (send* reply) ;; reply has been labeled with cust
    (if (emptyq? queue)
        (become (serializer-beh svc :timeout timeout))
      (multiple-value-bind (next-req new-queue) (popq queue)
        (destructuring-bind (next-cust . next-msg) next-req
          (let ((lbl  (label tag next-cust))
                (svct (timed-service svc timeout)))
            (send* svct lbl next-msg)
            (become (busy-serializer-beh svc timeout tag new-queue))
            )))
      ))
   ((cust . msg)
    (become (busy-serializer-beh svc timeout tag
                                 (addq queue (cons cust msg)))))
   ))

(defun serializer (svc &key (timeout *timeout*))
  (create (serializer-beh svc :timeout timeout)))

;; -----------------------------------------------------------

(defun serializer-sink (act)
  ;; Turn an actor into a sink. Actor must accept a cust argument,
  ;; and always send a response to cust - even though it appears to be
  ;; a sink from the caller's perspective.
  (label (serializer act) sink))


(defun safe-serializer (action &key (timeout *timeout*) on-timeout supv)
  ;; If timeout happens, by default the serializer will be unblocked
  ;; with message :TIMEOUT.
  ;;
  ;; If ON-TIMEOUT is specified it will instead be called with the
  ;; customer and message that was attempted. It is up to ON-TIMEOUT
  ;; to clear the way with the SERIALIZER, perform retries, shut down
  ;; the service, or whatever.. Rquests held in the SERIALIZER queue
  ;; will remain blocked until the SERIALIZER hears a response.
  (let ((wd (watchdog-timer action
                              :timeout    timeout
                              :on-timeout on-timeout
                              :supv       supv)))
    (values (serializer wd)
            wd)
    ))

;; ----------------------------------------------------------

#|
(defun tst (n)
  (labels ((doit-beh (&optional (n 0))
             (lambda (&rest _)
               (declare (ignore _))
               (let ((newct (1+ n)))
                 (send println newct)
                 (become (doit-beh newct))))))
    (let* ((dst (create (doit-beh)))
           (x (safe-serializer
               (α (cust)
                 (sleep 0.19999)
                 (send cust :ok))
               :timeout 0.2)
              ))
      (dotimes (ix n)
        (send x dst))
      )))
(tst 100)
|#
