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
;; lists, instead of the imperative DELETE. REMOVE does not damage the
;; shared list, so multiple threads can happily access it in parallel.
;; It can only be changed for all to see, as a result of a successful
;; BECOME.
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
;; But you still cannot safely mutate anything outside of the Actor
;; body. And that is why we have a preference for Actors containing
;; useful shared information, instead of using global vars. If
;; something shared needs occasional mutation, it is safer to make it
;; happen within an Actor body, with or without SERIALIZERs. We only
;; need SERIALIZERS when the mutation cannot happen solely via BECOME.
;;
;; ----------------------------------------------------------------
;; *** ACTORS ARE LOCK-FREE.
;;       SO, CAN DEADLOCKS BE ELIMINATED BY USING ACTORS? ***
;;
;; Not quite: It is certainly possible to develop a logical deadlock
;; between two logical threads of Actor activity. Suppose one Actor
;; needs to use SERIALIZED resources A and B. And another Actor neeeds
;; to use SERIALIZED resources B and A.
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
;; never happen, and so the two logical threads of activity have come
;; to a halt, and resources A and B are now permanently off-limits to
;; all other Actors as well. Any other Actors wanting to use either
;; resource will find themselves enqueued, waiting for an event that
;; will never happen.
;;
;; Other, unrelated activities will still be running, but the section
;; of the population needing resource A or B has now become logically
;; blocked.
;;
;; So you need the same kind of discipline that you use with MPX LOCKS
;; - ordered acquisition of resources. Always first acquire resource
;; A, then resource B. As long as all Actors use this same discipline,
;; they will not become logically deadlocked.
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
;; at the same instant. You can't see them happen, and neither can
;; anyone else running in parallel with you.)
;;
;; That message SEND back to the customer might not happen until many
;; Actor blocks beyond the SERIALIZER. But somewhere along that
;; logical thread, a message must be sent to the customer. Failing to
;; do so results in a permanent logical blocking for any other chains
;; of activity that need to use the same resource.
;;
;; Unlike in CALL/RETURN architectures, we don't really have an
;; UNWIND-PROTECT on which to rely. We must exercise manual
;; discipline. The Dynamic Environments accompanying each message can
;; help, but you must take explicit action to invoke the UNWIND chain
;; in the environment.
;;
;; It is difficult to defensively program against all possible future
;; abuses of your Actor system. In most cases you will need to rely on
;; timeout mechanisms to help out. Just like in the real world
;; outside...
;; -----------------------------------------------------------------

;; ---------------------------------------
;; As Actor messages become enqueued, waiting to be released to use
;; a guarded Actor, we keep their associated dyn env along with the
;; messages. When a new message is released, it will be released into
;; the same dynaminc env with which it arrived.
;;
;; This could become important as different logical threads may have
;; their own set of dynamic BINDINGS, HANDLERS, UNWIND actions, and
;; CATCH tags, that may get used or triggered in the Actors beyond the
;; SERIALIZER gateway.
;;
;; The SERIALIZER sets up its own UNWIND handler so that it can
;; release other messages, should anything untoward happen as a result
;; of a released Actor message.

(def-beh serializer-beh (act)
  ;; Quiescent state - nobody in waiting, just flag him through, but
  ;; enter the busy state.
  ((cust . msg)
   (let ((tag (tag self)))
     (send* act tag msg)
     (become (busy-serializer-beh
              act tag cust +emptyq+))
     )))


(def-beh busy-serializer-beh (act tag in-cust queue)
  ;; Busy state - new arriving messages get enqueued until we receive
  ;; a message through our interposed customer TAG.
  ((atag . msg) when (eql atag tag)
   (send* in-cust msg)
   (if (emptyq? queue)
       (become (serializer-beh act))
     (multiple-value-bind (next-req new-queue) (popq queue)
       (destructuring-bind (next-cust . next-msg) next-req
         (let ((new-tag (tag self)))
           (send* act new-tag next-msg)
           (become (busy-serializer-beh
                    act new-tag next-cust new-queue))
           )))
     ))

  (msg
   (become (busy-serializer-beh
            act tag in-cust
            (addq queue msg))
           )))

;; -----------------------------------------------------------

(defun serializer (act)
  (create (serializer-beh act)))


(defun serializer-sink (act)
  ;; Turn an actor into a sink. Actor must accept a cust argument,
  ;; and always send a response to cust - even though it appears to be
  ;; a sink from the caller's perspective.
  (label (serializer act) sink))


(defun safe-serializer (action &key timeout on-timeout supv)
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