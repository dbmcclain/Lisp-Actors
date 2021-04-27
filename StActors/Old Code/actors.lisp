;; Actors -- DM/RAL 10/17
;;
;; This package of code is a shameless hack, starting with the code in
;; CL-ACTORS, and heavily extending it to my needs.
;;
;; Actors are bodies of behavior code and associated private state
;; data that perform actions against arguments asynchronously sent to
;; them via non-blocking message sends.
;;
;; Actors are free to peform blocking waits in their body code. (just
;; try to stop that from ever happening!). Stalled Actors
;; could either be the result of blocking waits or from intensely
;; compute bound activity.
;;
;; Actors run under the supervision of a pool of Executive system
;; threads. As Executives become tied up with their Actors performing
;; stalling, a watchdog checks periodically and will spawn additional
;; Executive threads to prevent other Actors from becoming stalled
;; waiting to run.
;;
;; Actors can change their behavior for use on future message sends,
;; but be mindful of the effect if you change its interface message
;; arguments.
;;
;; Only one invocation of an actor can occur at any one time, and so
;; guarantees single-thread access to private state data.
;;
;; Macro DEF-FACTORY defines an actor factory function, with initial
;; state data bindings, and behavior code, for a particular class of
;; actor. Calling a defined factory function actually constructs an
;; Actor structure, giving the user a chance to override initial
;; default private binding values, and enters the newly constucted
;; Actor into the system.
;;
;; Execitives run whenever a ready actor is available. The executives
;; will block waiting for ready actors. Once a ready actor is
;; obtained, the actor's behavior code runs on the same thread as the
;; executive, blocking the executive from any further action until the
;; actor returns.
;;
;; Extant Actors live in the heap and are placed into the system ready
;; queue once a message is sent to them. The executives remove actors
;; from the ready queue for execution of their behavior code, then
;; return them to either the ready queue if more messages await, or
;; back to the heap to await a new message.
;;
;; Queues are managed in FIFO order, but an Actor that continuously
;; receives messages while running will be kept running by its
;; Executive until there are no remaining messages in the Actor's
;; mailbox. Messages are handled in FIFO order of receipt. There is no
;; selective retrieval from the Actor's mailbox. Every message is sent
;; to the Actor for handling.
;;
;; We prevent stalling of other Actors by way of having additional
;; Executive threads to run them. There are no priority distinctions
;; among actors.  Actors in the ready queue can be executed by any
;; available Executive thread in the Executive Pool.
;;
;; The system ready queue is an SMP-safe queue shared among all
;; Executive threads and any other message sending threads located
;; anywhere in the running Lisp image.
;;
;;  (an Actor) <-- message send
;;       |
;;       |        Executive - run actor code / wait for ready
;;       v       /
;;  (Ready Queue) -- Executive - run actor code / wait for ready
;;               \
;;                Executive - run actor code / wait for ready
;;
;; Actors cannot depend on running in any particular OS thread, but
;; their behavior code can be assumed to operate without preemption
;; from another instance of the same actor. An actor can only be alive
;; on one thread at any moment, and will run to completion in the same
;; thread in which it fired. Internal state is SMP safe from
;; alteration by other code running in parallel on another OS thread.
;; Reentrant behavior code is unnecessary.
;;
;; Message sends can refer to target actors by actual reference to an
;; Actor structure, or by name, for an actor registered in the Actor
;; Directory. Using a name reference causes a lookup in the Actor
;; Directory to locate the associated Actor instance.  Actors should
;; be registered with the directory using string or interned symbol
;; names.  Any other name will cause the attempted registration to be
;; ignored. Actors are otherwise unnamed, as they correspond in the
;; green-threads world to LAMBDA for the Lisp world.
;;
;; Communication with actors is facilitated by function GET, which
;; first sends a message, then awaits a response to a privately held
;; replyTo mailbox. By convention, in the actor behavior code, the
;; replyTo mailbox is always the last given argument in a message. The
;; communication back to the message sender occurs by SEND to that
;; replyTo argument. There be trouble if GET is used on a message to
;; which the actor doesn't respond. While SEND is non-blocking, the
;; mailbox-read is.
;;
;; SEND is always non-blocking. SEND is overloaded as a method to
;; further support non-blocking sending of messages through Reppy
;; Channels, mp:mailboxes, and mp:procedures with proc-mailboxes, in
;; addition to sending to actors either directly or by name lookup.
;;
;; Doug Hoyt's DLAMBDA is an excellent mechanism for producing actors
;; which provide shared data structures, which the Actor system
;; guarantees as single thread access/modify to private state data,
;; free from meddlesome interference from other threads. See below for
;; SHARED-QUEUE, SHARED-STACK, SHARED-MAP, and SHARED-SET.  No need
;; for locks and other SMP coordination mechanisms for shared memory
;; access to persistent actor private state data.
;;
;; Persistence, here, refers to data that retains memory between
;; invocatons of the same actor instance. Once terminated, an actor's
;; private persistent state data also becomes inaccessible. This
;; single thread freedom won't be true for shared global data, which
;; still requires SMP coordination among threads.
;;
;; ---------------------------------------------------------------

(in-package #:actors-base)

;; --------------------------------------------------------------------

(defclass actor ()
  ((messages
    :initform (make-prio-mailbox)
    :accessor actor-messages
    :documentation "Message stream sent to actor")
   ;; the main communications mailbox for use by SEND
   
   (behavior
    :accessor actor-behavior
    :initform nil
    :initarg :behav)
   ;; behavior holds the behavior of the Actor

   (save
    :accessor actor-save)
   ;; holds the information needed to effect a REVERT
   
   (priority
    :accessor actor-priority
    :initform 0
    :initarg :priority)

   (running
    :accessor actor-running-p-ref
    :initform (list nil)) ;; CAS ref
   ;; This is a Ref Cell for use with CAS testing and setting. Its CAR
   ;; will be NIL when the Actor is neither in the ready queue, nor
   ;; being executed by an Executive thread. It will be T when it is
   ;; enqueued or while being executed.

   (lock
    :accessor actor-lock
    :initform (make-lock :name :Actor-lock))
   ;; in an SMP environment, me must use locking to gain unfettered
   ;; access to consistent state

   (properties
    :accessor actor-properties
    :initform nil
    :initarg  :properties)
   ;; a general purpose properties list for use by the Actor itself.
   ;; Since this slot is accessible to any other thread, access to it
   ;; should be guarded by the lock.
   ;;
   ;; Local state can be kept either in closed-over lexical bindings
   ;; in the behavior closures, or in this list. Access to state is
   ;; probably faster with lexical bindings in the closure. But state
   ;; kept here will be more easily inspected. It's up to you...
   )
  (:metaclass harlequin-common-lisp:funcallable-standard-class))

(defmethod initialize-instance :after ((actor actor) &key &allow-other-keys)
  (clos:set-funcallable-instance-function
   actor
   (lambda (&rest msg)
     (if (eq actor (current-actor))
         (apply (actor-behavior actor) msg)
       (apply #'ask actor msg)))))

(defmethod actor-lambda-list ((actor actor))
  (cdr (lw:function-lambda-list (actor-behavior actor))))

(defmethod has-messages-p ((actor actor))
  (mailbox-not-empty-p (actor-messages actor)))

(defmethod has-behavior-p ((actor actor))
  (actor-behavior actor))

;; ----------------------------------------------------------

(defmacro with-locked-actor ((actor &rest args) &body body)
  `(with-lock ((actor-lock ,actor) ,@args)
     ,@body))

(defmacro with-actor-values (bindings actor &body body)
  ;; get a consistent collection of actor slot values
  (let ((g!actor   (gensym-like :actor-)))
    (multiple-value-bind (syms accessors)
        (split-bindings bindings)
      `(let ((,g!actor ,actor))
         (multiple-value-bind ,syms
             (with-locked-actor (,g!actor)
               (values ,@(mapcar #`(,a1 ,g!actor) accessors)))
           ,@body))
      )))

;; ----------------------------------------------------------

(defmethod get-actor-property ((actor actor) key &optional default)
  (with-locked-actor (actor)
    (getf (actor-properties actor) key default)))

(defmethod set-actor-property ((actor actor) key val)
  (with-locked-actor (actor)
    (setf (getf (actor-properties actor) key) val)))

(defsetf get-actor-property set-actor-property)

;; ------------------------------------------------------------

(defmethod set-actor-priority ((actor actor) prio)
  (setf (actor-priority actor) prio))
