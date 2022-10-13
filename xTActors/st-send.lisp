;; st-send.lisp -- Single-Thread Actor dispatch
;;
;; CALL-ACTOR - Most useful for managing an edge Actor in charge of a
;; shared resource, and when that Actor must be available, whether or
;; not the Actors system is up and running.
;;
;; The dispatch loop exits back to caller once all flurry of activity
;; is finished. Seems like a neat idea, but it can fail to work as
;; hoped if:
;;
;;     - the Actor being called upon is guarded by a SERIALIZER, and
;;       that SERIALIZER is currently busy servicing other clients.
;;
;;       You'll be put on the waiting list, exit immediately since
;;       your flurry is finished, and then receive a response sometime
;;       later, after you have already stopped listening for a result.
;;
;;       In this case you should have used ASK. But that only works if
;;       the Actors system is up and runninig on one or more threads
;;       alongside you. And using ASK from within an Actor is useless
;;       - just use a BETA continuation form.
;;
;;       In general, you can't know if you are calling through an
;;       intervening SERIALIZER. But you can ascertain whether or not
;;       Actors are running (use RUNNING-ACTORS-P). And if you *are*
;;       an Actor, then you know Actors are running when you execute.
;;
;;       As for using ASK: if there is any question about whether you
;;       are being executed in an Actor thread, just query SELF. It
;;       will be non-NIL if queried from an Actor thread, and NIL
;;       otherwise.
;;
;;       If you ever need to perform an action from outside of the
;;       Actors system, use MP:FUNCALL-ASYNC. This works whether or
;;       not you are an Actor.
;;
;;       NOTE: There is a special kind of SERIALIZER, called
;;       BLOCKING-SERIALIZER, that overcomes many of these issues. But
;;       beware that it uses locking with CALL-ACTOR. And so if its
;;       guarded Actor is not just an edge Actor, and might produce
;;       feedback loops on itself, then this would lead to a deadlock
;;       in single-threaded situations.
;;
;;
(in-package :com.ral.actors.base)

(defun stsend (actor &rest msg)
  #F
  ;; Single-threaded SEND - runs entirely in the thread of the caller.
  ;;
  ;; We still need to abide by the single-thread-only exclusive
  ;; execution of Actor BECOME. There might be several other instances
  ;; of this running, or else some of the multithreaded versions.
  ;;
  ;; SENDs are optimistically committed in the event queue. In case of
  ;; error these are rolled back.
  ;;
  (let (qhd qtl qsav evt pend-beh)
    (macrolet ((qreset ()
                 `(if (setf qtl qsav)              ;; unroll committed SENDs
                      (setf (msg-link (the msg qtl)) nil)
                    (setf qhd nil))))
      (flet ((%send (actor &rest msg)
               (cond (evt
                      ;; reuse last message frame if possible
                      (setf (msg-actor (the msg evt)) (the actor actor)
                            (msg-args  (the msg evt)) msg
                            (msg-link  (the msg evt)) nil))
                     (t
                      (setf evt (msg (the actor actor) msg))) )
               (setf qtl
                     (if qhd
                         (setf (msg-link (the msg qtl)) evt)
                       (setf qhd evt))
                     evt nil))
             
             (%become (new-beh)
               (setf pend-beh new-beh)))
        
        (declare (dynamic-extent #'%send #'%become))
        
        (let ((*current-actor*    nil)
              (*current-message*  nil)
              (*current-behavior* nil)
              (*send*             #'%send)
              (*become*           #'%become))
          (declare (list *current-message*))
          
          (send* actor msg)
          (loop
             while qhd
             do
               (with-simple-restart (abort "Handle next event")
                 (handler-bind
                     ((error (lambda (c)
                               (declare (ignore c))
                               (qreset))
                             ))
                   (loop
                      ;; keep going until there are no more messages
                      while (when (setf evt qhd)
                              (setf qhd (msg-link (the msg evt)))
                              evt)
                      do
                        (setf self     (msg-actor (the msg evt))
                              self-msg (msg-args (the msg evt))
                              qsav     (and qhd qtl))
                        (tagbody
                         again
                         (setf pend-beh (actor-beh (the actor self))
                               self-beh pend-beh)
                         (apply (the function pend-beh) self-msg)
                         
                         ;; Using same CAS BECOME protocol here as
                         ;; with all other Actors
                         (cond ((or (eq pend-beh self-beh)
                                    (mpc:compare-and-swap (actor-beh (the actor self)) self-beh pend-beh)))
                               
                               (t
                                ;; Actor was in use, try again
                                (setf evt (or evt qtl))
                                (qreset)
                                (go again))
                               )))
                   )))
          )))))

(defmacro with-single-thread (&body body)
  `(let ((*send* #'stsend))
     ,@body))

(defvar *NO-ANSWER* #())

(defun call-actor (ac &rest args)
  ;; Invoking an Actor from procedural code.  Assumes Actor, ac, takes
  ;; a customer argument, which we supply internally.
  ;;
  ;; Actor may spawn logical threads that will run in our process. The
  ;; dispatch loop returns after all spawned Actor activity has
  ;; ceased.
  ;;
  ;; Careful! This can only be assured safe in single-threaded
  ;; environments.  Otherwise, use ASK.
  ;;
  ;; If you happen to call upon a busy SERIALIZER, or on any Actor
  ;; that does not produce a response, then you will get *NO-ANSWER*.
  ;;
  (let* ((ans   *no-answer*)
         (cust  (create
                 (lambda (&rest ans-args)
                   (setf ans ans-args)))))
    (with-single-thread
      (send* ac cust args))
    (values-list ans)))

