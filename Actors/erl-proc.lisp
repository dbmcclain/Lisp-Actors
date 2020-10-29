;; erl-proc.lisp -- Experimental Erlang-style Processses in Actors.
;;
;; DM/RAL  12/19
;; --------------------------------------------------------------
#|

      Erlang-like Processes in an Actors context...

In Erlang, Processes define regions of unshared memory under sole
control of each Process. We can do likewise with Actors.

Processes run concurrently, and so do Actors.

Erlang uses process links as a method of handling error conditions. If
an error arises, the Process simply dies and propagates its death
with an EXIT message sent ao all linked processes.

But we are Lisp, with an elaborate Condition system. We can choose to
handle errors in any number of ways. And we can mimic the Erlang
approach with errors generating :EXIT messages.

Erlang Processes are much like Threads, (green threads), which can be
killed and prevented from processing any further messages. We can
mimic this behavior by messing with the busy-status of an Actor, so
that it won't be able to process any additional messages. But at the
same time, you could simply stop sending messages to an Actor.

Actors are not threads, so killing an Actor carries no meaning, other
than to abort its execution if currently running on an Executive
thread. But the Actor itself is not kiiled, and remains available to
process a next incoming message.

Actors are more like functions that can run concurrently, but which
can define private state with single-thread semantics, and which
communicate via message passing.

In Erlang a Process only handles messages after performing a "?" or
"RECV" operation. With Actors, sending a message wakes up the Actor to
process incoming messages. No explicit query by the Actor is needed.

So within Actor constraints, we can define a Process Actor that
understands Erlang process links, so that after an incident which
generates an :EXIT message to the Actor, it can be prevented from
handling any further messages, and it can propagate its :EXIT to all
other linked Process Actors.

How useful is this?

|#
;; --------------------------------------------------------------

(in-package :actors.erl)

(defclass process (actor)
  ((links
    :accessor process-links
    :initarg  :links
    :initform nil)
   (trapping-exits
    :accessor trapping-exits
    :initarg  :trap-exits
    :initform nil)
   ))

;; --------------------------------------------------

(defun link-to (proc)
  (pushnew proc (process-links (current-actor))))

(defun unlink-from (proc)
  (um:deletef (process-links (current-actor)) proc))

(defun make-process (fn &key properties link monitor trap-exits)
  (let ((links (if (listp link)
                   link
                 ;; else
                 (when-let (link (if (eq link t)
                                     (current-actor)
                                   link))
                   (list link))))
        (monitor (if (eq monitor t)
                     (current-actor)
                   monitor))
        (proc    nil))
    (when monitor
      (push (dlambda*
              (:link (proc)
               (declare (ignore proc))
               )
              (:unlink (proc)
               (declare (ignore proc))
               )
              (:exit (from reason)
               (send monitor :down proc from reason)))
            links))
    (prog1
        (setf proc (make-instance 'process
                                  :user-fn    fn
                                  :properties properties
                                  :trap-exits trap-exits
                                  :links      links
                                  ))
      (dolist (parent links)
        (send parent :link proc)))
    ))

;; --------------------------------------------------

(defmethod dispatch-message :around ((*current-actor* process) &rest msg)
  (declare (special *current-actor*))
  (flet ((handle-below ()
           (handler-case
               (call-next-method)
             (error (c)
               (dispatch-message *current-actor* :exit *current-actor* c))
             )))
    (dcase* msg
      
      (:link (proc)
       ;; proc should be a SEND target that understands :UNLINK and
       ;; :EXIT messages
       (link-to proc))
      
      (:unlink (proc)
       (unlink-from proc))
      
      (:exit (from reason)
       (flet ((kill-and-propagate ()
                (nullify *current-actor*)
                (let ((links (shiftf (process-links *current-actor*) nil)))
                  (dolist (proc links)
                    (send proc :exit from reason))
                  (unless links
                    (when (typep reason 'error)
                      (error reason)))
                  (abort))
                ))
         (unlink-from from)
         (cond
          ((or (eq from *current-actor*)
               (eq reason :kill))
           ;; if we sent :exit, or anyone sent us :kill
           (kill-and-propagate))
          
          ((eq reason :normal)
           ;; if someone else died normally, ignore unless we are
           ;; trapping exits
           (when (trapping-exits *current-actor*)
             (handle-below))) 
          
          ((trapping-exits *current-actor*)
           ;; someone else died for some reason and we are trapping exits
           (handle-below))
          
          (t
           ;; someone else died for some reason and we are not trapping exits
           (kill-and-propagate))
          )))

      (t (&rest _)
         (declare (ignore _))
         ;; any other message
         (handle-below))
      )))

;; --------------------------------------------------

(defun spawn-link (fn &rest args)
  ;; If called from within an Actor, that Actor should understand
  ;; :LINK, :UNLINK, and :EXIT messages.
  (let ((proc (make-process (lambda (&rest args)
                              (apply fn args)
                              (exit :normal))
                            :link t)))
    (apply 'send proc args)
    proc))

(defun spawn-monitor (fn &rest args)
  ;; If called from within an Actor, that Actor should understand
  ;; :DOWN messages
  (let ((proc (make-process (lambda (&rest args)
                              (apply fn args)
                              (exit :normal))
                            :monitor t)))
    (apply 'send proc args)
    proc))

;; --------------------------------------------------

(defun link-between (proc-1 proc-2)
  (when (and proc-1 proc-2)
    (send proc-1 :link proc-2)
    (send proc-2 :link proc-1)))

(defun unlink-between (proc-1 proc-2)
  (when (and proc-1 proc-2)
    (send proc-1 :unlink proc-2)
    (send proc-2 :unlink proc-1)))

;; --------------------------------------------------

(defun link (proc)
  (link-between (current-actor) proc))

(defun unlink (proc)
  (unlink-between (current-actor) proc))

;; --------------------------------------------------

(defun exit (reason &optional (proc (current-actor)))
  (send proc :exit (current-actor) reason))

(defun trap-exits (t/f)
  (setf (trapping-exits (current-actor)) t/f))

;; --------------------------------------------------



     