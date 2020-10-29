;; ----------------------------------------------------------------------------
;; BECOME, WAIT -- this all works just fine, but it really is
;; counterproductive compared to just using a blocking wait inside the
;; body of the Actor.
;;
;; Either way, some Executive may end up in a blocking wait. And the
;; advantage to doing it in-line in the Actor is that message order is
;; preserved in the Actor's mailbox queue.  Using WAIT disturbs that
;; ordering.
;;
;; You can't eradicate blocking waits by spawning them off into
;; another thread. Either you block waiting, or your spawned task
;; blocks waiting. Blocking wait is a conserved quantity.

(defmacro become (args state &body body)
  `(next-time (behav ,args ,state
                ,@body)) )

(defun re-enqueue (actor msg-lst)
  (map nil (lambda (msg)
             (apply #'send actor msg))
       (nreverse msg-lst)))

(defmacro wait (args wait-form &body body)
  (let ((a!self   (anaphor 'self))
        (a!me     (anaphor 'me))
        (g!unique (gensym-like :msg-))
        (g!msg    (gensym-like :msg-))
        (g!queue  (gensym-like :queue-))
        (g!old-me (gensym-like :old-me-))
        (g!ans    (gensym-like :ans-)))
    `(progn
       (spawn (lambda ()
                (send ,a!self ',g!unique
                      (um.dispq:capture-ans-or-exn
                       (lambda ()
                         ,wait-form)))
                ))
       (become (&rest ,g!msg)
           ((,g!old-me #',a!me)
            ,g!queue)
         (um:dcase ,g!msg
           (,g!unique (,g!ans)
                      (apply (lambda ,args
                               (unwind-protect
                                   (progn
                                     ,@body)
                                 (re-enqueue ,a!self ,g!queue)
                                 (next-time ,g!old-me)))
                             (um.dispq:recover-ans-or-exn ,g!ans)))
           (t (&rest ,g!msg)
              (push ,g!msg ,g!queue))
           )))
    ))

#|
(progn
  (doit-toit)
  (become (&rest msg)
          ()
          (didit-already)))

(let ((x (make-actor (&rest msg)
             ()
           (um:dcase msg
             (:ok ()
              (pr :hello self #'me)
              (wait (&rest msg)
                  (progn
                    (sleep 1)
                    (list 1 2 3))
                (pr :aftwait msg self #'me)))
             (:other ()
              (pr :other))
             ))
         ))
  (send x :other)
  (send x :ok)
  (send x :other))


(progn
  (doit-toit)
  (wait (&rest msg)
      (wait-read-from-disk)
    (didit-already)))

|#
