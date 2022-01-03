
(in-package :com.ral.actors.base)

(defun stsend (actor &rest msg)
  ;; Single-threaded SEND - runs entirely in the thread of the caller.
  ;;
  ;; We still need to abide by the single-thread-only exclusive
  ;; execution of Actors. There might be several other instances of
  ;; this running, or else some of the multithreaded versions.
  (let (qhd qtl qsav evt pend-beh)
    (flet ((send (actor &rest msg)
             (when actor
               (cond (evt
                      (setf (msg-actor (the msg evt)) actor
                            (msg-args  (the msg evt)) msg
                            (msg-link  (the msg evt)) nil))
                     (t
                      (setf evt (msg actor msg))) )
               (setf qtl
                     (if qhd
                         (setf (msg-link (the msg qtl)) evt)
                       (setf qhd evt))
                     evt nil)
               ))
           (become (new-beh)
             (setf pend-beh new-beh)))
      
      (declare (dynamic-extent #'send #'become))

      (let ((*current-actor*    nil)
            (*whole-message*    nil)
            (*current-behavior* nil)
            (*send*             #'send)
            (*become*           #'become))
        
        (send* actor msg)
        (loop
           while qhd
           do
             (with-simple-restart (abort "Handle next event")
               (handler-bind
                   ((error (lambda (c)
                             (declare (ignore c))
                             (setf (actor-beh self) self-beh)
                             (if (setf qtl qsav)
                                 (setf (msg-link (the msg qtl)) nil)
                               (setf qhd nil))
                             )))
                 (loop
                    while (when (setf evt qhd)
                            (setf qhd (msg-link (the msg evt)))
                            evt)
                    do
                      (setf self      (msg-actor (the msg evt))
                            self-beh  (sys:atomic-exchange (actor-beh (the actor self)) nil))
                      (cond
                       (self-beh
                        (setf *whole-message* (msg-args (the msg evt))
                              qsav            (and qhd qtl)
                              pend-beh        self-beh)
                        (apply (the function self-beh) *whole-message*)
                        (setf (actor-beh self) (the function pend-beh)))
                       
                       (t
                        (send* self *whole-message*))
                       ))
                 ))))
      )))

(defmacro with-single-thread (&body body)
  `(let ((*send* 'stsend))
     ,@body))
