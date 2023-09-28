;; sponsors.lisp -- Sponsored Actors = dedicated threads running message dispatch
;;
;; Sponsors have use in setting up dedicated I/O port handlers which have to undergo
;; indefnite periods of blocking wait on the port. By isolating Actors to such a Sponsor
;; we avoid tying up other dispatch loops waiting for an Actor to clear for execution.
;; Message sends to sponsored Actors is fast, being just a mailbox send.
;;
;; DM/RAL 01/22
;; -----------------------------------------------------------------------------

(in-package :com.ral.actors.base)

(defvar *current-sponsor*  nil)

(define-symbol-macro self-sponsor  *current-sponsor*)

(defun sponsor-beh (mbox thread)
  (alambda
   ((:shutdown)
    (mp:process-terminate thread)
    (become (sink-beh)))
   
   ((actor . msg) when (actor-p actor)
    (mp:mailbox-send mbox (msg actor msg)))
   ))
  
(defun make-sponsor (name)
  (let* ((spon   (create))
         (mbox   (mp:make-mailbox))
         (thread (mp:process-run-function name () 'run-sponsor spon mbox)))
    (setf (actor-beh spon) (sponsor-beh mbox thread))
    spon))

(defun in-sponsor (spon actor)
  (if spon
      (actor (&rest msg)
        (send* spon actor msg))
    actor))

(defun in-this-sponsor (actor)
  (in-sponsor self-sponsor actor))

(defun run-sponsor (*current-sponsor* mbox)
  #F
  ;; Single-threaded - runs entirely in the thread of the Sponsor.
  ;;
  ;; We still need to abide by the single-thread-only exclusive
  ;; execution of Actors. There might be several other instances of
  ;; this running, or else some of the multithreaded versions.
  ;;
  ;; SENDs are optimistically committed in the event queue. In case of
  ;; error these are rolled back.
  ;;
  (let (qhd qtl qsav evt pend-beh)
    (macrolet ((addq (evt)
                 `(setf qtl
                        (if qhd
                            (setf (msg-link (the msg qtl)) ,evt)
                          (setf qhd ,evt)))
                 )
               (qreset ()
                 `(if (setf qtl qsav)
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
               (addq evt)
               (setf evt nil))

             (%become (new-beh)
               (setf pend-beh new-beh))
             
             (%abort-beh ()
               (setf pend-beh self-beh
                     evt      (or evt qtl))
               (qreset)))
        
        (declare (dynamic-extent #'%send #'%become #'%abort-beh))
        
        (let ((*send*      #'%send)
              (*become*    #'%become)
              (*abort-beh* #'%abort-beh))
          
          (with-simple-restart (abort "Terminate Actor thread")
            (loop
               (with-simple-restart (abort "Handle next event")
                 (handler-bind
                     ((error (lambda (c)
                               (declare (ignore c))
                               (qreset)) ;; unroll SENDs
                             ))
                   (loop
                      (when (mp:mailbox-not-empty-p mbox)
                        (let ((evt (mp:mailbox-read mbox)))
                          (addq evt)))
                      (if (setf evt qhd)
                          (setf qhd (msg-link (the msg evt)))
                        (setf evt (mp:mailbox-read mbox)))
                      (setf qsav (and qhd qtl))
                      (let ((*current-actor*   (msg-actor (the msg evt)))
                            (*current-message* (msg-args  (the msg evt))))
                        (declare (list  *current-message*)
                                 (actor *current-actor*))
                        (tagbody
                         retry
                         (setf pend-beh (actor-beh (the actor self)))
                         (let ((*current-behavior* pend-beh))
                           (declare (function *current-behavior*))
                           (apply (the function pend-beh) self-msg)
                           (cond ((or (eq pend-beh self-beh)
                                      (%actor-cas self self-beh pend-beh)))
                                 
                                 (t
                                  ;; Actor was mutated beneath us, go again
                                  (setf evt (or evt qtl))
                                  (qreset)
                                  (go retry))
                                 )))
                        ))
                   )))
            )))
      )))


