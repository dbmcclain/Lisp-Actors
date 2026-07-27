
(um:make-encapsulated-type E E? D)

(defclass actor-mailbox ()
  ;; by default - a queue of indefinite capacity
  ((lock   :reader   actor-mailbox-lock
           :initform (mp:make-lock))
   (msgs   :reader   actor-msgs
           :initarg  :queue)
   (replay :accessor actor-message-replay
           :initform nil))
  (:default-initargs
   :queue (E (hcl:make-unlocked-queue))))

(defclass limited-actor-mailbox (actor-mailbox)
  ;; A queue with a limited capacity.  Additional SEND's will block
  ;; waiting until they can deliver the message, or deliver a NIL
  ;; result as a result of a timeout. By default the limit is one
  ;; message.
  ()
  (:default-initargs
   :queue (E (mp:make-mailbox :size 1))
   ))

(defun rd-mbox (mbox)
  (um:rd (D mbox)))

(defgeneric unsafe-mailbpx-not-empty-p (mbox)
  (:method ((mbox actor-mailbox))
   (or (actor-message-replay mbox)
       (hcl:unlocked-queue-ready (D (actor-msgs mbox)))))

  (:method ((mbox limited-actor-mailbox))
   (or (actor-message-replay mbox)
       (mp:mailbox-not-empty-p (D (actor-msgs mbox))))))

(defgeneric next-queue-message (mbox)
  (:method ((mbox actor-mailbox))
   (let (msg)
     (um:rmw (D (actor-msgs mbox))
             (lambda (mb)
               (setf msg (hcl:unlocked-queue-read mb))
               mb))
     msg))
  (:method ((mbox limited-actor-mailbox))
   (let (msg)
     (um:rmw (D (actor-msgs mbox))
             (lambda (mb)
               (setf msg (mp:mailbox-read mb nil 0))
               mb))
     msg)))
  
(defun next-message (mbox)
  (if (actor-message-replay mbox)
      (pop (actor-message-replay mbox))
    (next-queue-message mbox)))


(defmethod mailbox-not-empty-p ((mbox E))
  (hcl:unlocked-queue-ready (rd-mbox mbox)))

(defmethod next-queue-message ((mbox E))
  (let (msg)
    (um:rmw (D mbox) (lambda (mb)
                       (setf msg (hcl:unlocked-queue-read mb))
                       mb))
    msg))
