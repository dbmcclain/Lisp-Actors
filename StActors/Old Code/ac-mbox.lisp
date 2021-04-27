(defclass actor-mailbox ()
  ((lock   :reader   actor-mailbox-lock
           :initform (mp:make-lock))
   (msgs   :reader   actor-msgs
           :initform (hcl:make-unlocked-queue))
   (replay :accessor actor-message-replay
           :initform nil)))

(defclass limited-actor-mailbox (actor-mailbox)
  ((msgs   :initform (mp:make-mailbox :size 1))
   ))

;; -----------------------------------------------------

(defmacro with-actor-mailbox-locked (mbox &body body)
  `(mp:with-lock ((actor-mailbox-lock ,mbox))
     ,@body))

(defgeneric next-message (mbox)
  (:method ((mbox actor-mailbox))
   (or (pop (actor-message-replay mbox))
       (mp:with-lock (actor-mailbox-lock mbox)
         (hcl:unlocked-queue-read (actor-msgs mbox)))
       ))
  (:method ((mbox limited-actor-mailbox))
   (or (pop (actor-message-replay mbox))
       (mp:mailbox-read (actor-msgs mbox) nil 0))))
  
(defgeneric send-message (mbox msg)
  (:method ((mbox actor-mailbox) msg)
   ;; always succeeds
   (mp:with-lock (actor-mailbox-lock mbox)
     (unlocked-queue-send (actor-msgs mbox) msg)))
  (:method ((mbox limited-actor-mailbox) msg)
   ;; might not succeed
   (mp:with-lock (actor-mailbox-lock mbox)
     (mp:mailbox-send-limited (actor-msgs mbox) msg 1 *timeout*))))

(defgeneric mailbox-not-empty-p (mbox)
  ;; called only under lock by Actor itself
  (:method ((mbox actor-mailbox))
   (or (actor-message-reply mbox)
       (hcl:unlocked-queue-ready-p (actor-msgs mbox))))
  
  (:method ((mbox limited-actor-mailbox))
   (or (actor-message-replay mbox)
       (mp:mailbox-not-empty-p (actor-msgs mbox)))))

