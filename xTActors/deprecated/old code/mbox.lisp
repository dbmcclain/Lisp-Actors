
(in-package :com.ral.actors.mbox)

(defstruct mbox
  (lock  (mp:make-lock))
  msgqhd
  msgqtl
  thrq)

(defstruct (msg
            (:constructor msg (actor args &optional link)))
  link
  (actor (make-actor) :type actor)
  (args  nil          :type list))

(defvar *central-mail*  (make-mbox))

(defun mbox-send (mbox msg)
  (declare (mbox mbox)
           (msg  msg))
  (mp:with-lock ((mbox-lock mbox))
    (let (cell)
      (cond ((setf cell (pop (mbox-thrq mbox)))
             (setf (sys:globally-accessible (car (the cons cell))) msg))
            
            (t
             (setf (msg-link msg) nil
                   (msgqtl mbox)
                   (if (mbox-msgqhd mbox)
                       (setf (msg-link (the msg (msgq-tl mbox)) msg))
                     (setf (mbox-msgqhd mbox) msg))
                   ))
            ))))

(defun #1=mbox-recv (mbox)
  (declare (mbox mbox))
  (let ((cell  (list nil)))
    (declare (cons cell)
             (dynamic-extent cell))
    (mp:with-lock ((mbox-lock mbox))
      (let (msg)
        (cond ((setf msg (mbox-msgqhd mbox))
               (unless (setf (mbox-msgqhd mbox) (msg-link (the msg msg)))
                 (setf (mbox-msgqtl mbox) nil))
               (return-from #1# msg))

              (t
               (push cell (the list (mbox-thrq mbox))))
              )))
    (loop until (car cell) do (mp:process-allow-scheduling))
    (car cell)
    ))

