

(let ((junk 0))
  (defun foo (x) (+ x junk))
  (defun bar (x) (setf junk x)))

(defun count-all-objects ()
  (let ((ct 0))
    (sweep-all-objects (lambda (&rest args)
                         (declare (ignore args))
                         (incf ct)))
    ct))

;; --------------------------------------------------

(defvar *current-agent*  nil)

(defmacro define-agent (bindings &rest body)
  (let ((g!lock  (gensym "lock"))
        (g!this  (gensym "this"))
        (g!msg   (gensym "msg")))
    `(let ((,g!lock  (mp:make-lock))
           (,g!this  nil)
           ,@bindings)
       (setf ,g!this
             (lambda (&rest ,g!msg)
               (mp:with-lock (,g!lock)
                 (let ((*current-agent* ,g!this))
                   (dcase ,g!msg
                     ,@body))))
             ))
    ))

(defun self-call (&rest msg)
  (apply *current-agent* msg))

(defun call (agent &rest msg)
  (apply agent msg))

;; --------------------------------------------------

(defvar *async-mbox* (mp:make-mailbox))
(defvar *async-proc* nil)
  
(defun dispatch (&rest msg)
  (mp:mailbox-send *async-mbox* msg))

(defun runner ()
  (loop
   (let ((msg (mp:mailbox-read *async-mbox*)))
     (ignore-errors
       (apply (car msg) (cdr msg)))
     )))

(unless *async-proc*
  (setf *async-proc*
        (mp:process-run-function "async-runner" ()
                                 'runner)))

