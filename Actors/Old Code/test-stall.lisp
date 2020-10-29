
(defvar *done*    nil)
(defvar *lock*    (mp:make-process-lock))
(defvar *threads* nil)
(defvar *last-heartbeat* 0)
(defvar *watchdog* nil)

(defun setup-threads ()
  (setf *done* nil
	*threads*
	(loop repeat 4 collect
	      (mp:process-run-function "test thread"
		(lambda ()
		  (setf *last-heartbeat* (get-universal-time))
		  (sleep 10)
		  (mp:with-process-lock (*lock*)
		    (setf *threads* (remove mp:*current-process* *threads*))
		    (setf *done* t))
		  (format t "~&Thread ~A done" mp:*current-process*))))))

(defun setup-watchdog ()
  (unless *watchdog*
    (mp:process-run-function "watchdog thread"
      (lambda ()
	(block :outer
	  (loop 
	    (sleep 1)
	    (when (check-for-stall)
	    (return-from :outer))))))))

(defun kill-threads ()
  (mapc 'mp:process-kill (shiftf *threads* nil)))
  
(defun check-for-stall ()
  (let (age)
    (unless (or *done*
		(progn
		  (setf age (- (get-universal-time) *last-heartbeat*))
		  (< age 3))) ;; 3 sec timeout
      (setf *watchdog* nil)
      (mp:process-run-function "Stall Handler thread"
	(lambda ()
	  (restart-case
	      (error "Worker threads are stalled. ~&Last heartbeat was ~A sec ago." age)
	    (:do-nothing-just-wait ()
	      :report "It's okay, just wait"
	      (setup-watchdog))
	    (:stop-threads ()
		:report "Stop threads"
	      (kill-threads))
	    )))
      )))

(defun run-test ()
  (setup-threads)
  (setup-watchdog))


		  