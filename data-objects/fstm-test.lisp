
(in-package :fstm)

(defun show-rolls (&optional (duration 1))
  (let ((pcnt (if (zerop *ncomms*)
                  0
                (/ *nrolls* *ncomms* 0.01)))
        (rate (if (zerop *ncomms*)
                  0
                (/ *ncomms* duration))))
    (list :retrys *nrolls*
          :commits   *ncomms*
          :percent-retrys pcnt
          :commits-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
          :duration duration
          :commits-per-sec  rate)))

(defun reset ()
  (setf *nrolls* 0)
  (setf *ncomms* 0))

(defparameter *a* (var 0))
(defparameter *b* (var 0))

(defparameter xretries 0)
  
(defun check-invariant ()
  (let (a b)
    (atomic
      (setf a (fstm:val *a*)
            b (fstm:val *b*))
      (unless (= b (* 2 a))
        (retry))
      #|
       (unless (and nil (= b (* 2 a)))
         (sys:atomic-incf xretries)
         (retry))
       |#
      )
    (when (/= b (* 2 a))
      ;; (format t "~%a = ~A, b = ~A  (~A)" a b (mp:get-current-process))
      (ac:log-info :SYSTEM-LOG "Invariant broken: A = ~A, B = ~A" a b))))
  
(defun common-code (delta)
  (atomic
    (let* ((refa (fstm:ref *a*))
           (refb (fstm:ref *b*))
           (a    (+ delta (ref:wval refa)))
           (b    (* 2 a)))
      (setf (ref:wval refa) a
            (ref:wval refb) b)
      )))

(defparameter *ct* 1000000)
  
(defun count-up ()
  (loop repeat *ct* do (common-code 1))
  (check-invariant))
  
(defun count-down ()
  (loop repeat *ct* do (common-code -1))
  (check-invariant))
  
(defun checker (&rest procs)
  (let ((start (usec:get-time-usec)))
    (loop while (some #'mp:process-alive-p procs)
          do (check-invariant))
    (let ((stop (usec:get-time-usec)))
      (ac:log-info :SYSTEM-LOG (show-rolls (* 1e-6 (- stop start))))) ))

(defun tst0 ()
  (ac:log-info :SYSTEM-LOG "Start FSTM Test...")
  (setf *a* (var 0)
        *b* (var 0)
        xretries 0)
  (reset)
  (ac:spawn-worker #'count-down)
  (ac:spawn-worker #'count-up))
  
(defun tst1 (&optional (ct 1000000))
  ;; only one thread for no-contention timings
  (setf *ct* ct)
  (setf *a* (var 0)
        *b* (var 0))
  (reset)
  (let ((start (usec:get-time-usec)))
    (count-down)
    (let ((stop (usec:get-time-usec)))
      (show-rolls (float (/ (- stop start) ct))))
    ))

(defun tst2 (&optional (ct 1000000))
  (setf *ct* ct)
  (setf *a* (var 0)
        *b* (var 0))
  (reset)
  (let ((start (usec:get-time-usec)))
    (ac:par
     (count-down)
     (count-up))
    (let ((stop (usec:get-time-usec)))
      (show-rolls (float (/ (- stop start) ct))))
    ))

(defun tst3 (&optional (ct 1000000))
  (setf *ct* ct)
  (setf *a* (var 0)
        *b* (var 0))
  (reset)
  (let ((start (usec:get-time-usec)))
    (ac:par
     (count-down)
     (count-up)
     (count-down))
    (let ((stop (usec:get-time-usec)))
      (show-rolls (float (/ (- stop start) ct))))
    ))

;; -------------------------------------------

(defun tst4 ()
  ;; only one thread for no-contention timings
  (setf *a* (var 0)
        *b* (var 0))
  (reset)
  (let ((start (usec:get-time-usec)))
    (count-down)
    (let ((stop (usec:get-time-usec)))
      (float (/ (- stop start) *ct*)))))

