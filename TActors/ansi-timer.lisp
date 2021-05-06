(in-package :ansi-timer)

(defvar *timeout-queue*  (priq:make-lifo))
(defvar *cancel-queue*   (priq:make-lifo))
(defvar *cycle-bits*     0.0d0)
(defvar *last-check*     0)
(defvar *timeout-tree*   (maps:empty))

(defclass timer ()
  ((period   :accessor timer-period
             :initarg  :period
             :initform nil
             :documentation "True if timer should repeat periodically.")
   (t0       :accessor timer-t0
             :initarg  :t0
             :initform 0
             :documentation "Absolute universal-time when timer is set to expire.")
   (fn       :accessor timer-fn
             :initarg  :fn
             :initform (constantly nil))
   (args     :accessor timer-args
             :initarg  :args
             :initform nil)
   ))

(defun make-timer (fn &rest args)
  (make-instance 'timer
                 :fn   fn
                 :args args))

(defmethod schedule-timer ((timer timer) t0 &optional repeat)
  (setf (timer-t0 timer) t0
        (timer-period timer) repeat)
  (priq:addq *timeout-queue* timer))

(defmethod schedule-timer-relative ((timer timer) trel &optional repeat)
  (let ((t0 (+ trel (get-universal-time))))
    (setf (timer-t0 timer)     t0
          (timer-period timer) repeat)
    (priq:addq *timeout-queue* timer)))

(defmethod unschedule-timer ((timer timer))
  (priq:addq *cancel-queue* timer))


(defun #1=check-timeouts ()
  ;; read new requests
  (loop for timer = (priq:popq *timeout-queue*)
        while timer
        do
        (let* ((t0     (timer-t0 timer))
               (timers (maps:find t0 *timeout-tree*))
               (new    (cons timer (delete timer timers))))
          (setf *timeout-tree* (maps:add t0 new *timeout-tree*))))
  ;; process cancellations
  (loop for timer = (priq:popq *cancel-queue*)
        while timer
        do
        (let* ((t0     (timer-t0 timer))
               (timers (maps:find t0 *timeout-tree*)))
          (when timers
            (let ((rem (delete timer timers)))
              (setf *timeout-tree* (if rem
                                       (maps:add t0 rem *timeout-tree*)
                                     (maps:remove t0 *timeout-tree*)))))
          ))
  ;; check our current time
  (let ((now (get-universal-time)))
    (if (= now *last-check*)
        (incf now (incf *cycle-bits* 0.1d0))
      (setf *cycle-bits* 0.0d0
            *last-check* now))
    ;; fire off expired timers
    (maps:iter (lambda (t0 timer-list)
                 (when (> t0 now)
                   (return-from #1#))
                 (setf *timeout-tree* (maps:remove t0 *timeout-tree*))
                 (dolist (timer timer-list)
                   (let ((per (timer-period timer)))
                     (when per
                       (schedule-timer-relative timer per per)))
                   (apply (timer-fn timer) (timer-args timer))
                   #|
                   (multiple-value-bind (ans err)
                       (ignore-errors
                         (apply (timer-fn timer) (timer-args timer)))
                     (declare (ignore ans))
                     (when err
                       (unschedule-timer timer)))
                   |#
                   ))
               *timeout-tree*)))
      
(defun make-master-timer ()
  (mpcompat:process-run-function "Master Timer"
    '()
    (lambda ()
      (loop
       (sleep 0.1)
       (check-timeouts)))))

(defvar *master-timer* (make-master-timer))

