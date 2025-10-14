;; micro-actor-tests.lisp -- A collection of timing benchmarks
;;
;; DM/RAL  04/21
;; ----------------------------------------------------------
(in-package :stac)
;; ----------------------------------------------------
;; test

;; Test behavior with induced error
(send (make-actor (lambda (x) (/ 0))) nil)

(defvar *tstlock* (mp:make-lock))

(defvar *stopwatch*
  (mp:process-run-function
   "StopWatch"
   ()
   (let ((start 0))
     (lambda ()
       (loop
        (let* ((val (mp:process-wait-for-event))
               (msg (format nil "~%Elapsed Time: ~F"
                           (- val (shiftf start val)))))
          (ac:with-worker ()
            (princ msg))
          ))))
   ))

(defun log-time ()
  (mp:process-send *stopwatch* #|(usec:get-time-usec)|# (get-universal-time)))

(defun make-test-beh (ct &optional prev)
  (let (start)
    (um:dlambda
      (:start (first)
       (if (zerop ct)
           (send first :finish-construction self)
         (let ((actor (make-actor (make-test-beh (1- ct) self))))
           (send actor :start first))))
      (:finish-construction (last)
       (setf start (usec:get-time-usec))
       (log-time)
       (send last :end))
      (:end ()
       (if prev
           (send prev :end)
         (progn
           (log-time)
           (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec)
                                           start) ct 4.0))))) 
      )))
  
(let ((actor (make-actor (make-test-beh 100000)))
      (mbox  (mp:make-mailbox)))
  (send actor :start actor))

(defun make-test-beh-ac (ct &optional prev)
  (let (start)
    (um:dlambda
      (:start (first)
       (if (zerop ct)
           (ac:send first :finish-construction ac:self)
         (let ((actor (ac:make-actor (make-test-beh-ac (1- ct) ac:self))))
           (ac:send actor :start first))))
      (:finish-construction (last)
       (setf start (usec:get-time-usec))
       (log-time)
       (ac:send last :end))
      (:end ()
       (if prev
           (ac:send prev :end)
         (progn
           (log-time)
           (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) 4.0 ct)))))
      )))

(let ((actor (ac:make-actor (make-test-beh-ac 1000000))))
  (ac:send actor :start actor))


(defun make-test-beh (ct prev)
  (let (start)
    (um:dlambda
      (:start (head)
       (if (zerop ct)
           (send head :tail self)
         (let ((actor (make-actor (make-test-beh (1- ct) self))))
           (send actor :start head))))
      (:tail (last)
       (setf start (usec:get-time-usec))
       (send last :meas))
      (:meas ()
       (if prev
           (send prev :meas)
         (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) ct))))
      )))

(let* ((actor (make-actor (make-test-beh 1000000 nil))))
  (send actor :start actor))

(defun make-test-beh-ac (ct prev)
  (let (start)
    (um:dlambda
      (:start (head)
       (if (zerop ct)
           (ac:send head :tail ac:self)
         (let ((actor (ac:make-actor (make-test-beh-ac (1- ct) ac:self))))
           (ac:send actor :start head))))
      (:tail (last)
       (setf start (usec:get-time-usec))
       (ac:send last :meas))
      (:meas ()
       (if prev
           (ac:send prev :meas)
         (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) ct))))
      )))

(let* ((actor (ac:make-actor (make-test-beh-ac 1000000 nil))))
  (ac:send actor :start actor))

(time (dotimes (ix 1000000)
        (ac:make-actor (make-test-beh 1000000 nil 0))))

(time (dotimes (ix 1000000)
        ix))


