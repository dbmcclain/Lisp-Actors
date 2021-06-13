
(in-package :fft)

(defvar twids-actor
  (ac:make-actor
   (ac:ensure-par-safe-behavior ;; because we mutate local state
    (let ((stwids (make-array 22
                              :initial-element nil
                              :adjustable t
                              :weak t))
          (dtwids (make-array 22
                              :initial-element nil
                              :adjustable t
                              :weak t))
          (ka-queue nil)
          (ka-timer nil))
      (labels ((release-twids (twids cache destroy-fn)
                 (cond ((assoc twids ka-queue)
                        ;; still in use - so setup to try again later
                        (hcl:flag-special-free-action twids))
                       (t
                        ;; no longer in use - discard the FFT setup
                        ;; (ac:send ac:*sponsor* ac:println (format nil "Destroy TWID: ~S" twids))
                        (nsubstitute nil twids cache)
                        (funcall destroy-fn (twids-psetup twids)))
                       ))
               (scan-twids ()
                 ;; remove expired twid refs to hand over to GC special-free-action
                 ;; (ac:send ac:*sponsor* ac:println "Scanning TWIDS")
                 (let ((expiration (- (get-universal-time) 30)))
                   (if (um:deletef-if ka-queue (lambda (pair)
                                                 (> expiration (cdr pair))))
                       (mp:schedule-timer-relative ka-timer 30)
                     (setf ka-timer nil))
                   ))
               (mark-twids (twids)
                 ;; hold a reference on the twids for 30 sec beyond last use
                 (let ((pair (assoc twids ka-queue)))
                   (if pair
                       (setf (cdr pair) (get-universal-time))
                     (um:aconsf ka-queue twids (get-universal-time)))
                   (unless ka-timer
                     (setf ka-timer (mp:make-timer #'ac:send ac:*slow-sponsor* ac:self :scan-twids))
                     (mp:schedule-timer-relative ka-timer 31))
                   ))
               (get-twids (cache slot create-fn)
                 ;; find or create an FFT setup at least as large as we need
                 (let ((twids (or (find-if #'identity cache :start slot)
                                  (let ((twids (funcall create-fn)))
                                    (hcl:flag-special-free-action twids)
                                    (setf (aref cache slot) twids))
                                  )))
                   (mark-twids twids) ;; mark ref in keep-alive queue
                   (twids-psetup twids))))
        (ac:alambda
         ((cust :get-twids 'single-float lg2nx)
          (ac:send cust (get-twids stwids (- lg2nx 3)
                                   (lambda ()
                                     (stwids (create-fft-setup lg2nx))))
                   ))
         ((cust :get-twids 'double-float lg2nx)
          (ac:send cust (get-twids dtwids (- lg2nx 3)
                                   (lambda ()
                                     (dtwids (create-fft-setupD lg2nx))))
                   ))
         ((:scan-twids)
          (scan-twids))
         ((:release-twids twids)
          (cond ((stwids-p twids)
                 (release-twids twids stwids #'destroy-fft-setup))
                ((dtwids-p twids)
                 (release-twids twids dtwids #'destroy-fft-setupD))
                ))
         ))))))

(defun get-twids (nx prec)
  ;; NOTE: the ASK won't be permitted to run from ac:*slow-sponsor*.
  ;; Run your FFT's in other threads.
  (let ((lg2nx (um:ceiling-log2 nx)))
    (assert (and (>= lg2nx 3)
                 (<= lg2nx 24)))
    (ac:ask ac:*slow-sponsor* twids-actor :get-twids prec lg2nx)))

(defun get-stwids (nx)
  (get-twids nx 'single-float))

(defun get-dtwids (nx)
  (get-twids nx 'double-float))

(defun free-twids (obj)
  (when (twids-p obj)
    (ac:send ac:*slow-sponsor* twids-actor :release-twids obj)))

                                           
  
