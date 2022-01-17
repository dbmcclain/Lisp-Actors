;; fft-fixup.lisp -- Replace lock-based code in FFT package with an Actors-based solution
;;
;; DM/RAL 06/21
;; --------------------------------------------------------------------------------------

;; FFT Setups are allocated by the Apple VecLib as C-structs that must
;; be created before first use, and destroyed when no longer needed.
;; They are allocated in the C Heap.
;;
;; Here we use an Actors based approach toward managing these FFT
;; Setups. (Twiddle factors). An FFT needs twiddle factors at least as
;; large as its ceiling log2 size, or larger.
;;
;; We make use of LW's Special Free Actions called by the GC to remove
;; these C structs, but only after they have aged at least 30 sec.
;; This avoids constantly creating and releasing in quick succession.
;; Chances are pretty good that, if you used a particular size FFT,
;; you will use that same size (or larger) again. By keeping them
;; alive for at least 30 sec we keep the overhead of constructing them
;; to acceptably low frequency.

(in-package :fft)

(defvar twids-actor
  (ac:par-safe ;; because we mutate local state
   (ac:make-actor
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
                        ;; oops! still in use - so setup to try again later
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
    (assert (<= 3 lg2nx 24))
    (ac:foreign-ask twids-actor :get-twids prec lg2nx)))

(defun get-stwids (nx)
  ;; called by FFT routines
  (get-twids nx 'single-float))

(defun get-dtwids (nx)
  ;; called by FFT routines
  (get-twids nx 'double-float))

(defun free-twids (obj)
  ;; already recorded as a special free action
  (when (twids-p obj)
    (ac:foreign-send twids-actor :release-twids obj)))

