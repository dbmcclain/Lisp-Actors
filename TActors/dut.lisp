;; dut.lisp -- General Purpose Dataflow for Collecting Timing Data
;;
;; DM/RAL 05/21
;; -------------------------------------------------------------
(in-package :ac)
;; -------------------------------------------------------------
;; General Utility Dataflow Widgets

(defun med3 (dut)
  ;; Call DUT 3 times and return median of its data values
  (make-actor
   (lambda (cust &rest parms)
     (let* ((data   (make-array 3))
            (ix     2))
       (beta (datum)
           (send* dut beta parms)
         (setf (aref data ix) datum)
         (decf ix)
         (if (minusp ix)
             (send cust (vmath:median data))
           (send* dut self parms)))
       ))))

(defun data-point (dut &optional (dataprep #'identity))
  ;; Actor to make pairs (X, Y) of data coming from a DUT.
  ;;
  ;; The DUTFN is a behavior creation function that expects the logN
  ;; parameter as its only paramter. DUTFN is called to construct a
  ;; DUT Actor that expects only a Customer arg in a message,
  (make-actor
   (lambda (cust param)
     (beta (y)
         (send dut beta param)
       (send cust (funcall dataprep (list param y)))
       ))))

(defun pairs-collector (from to by datapt)
  ;; Automated collection of data pairs (X, Y) from a DUT
  (make-actor
   (lambda (cust)
     (let* ((data   nil)
            (x      from))
       (beta (pair)
           (send datapt beta x)
         (push pair data)
         (incf x by)
         (if (> x to)
             (send cust (nreverse data))
           (send datapt self x)))
       ))))

(defun simple-collector (npts niter dut)
  ;; collect a large number of samples, npts, all normalized by niters
  ;; which are the number of iterations of the DUT being measured. At
  ;; the end it feeds the collected data values to customer.
  (make-actor
   (lambda (cust &rest parms)
     (let ((arr (make-array npts :element-type 'single-float))
           (ix  0))
       (beta (x)
            (send* dut beta parms)
         (setf (aref arr ix) (coerce (/ x niter) 'single-float))
         (incf ix)
         (if (>= ix npts)
             (send cust arr)
           (send* dut self parms))
         ))
     )))

(defun histogram ()
  (make-actor
   (lambda (arr)
     (plt:histogram 'plt arr
                    :clear t
                    :title  "Timing Histogram"
                    :xtitle "Time [μs]"
                    :ytitle "Counts"
                    ))))

(defun statistics ()
  (make-actor
   (lambda (cust arr)
     (send cust (list
                 :mean   (vm:mean arr)
                 :stdev  (vm:stdev arr)
                 :median (vm:median arr)
                 :mad    (vm:mad arr)
                 )))))
    
;; ---------------------------------------------------------------
;; Looking for MT Performance Resonance
#|
(progn
  (defun burn-time (niter)
    (loop repeat niter do
          (random 1d0)))
  
  (defun make-erfc-tree-beh (nlev)
    (lambda (cust niter)
      (cond ((zerop nlev)
             (burn-time niter)
             (send cust))
            (t
             (send (make-actor (make-erfc-tree-beh (1- nlev))) self niter)
             (send (make-actor (make-erfc-tree-beh (1- nlev))) self niter)
             (become (lambda* _
                       (become (lambda* _
                                 (send cust))))))
            )))
  
  (defun make-erfc-fbomb ()
    ;; a DUT function parameterized by Log2(N)
    (make-actor
     (lambda (cust niter)
       (let ((top  (make-actor (make-erfc-tree-beh 10))))
         (send top cust niter)
         ))))
  
  (defun* dataprep ((niter dt))
    (list niter             ;; = nbr of Actors in tree
          (/ (float dt 1d0) ;; = time per Actor
             #.(ash 1 10) niter))))

(let ((dut   (med3 (timing (make-erfc-fbomb))))
      (limit 256)
      (plt 'plt2))
  (beta (tbl)
      (send (pairs-collector 1 limit 1
                             (data-point dut #'dataprep))
            beta)
    ;; (break)
    (let ((xs (map 'vector #'first tbl))
          (ys (map 'vector #'second tbl)))
      (plt:plot plt xs ys
                :clear t
                :title  "10-ply Workload Fork-Bomb Timings"
                :xtitle "N [Iters of RANDOM]"
                :ytitle "Time per Actor per Iter [µs]"
                :ylog t
                :xlog t
                ;; :yrange '(0.0 20)
                :legend "SingleThread"
                :symbol :circle
                :plot-joined t))
    ))
|#
;; ---------------------------------------------------------
;; Iterate with Actors instead of DO-LOOP
#|
(progn
  (defun make-erfc-tree-beh (niter)
    (lambda (cust n)
      (cond ((zerop n)
             (let ((k-iter (actor (nn)
                             (if (zerop nn)
                                 (send cust)
                               (progn
                                 (random 1d0)
                                 (send self (1- nn)))))))
               (send k-iter niter)))
            (t
             (send (make-actor (make-erfc-tree-beh niter)) self (1- n))
             (send (make-actor (make-erfc-tree-beh niter)) self (1- n))
             (become (lambda* _
                       (become (lambda* _
                               (send cust))))))
            )))
  
  (defun make-erfc-fbomb-beh (spon niter)
    ;; a DUT function parameterized by Sponsor and Log2N
    (lambda (cust)
      (let ((top  (make-actor (make-erfc-tree-beh niter))))
        (send spon top cust 10))))
  
  (defun* dataprep ((niter dt))
    (list niter         ;; = nbr of Actors in tree
          (/ (float dt 1d0) ;; = time per Actor
             niter))))

(let ((dut   (um:curry #'make-erfc-fbomb-beh nil))
      (limit 256))
  (send (make-actor (make-collector-beh 1 limit 1
                                        (make-actor (make-data-point-beh dut #'dataprep))
                                        ))
        (actor (tbl)
          ;; (break)
          (let ((xs (map 'vector #'first tbl))
                (ys (map 'vector #'second tbl)))
            (plt:plot 'plt xs ys
                      :clear t
                      :title  "10-ply Workload Fork-Bomb Timings"
                      :xtitle "N [Iters of RANDOM]"
                      :ytitle "Time per Iter [µs]"
                      :ylog t
                      :xlog t
                      ;; :yrange '(0.0 20)
                      :legend "SingleThread"
                      :symbol :circle
                      :plot-joined t))
          (let ((dut   (um:curry #'make-erfc-fbomb-beh t)))
            (send (make-actor (make-collector-beh 1 limit 1
                                                  (make-actor (make-data-point-beh dut #'dataprep))
                                                  ))
                  (actor (tbl)
                    ;; (break)
                    (let ((xs (map 'vector #'first tbl))
                          (ys (map 'vector #'second tbl)))
                      (plt:plot 'plt xs ys
                                :color :red
                                :legend "8 MultiThread"
                                :symbol :circle
                                :plot-joined t)))))
          )))
|#
;; -----------------------------------------------------------------
;; SEND/Dispatch Timing
#|
(defun make-send-self-tst ()
  #F
  (make-actor
   (lambda (cust niter)
     (declare (fixnum niter))
     (if (zerop niter)
         (send cust)
       (send self cust (1- niter))))
   ))

(let* ((niter 10000)
       (npts  10000)
       (dut   (simple-collector npts niter
                                (med3
                                 (timing
                                  (make-send-self-tst))))))
  (let ((act (actor (cust)
               (beta (arr)
                   (send dut beta niter)
                 (send (histogram) arr)
                 (send (statistics) println arr)
                 (send cust)))))
    (send (timing act) println)))
|#
