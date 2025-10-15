;; dut.lisp -- General Purpose Dataflow for Collecting Timing Data
;;
;; DM/RAL 05/21
;; -------------------------------------------------------------
(in-package :com.ral.actors)
;; -------------------------------------------------------------
;; General Utility Dataflow Widgets

(defun med3 (dut)
  ;; Call DUT 3 times and return median of its data values
  (create
   (lambda (cust &rest parms)
     (beta (y1)
         (send* dut beta parms)
       (beta (y2)
           (send* dut beta parms)
         (beta (y3)
             (send* dut beta parms)
           (let ((minv (min y1 y2 y3))
                 (maxv (max y1 y2 y3)))
             (send cust (- (+ y1 y2 y3) minv maxv))
             )))))))

(defun minum (dut)
  (create
   (lambda (cust &rest parms)
     (beta (datum1)
         (send* dut beta parms)
       (beta (datum2)
           (send* dut beta parms)
         (beta (datum3)
             (send* dut beta parms)
           (send cust (min datum1 datum2 datum3))
           ))))))

(defun data-point (dut &optional (dataprep #'identity))
  ;; Actor to make pairs (X, Y) of data coming from a DUT.
  ;;
  ;; The DUTFN is a behavior creation function that expects the logN
  ;; parameter as its only paramter. DUTFN is called to construct a
  ;; DUT Actor that expects only a Customer arg in a message,
  (create
   (lambda (cust param)
     (beta (y)
         (send dut beta param)
       (send cust (funcall dataprep (list param y)))
       ))))

(defun pairs-collector (from to by datapt)
  ;; Automated collection of data pairs (X, Y) from a DUT
  (create
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
  (create
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
  (create
   (lambda (arr &rest args)
     (apply #'plt:histogram 'plt arr
            :clear t
            :title  "Timing Histogram"
            :xtitle "Time [μs]"
            :ytitle "Counts"
            args
            ))))

(defun statistics ()
  (create
   (lambda (cust arr)
     (send cust (list
                 :mean   (vm:mean arr)
                 :stdev  (vm:stdev arr)
                 :median (vm:median arr)
                 :mad    (vm:mad arr)
                 :min    (reduce #'min arr)
                 )))))
    
;; ---------------------------------------------------------------
;; Looking for MT Performance Resonance
#|
(progn
  (defun burn-time (niter)
    (loop repeat niter do
          (random 1d0)))
  
  (defun erfc-tree-beh (nlev)
    (lambda (cust niter)
      (cond ((zerop nlev)
             (burn-time niter)
             (send cust))
            (t
             (send (create (erfc-tree-beh (1- nlev))) self niter)
             (send (create (erfc-tree-beh (1- nlev))) self niter)
             (become (lambda* _
                       (become (lambda* _
                                 (send cust))))))
            )))
  
  (defun make-erfc-fbomb ()
    ;; a DUT function parameterized by Log2(N)
    (create
     (lambda (cust niter)
       (let ((top  (create (erfc-tree-beh 10))))
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
  (defun erfc-tree-beh (niter)
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
             (send (create (erfc-tree-beh niter)) self (1- n))
             (send (create (erfc-tree-beh niter)) self (1- n))
             (become (lambda* _
                       (become (lambda* _
                               (send cust))))))
            )))
  
  (defun erfc-fbomb-beh (spon niter)
    ;; a DUT function parameterized by Sponsor and Log2N
    (lambda (cust)
      (let ((top  (create (erfc-tree-beh niter))))
        (send spon top cust 10))))
  
  (defun* dataprep ((niter dt))
    (list niter         ;; = nbr of Actors in tree
          (/ (float dt 1d0) ;; = time per Actor
             niter))))

(let ((dut   (um:curry #'erfc-fbomb-beh nil))
      (limit 256))
  (send (create (collector-beh 1 limit 1
                                        (create (data-point-beh dut #'dataprep))
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
          (let ((dut   (um:curry #'erfc-fbomb-beh t)))
            (send (create (collector-beh 1 limit 1
                                                  (create (data-point-beh dut #'dataprep))
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
  (create
   (lambda (cust niter)
     (declare (fixnum niter))
     (if (zerop niter)
         (send cust)
       (send self cust (1- niter))))
   ))

(let* ((niter 10000)
       (npts  1000)
       (dut   (simple-collector npts niter
                                (minum ;; med3
                                 (timing
                                  (make-send-self-tst))))))
  (let ((act (actor (cust)
               (beta (arr)
                   (send dut beta niter)
                 (send (histogram) arr :xrange '(0 0.1))
                 (send (statistics) println arr)
                 (send cust)))))
    (send (timing act) println)))

(let* ((niter 10000)
       (npts  1000)
       (dut   (simple-collector npts niter
                                (minum ;; med3
                                 (timing
                                  (make-send-self-tst))))))
  (let ((act (actor (cust)
               (beta (arr)
                   (send dut beta niter)
                 (send (histogram) arr :xrange '(0 0.5))
                 (send (statistics) println arr)
                 (send cust)))))
    (stsend (timing act) println)))

(let ((v (vm:gnoise 10000)))
  (list (vm:median v)
        (vm:mad v)))

|#
