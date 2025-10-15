;; dut.lisp -- General Purpose Dataflow for Collecting Timing Data
;;
;; DM/RAL 05/21
;; -------------------------------------------------------------
(in-package :ac)
;; -------------------------------------------------------------
;; General Utility Dataflow Widgets

(defun make-timing-beh (dut)
  ;; An Actor to collect the timing in microsec of a DUT
  (lambda (cust)
    (let* ((timer  (timer)))
      (send timer :start)
      (@bind _
          (send dut @bind)
        (send timer :stop cust))
      )))

(defun make-med3-beh (dut)
  ;; Call DUT 3 times and return median of its data values
  (lambda (cust)
    (let* ((data   (make-array 3))
           (ix     2))
      (@bind (datum)
          (send dut @bind)
        (setf (aref data ix) datum)
        (decf ix)
        (if (minusp ix)
            (send cust (vmath:median data))
          (send dut self)))
      )))

(defun make-data-point-beh (dutfn &optional (dataprep #'identity))
  ;; Actor to make pairs (X, Y) of data coming from a DUT.
  ;;
  ;; The DUTFN is a behavior creation function that expects the logN
  ;; parameter as its only paramter. DUTFN is called to construct a
  ;; DUT Actor that expects only a Customer arg in a message,
  (lambda (cust logn)
    (let ((dut  (α (make-timing-beh
                    (α (funcall dutfn logn))))))
      (@bind (dt)
          (send (α (make-med3-beh dut)) @bind)
        ;; dt represents Median of 3 timings
        (send cust
              ;; send crafted (X,Y) to customer
              (funcall dataprep (list logn dt))))
      )))

(defun make-collector-beh (from to by datapt)
  ;; Automated collection of data pairs (X, Y) from a DUT
  (lambda (cust)
    (let* ((data   nil)
           (x      from))
      (@bind (pair)
          (send datapt @bind x)
        (push pair data)
        (incf x by)
        (if (> x to)
            (send cust (nreverse data))
          (send datapt self x)))
      )))

(defun make-simple-collector-beh (npts niter dut)
  ;; collect a large number of samples, npts, all normalized by niters
  ;; which are the number of iterations of the DUT being measured. At
  ;; the end it feeds the collected data values to customer.
  (let ((ans (make-array npts :element-type 'single-float))
        (ix  0))
    (lambda (cust)
      (@bind (dt)
          (send dut @bind)
        (setf (aref ans ix) (coerce (/ dt niter) 'single-float))
        (incf ix)
        (if (>= ix npts)
            (send cust ans)
          (send dut self))
        ))
    ))

(defun make-histo-beh ()
  (lambda (arr)
    (plt:histogram 'plt arr
                   :clear t
                   :title  "Timing Histogram"
                   :xtitle "Time [µs]"
                   :ytitle "Counts"
                   )))

(defun make-statistics-beh ()
  (lambda (cust arr)
    (send cust (list
                :mean   (vm:mean arr)
                :stdev  (vm:stdev arr)
                :median (vm:median arr)
                :mad    (vm:mad arr)
                ))))
    
;; ---------------------------------------------------------------
#|
;; Example:
;; Do-Nothing Fork-Bomb

(defun make-tree-beh ()
  (lambda (cust n)
    (cond ((zerop n)
           (send cust))
          (t
           (send (α (make-tree-beh)) self (1- n))
           (send (α (make-tree-beh)) self (1- n))
           (become (lambda* _
                     (become (lambda* _
                               (send cust))))))
          )))

(defun make-fbomb-beh (spon logn)
  ;; a DUT function parameterized by Sponsor and Log2N
  (lambda (cust)
    (let ((top  (α (make-tree-beh))))
      (sendx spon top cust logn))))


(defun* dataprep ((logn dt))
  (list (1+ logn)         ;; = nbr of Actors in tree
        (/ (float dt 1d0) ;; = time per Actor
           (1- (ash 2 logn)))
        ))

;; --------------

(let ((dut (um:curry #'make-fbomb-beh nil)))
  (send (α (make-data-point-beh dut #'dataprep)) (println) 10))

(let ((dut (um:curry #'make-fbomb-beh nil)))
  (send (α (make-collector-beh 5 10 1
                               (α (make-data-point-beh dut #'dataprep))))
        (println)))

;; -----------------------------------------------
;; CPS Direct Funcall Version

(defun run-direct-funcall-tree-bomb (n)
  (cond ((zerop n))
        (t
         (run-direct-funcall-tree-bomb (1- n))
         (run-direct-funcall-tree-bomb (1- n)))
        ))

(defun make-cps-fbomb-beh (logn niter)
  ;; a DUT function parameterized by Sponsor and Log2N
  (lambda (cust)
    (loop repeat niter do
          (run-direct-funcall-tree-bomb logn))
    (send cust)))

;; --------------
(let ((dut (um:curry #'make-fbomb-beh nil)))
  (send (α (make-collector-beh 5 24 1
                               (α (make-data-point-beh dut #'dataprep))
                               ))
      (actor (tbl)
        (let ((xs (map 'vector #'first tbl))
              (ys (map 'vector #'second tbl)))
          (plt:plot 'plt xs ys
                    :clear t
                    :title  "Fork-Bomb Timings [SingleThread]"
                    :xtitle "Log2[N Actors]"
                    :ytitle "Time per Actor [microsec]"
                    :symbol :circle
                    :plot-joined t)
          ))))

(let ((dut (um:curry #'make-fbomb-beh t)))
  (send (α (make-collector-beh 5 24 1
                               (α (make-data-point-beh dut #'dataprep))
                               ))
      (actor (tbl)
        (let ((xs (map 'vector #'first tbl))
              (ys (map 'vector #'second tbl)))
          (plt:plot 'plt xs ys
                    :clear t
                    :title  "Fork-Bomb Timings [MultiThread]"
                    :xtitle "Log2[N Actors]"
                    :ytitle "Time per Actor [microsec]"
                    :symbol :circle
                    :plot-joined t)
          ))))

(let ((dut   (um:curry #'make-fbomb-beh nil))
      (limit 21))
  (send (α (make-collector-beh 5 limit 1
                               (α (make-data-point-beh dut #'dataprep))
                               ))
        (actor (tbl)
          (let ((xs (map 'vector #'first tbl))
                (ys (map 'vector #'second tbl)))
            (plt:plot 'plt xs ys
                      :clear t
                      :title  "Fork-Bomb Timings"
                      :xtitle "Log2[N Actors]"
                      :ytitle "Time per Actor [microsec]"
                      :yrange '(0 2)
                      :legend "SingleThread"
                      :symbol :circle
                      :plot-joined t))
          (let ((dut (um:curry #'make-fbomb-beh t)))
            (send (α (make-collector-beh 5 limit 1
                                         (α (make-data-point-beh dut #'dataprep))
                                         ))
                  (actor (tbl)
                    (let ((xs (map 'vector #'first tbl))
                          (ys (map 'vector #'second tbl)))
                      (plt:plot 'plt xs ys
                                :title  "Fork-Bomb Timings [MultiThread]"
                                :xtitle "Log2[N Actors]"
                                :ytitle "Time per Actor [microsec]"
                                :color :red
                                :legend "8 MultiThread"
                                :symbol :circle
                                :plot-joined t))
                    (let ((dut (um:rcurry #'make-cps-fbomb-beh 200)))
                      (send (α (make-collector-beh 5 limit 1
                                                   (α (make-data-point-beh dut #'dataprep))
                                                   ))
                            (actor (tbl)
                              (let ((xs (map 'vector #'first tbl))
                                    (ys (map 'vector #'second tbl)))
                                (plt:plot 'plt xs ys
                                          :color :blue
                                          :legend "200x CPS Funcall"
                                          :symbol :circle
                                          :plot-joined t)
                                ))))
                    )))
          )))

;; -----------------------------------------------------
;; Erfc Fork-Bomb

(progn
  (defun burn-time ()
    (loop repeat 1000 do
          (random 1d0)))
  
  (defun make-erfc-tree-beh ()
    (lambda (cust n)
      (cond ((zerop n)
             (burn-time)
             (send cust))
            (t
             (send (α (make-erfc-tree-beh)) self (1- n))
             (send (α (make-erfc-tree-beh)) self (1- n))
             (become (lambda* _
                       (become (lambda* _
                                 (send cust))))))
            )))
  
  (defun make-erfc-fbomb-beh (spon logn)
    ;; a DUT function parameterized by Sponsor and Log2N
    (lambda (cust)
      (let ((top  (α (make-erfc-tree-beh))))
        (sendx spon top cust logn))))
  

  (defun run-cps-erfc-tree (n)
    (cond ((zerop n)
           (burn-time))
          (t
           (run-cps-erfc-tree (1- n))
           (run-cps-erfc-tree (1- n)))
          ))
  
  (defun make-cps-erfc-adapter-beh (logn niter)
    ;; a DUT function parameterized by Sponsor and Log2N
    (lambda (cust)
      (loop repeat niter do
            (run-cps-erfc-tree logn))
      (send cust)))
  
  (defun* dataprep ((logn dt))
    (list logn         ;; = nbr of Actors in tree
          (/ (float dt 1d0) ;; = time per Actor
             1e3 (ash 1 logn))
          )))

(let ((dut   (um:curry #'make-erfc-fbomb-beh nil))
      (limit 12))
  (@bind (tbl)
      (send (α (make-collector-beh 3 limit 1
                                   (α (make-data-point-beh dut #'dataprep))
                                   ))
            @bind)
    ;; (break)
    (let ((xs (map 'vector #'first tbl))
          (ys (map 'vector #'second tbl)))
      (plt:plot 'plt xs ys
                :clear t
                :title  "Erfc Fork-Bomb Timings"
                :xtitle "Log2[N Actors]"
                :ytitle "Time per Actor [ms]"
                :yrange '(0.0 4.0)
                :legend "SingleThread"
                :symbol :circle
                :plot-joined t))
    (let ((dut   (um:curry #'make-erfc-fbomb-beh t)))
      (@bind (tbl)
          (send (α (make-collector-beh 3 limit 1
                                       (α (make-data-point-beh dut #'dataprep))
                                       ))
                @bind)
        ;; (break)
        (let ((xs (map 'vector #'first tbl))
              (ys (map 'vector #'second tbl)))
          (plt:plot 'plt xs ys
                    :color :red
                    :legend "8 MultiThread"
                    :symbol :circle
                    :plot-joined t))
        (let ((dut (um:rcurry #'make-cps-erfc-adapter-beh 1)))
          (@bind (tbl)
              (send (α (make-collector-beh 3 limit 1
                                           (α (make-data-point-beh dut #'dataprep))
                                           ))
                    @bind)
            ;; (break)
            (let ((xs (map 'vector #'first tbl))
                  (ys (map 'vector #'second tbl)))
              (plt:plot 'plt xs ys
                        :color :blue
                        :legend "CPS Funcall"
                        :symbol :circle
                        :plot-joined t)
              ))))
      )))

|#
;; -------------------------------------------------------------------
#|
(progn
  (defun make-tst-beh (niter)
    (lambda (cust)
      (@bind (nn)
          (send @bind niter)
        (if (zerop nn)
            (send cust)
          (send self (1- nn))))
      )))

(let* ((niter 10000)
       (npts  1000)
       (dut   (α (make-simple-collector-beh npts niter
                                            (α (make-med3-beh
                                                (α (make-timing-beh
                                                    (α (make-tst-beh niter))))))))))
  (@bind (arr)
      (sendx nil dut @bind)
    (send (α (make-histo-beh)) arr)
    (send (α (make-statistics-beh)) (println) arr)
    (plt:histogram 'plt2 arr
                   :clear t
                   :title "Send/Dispatch Timing"
                   :xtitle "Time [µs]"
                   :ytitle "Counts"
                   :xrange '(0.14 0.25)
                   )))
|#
