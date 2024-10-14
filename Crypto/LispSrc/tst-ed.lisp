
(in-package :edec)

(defun tst-curves (&optional (n 1000))
  (labels ((iter ()
             (print (ed-curve-name *edcurve*))
             (time
              (dotimes (ix n)
                (ed-nth-pt ix)))))
    (with-ed-curve :curve-1174
      (iter))
    (with-ed-curve :curve-e382
      (iter))
    (with-ed-curve :curve-41417
      (iter))
    (with-ed-curve :curve-E521
      (iter))))

(let* ((bits  '(251 382 414 521))
       (times '(7.8 13.1 15.3 22.2))
       (times2 '(7.9 13.1 14.9 22.1))
       (times3 '(7.9 13.1 15.1 22.0)))
  (plt:plot 'plt bits times
            :clear  t
            :ylog   t
            :plot-joined t
            :symbol :circle
            :title  "Point Multiply Timing"
            :xtitle "Curve Size [bits]"
            :ytitle "Time [ms]")
  (plt:plot 'plt bits times2
            :symbol :circle)
  (plt:plot 'plt bits times3
            :symbol :circle)
  (multiple-value-bind (xmn ywmn slope wsigma niter)
      (linfit:regression
       (coerce (append bits bits bits) 'vector)
       (coerce (mapcar (um:rcurry 'log 2) (append times times2 times3)) 'vector)
       1)
    (list :xmn    xmn
          :ywmn   ywmn
          :slope  slope
          :wsigma wsigma
          :niter  niter))
  )
            
(labels ((ag-cost (n m k)
           (+ (1+ (* 2 n m))
              (* (/ k m) (+ 11
                      4
                      (* 3 (+ (log n 2) (log m 2)))))))
         (lst-cost (n k)
           (+ (1+ (* 2 n))
              (* k (+ 11
                        4
                        (* 3 (log n 2)))))))
  (let* ((n  64)
         (ms '(1 2 4 8 16 32 64))
         (ags4 (mapcar (um:curry #'ag-cost 64 4) ms))
         (ags2 (mapcar (um:curry #'ag-cost 64 2) ms))
         (lsts (mapcar (um:curry #'lst-cost 64) ms)))
    (plt:plot 'xplt ms lsts
              :clear t
              :title "Memory Cost for Range Proofs"
              :xtitle "Total Proofs"
              :ytitle "Number of Field Parameters"
              :legend "List Aggregation"
              :symbol :circle
              :plot-joined t
              :xlog nil
              :ylog t)
    (plt:plot 'xplt ms ags4
              :symbol :circle
              :legend "4-way Aggregation"
              :plot-joined t
              :color :red)
    (plt:plot 'xplt ms ags2
              :symbol :circle
              :legend "2-way Aggregation"
              :plot-joined t
              :color :blue)))
