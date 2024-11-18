
(in-package #:com.ral.interpolation)

(defclass <monotonic-spline-data> ()
  ((xs-vector   :accessor xs-vector  :initarg :xs)
   (ys-vector   :accessor ys-vector  :initarg :ys)
   (ms-vector   :accessor ms-vector  :initarg :ms)
   (prev-k      :accessor prev-k                     :initform nil)
   ))

(defmethod monotonic-spline ((xs vector) (ys vector))
  (let* ((nel   (array-total-size xs))
         (nelm1 (1- nel)))
    (assert (>= (array-total-size ys) nel))
    
    (let ((dels (make-array nelm1))
          (ms   (make-array nel)))

      ;; compute secants
      (labels ((dely/delx (x1 x2 y1 y2)
                 (/ (- y2 y1) (- x2 x1))))

        (um:nlet iter ((ix    0)
                       (xprev (aref xs 0))
                       (yprev (aref ys 0)))
          (let ((ixp1 (1+ ix)))
            (when (< ixp1 nel)
              (let ((xnext (aref xs ixp1))
                    (ynext (aref ys ixp1)))
                (setf (aref dels ix) (dely/delx xprev xnext yprev ynext))
                (go-iter ixp1 xnext ynext)) ))))

      ;; compute slopes
      (labels ((avg (a b)
                 (* 0.5d0 (+ a b))))

        (um:nlet iter ((ix   1)
                       (prev (setf (aref ms 0) (aref dels 0))))
          (if (< ix nelm1)
              ;; midpoint slopes use average of secants
              (let ((next (aref dels ix)))
                (setf (aref ms ix) (avg prev next))
                (go-iter (1+ ix) next))
            ;; endpoint slopes use one-sided diffs
            (setf (aref ms ix) prev))))
      
      ;; fixup slopes to preserve monotonicity
      (um:nlet iter ((ix     0)
                     (msprev (aref ms 0)))
        (let ((ixp1 (1+ ix)))
          (when (< ixp1 nel)
            (let ((delk (aref dels ix)))
              (if (zerop delk)
                  (progn
                    (setf (aref ms ix)   0d0
                          (aref ms ixp1) 0d0)
                    (go-iter ixp1 0d0))
                ;; else
                (let* ((msixp1 (aref ms ixp1))
                       (alpha  (/ msprev delk))
                       (beta   (/ msixp1 delk))
                       (sumsq  (+ (* alpha alpha) (* beta beta))))
                  (when (> sumsq 9d0)
                    ;; (print "adjusting to 3-circle")
                    (let ((tau (/ 3d0 (sqrt sumsq))))
                      (setf (aref ms ix)   (* tau msprev)
                            msixp1         (* tau msixp1)
                            (aref ms ixp1) msixp1)))
                  (go-iter ixp1 msixp1)) )))))
      
      (make-instance '<monotonic-spline-data>
                     :xs  xs
                     :ys  ys
                     :ms  ms) )))



(defmethod monotonic-splint ((mspl <monotonic-spline-data>) (x real)
                             &optional (prev-index (prev-k mspl)))
  (with-accessors ((xs      xs-vector )
                   (ys      ys-vector )
                   (ms      ms-vector)
                   (prev-k  prev-k    )) mspl
    
    (let* ((klo (locate xs x prev-index))
           (khi (1+ klo)))
      
      (setf prev-k klo)
      
      (when (>= khi (array-total-size xs))
          ;; check for off right-end of table
          (decf klo)
          (decf khi))
      
      (let* ((xlower (aref xs klo))
             (xupper (aref xs khi))
             (ylower (aref ys klo))
             (yupper (aref ys khi))
             (mlower (aref ms klo))
             (mupper (aref ms khi))
             (h      (- xupper xlower))
             (tee    (progn
                       (assert (not (zerop h))) ;; xs must be distinct
                       (/ (- x xlower) h))))

        ;; cubic Hermite spline interpolation in x [0,1)
        (labels ((h00 (x)
                   (let ((xx (- 1d0 x)))
                     (* (+ 1d0 x x) xx xx)))
                 (h10 (x)
                   (let ((xx (- 1d0 x)))
                     (* x xx xx)))
                 (h01 (x)
                   (* x x (- 3d0 x x)))
                 (h11 (x)
                   (* x x (- x 1d0))))
          
          (+ (* ylower   (h00 tee))
             (* h mlower (h10 tee))
             (* yupper   (h01 tee))
             (* h mupper (h11 tee))) )))))

#|
(defun sub-array (arr start nel)
  (make-array nel
              :element-type (array-element-type arr)
              :displaced-to arr
              :displaced-index-offset start))

(defun tand (x)
  (tan (/ (* pi x) 180)))

(defun der (fn x)
  (let ((dx (* 0.001 x)))
    (/ (- (funcall fn (+ x dx))
          (funcall fn (- x dx)))
       (* 2 dx))))

(defun tstplot (fn dom npts &key
                   (title "Interpolation Error")
                   (xtitle "X")
                   (ytitle "Error")
                   (yrange '(-11 11)))
  (let* ((x0   (first dom))
         (xn   (second dom))
         (xs   (vm:vectorwise ((x (vm:framp (1- npts))))
                              (+ (* (1+ x) (/ (- xn x0) npts)) x0)))
         (ys   (vm:vectorwise ((x xs))
                              (funcall fn x)))
         (spl  (spline xs ys :natural :natural))
         (spl2 (spline xs ys (der fn (bref xs 1)) (der fn (bref xs (1- npts)))))
         (splm (monotonic-spline xs ys))
         (pxs  (vm:vectorwise ((x (vm:framp 500)))
                              (+ (* x (/ (- xn x0) 500)) x0))))

    (plt:plot 'xx pxs (ms-vector splm) :clear t :symbol :circle :plot-joined t)
    
    (plt:plot 'plt pxs (map 'vector (lambda (x) (- (splint spl x)
                                                   (funcall fn x)))
                            pxs)
              :thick 2
              :clear t
              :title  title
              :xtitle xtitle
              :ytitle ytitle
              :yrange yrange
              :legend "natural cubic spline")
    (plt:plot 'plt2 pxs (map 'vector (lambda (x)
                                       (funcall fn x))
                             pxs)
              :clear t
              :thick 2)
    (plt:plot 'plt2 pxs (map 'vector (lambda (x)
                                       (splint spl x))
                             pxs)
              :color :red
              :thick 2
              :legend "natural cubic spline")
    (plt:plot 'plt2 pxs (map 'vector (lambda (x)
                                       (splint spl2 x))
                             pxs)
              :color :magenta
              :thick 2
              :legend "der match cubic spline")
    (plt:plot 'plt pxs (map 'vector (lambda (x) (- (splint spl2 x)
                                                   (funcall fn x)))
                            pxs)
              :color :cyan
              :thick 2
              :legend "der match cubic spline")
    (labels ((int (xs ys x n)
               (let* ((k (locate-subtable xs x (1+ n)))
                      (xs (sub-array xs k (1+ n)))
                      (ys (sub-array ys k (1+ n))))
                 (ratint xs ys x)
                 )))
      (plt:plot 'plt pxs (map 'vector (lambda (x) (- (int xs ys x 3)
                                                     (funcall fn x)))
                              pxs)
                :color :red
                :thick 2
                :legend "3rd order rational"))
    
    (labels ((int (xs ys x n)
               (let* ((k (locate-subtable xs x (1+ n)))
                      (xs (sub-array xs k (1+ n)))
                      (ys (sub-array ys k (1+ n))))
                 (polint xs ys x))))
      (plt:plot 'plt pxs (map 'vector (lambda (x) (- (int xs ys x 3)
                                                     (funcall fn x)))
                              pxs)
                :color :blue
                :thick 2
                :legend "3rd order polynomial"))
    
    (plt:plot 'plt pxs (map 'vector (lambda (x) (- (monotonic-splint splm x)
                                                   (funcall fn x)))
                            pxs)
              :color :green
              :thick 2
              :legend "monotonic spline")
    ))

(tstplot #'tand '(80 90) 10
         :title "Interpolation Error for Tan(x) [tbl:(81, 82, .. 89)]"
         :xtitle "X [deg]"
         :yrange '(-4 4))

(tstplot #'(lambda (x)
             (expt (/ (- 1d0 x)) 2))
         '(0 1)
         10
         :yrange '(-15 15)
         :title "Interpolation Error for 1/(1-x)^2 [tbl:(0, 0.1, ... 0.9)]")

(tstplot #'(lambda (x)
             (expt (/ (- 1d0 x)) 1))
         '(0 0.99)
         10
         :yrange '(-0.5 0.5)
         :title "Interpolation Error for 1/(1-x) [tbl:(0.1, 0.2, ... 0.9)]")

(tstplot #'(lambda (x)
             (exp x))
         '(0 1)
         10
         :yrange '(-0.0015 0.0015)
         :title "Interpolation Error for Exp(x) [tbl:(0.1, 0.2, .. 0.9)]")

(tstplot #'(lambda (x)
             (expt x 1))
         '(0 1)
         10
         :yrange '(-0.0001 0.0001)
         :title "Interpolation Error for x [tbl:(0.1, 0.2, .. 0.9)]")

(tstplot #'(lambda (x)
             (expt x 2))
         '(0 1)
         10
         :yrange '(-0.0015 0.0015)
         :title "Interpolation Error for x^2 [tbl:(0.1, 0.2, .. 0.9)]")

(tstplot #'(lambda (x)
             (expt x 3))
         '(0 1)
         10
         :yrange '(-0.004 0.004)
         :title "Interpolation Error for x^3 [tbl:(0.1, 0.2, .. 0.9)]")

(tstplot #'(lambda (x)
             (expt x 4))
         '(0 1)
         10
         :yrange '(-0.005 0.005)
         :title "Interpolation Error for x^4 [tbl:(0.1, 0.2, .. 0.9)]")

(tstplot #'(lambda (x)
             (expt x 5))
         '(0 1)
         10
         :yrange '(-0.01 0.01)
         :title "Interpolation Error for x^5 [tbl:(0.1, 0.2, .. 0.9)]")

|#