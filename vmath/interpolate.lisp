
(in-package #:com.ral.interpolation)

;; polynomial interpolation

(defmacro bref (arr ix)
  `(aref ,arr (1- ,ix)))

(defmethod linint ((xs vector) (ys vector) (x real) &optional prev-ix)
  (let* ((nel (length xs)))
    (assert (<= nel (length ys)))
    (let ((ix  (locate xs x prev-ix)))
      (when (>= ix (1- nel))
        (decf ix))
      (let* ((x<    (aref xs ix))
             (ixp1  (1+ ix))
             (fr (/ (- x x<)
                    (- (aref xs ixp1) x<))))
        (values
         (+ (* (aref ys ixp1) fr)
            (* (aref ys ix) (- 1 fr)))
         ix)
        ))))


(defmethod polint ((xs vector) (ys vector) (x real))
  ;; given vectors xs and ys, and an abscissa x,
  ;; return the interpolate y and error estimate dy.
  (let* ((nel (length xs)))
    (assert (<= nel (length ys)))
    (let ((dif (abs (- x (bref xs 1))))
          (ns  1)
          (cs (copy-seq ys))
          (ds (copy-seq ys)))
      
      (loop for ix from 2 to nel do
            (let ((dift (abs (- x (bref xs ix)))))
              (if (< dift dif)
                  (setf ns  ix
                        dif dift))
              ))
      
      (let ((y (bref ys ns))
            dy)
        (decf ns)
        (loop for mx from 1 below nel do
              (loop for ix from 1 to (- nel mx) do
                    (let* ((ho (- (bref xs ix) x))
                           (hp (- (bref xs (+ ix mx)) x))
                           (w  (- (bref cs (1+ ix)) (bref ds ix)))
                           (den (- ho hp)))
                      
                      (assert (not (zerop den)))
                      ;; only happens if two xs's are identical
                      ;; (within roundoff)
                      
                      (let ((sf (/ w den)))
                        (setf (bref ds ix) (* sf hp)
                              (bref cs ix) (* sf ho))
                        )))
              
              ;; (format t "~&ns = ~A" ns)
              (if (< (* 2 ns) (- nel mx))
                  (setf dy (bref cs (1+ ns)))
                (progn
                  (setf dy (bref ds ns))
                  (decf ns)))
              
              (incf y dy))
        
        (values y dy)
        ))))

                  
;; -------------------------------------------------------------
;; rational (Pade) interpolation

(defmethod ratint ((xs vector) (ys vector) (x real)) ;; -> y, dy
  (let ((nel (length xs)))
    (assert (>= (length ys) nel))
    (let ((ns  1)
          (cs  (copy-seq ys))
          (ds  (copy-seq ys))
          (hh  (abs (- x (bref xs 1)))))
      
      (loop for ix from 1 to nel do
            (let ((h (abs (- x (bref xs ix)))))
              (cond ((zerop h)
                     (return-from ratint (values (bref ys ix) 0.0)))

                    ((< h hh)
                     (setf ns ix
                           hh h)))
              (incf (bref ds ix) 1.0d-25) ;; prevent rare zero-over-zero condition
              ))
      
      (let ((y (bref ys ns))
            dy)
        (decf ns)
        (loop for mx from 1 below nel do
              (loop for ix from 1 to (- nel mx) do
                    (let* ((w (- (bref cs (1+ ix)) (bref ds ix)))
                           (h (- (bref xs (+ ix mx)) x))  ;; never zero by here
                           (tt (/ (* (- (bref xs ix) x)
                                     (bref ds ix))
                                  h))
                           (dd (- tt (bref cs (1+ ix)))))

                      (assert (not (zerop dd)))
                      ;; indicates that the interpolating function has
                      ;; a pole at the requested value of x

                      (let ((sf (/ w dd)))
                        (setf (bref ds ix) (* sf (bref cs (1+ ix)))
                              (bref cs ix) (* sf tt)))
                      ))
              
              (if (< (* 2 ns) (- nel mx))
                  (setf dy (bref cs (1+ ns)))
                (progn
                  (setf dy (bref ds ns))
                  (decf ns)))
              
              (incf y dy))
        
        (values y dy)
        ))))


#|
(setf xs (vm:vectorwise ((x (vm:framp 3)))
                        (+ x 86)))
(setf ys (vm:vectorwise ((x xs))
                        (tan (* pi (/ x 180)))))

|#

;; -----------------------------------------------------------------------------
;; Cubic Spline Interpolation

(defclass <spline-data> ()
  ((xs-vector   :accessor xs-vector  :initarg :xs)
   (ys-vector   :accessor ys-vector  :initarg :ys)
   (y2s-vector  :accessor y2s-vector :initarg :y2s)
   (prev-k      :accessor prev-k                     :initform nil)
   ))


(defmethod spline ((xs vector) (ys vector)
                   &optional
                   (dy/dx-1 :natural)
                   (dy/dx-n :natural)) ;; -> <spine-data> object
  ;; xs must be ascending: x1 < x2 < ... < xn
  ;; valuees of dy/dx-1 and dy/dx-n for first and last points can either be
  ;; numeric values or keyword :natural to force natural spline fits
  ;; (zero 2nd derivative) at boundary.
  ;;
  ;; Routine returns vector of second derivatives used for spline interpolation below.
  ;; This routine is called just once to analyze the data and prepare the 2nd ders.
  (let ((nel (length xs)))
    (assert (>= (length ys) nel))

    (let ((u  (make-array (1- nel)))
          (y2 (make-array nel)))

      (labels ((dely/delx (x1 x2 y1 y2)
                 (/ (- y2 y1) (- x2 x1)))
               
               (calc1 (x1 x2 y1 y2 dy/dx)
                 (* (/ 3d0 (- x2 x1))
                    (- (dely/delx x1 x2 y1 y2) dy/dx))))
        
        (case dy/dx-1
          (:natural
           (setf (bref y2 1) 0.0
                 (bref u  1) 0.0))
          
          (t
           (setf (bref y2 1) -0.5
                 (bref u  1) (calc1 (bref xs 1) (bref xs 2)
                                    (bref ys 1) (bref ys 2)
                                    dy/dx-1)
                 )))
        
        (loop for ix from 2 below nel do
              (let* ((sig  (dely/delx (bref xs (1- ix)) (bref xs (1+ ix))
                                      (bref xs (1- ix)) (bref xs ix)))
                     (p (+ (* sig (bref y2 (1- ix))) 2d0)))
                
                (setf (bref y2 ix) (/ (- sig 1d0) p)
                      (bref u  ix) (- (dely/delx (bref xs ix) (bref xs (1+ ix))
                                                 (bref ys ix) (bref ys (1+ ix)))
                                      (dely/delx (bref xs (1- ix)) (bref xs ix)
                                                 (bref ys (1- ix)) (bref ys ix)))
                      (bref u ix) (/  (- (/ (* 6d0 (bref u ix))
                                            (- (bref xs (1+ ix)) (bref xs (1- ix))))
                                         (* sig (bref u (1- ix))))
                                      p))
                ))
        
        (let (qn un)
          (case dy/dx-n
            (:natural
             (setf qn 0.0
                   un 0.0))
            
            (t
             (setf qn 0.5
                   un (- (calc1 (bref xs (1- nel)) (bref xs nel)
                                (bref ys (1- nel)) (bref ys nel)
                                dy/dx-n))
                   )))
          
          (setf (bref y2 nel) (/ (- un (* qn (bref u (1- nel))))
                                 (+ (* qn (bref y2 (1- nel))) 1.0)))
          
          (loop for kx from (1- nel) downto 1 do
                (setf (bref y2 kx) (+ (* (bref y2 kx) (bref y2 (1+ kx)))
                                      (bref u kx))
                      ))
          (make-instance '<spline-data>
                         :xs  xs
                         :ys  ys
                         :y2s y2)
          )))))


(defmethod splint ((spl <spline-data>) (x real) &optional (prev-index (prev-k spl)))
  ;; Spline data spl comes from a previous call to spline (above).
  ;; You can override the memory of the previous index from a previous call to splint
  ;; by furnishing your own prev-index. Prev-index null causes a full bisection lookup.
  (with-accessors ((xs      xs-vector )
                   (ys      ys-vector )
                   (d2y/dx2 y2s-vector)
                   (prev-k  prev-k    )) spl
    
    (let* ((klo (1+ (locate xs x prev-index)))
           (khi (1+ klo)))
      
      (setf prev-k (1- klo))
      
      (if (>= klo (length xs))
          ;; check for off right-end of table
          (progn
            (decf klo)
            (decf khi)))
      
      (let ((h (- (bref xs khi) (bref xs klo))))
        
        (assert (not (zerop h))) ;; xs must be distinct
        
        (let ((a (/ (- (bref xs khi) x) h))
              (b (/ (- x (bref xs klo)) h)))
          
          (labels ((cubic (c d2y/dx2)
                     (* (- (* c c c) c) d2y/dx2)))
            
            (+ (* a (bref ys klo))
               (* b (bref ys khi))
               (* (/ (* h h) 6d0)
                  (+ (cubic a (bref d2y/dx2 klo))
                     (cubic b (bref d2y/dx2 khi)))
                  ))
            ))))))
                                  
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
  (let* ((x0 (first dom))
         (xn (second dom))
         (xs (vm:vectorwise ((x (vm:framp (1- npts))))
                            (+ (* (1+ x) (/ (- xn x0) npts)) x0)))
         (ys (vm:vectorwise ((x xs))
                            (funcall fn x)))
         (spl (spline xs ys :natural :natural))
         (spl2 (spline xs ys (der fn (bref xs 1)) (der fn (bref xs (1- npts)))))
         (pxs (vm:vectorwise ((x (vm:framp 500)))
                             (+ (* x (/ (- xn x0) 500)) x0))))

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
              :color :orange
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
