;; surfplot.lisp - Surface plotting
;;
;; DM/MCFA  08/99
;; -----------------------------------------------------------

(in-package #:com.ral.surface-plots)

(defstruct vpa  ;; viewpoint angles
  (cis-theta (cis 0) :type complex)
  (cis-phi   (cis 0) :type complex))

(defvar *dtor*  (/ (atan 1) 45))

(defparameter *m*
  (make-vpa :cis-theta (cis (* *dtor* 30))
	    :cis-phi   (cis (* *dtor* 15))))

(defun rot (theta phi)
  (setf (vpa-cis-theta *m*) (cis (* *dtor* theta))
        (vpa-cis-phi   *m*) (cis (* *dtor* phi))))

(defun maref (arr iy ix)
  ;; like aref for 2-D arrays, but for vector of vectors
  (aref (aref arr iy) ix))

(defun xy-plane-xform (z zrange)
  (let ((eltyp      (array-element-type z))
        (conj-theta (conjugate (vpa-cis-theta *m*)))
        (xmax       most-negative-double-float)
        (xmin       most-positive-double-float))
    (let* ((zmax (row-major-aref z 0))
           (zmin zmax))
      (destructuring-bind (ydim xdim) (array-dimensions z)
        (let ((xs (make-array ydim)))
          (dotimes (iy ydim)
            (let ((xvec (make-array xdim
                                    ;;:element-type eltyp
                                    :initial-element 0.0))
                  (zvec (make-array xdim
                                    :displaced-to z
                                    :displaced-index-offset (* iy xdim)
                                    :element-type eltyp)))
              (setf (aref xs iy) xvec)
              (dotimes (ix xdim)
                (let* ((v    (* conj-theta (complex ix iy)))
                       (vre  (realpart v))
                       (zval (aref zvec ix)))
                  (setf (aref xvec ix) v
                        xmax           (max xmax vre)
                        xmin           (min xmin vre)
                        zmax           (max zmax zval)
                        zmin           (min zmin zval))
                  ))
              ))
          ;; xs contains complex values for use in following transforms
          (list ydim xdim xs xmin xmax
                (if zrange (elt zrange 0) zmin)
                (if zrange (elt zrange 1) zmax))
          )))))

(defun xyz-xform (z &key zrange)
  (let ((eltyp (array-element-type z)))
    (destructuring-bind (ydim xdim xs xmin xmax zmin zmax)
        (xy-plane-xform z zrange)
      (destructuring-bind (wysize wxsize) (sg:get-wsize)
        (let ((xsf      (/ (* 0.95 wxsize) (- xmax xmin)))
              (zsf      (/ (* 0.8  wysize) (- zmax zmin)))
              (ys       (make-array ydim 
                                    ;;:element-type eltyp
                                    ))
              (zs       (make-array ydim 
                                    ;;:element-type eltyp
                                    ))
              (conj-phi (conjugate (vpa-cis-phi *m*)))
              (ymin     most-positive-double-float))
          (dotimes (iy ydim)
            (let ((yvec (make-array xdim :element-type eltyp))
                  (zvec (make-array xdim :element-type eltyp))
                  (xvec (aref xs iy))
                  (pvec (make-array xdim
                                    :displaced-to z
                                    :displaced-index-offset (* iy xdim)
                                    :element-type (array-element-type z))))
              (setf (aref ys iy) yvec
                    (aref zs iy) zvec)
              (dotimes (ix xdim)
                (let* ((p   (aref xvec ix))
                       (pre (* xsf (realpart p)))
                       (pim (* xsf (imagpart p)))
                       (q   (* conj-phi
                               (complex (* zsf (aref pvec ix)) pim)))
                       (qr  (realpart q))
                       (qi  (imagpart q)))
                  (setf (aref xvec ix) pre
                        (aref yvec ix) qr
                        (aref zvec ix) (- qi)
                        ymin           (min ymin qr))
                  ))
              ))
          (wxform xs ys xmin ymin)  ;; side-effecting in-place update
          (list xs ys zs))
        ))))
  
(defun wxform (ximg yimg xmin ymin)
  ;; Window transform -- converts scientific values in ximg and yimg
  ;; into display coordinates.
  ;; This performs in-place offsetting of image arrays
  ;; It also inverts the yimg for display purposes
  (destructuring-bind (wysize wxsize) (sg:get-wsize)
    (let ((offx (* 0.025 wxsize))
          (offy #+:MAC   (* 0.025 wysize)
                #+:WIN32 (* 0.975 wysize)
                ))
      (dotimes (iy (length ximg))
        (let ((xvec (aref ximg iy))
              (yvec (aref yimg iy)))
          (dotimes (ix (length xvec))
            (setf (aref xvec ix) (+ offx (- (aref xvec ix) xmin))
                  (aref yvec ix)
                  #+:MAC   (+ offy (- (aref yvec ix) ymin))
                  #+:WIN32 (+ offy (- ymin (aref yvec ix)))
                  )
            ))
        ))
    (list ximg yimg)))

(defvar *red-cmap*
  (let ((v (make-array 256 :element-type 'integer)))
    (dotimes (ix 256 v)
      (setf (aref v ix)
	    (sg:rgb (min (truncate (* 255 ix) 176) 255)
		    (max (min (truncate (* 255 (- ix 120)) 135) 255) 0)
		    (max (min (truncate (* 255 (- ix 190)) 65)  255) 0))
	    ))
    ))

(defvar *gray-cmap*
  (let ((v (make-array 256 :element-type 'integer)))
    (dotimes (ix 256 v)
      (setf (aref v ix)
	    (sg:rgb ix ix ix)))
    ))

(defun red-color-fn (iy ix ximg yimg zimg)
  (color-map-color-fn iy ix ximg yimg zimg *red-cmap*))

(defun gray-color-fn (iy ix ximg yimg zimg)
  (color-map-color-fn iy ix ximg yimg zimg *gray-cmap*))

(defun color-map-color-fn (iy ix ximg yimg zimg cmap)
  ;; yimg is inverted for display
  (let* ((vnorm  (unit-normal-vector iy ix ximg yimg zimg))
         (ctheta (aref vnorm 2)))  ; the Z-component toward viewer
    (if (plusp ctheta)
        (aref cmap (+ 128 (round (* 127 ctheta ctheta))))
      sg:$GRAY50)))

(defun rect (iy ix ximg yimg)
  ;; return a list of rect vertices (y,x)
  (labels ((getpair (iy ix)
                    (list (maref yimg iy ix)
                          (maref ximg iy ix))))
    (list
     (getpair iy      ix)
     (getpair iy      (1+ ix))
     (getpair (1+ iy) (1+ ix))
     (getpair (1+ iy) ix))
    ))

(defun vdotprod (v1 v2)
  (reduce #'+ (map 'list #'* v1 v2)))

(defun vnormalize (v)
  (let ((len (sqrt (vdotprod v v))))
    (map 'vector #'(lambda (x)
                     (/ x len))
         v)
    ))

#| ;; ... test code ...
(defun dvec (iy ix img dy dx)
  (destructuring-bind (ximg yimg zimg) (xyz-xform img)
    (labels
        ((dydx (dy dx img)
               (- (maref img (+ iy dy) (+ ix dx))
                  (maref img iy ix))))
      (mapcar #'(lambda (img)
                  (dydx dy dx img))
              (list ximg yimg zimg)))))

(defun normal-vector (iy ix img)
  (destructuring-bind (ximg yimg zimg) (xyz-xform img)
    (labels
        ((dydx (dy dx img)
               (- (maref img (+ iy dy) (+ ix dx))
                  (maref img iy ix)))
         (dvec (dy dx)
               (mapcar #'(lambda (img)
                           (dydx dy dx img))
                       (list ximg yimg zimg))))
      (let* ((dv1 (dvec 0 1))
             (dv2 (dvec 1 0)))
        (destructuring-bind (x1 y1 z1) dv2 ;; subtle reversal here...
          (destructuring-bind (x2 y2 z2) dv1
            (vnormalize
             (vector
              ;; remember y is inverted here
              (- (* y1 z2) (* z1 y2))
              (- (* x1 z2) (* z1 x2))
              (- (* x1 y2) (* y1 x2))))
            )))
      )))
    
|#

(defun unit-normal-vector (iy ix ximg yimg zimg)
  ;; yimg is inverted (top to bottom) for display
  (labels
      ((dydx (dy dx img)
             (- (maref img (+ iy dy) (+ ix dx))
                (maref img iy ix)))
       (dvec (dy dx)
             (mapcar #'(lambda (img)
                         (dydx dy dx img))
                     (list ximg yimg zimg))))
    (let* ((dv1 (dvec 0 1))
           (dv2 (dvec 1 0)))
      #+:MAC
      (destructuring-bind (x1 y1 z1) dv1
        (destructuring-bind (x2 y2 z2) dv2
          (vnormalize
           (vector
            (- (* y1 z2) (* z1 y2))
            (- (* x1 z2) (* z1 x2))
            (- (* x1 y2) (* y1 x2))))
          ))
      #+:WIN32
      (destructuring-bind (x1 y1 z1) dv2  ;; subtle reversal here...
        (destructuring-bind (x2 y2 z2) dv1
          (vnormalize
           (vector
            ;; remember y is inverted here
            (- (* y1 z2) (* z1 y2))
            (- (* x1 z2) (* z1 x2))
            (- (* x1 y2) (* y1 x2))))
          ))
      )))

#|
;; ...this would be okay except for scaling...
(defun unit-normal-vector (iy ix img)
  ;; original coords (RHS) and rank-2 image array
  ;; X -> left to right
  ;; Y -> front to back
  ;; Z -> bottom to top
  ;;
  ;; permute coords (x, y, z) -> (x, z, -y)
  ;; so that still RHS but in viewer space:
  ;; X -> left to right
  ;; Y -> bottom to top
  ;; Z -> back to front
  (let ((z0  (aref img iy ix)))
    (vnormalize
     (xform-vector
      (vector  ;; ...do the algebra and see...
       (- z0 (aref img iy (1+ ix))))
       1
       (- (aref img (1+ iy) ix) z0))
      ))
    ))
|#

(defun xform-vector (v)
  ;; X - in plane left to right
  ;; Y - in plane bottom to top
  ;; Z - out of plane toward viewer
  (let* ((p  (* (conjugate (vpa-cis-theta *m*))
                (complex (aref v 2) (aref v 0))))
         (q  (* (conjugate (vpa-cis-phi *m*))
                (complex (realpart p) (aref v 1)))))
    (vector (imagpart p)
            (imagpart q)
            (realpart q))
    ))

(defstruct lamp
  (dir-vector nil :type list)
  (rgb-triple nil :type list)
  (intensity  0   :type number))

(defparameter *lamps*
  ;; an assoc list of direction vectors, rgb colors, and intensities
  ;; Directions: X => from right to left in image plane
  ;;             Y => from bottom to top in image plane
  ;;             Z => out of image plane toward viewer
  (mapcar #'(lambda (lamp)
	      (destructuring-bind (dir color inten) lamp
		(make-lamp :dir-vector dir
			   :rgb-triple color
			   :intensity  inten)))
	  '(;; dir       (r,g,b)      inten
	    (( 2  1  0) (255 128   0)  1)
	    ((-2  1  0) (128 255   0)  2)
	    (( 1  0  1) (255 190   0)  5)
	    ((-1  0  1) (0    64 255)  3)
	    (( 0  0  1) (255   0 255)  3)
	    )))

;; diffuse light is assumed to have intensity 1.0
;; it has no specific source direction and so applies equally
;; to all faces. It is affected by the view angle to the face.
(defparameter *diffuse-light* '(160 160 190))  ;; an rgb triple

(defun lamps-color-of (vnorm)
  ;; vnorm is a unit surface normal vector
  (let* ((zfrac (aref vnorm 2))  ;; the amount in dir of viewer
         (fracs (mapcar #'(lambda (lamp)
                            ;; a weighted sum of amount in dir to viewer
                            ;; intensity, and amount in dir to lamp
                            (max 0 (* zfrac
                                      (lamp-intensity lamp)
                                      (vdotprod vnorm
                                                (vnormalize
                                                 (lamp-dir-vector lamp))))
                                 ))
                        *lamps*))
         (tfrac (1+ (reduce #'+ fracs))) ;; total weight
         (lvals (mapcar #'(lambda (lamp frac)
                            ;; form the weighted colors (r,g,b)
                            (mapcar #'(lambda (v)
                                        (* frac v))
                                    (lamp-rgb-triple lamp)))
                        *lamps* fracs))
         (fcolr (reduce #'(lambda (rslt l)
                            ;; sum the colors incl ambient
                            (mapcar #'+ rslt l))
                        lvals
                        :initial-value *diffuse-light*)))
    ;; convert weighted (r,g,b) colors to rgb value
    (apply #'sg:rgb (mapcar #'(lambda (fc)
                                (round fc tfrac))
                            fcolr))
    ))

(defun lamps-color-fn (iy ix ximg yimg zimg)
  (lamps-color-of (unit-normal-vector iy ix ximg yimg zimg)))

#+:WIN32
(defun plot-surface (img &key zrange (colorfn #'red-color-fn))
  ;; img must be a rank 2 array
  (destructuring-bind (ximg yimg zimg) (xyz-xform img :zrange zrange)
    (sg:delay-update)
    (unwind-protect
        (progn
          (sg:werase sg:$WHITE)
          ;;
          ;; plot from back to front
          ;;
          (destructuring-bind (ny nx) (array-dimensions img)
            (do ((iy (- ny 2) (1- iy)))
                ((minusp iy))
              (let ((colrs nil)
                    (rects nil))
                (dotimes (ix (1- nx))
                  (push (funcall colorfn iy ix ximg yimg zimg) colrs)
                  (push (rect iy ix ximg yimg) rects))
                (if (plusp (imagpart (vpa-cis-theta *m*)))
                    ;; sin theta > 0 => plot from left to right
                    (sg:plot-polys (nreverse rects) (nreverse colrs))
                  ;; else plot from right to left
                  (sg:plot-polys rects colrs)))
              )))
      (sg:update))
    ))

#+:MAC
(defun plot-surface (img &key zrange (colorfn #'red-color-fn))
  ;; img must be a rank 2 array
  (destructuring-bind (ximg yimg zimg) (xyz-xform img :zrange zrange)
    (destructuring-bind (ny nx) (array-dimensions img)
      (let* ((colrs nil)
	     (rects nil)
	     (recfn #'(lambda (iy ix)
			(push (funcall colorfn iy ix ximg yimg zimg) colrs)
			(push (rect iy ix ximg yimg) rects)))
	     (rowfn (if (plusp (imagpart (vpa-cis-theta *m*)))
			;; sin theta > 0 => plot from left to right
			#'(lambda (iy)
			    (do ((ix (- nx 2) (1- ix)))
				((minusp ix))
			      (funcall recfn iy ix)))
		      ;; else plot from right to left
		      #'(lambda (iy)
			  (do ((ix 0 (1+ ix)))
			      ((> ix (- nx 2)))
			    (funcall recfn iy ix)))
		      )))
	;;
	;; plot from back to front -- i.e., cache from front to back
	;;
	(do ((iy 0 (1+ iy)))
	    ((> iy (- ny 2)))
	  (funcall rowfn iy))
	(sg:werase sg:$WHITE)
	(sg:plot-polys rects colrs)
	))))

(defun lego-xform (z &key zrange)
  ;;
  ;; The Lego transform differs from a surface transform in that
  ;; we need to know the location of the base plane points
  ;; in addition to the location of the Z surface.
  ;;
  (let ((eltyp (array-element-type z)))
    (destructuring-bind (ydim xdim xs xmin xmax zmin zmax)
        (xy-plane-xform z zrange)
      (destructuring-bind (wysize wxsize) (sg:get-wsize)
        (let ((xsf (/ (* 0.95 wxsize) (- xmax xmin)))
              (zsf (/ (* 0.80 wysize) (- zmax zmin)))
              (ys  (make-array ydim :element-type eltyp))
              (zs  (make-array ydim :element-type eltyp))
              (iphi (imagpart (vpa-cis-phi *m*)))
              (iphr (realpart (vpa-cis-phi *m*)))
              (ymin most-positive-double-float))
          (dotimes (iy ydim)
            (let ((yvec (make-array xdim :element-type eltyp))
                  (zvec (make-array xdim :element-type eltyp))
                  (xvec (aref xs iy))
                  (pvec (make-array xdim
                                    :displaced-to z
                                    :displaced-index-offset (* iy xdim)
                                    :element-type (array-element-type z))))
              (setf (aref ys iy) yvec
                    (aref zs iy) zvec)
              (dotimes (ix xdim)
                (let* ((p   (aref xvec ix))
                       (pre (* xsf (realpart p)))
                       (pim (* xsf (imagpart p)))
                       (qr  (* iphi pim))
                       (qi  (* zsf iphr (- (aref pvec ix) zmin))))
                  (setf (aref xvec ix) pre
                        (aref yvec ix) qr
                        (aref zvec ix) #+:MAC (- qi) #+:WIN32 qi
                        ymin           (min ymin qr))
                  ))
              ))
          (wxform xs ys xmin ymin)  ;; side-effecting in-place update
          (list xs ys zs))
        ))))
  
(defun lego-plot (img &key zrange)
  (destructuring-bind (ximg yimg zimg) (lego-xform img :zrange zrange)
    (labels
        ((rect1 (ix iy dy)
                ;; left side
                (let ((x1 (maref ximg iy ix))
                      (y1 (maref yimg iy ix))
                      (x2 (maref ximg (1+ iy) ix))
                      (y2 (maref yimg (1+ iy) ix)))
                  (list (list y1 x1)
                        (list y2 x2)
                        (list (- y2 dy) x2)
                        (list (- y1 dy) x1))
                  ))
         (rect2 (ix iy dy)
                ;; top
                (let ((x1 (maref ximg iy ix))
                      (y1 (maref yimg iy ix))
                      (x2 (maref ximg iy (1+ ix)))
                      (y2 (maref yimg iy (1+ ix))))
                  (list (list y1 x1)
                        (list y2 x2)
                        (list (- y2 dy) x2)
                        (list (- y1 dy) x1))
                  ))
         (rect3 (ix iy dy)
                ;; right side
                (let ((x1 (maref ximg (1+ iy) (1+ ix)))
                      (y1 (maref yimg (1+ iy) (1+ ix)))
                      (x2 (maref ximg iy (1+ ix)))
                      (y2 (maref yimg iy (1+ ix))))
                    (list (list y1 x1)
                          (list y2 x2)
                          (list (- y2 dy) x2)
                          (list (- y1 dy) x1))
                    ))
         (rect4 (ix iy dy)
                ;; front
                (list (list (- (maref yimg iy ix) dy)
                            (maref ximg iy ix))
                      (list (- (maref yimg iy (1+ ix)) dy)
                            (maref ximg iy (1+ ix)))
                      (list (- (maref yimg (1+ iy) (1+ ix)) dy)
                            (maref ximg (1+ iy) (1+ ix)))
                      (list (- (maref yimg (1+ iy) ix) dy)
                            (maref ximg (1+ iy) ix)))
                ))
      (let ((color-front
             (lamps-color-of (xform-vector #(0 0 1))))
            (color-rside
             (lamps-color-of (xform-vector #(1 0 0))))
            (color-lside
             (lamps-color-of (xform-vector #(-1 0 0))))
            (color-top
             (lamps-color-of (xform-vector #(0 1 0)))))
	(sg:werase sg:$WHITE)
        #+:MAC
	(sg:with-delayed-update
	 ;;
	 ;; plot from back to front
	 ;;
	 (destructuring-bind (ny nx) (array-dimensions img)
	   (do ((iy (- ny 2) (1- iy)))
	       ((minusp iy))
	     (let ((colrs nil)
		   (rects nil))
	       (if (plusp (imagpart (vpa-cis-theta *m*)))
		   (progn
		     ;; plot from left to right
		     (dotimes (ix (1- nx))
		       (let ((dy (maref zimg iy ix)))
			 (setf colrs (list* color-top
					    color-rside
					    color-front
					    colrs))
			 (setf rects (list* (rect2 ix iy dy)
					    (rect3 ix iy dy)
					    (rect4 ix iy dy)
					    rects))
			 ))
		     (sg:plot-polys (nreverse rects) colrs))
		 
		 (progn
		   ;; plot from right to left
		   (dotimes (ix (1- nx))
		     (let ((dy (maref zimg iy ix)))
		       (setf colrs (list* color-lside
					  color-top
					  color-front
					  colrs))
		       (setf rects (list* (rect1 ix iy dy)
					  (rect2 ix iy dy)
					  (rect4 ix iy dy)
					  rects))
		       ))
		   (sg:plot-polys rects colrs)))
	       ))))
        #+:WIN32
        (progn
                  (sg:delay-update)
        (unwind-protect
            (progn
              (sg:werase sg:$WHITE)
              ;;
              ;; plot from back to front
              ;;
              (destructuring-bind (ny nx) (array-dimensions img)
                (do ((iy (- ny 2) (1- iy)))
                    ((minusp iy))
                  (let ((colrs nil)
                        (rects nil))
                    (if (plusp (imagpart (vpa-cis-theta *m*)))
                        (progn
                          ;; plot from left to right
                          (dotimes (ix (1- nx))
                            (let ((dy (maref zimg iy ix)))
                              (setf colrs (list* color-top
                                                 color-rside
                                                 color-front
                                                 colrs))
                              (setf rects (list* (rect2 ix iy dy)
                                                 (rect3 ix iy dy)
                                                 (rect4 ix iy dy)
                                                 rects))
                              ))
                          (sg:plot-polys (nreverse rects) colrs))
                      
                      (progn
                        ;; plot from right to left
                        (dotimes (ix (1- nx))
                          (let ((dy (maref zimg iy ix)))
                            (setf colrs (list* color-lside
                                               color-top
                                               color-front
                                               colrs))
                            (setf rects (list* (rect1 ix iy dy)
                                               (rect2 ix iy dy)
                                               (rect4 ix iy dy)
                                               rects))
                            ))
                        (sg:plot-polys rects colrs)))
                    ))))
          (sg:update)))

        ))))

#|  ;; from NML
let gen_test_img() =
  let x = (ramp 128 - 64 )/ 5 in
  let y = sin x / x in
    y.[64] <- 1;
    y >*< y

let test() =
  let img = gen_test_img() in
    window (0,pos: [100,200]);
    plot img.[64:*,64:*];
    window (1,pos: [400,300]);
    lego_plot img.[64:*,64:*]


(defun gen-test-img ()
  (let* ((x (vm:vectorwise ((x (vm:framp 32)))
                           (/ x 5)))
         (y (vm:vectorwise ((x x))
                           (if (zerop x)
                               1
                             (/ (sin x) x)))))
    (vm:outer-prod y y)))

(defun test ()
  (let ((img (gen-test-img)))
    (sg:wset 1)
    (plot-surface img)
    (sg:wset 2)
    (lego-plot img)))

|#
