
(in-package :ac)

(defvar *nfarm*  4)
(defvar *farm*
  (coerce (loop for ix from 0 below *nfarm* collect
                (make-sponsor (format nil "Farm Sponsor ~d" (1+ ix))))
          'vector))

(defun farmer-fft (spons-ix)
  (actor (cust arr dst-arr)
    (destructuring-bind (nrows ncols)
        (array-dimensions arr)
      (let* ((vec    (make-array ncols
                                 :element-type (array-element-type arr)))
             (rmarr  (make-array (* nrows ncols)
                                 :element-type (array-element-type arr)
                                 :displaced-to arr))
             (ngrp   (ceiling nrows *nfarm*))
             (start  (* ngrp spons-ix))
             (end    (min nrows (+ start ngrp))))
        (loop for row from start below end do
              (progn
                (replace vec rmarr :start2 (* ncols row))
                (let ((fvec (vm:shifth (fft:fwd vec))))
                  (plt:cmplx-plot spons-ix fvec :clear t)
                  (loop for ix from 0 below ncols do
                        (setf (aref dst-arr ix row)
                              (aref fvec ix)))
                  )))
        (send cust)
        ))))

(defun fft-block (spons-ix)
  (actor (cust arr dst-arr)
     (let ((sw-in  (sponsor-switch (aref *farm* spons-ix)))
           (fft    (farmer-fft spons-ix))
           (sw-out (sponsor-switch *current-sponsor*)))

       (β _ (send sw-in β)
         (β _ (send fft β arr dst-arr)
           (send sw-out cust)))
       )))

(defun fft-farm ()
  (actor (cust)
     (send cust
           (loop for ix from 0 below *nfarm* collect
                 (fft-block ix)))
   ))

(defun par-fft ()
  (actor (cust arr)
     (β (ffts) (send (fft-farm) β)
       (let ((dst-arr (make-array (reverse (array-dimensions arr)))))
         (β _ (send par β ffts arr dst-arr)
           (send cust dst-arr)))
       )))

(defun inspector ()
  (actor msg
    (inspect msg)))

(defun show-mag-img ()
  (actor (img)
     (let* ((dims (array-dimensions img))
            (len  (reduce #'* dims))
            (mimg (make-array dims
                              :element-type 'single-float)))
       (loop for ix from 0 below len do
             (setf (row-major-aref mimg ix)
                   (coerce
                    (log (max 1e-3 (abs (row-major-aref img ix))))
                    'single-float)))
       (plt:tvscl 'imgm mimg :magn 4)) ;; (vm:shifth mimg))))
     ))

#|
(send (actor (cust)
        (let ((spon *current-sponsor*))
          (β _ (send (sponsor-switch (aref *farm* 3)) β)
            (send println *current-sponsor*)
            (β _ (send (sponsor-switch spon) β)
              (send cust *current-sponsor*)))))
      println)
      
 
(let ((img (make-array '(256 256)
                       :initial-element 0f0
                       :element-type 'single-float)))
  (loop for row from 126 to 130 do
        (loop for col from 126 to 130 do
             (setf (aref img row col) 1f0)))
  (plt:window 'img :xsize 1024 :ysize 1024)
  (plt:window 'imgm :xsize 1024 :ysize 1024)
  (plt:tvscl 'img img :magn 4)
  #|
  (send (timing 
         (actor (cust)
           (β (ans-img) (send (par-fft) β img)
             (send (show-mag-img) ans-img)
             (send cust))))
        println)
  |#
  (let ((fimg (fft2d:fwd-magnitude img)))
    (plt:tvscl 'imgm fimg :magn 4))
  )
|#
