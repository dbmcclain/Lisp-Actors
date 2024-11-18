;; fft.lisp -- interface to FFTX 2-D FFT Routines
;;
;; DM/MCFA  08/99
;; --------------------------------------------------

(in-package #:com.ral.fft)

;; ------------------------------------------------------------------
;; Show us the FFT Library Version during loading...
;;
(print (getFFTVersionString ""))
;; ------------------------------------------------------------------

;; ---------------------------------------------------------------

(defmethod check-dimension ((arr vector))
  ;; return array dimensions (ny, nx) of arr rounded up to next
  ;; power of 2.
  (max 8 (um:ceiling-pwr2 (length arr))))

(defmethod check-dimension ((arr fft-buffer))
  (fft-buffer-nx arr))
  
(defmethod effective-array-dimension ((arr vector))
  ;; return a pair (ny,nx) of array dimensions for arr.
  (length arr))

(defmethod effective-array-dimension ((arr fft-buffer))
  (fft-buffer-nx arr))


(defun effective-type (type)
  (cond ((and (consp type)
              (eql 'complex (car type)))
         (cadr type))
        (t
         type)))

(defmethod effective-array-element-type ((arr array))
  (let ((type (array-element-type arr)))
    (cond ((eql type 't)
           (effective-type (type-of (row-major-aref arr 0))))
          (t
           (effective-type (array-element-type arr)))
          )))

(defmethod effective-array-element-type ((arr fft-buffer))
  (effective-type (array-element-type (fft-buffer-r arr))))

(defun effective-ctype (precision)
  (ecase precision
    ((:float :single :single-float :altivec single-float) 
     (values :float 'single-float))
    ((:double :double-float :fftw double-float)
     (values :double 'double-float))))

(defun vec (arr &key (size (array-total-size arr)) (offset 0))
  (make-array size
              :displaced-to arr
              :displaced-index-offset offset
              :element-type (array-element-type arr)))

(defun copy-array-to-real-cvect (arr cdst nx &key precision)
  (let ((nxa (effective-array-dimension arr))
        (pad (coerce 0 precision)))
    (dotimes (ix nxa)
      (setf (fli:dereference cdst :index ix)
            (coerce (aref arr ix) precision)))
    ;; pad remainder of row with zeros to get to power-of-two size
    (dotimes (ix (- nx nxa))
      (setf (fli:dereference cdst :index (+ nxa ix)) pad))
    ))

(defun copy-array-to-complex-cvect (arr cdst nx &key precision)
  (let ((nxa (effective-array-dimension arr))
        (pad (coerce 0 precision)))
    (dotimes (ix nxa)
      (let ((v (aref arr ix)))
        (setf (fli:dereference cdst :index (+ ix ix))
              (coerce (realpart v) precision)
              (fli:dereference cdst :index (+ ix ix 1))
              (coerce (imagpart v) precision))))
    ;; pad remainder of row with zeros to get to power-of-two size
    (let ((coff (+ nxa nxa)))
      (dotimes (ix (* 2 (- nx nxa)))
        (setf (fli:dereference cdst :index (+ coff ix)) pad))
      )))

(defun make-result-array (nx precision)
  (make-array nx :element-type precision))

(defun make-complex-result-array (nx precision)
  (declare (ignore precision))
  (make-array nx :element-type 'complex))

(defun convert-real-cvect-to-array (csrc nx
                                         &key
                                         dest
                                         precision)
  (let ((rslt (or dest (make-result-array nx precision))))
    (fli:replace-foreign-array rslt csrc)
    ))

(defun convert-complex-cvect-to-array (csrc nx
                                            &key
                                            dest
                                            precision)
  (let ((rslt  (or dest (make-complex-result-array nx precision))))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix nx) rslt)
      (setf (aref rslt rix)
            (complex (fli:dereference csrc :index cix)
                     (fli:dereference csrc :index (1+ cix))))
      )))

(defun db10 (x)
  (if (plusp x)
      (* 10.0D0 (log x 10.0D0))
    -140.0D0))

(defun dtor (x)
  (* #.(/ pi 180d0) x))

(defun rtod (x)
  (* #.(/ 180d0 pi) x))

(defun phase-deg (x)
  (rtod (phase x)))

(defun convert-complex-cvect-magnitudes-to-array (csrc nx
                                                       &key
                                                       dest
                                                       precision)
  (let ((rslt (or dest (make-result-array nx precision))))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix nx) rslt)
      (setf (aref rslt rix)
            (coerce
             (abs (complex (fli:dereference csrc :index cix)
                           (fli:dereference csrc :index (1+ cix))))
             precision))
      )))

(defun convert-complex-cvect-power-to-array (csrc nx
                                                  &key
                                                  dest
                                                  precision)
  (let ((rslt (or dest (make-result-array nx precision))))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix nx) rslt)
      (setf (aref rslt rix)
            (let ((c (complex (fli:dereference csrc :index cix)
                              (fli:dereference csrc :index (1+ cix)))))
              (coerce
               (realpart (* c (conjugate c)))
               precision)))
      )))

(defun convert-complex-cvect-magnitudes-db-to-array (csrc nx
							  &key
							  dest
							  precision)
  (let ((rslt (or dest (make-result-array nx precision))))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix nx) rslt)
      (setf (aref rslt rix)
            (let ((c (complex (fli:dereference csrc :index cix)
                              (fli:dereference csrc :index (1+ cix)))))
              (coerce
               (db10 (realpart (* c (conjugate c))))
               precision)))
      )))

(defun convert-complex-cvect-phases-to-array (csrc nx
                                                   &key
                                                   dest
                                                   precision)
  (let ((rslt (or dest (make-result-array nx precision))))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix nx) rslt)
      (setf (aref rslt rix)
            (coerce
             (phase (complex (fli:dereference csrc :index cix)
                             (fli:dereference csrc :index (1+ cix))))
             precision))
      )))

(defun convert-complex-cvect-phases-deg-to-array (csrc nx
                                                   &key
                                                   dest
                                                   precision)
  (let ((rslt (or dest (make-result-array nx precision))))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix nx) rslt)
      (setf (aref rslt rix)
            (coerce
             (phase-deg
              (complex (fli:dereference csrc :index cix)
                       (fli:dereference csrc :index (1+ cix))))
             precision))
      )))

#|
(defun tst (arr)
  (let ((nx (check-dimension arr)))
    (fli:with-dynamic-foreign-objects ()
      (let ((cvec (fli:allocate-dynamic-foreign-object
                   :type :double
                   :nelems nx)))
        (copy-array-to-real-cvect arr cvec nx)
        (convert-real-cvect-to-array cvec nx))
      )))

(defun tstz (arr)
  (let ((nx (check-dimension arr)))
    (fli:with-dynamic-foreign-objects ()
      (let ((cvec (fli:allocate-dynamic-foreign-object
                   :type :double
                   :nelems (* 2 nx))))
        (copy-array-to-complex-cvect arr cvec nx)
        (convert-complex-cvect-to-array cvec nx))
      )))
|#

(defun r2c-body (arr finish-fn &key
                     (precision (effective-array-element-type arr))
                     dest)
  (um:bind*
      ((nx (check-dimension arr))
       (:values (ctype ltype) (effective-ctype precision)))
    (fli:with-dynamic-foreign-objects ()
      (let ((cdst (fli:allocate-dynamic-foreign-object
                   :type   ctype
                   :nelems (* 2 nx))))
        (fli:with-dynamic-foreign-objects ()
          (let ((csrc (fli:allocate-dynamic-foreign-object
                       :type   ctype
                       :nelems nx)))
            (copy-array-to-real-cvect arr csrc nx
                                      :precision ltype)
            (if (eq ctype :double)
                (multiple-value-bind (prtmp pitmp) (get-dtmp nx)
                  (with-dtwids (twids nx)
                    (d2zfft nx csrc cdst prtmp pitmp twids)))
              (multiple-value-bind (prtmp pitmp) (get-stmp nx)
                (with-stwids (twids nx)
                  (r2cfft nx csrc cdst prtmp pitmp twids)))
              )))
        (funcall finish-fn cdst nx
                 :dest dest
                 :precision ltype))
      )))

(defun r2c (arr &key
                (precision (effective-array-element-type arr))
                dest)
  (r2c-body arr #'convert-complex-cvect-to-array
            :precision precision
            :dest dest))

(defun slow-fwd-magnitude (arr precision dest)
  (r2c-body arr #'(lambda (cdst nx &key dest precision)
                    (convert-complex-cvect-magnitudes-to-array 
                     cdst
                     (half-dim nx)
                     :dest dest
                     :precision precision))
            :precision precision
            :dest dest))

(defun slow-fwd-power (arr precision dest)
  (r2c-body arr #'(lambda (cdst nx &key dest precision)
                    (convert-complex-cvect-power-to-array 
                     cdst
                     (half-dim nx)
                     :dest dest
                     :precision precision))
            :precision precision
            :dest dest))

(defun slow-fwd-magnitude-db (arr precision dest)
  (r2c-body arr #'(lambda (cdst nx &key dest precision)
                    (convert-complex-cvect-magnitudes-db-to-array 
                     cdst
                     (half-dim nx)
                     :dest dest
                     :precision precision))
            :precision precision
            :dest dest))

(defun slow-fwd-phase (arr precision dest)
  (r2c-body arr #'(lambda (cdst nx &key dest precision)
                    (convert-complex-cvect-phases-to-array 
                     cdst
                     (half-dim nx)
                     :dest dest
                     :precision precision))
            :precision precision
            :dest dest))

(defun slow-fwd-phase-deg (arr precision dest)
  (r2c-body arr #'(lambda (cdst nx &key dest precision)
                    (convert-complex-cvect-phases-deg-to-array 
                     cdst
                     (half-dim nx)
                     :dest dest
                     :precision precision))
            :precision precision
            :dest dest))

;; -----------------------------------------------------------

(defun c2r (arr &key
                (precision (effective-array-element-type arr))
                dest)
  (um:bind*
      ((nx (check-dimension arr))
       (:values (ctype ltype) (effective-ctype precision)))
    (fli:with-dynamic-foreign-objects ()
      (let ((cdst (fli:allocate-dynamic-foreign-object
                   :type   ctype
                   :nelems nx)))
        (fli:with-dynamic-foreign-objects ()
          (let ((csrc (fli:allocate-dynamic-foreign-object
                       :type   ctype
                       :nelems (* 2 nx))))
            (copy-array-to-complex-cvect arr csrc nx
                                         :precision ltype)
            (if (eq ctype :double)
                (with-dtwids (twids nx)
                  (multiple-value-bind (prtmp pitmp) (get-dtmp nx)
                    (z2dfft nx csrc cdst prtmp pitmp twids)))
              (with-stwids (twids nx)
                (multiple-value-bind (prtmp pitmp) (get-stmp nx)
                  (c2rfft nx csrc cdst prtmp pitmp twids))))
            ))
        (convert-real-cvect-to-array cdst nx
                                     :dest dest
                                     :precision ltype))
      )))

;; --------------------------------------------------------------
;; fast routines for split-complex FFT requirements

(defun do-copy-array-to-split-complex-cvect (type arr dst-r roff dst-i ioff nx nxa)
  (let* ((zero  (coerce 0 type))
         (vdstr (vec dst-r :offset roff :size nx))
         (vdsti (vec dst-i :offset ioff :size nx)))
    (map-into vdstr (lambda (x)
                      (coerce (realpart x) type))
              arr)
    (map-into vdsti (lambda (x)
                      (coerce (imagpart x) type))
              arr)
    (when (> nx nxa)
      (fill vdstr zero :start nxa)
      (fill vdsti zero :start nxa))
    ))

(defun do-convert-split-complex-cvect-to-array (type src-r roff src-i ioff nx dest)
  (let ((ans  (or dest (make-complex-result-array nx type)))
        (vecr (vec src-r :offset roff :size nx))
        (veci (vec src-i :offset ioff :size nx)))
    (map-into ans #'complex vecr veci)
    ans))

;; ----------------------------------------------------

(defun d-copy-array-to-split-complex-cvect (&rest args)
  (apply #'do-copy-array-to-split-complex-cvect 'double-float args))

(defun d-convert-split-complex-cvect-to-array (&rest args)
  (apply #'do-convert-split-complex-cvect-to-array 'double-float args))

(defun unsafe-z2z (arr dir nx nxa dest)
  (declare (optimize (float 0) (safety 0) (speed 3)))
  (declare (fixnum nx nxa))
  (multiple-value-bind (tmp-r roff ptr tmp-i ioff pti)
      (get-split-temp-array nx 'double-float)
    (declare (type (array double-float (*)) tmp-r tmp-i))
    (d-copy-array-to-split-complex-cvect arr tmp-r roff tmp-i ioff nx nxa)
    (with-dtwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (get-dtmp nx)
        (unsafe-z2zfft nx ptr pti dir prtmp pitmp twids)))
    (d-convert-split-complex-cvect-to-array tmp-r roff tmp-i ioff nx dest)
    ))

;; --------------------------------------------------------------

(defun s-copy-array-to-split-complex-cvect (&rest args)
  (apply #'do-copy-array-to-split-complex-cvect 'single-float args))

(defun s-convert-split-complex-cvect-to-array (&rest args)
  (apply #'do-convert-split-complex-cvect-to-array 'single-float args))

(defun unsafe-c2c (arr dir nx nxa dest)
  (declare (optimize (float 0) (safety 0) (speed 3)))
  (declare (fixnum nx nxa))
  (multiple-value-bind (tmp-r roff ptr tmp-i ioff pti)
      (get-split-temp-array nx 'single-float)
    (declare (type (array single-float (*)) tmp-r tmp-i))
    (s-copy-array-to-split-complex-cvect arr tmp-r roff tmp-i ioff nx nxa)
    (with-stwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (get-stmp nx)
        (unsafe-c2cfft nx ptr pti dir prtmp pitmp twids)))
    (s-convert-split-complex-cvect-to-array tmp-r roff tmp-i ioff nx dest)
    ))

;; --------------------------------------------------------------

(defun z2z (arr direction precision dest)
  (um:bind*
      ((nx  (check-dimension arr))
       (:declare (type fixnum nx))
       (nxa (effective-array-dimension arr))
       (:declare (type fixnum nxa))
       (:values (ctype ltype) (effective-ctype precision))
       (:declare (ignore ctype)))
    (labels ((setup-dst (dst)
               (unless (eq dst arr)
                 (copy-fft-buffer-contents arr dst))
               dst))
      (cond ((and (fft-buffer-p arr)
                  (eql ltype 'single-float)
                  (eql (array-element-type (fft-buffer-r arr)) 'single-float))
             (let ((dst (setup-dst (or dest arr))))
               (with-stwids (twids nx)
                 (multiple-value-bind (prtmp pitmp) (get-stmp nx)
                   (unsafe-c2cfft nx
                                  (fft-buffer-pr dst)
                                  (fft-buffer-pi dst)
                                  direction
                                  prtmp pitmp twids)))
               dst))
              
            ((and (fft-buffer-p arr)
                  (eql ltype 'double-float)
                  (eql (array-element-type (fft-buffer-r arr)) 'double-float))
             (let ((dst (setup-dst (or dest arr))))
               (with-dtwids (twids nx)
                 (multiple-value-bind (prtmp pitmp) (get-dtmp nx)
                   (unsafe-z2zfft nx
                                  (fft-buffer-pr dst)
                                  (fft-buffer-pi dst)
                                  direction
                                  prtmp pitmp twids)))
               dst))
              
            ((eql ltype 'single-float)
             (unsafe-c2c arr direction nx nxa dest))
              
            ((eql ltype 'double-float)
             (unsafe-z2z arr direction nx nxa dest))
              
            (t (error "Invalid precision for FFT"))
            ))))

;; --------------------------------------------------------------

(defun fwd (arr &key
                (precision (effective-array-element-type arr))
                dest)
  (z2z arr $fftw-forward precision dest))


(defun inv (arr &key
                (precision (effective-array-element-type arr))
                dest)
  (z2z arr $fftw-inverse precision dest))

;; --------------------------------------------------------------

(defun fast-capable-p (arr)
  (fft-buffer-p arr))

(defun fast-fft-oper (arr after-fn dest)
  (um:bind*
      ((:values (rarr roff ptr) (get-real arr))
       (:values (iarr ioff pti) (get-imag arr))
       (type    (array-element-type rarr))
       (dst     (or dest (fft-buffer-hr arr)))
       (nx      (fft-buffer-nx arr)))
    (set-imag arr 0)
    (if (eql type 'single-float)
        (with-stwids (twids nx)
          (multiple-value-bind (prtmp pitmp) (get-stmp nx)
            (unsafe-c2cfft
             nx
             ptr pti $fftw-forward
             prtmp pitmp twids)))
      (with-dtwids (twids nx)
        (multiple-value-bind (prtmp pitmp) (get-dtmp nx)
          (unsafe-z2zfft
           nx
           ptr pti $fftw-forward
           prtmp pitmp twids))))
    (dotimes (ix (length dst))
      (setf (aref dst ix) (funcall after-fn
                                   (aref rarr (+ ix roff))
                                   (aref iarr (+ ix ioff)))))
    dst))
    
;; --------------------------------------------------------------

(defun fwd-magnitude (arr &key
                          (precision (effective-array-element-type arr))
                          dest)
  (cond ((fast-capable-p arr)
         (fast-fwd-magnitude arr dest))

        (t (slow-fwd-magnitude arr precision dest))
        ))

(defun fast-fwd-magnitude (arr dest)
  (fast-fft-oper arr (lambda (r i)
                       (sqrt (+ (* r r) (* i i))))
                 dest))


;; --------------------------------------------------------------

(defun fwd-power (arr &key
                      (precision (effective-array-element-type arr))
                      dest)
  (cond ((fast-capable-p arr)
         (fast-fwd-power arr dest))

        (t (slow-fwd-power arr precision dest))
        ))

(defun fast-fwd-power (arr dest)
  (fast-fft-oper arr (lambda (r i)
                       (+ (* r r) (* i i)))
                 dest))
    
;; --------------------------------------------------------------

(defun fwd-magnitude-db (arr &key
			     (precision (effective-array-element-type arr))
			     dest)
  (cond ((fast-capable-p arr)
         (fast-fwd-magnitude-db arr dest))

        (t (slow-fwd-magnitude-db arr precision dest))
        ))

(defun fast-fwd-magnitude-db (arr dest)
  (fast-fft-oper arr (lambda (r i)
                       (db10 (+ (* r r) (* i i))))
                 dest))

;; --------------------------------------------------------------

(defun fwd-phase (arr &key
                      (precision (effective-array-element-type arr))
                      dest)
  (cond ((fast-capable-p arr)
         (fast-fwd-phase arr dest))

        (t (slow-fwd-phase arr precision dest))
        ))

(defun fast-fwd-phase (arr dest)
  (fast-fft-oper arr (lambda (r i)
                       (atan i r))
                 dest))

;; --------------------------------------------------------------

(defun fwd-phase-deg (arr &key
			  (precision (effective-array-element-type arr))
			  dest)
  (cond ((fast-capable-p arr)
         (fast-fwd-phase-deg arr dest))

        (t (slow-fwd-phase-deg arr precision dest))
        ))

(defun fast-fwd-phase-deg (arr dest)
  (fast-fft-oper arr (lambda (r i)
                       (rtod (atan i r)))
                 dest))

;; --------------------------------------------------------------

#|
(defun doitf (n)
  (let ((x (vm:gnoise 4096))
        (y (make-array 4096 :element-type 'complex)))
    (time
     (dotimes (ix n)
      (fft:fwd x :dest y :precision :float)))))
(compile 'doitf)

(defun doitd (n)
  (let ((x (vm:gnoise 4096))
        (y (make-array 4096 :element-type 'complex)))
    (time
     (dotimes (ix n)
       (fft:fwd x :dest y :precision :double)))))
(compile 'doitd)

(defun doitfs (n)
  (let ((x (make-fft-buffer 4096 'single-float))
        (y (make-fft-buffer 4096 'single-float)))
    (um:move (vm:gnoise 4096) 0 (fft-buffer-r x) 0 4096)
    (um:move (vm:gnoise 4096) 0 (fft-buffer-i x) 0 4096)
    (time
     (dotimes (ix n)
      (fft:fwd x :dest y :precision :float)))))
(compile 'doitf)

(defun doitds (n)
  (let ((x (make-fft-buffer 4096 'double-float))
        (y (make-fft-buffer 4096 'double-float)))
    (map-into (fft-buffer-r x) (um:rcurry #'coerce 'double-float) (vm:gnoise 4096))
    (map-into (fft-buffer-i x) (um:rcurry #'coerce 'double-float) (vm:gnoise 4096))
    (time
     (dotimes (ix n)
       (fft:fwd x :dest y :precision :double)))))
(compile 'doitd)

|#

;; -- end of fft.lisp -- ;;

