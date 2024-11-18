;; dfft.lisp -- interface to Double Precision FFTX 2-D FFT Routines
;;
;; DM/MCFA  08/99
;; --------------------------------------------------

(in-package #:DFFT)

;; ------------------------------------------------------------------
;; Force pseudo-initialization of MKL to flush out its initialization errors
;; SHEESH!!
(fli:with-dynamic-foreign-objects ()
  (let* ((nbuf  80)
         (buf (fli:allocate-dynamic-foreign-object
              :type :char
              :nelems nbuf)))
    (getVersionString buf nbuf)
    (format t "~&~A" (fli:convert-from-foreign-string buf))))

;; ------------------------------------------------------------------
(defun dfloat (x)
  (coerce x 'double-float))

(defun check-dimensions (arr)
  ;; return array dimensions (ny, nx) of arr rounded up to next
  ;; power of 2.
  (case (array-rank arr)
    (1 (values 1 (um:ceiling-pwr2 (array-dimension arr 0))))
    (2 (values (um:ceiling-pwr2 (array-dimension arr 0))
               (um:ceiling-pwr2 (array-dimension arr 1))))
    (otherwise (error "FFT only handles 1 and 2 dimensional arrays"))))

(defun effective-array-dimensions (arr)
  ;; return a pair (ny,nx) of array dimensions for arr.
  (ecase (array-rank arr)
    (1 (values 1 (array-dimension arr 0)))
    (2 (values (array-dimension arr 0)
               (array-dimension arr 1)))
    ))

(defun copy-array-to-real-cvect (arr cdst ny nx)
  (multiple-value-bind (nya nxa) (effective-array-dimensions arr)
    (dotimes (iy nya)
      (let ((va (make-array nxa
                            :displaced-to arr
                            :displaced-index-offset (* iy nxa)
                            :element-type (array-element-type arr)))
            (coff (* iy nx)))
        (dotimes (ix nxa)
          (setf (fli:dereference cdst :index (+ coff ix))
                (dfloat (aref va ix))))
        (incf coff nxa)
        (dotimes (ix (- nx nxa))
          (setf (fli:dereference cdst :index (+ coff ix)) 0.0))
        ))
    (let ((coff (* nx nya)))
      (dotimes (ix (* nx (- ny nya)))
        (setf (fli:dereference cdst :index (+ coff ix)) 0.0)))
    ))

(defun copy-array-to-complex-cvect (arr cdst ny nx)
  (multiple-value-bind (nya nxa) (effective-array-dimensions arr)
    (dotimes (iy nya)
      (let ((va (make-array nxa
                            :displaced-to arr
                            :displaced-index-offset (* iy nxa)
                            :element-type (array-element-type arr)))
            (coff (* 2 iy nx)))
        (dotimes (ix nxa)
          (let ((v (aref va ix)))
            (setf (fli:dereference cdst :index (+ coff ix ix))
                  (dfloat (realpart v))
                  (fli:dereference cdst :index (+ coff ix ix 1))
                  (dfloat (imagpart v)))))
        (incf coff (+ nxa nxa))
        (dotimes (ix (* 2 (- nx nxa)))
          (setf (fli:dereference cdst :index (+ coff ix)) 0.0))
        ))
    (let ((coff (* 2 nx nya)))
      (dotimes (ix (* 2 nx (- ny nya)))
        (setf (fli:dereference cdst :index (+ coff ix)) 0.0)))
    ))

(defun vec (arr)
  (make-array (array-total-size arr)
              :displaced-to arr
              :element-type (array-element-type arr)))

(defun reduced-dimensions (ny nx)
  (if (> ny 1)
      (list ny nx)
    (list nx)))

(defun make-result-array (ny nx)
  (make-array (reduced-dimensions ny nx)
              :element-type 'double-float))

(defun make-complex-result-array (ny nx)
  (make-array (reduced-dimensions ny nx)
              :element-type 'complex))

(defun convert-real-cvect-to-array (csrc ny nx
                                         &key dest)
  (let* ((rslt  (or dest (make-result-array ny nx)))
         (vrslt (vec rslt))
         (arrsiz (* nx ny)))
    (do ((ix 0 (1+ ix)))
        ((>= ix arrsiz) rslt)
      (setf (aref vrslt ix) (fli:dereference csrc :index ix)))
    ))

(defun convert-complex-cvect-to-array (csrc ny nx
                                            &key dest)
  (let* ((rslt  (or dest (make-complex-result-array ny nx)))
         (vrslt (vec rslt))
         (arrsiz (* nx ny)))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix arrsiz) rslt)
      (setf (aref vrslt rix)
            (complex (fli:dereference csrc :index cix)
                     (fli:dereference csrc :index (1+ cix))))
      )))

(defun convert-complex-cvect-magnitudes-to-array (csrc ny nx
                                                       &key dest)
  (let* ((rslt  (or dest (make-result-array ny nx)))
         (vrslt (vec rslt))
         (arrsiz (* nx ny)))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix arrsiz) rslt)
      (setf (aref vrslt rix)
            (abs (complex (fli:dereference csrc :index cix)
                          (fli:dereference csrc :index (1+ cix)))))
      )))

(defun convert-complex-cvect-phases-to-array (csrc ny nx
                                                   &key dest)
  (let* ((rslt  (or dest (make-result-array ny nx)))
         (vrslt (vec rslt))
         (arrsiz (* nx ny)))
    (do ((cix 0 (+ cix 2))
         (rix 0 (1+ rix)))
        ((>= rix arrsiz) rslt)
      (setf (aref vrslt rix)
            (phase (complex (fli:dereference csrc :index cix)
                            (fli:dereference csrc :index (1+ cix)))))
      )))

#|
(defun tst (arr)
  (multiple-value-bind (ny nx) (check-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((cvec (fli:allocate-dynamic-foreign-object
                   :type :double
                   :nelems (* nx ny))))
        (copy-array-to-real-cvect arr cvec ny nx)
        (convert-real-cvect-to-array cvec ny nx))
      )))

(defun tstz (arr)
  (multiple-value-bind (ny nx) (check-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((cvec (fli:allocate-dynamic-foreign-object
                   :type :double
                   :nelems (* 2 nx ny))))
        (copy-array-to-complex-cvect arr cvec ny nx)
        (convert-complex-cvect-to-array cvec ny nx))
      )))
|#

(defun half-dim (n)
  (max 1 (truncate n 2)))

(defun r2c-body (arr finish-fn &key
                     (precision :float)
                     dest)
  (multiple-value-bind (ny nx) (check-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((cdst (fli:allocate-dynamic-foreign-object
                   :type   precision
                   :nelems (* 2 nx ny))))
        (fli:with-dynamic-foreign-objects ()
          (let ((csrc (fli:allocate-dynamic-foreign-object
                       :type   precision
                       :nelems (* nx ny))))
            (copy-array-to-real-cvect arr csrc ny nx)
            (if (eq precision :double)
                (d2zfftf nx ny csrc cdst)
              (r2cfftf nx ny csrc cdst))))
        (funcall finish-fn cdst ny nx
                 :dest dest))
      )))

(defun r2c (arr &key
                (precision :float)
                dest)
  (r2c-body arr #'convert-complex-cvect-to-array
            :precision precision
            :dest dest))

(defun fwd-magnitude (arr &key
                          (precision :float)
                          dest)
  (r2c-body arr #'(lambda (cdst ny nx &key dest)
                    (convert-complex-cvect-magnitudes-to-array cdst
                                                               (half-dim ny)
                                                               (half-dim nx)
                                                               :dest dest))
            :precision precision
            :dest dest))

(defun fwd-phase (arr &key
                      (precision :float)
                      dest)
  (r2c-body arr #'(lambda (cdst ny nx &key dest)
                    (convert-complex-cvect-phases-to-array cdst
                                                           (half-dim ny)
                                                           (half-dim nx)
                                                           :dest dest))
            :precision precision
            :dest dest))

(defun c2r (arr &key
                (precision :float)
                dest)
  (multiple-value-bind (ny nx) (check-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((cdst (fli:allocate-dynamic-foreign-object
                   :type   precision
                   :nelems (* nx ny))))
        (fli:with-dynamic-foreign-objects ()
          (let ((csrc (fli:allocate-dynamic-foreign-object
                       :type   precision
                       :nelems (* 2 nx ny))))
            (copy-array-to-complex-cvect arr csrc ny nx)
            (if (eq precision :double)
                (z2dfftf nx ny csrc cdst)
              (c2rfftf nx ny csrc cdst))))
        (convert-real-cvect-to-array cdst ny nx
                                     :dest dest))
      )))

(defun z2z (arr dir
                &key
                (precision :float)
                dest)
  (multiple-value-bind (ny nx) (check-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((cdst (fli:allocate-dynamic-foreign-object
                   :type   precision
                   :nelems (* 2 nx ny))))
        (copy-array-to-complex-cvect arr cdst ny nx)
        (if (eq precision :double)
            (z2zfftf nx ny cdst cdst dir)
          (c2cfftf nx ny cdst cdst dir))
        (convert-complex-cvect-to-array cdst ny nx
                                        :dest dest))
      )))

(defun fwd (arr &key
                (precision :float)
                dest)
  (z2z arr -1
       :precision precision
       :dest dest))

(defun inv (arr &key
                (precision :float)
                dest)
  (z2z arr 1
       :precision precision
       :dest dest))

;; -- end of dfft.lisp -- ;;

