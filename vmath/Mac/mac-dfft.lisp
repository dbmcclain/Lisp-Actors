;; mac-dfft.lisp -- interface to double-precision 1D FFT routines
;;
;; DM/MCFA  08/99
;; --------------------------------------------------

(in-package #:com.ral.dfft)

;; ------------------------------------------------------------------

;; ------------------------------------------------------------------

(defun d2z (arr)
  ;; in-place routine
  (let ((nx  (fft-buffer-nx arr)))
    (fft:with-dtwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (fft:get-dtmp nx)
        (fill (fft-buffer-i arr) 0d0)
        (fft:unsafe-z2zfft nx
                           (fft-buffer-pr  arr)
                           (fft-buffer-pi  arr)
                           fft:$fftw-forward
                           prtmp pitmp twids))
      )))

(defun z2d (arr)
  ;; in-place routine
  (let ((nx  (fft-buffer-nx arr)))
    (fft:with-dtwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (fft:get-dtmp nx)
        (fft:unsafe-z2zfft nx
                           (fft-buffer-pr  arr)
                           (fft-buffer-pi  arr)
                           fft:$fftw-inverse
                           prtmp pitmp twids))
      )))

(defun z2z (arr dir)
  ;; in-place-routine
  (let ((nx  (fft-buffer-nx arr)))
    (fft:with-dtwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (fft:get-dtmp nx)
        (fft:unsafe-z2zfft nx
                           (fft-buffer-pr  arr)
                           (fft-buffer-pi  arr)
                           dir
                           prtmp pitmp twids))
      )))

;; --------------------------------------------------------------

(defun fwd (arr &key dest)
  (cond (dest
         (copy-fft-buffer-contents arr dest)
         (fwd dest))

        (t (z2z arr fft:$fftw-forward))
        ))


(defun inv (arr &key dest)
  (cond (dest
         (copy-fft-buffer-contents arr dest)
         (inv dest))

        (t (z2z arr fft:$fftw-inverse))
        ))

;; --------------------------------------------------------------

(defun fast-real-fft-oper (arr after-fn dest)
  (um:bind*
      ((:values (rarr roff ptr) (get-real arr))
       (:declare (type fixnum roff))
       (:declare (type (array double-float (*)) rarr))
       (:values (iarr ioff pti) (get-imag arr))
       (:declare (type fixnum ioff))
       (:declare (type (array double-float (*)) iarr))
       (dst     (or dest (fft-buffer-hr arr)))
       (:declare (type (array double-float (*)) dst)))
    (set-imag arr 0d0)
    (let ((nx  (fft-buffer-nx arr)))
      (fft:with-dtwids (twids nx)
        (multiple-value-bind (prtmp pitmp) (fft:get-dtmp nx)
          (fft:unsafe-c2cfft nx
                             ptr pti fft:$fftw-forward
                             prtmp pitmp twids))))
    (dotimes (ix (length dst))
      (declare (type fixnum ix))
      (setf (aref dst ix) (funcall after-fn
                                   (aref rarr (+ ix roff))
                                   (aref iarr (+ ix ioff)))))
    dst))
    
;; --------------------------------------------------------------

(defun pwr (r i)
  (declare (type double-float r i))
  (+ (* r r) (* i i)))

(defun ampl (r i)
  (declare (type double-float r i))
  (sqrt (pwr r i)))

(defun db10 (r i)
  (declare (type double-float r i))
  (* 10d0 (log (pwr r i) 10d0)))

(defun rtod (x)
  (declare (type double-float x))
  (* #.(/ 180d0 pi) x))

(defun phs-deg (r i)
  (declare (type double-float r i))
  (rtod (phs r i)))

(defun phs (r i)
  (declare (type double-float r i))
  (atan i r))

;; -----------------------------------------------------------

(defun fwd-magnitude (arr &key dest)
  (fast-real-fft-oper arr #'ampl dest))

;; --------------------------------------------------------------

(defun fwd-power (arr &key dest)
  (fast-real-fft-oper arr #'pwr dest))
    
;; --------------------------------------------------------------

(defun fwd-magnitude-db (arr &key dest)
  (fast-real-fft-oper arr #'db10 dest))

;; --------------------------------------------------------------

(defun fwd-phase (arr &key dest)
  (fast-real-fft-oper arr #'phs dest))
    
;; --------------------------------------------------------------

(defun fwd-phase-deg (arr &key dest)
  (fast-real-fft-oper arr #'phs-deg dest))


#|
(defun doitd (n)
  (let* ((nfft 1024)
         (x (make-fft-buffer nfft))
         (y (make-fft-buffer nfft)))
    (set-real x (map 'vector (um:rcurry #'coerce 'double-float) (vm:gnoise nfft)))
    (set-imag x (map 'vector (um:rcurry #'coerce 'double-float) (vm:gnoise nfft)))
    (time
     (dotimes (ix n)
      (fwd x :dest y)))))
(compile 'doitd)

|#

;; -- end of mac-dfft.lisp -- ;;

