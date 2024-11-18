;; mac-sfft.lisp -- interface to single-precision 1D FFT routines
;;
;; DM/MCFA  08/99
;; --------------------------------------------------

(in-package #:com.ral.sfft)

;; ------------------------------------------------------------------

;; -----------------------------------------------------------

(defun r2c (arr)
  ;; in-place routine
  (fill (fft-buffer-i arr) 0f0)
  (let ((nx  (fft-buffer-nx arr)))
    (fft:with-stwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (fft:get-stmp nx)
        (fft:unsafe-c2cfft nx
                           (fft-buffer-pr  arr)
                           (fft-buffer-pi  arr)
                           fft:$fftw-forward
                           prtmp pitmp twids))
      )))

(defun c2r (arr)
  ;; in-place routine
  (let ((nx  (fft-buffer-nx arr)))
    (fft:with-stwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (fft:get-stmp nx)
        (fft:unsafe-c2cfft nx
                           (fft-buffer-pr  arr)
                           (fft-buffer-pi  arr)
                           fft:$fftw-inverse
                           prtmp pitmp twids))
      )))

(defun c2c (arr dir)
  ;; in-place routine
  (let ((nx  (fft-buffer-nx arr)))
    (fft:with-stwids (twids nx)
      (multiple-value-bind (prtmp pitmp) (fft:get-stmp nx)
        (fft:unsafe-c2cfft nx
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

        (t (c2c arr fft:$fftw-forward))))


(defun inv (arr &key dest)
  (cond (dest
         (copy-fft-buffer-contents arr dest)
         (inv dest))

        (t (c2c arr fft:$fftw-inverse))))

;; --------------------------------------------------------------

(defun fast-real-fft-oper (arr after-fn dest)
  (um:bind*
      ((:values (rarr roff ptr) (get-real arr))
       (:declare (type fixnum roff))
       (:declare (type (array single-float (*)) rarr))
       (:values (iarr ioff pti) (get-imag arr))
       (:declare (type fixnum ioff))
       (:declare (type (array single-float (*)) iarr))
       (dst     (or dest (fft-buffer-hr arr)))
       (:declare (type (array single-float (*)) dst)))
    ;; (set-imag arr 0f0)
    (let ((nx  (fft-buffer-nx arr)))
      (fft:with-stwids (twids nx)
        (multiple-value-bind (prtmp pitmp) (fft:get-stmp nx)
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
  (declare (type single-float r i))
  (+ (* r r) (* i i)))

(defun ampl (r i)
  (declare (type single-float r i))
  (sqrt (pwr r i)))

(defun db10 (r i)
  (declare (type single-float r i))
  (* 10f0 (log (pwr r i) 10f0)))

(defun rtod (x)
  (declare (type single-float x))
  (float (* #. (/ 180f0 pi) x) 1f0))

(defun phs-deg (r i)
  (declare (type single-float r i))
  (rtod (phs r i)))

(defun phs (r i)
  (declare (type single-float r i))
  (atan i r))

;; --------------------------------------------------------------

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
(defun doits (n)
  (let* ((nfft 1024)
         (x (make-fft-buffer nfft))
         (y (make-fft-buffer nfft)))
    (replace (fft-buffer-r x) (vm:gnoise nfft))
    (replace (fft-buffer-i x) (vm:gnoise nfft))
    (time
     (dotimes (ix n)
       (fwd x :dest y)))))
(compile 'doits)

|#

;; -- end of mac-sfft.lisp -- ;;

