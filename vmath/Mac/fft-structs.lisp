
(in-package #:com.ral.fft)

(defstruct twids
  expir
  refct
  prec
  log2n
  psetup)

(defstruct (fft-buffer
            (:constructor make-fft-buf))
  ny nx
  r i
  roff ioff
  hr
  siz
  pr pi)

(defmacro with-stwids ((stwids nx) &body body)
  `(do-with-twids (get-stwids ,nx) (lambda (,stwids)
                                     ,@body)))

(defmacro with-dtwids ((dtwids nx) &body body)
  `(do-with-twids (get-dtwids ,nx) (lambda (,dtwids)
                                     ,@body)))

;; ---------------------------------------------------------------

(defun half-dim (n)
  (1+ (truncate n 2)))

(defun nfloats (nb type-size)
  (assert (zerop (logand nb (1- type-size)))) ;; assure that our offsets are multiples of 4 bytes
  (truncate nb type-size))

(defun make-fft-buffer (ny nx type)
  (let* ((type-size (ecase type
                      (single-float 4)
                      (double-float 8)))
         (nya   (if (= 1 ny) 1 (max 8 (um:ceiling-pwr2 ny))))
         (nxa   (max 8 (um:ceiling-pwr2 nx)))
         (rarr  (make-array (+ (* nxa nya) 3) :element-type type :allocation :static))
         (roff  (nfloats (fft:get-align16-offset rarr) type-size))
         (iarr  (make-array (+ (* nxa nya) 7) :element-type type :allocation :static))
         (ioff  (nfloats (fft:get-align16-offset iarr) type-size))
         (hrarr (make-array (if (> ny 1)
                                (list (half-dim nya) (half-dim nxa))
                              (half-dim nxa))
                            :element-type type
                            :displaced-to rarr
                            :displaced-index-offset roff)))
    (if (zerop (logand (- (+ (* 4 ioff) (sys:object-address iarr))
                          (+ (* 4 roff) (sys:object-address rarr)))
                       (1- 1024)))
        ;; offset ioff by another 4 to avoid the Pentium quirk when two buffer addresses differ
        ;; by multiple of 1024 bytes.
        (incf ioff 4)) ;; bump by another 16 bytes
    (let ((ptr  (+ (* roff type-size) (fft:get-c-address rarr)))
          (pti  (+ (* ioff type-size) (fft:get-c-address iarr))))
      (assert (zerop (logand 15 ptr)))
      (assert (zerop (logand 15 pti)))
      
      (make-fft-buf
       :ny   nya
       :nx   nxa
       :r    rarr
       :roff roff
       :i    iarr
       :ioff ioff
       :hr   hrarr
       :pr   ptr
       :pi   pti)
      )))

(defun make-1d-fft-buffer (nx type)
  (make-fft-buffer 1 nx type))

(defun get-real (fftbuf)
  (values (fft-buffer-r fftbuf) (fft-buffer-roff fftbuf) (fft-buffer-pr fftbuf)))

(defun get-imag (fftbuf)
  (values (fft-buffer-i fftbuf) (fft-buffer-ioff fftbuf) (fft-buffer-pi fftbuf)))

(defmethod set-real (fftbuf (arr vector))
  (replace (fft-buffer-r fftbuf) arr
           :start1 (fft-buffer-roff fftbuf)))

(defmethod set-real (fftbuf (val real))
  (multiple-value-bind (buf off) (get-real fftbuf)
    (fill buf (coerce val (array-element-type buf)) :start off)))

(defmethod set-imag (fftbuf (arr vector))
  (replace (fft-buffer-i fftbuf) arr
           :start1 (fft-buffer-ioff fftbuf)))

(defmethod set-imag (fftbuf (val real))
  (multiple-value-bind (buf off) (get-imag fftbuf)
    (fill buf (coerce val (array-element-type buf)) :start off)))

(defun copy-fft-buffer-contents (src dst)
  (replace (fft-buffer-r dst) (fft-buffer-r src)
           :start1 (fft-buffer-roff dst)
           :start2 (fft-buffer-roff src))
  (replace (fft-buffer-i dst) (fft-buffer-i src)
           :start1 (fft-buffer-ioff dst)
           :start2 (fft-buffer-ioff src)))



