
(in-package #:com.ral.kaiser)

;; ---------------------------------------

(defun sfloat (x)
  (coerce x 'single-float))

(defun dfloat (x)
  (coerce x 'double-float))

(defun bref (arr &rest ixlist)
  (apply #'aref arr ixlist))

(defun (setf bref) (val arr &rest ixlist)
  (apply #'(setf aref) (coerce val (array-element-type arr))
         arr ixlist))

(defun make-sfvector (nel &key
                          initial-contents
                          (initial-element (and (null initial-contents)
                                                0.0e0)))
  (make-array nel
              :element-type    'single-float
              :initial-element  (and initial-element
                                     (sfloat initial-element))
              :initial-contents (and initial-contents
                                     (mapcar #'sfloat initial-contents))
              ))

(defun sfvector (&rest items)
  (make-sfvector (length items)
                 :initial-contents items))

;; ------------------------------------------------------
;; Kaiser Window Design of FIR Filters
;;
;; Pretty good filters, but in general, the cutoff frequencies
;; stated have -6 dB response instead of -3 dB. You can compensate
;; by edging the frequencies up or down, as needed to get -3 dB
;; at the cutoff, if that is necessary.
;;
;; ------------------------------------------------------
;; Modified Bessel Function I0
;;
(defun bessel-i0 (x)
  (do* ((d  0.0d0 (+ d 2.0d0))
        (ds 1.0d0 (/ (* ds x x) (* d d)))
        (s  1.0d0 (+ s ds)))
       ((<= ds (* 0.2d-8 s)) s)
    (declare (double-float d ds s))
    ))

(defun compute-fir-filter-order (a df)
  ;; estimate the required filter order
  ;; given stopband rejection A in -dB and
  ;; transition bandwidth df as fraction of sample rate.
  (/ (- a 8) (* 2.285 2 pi df)))

(defun beta-factor (a)
  ;; shape factor for the Kaiser windows.
  ;; Depends only on the ripple rejection ration A in -dB.
  (cond ((< a 21)  0.0)
        ((> a 50)  (* 0.1102 (- a 8.7)))
        (t         (+ (* 0.5842 (expt (- a 21) 0.4))
                      (* 0.07886 (- a 21))))
        ))

(defun kaiser-window (m a)
  ;; Kaiser window for order M and ripple -A dB
  (let* ((b     (beta-factor a))
         (i0b   (bessel-i0 b))
         (alpha (/ m 2)) ;; fractional divide here!
         (ans   (make-sfvector (1+ m))))
    (loop for ix from 0 to m do
          (setf (bref ans ix)
                (/ (bessel-i0
                    (* b
                       (sqrt
                        (- 1.0
                           (expt (/ (- ix alpha) alpha) 2)
                           ))))
                   i0b)
                ))
    ans))

(defun sinc (x)
  (if (zerop x)
      1.0
    (let ((u (* pi x)))
      (/ (sin u) u))))

