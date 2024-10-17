;; units.lisp
;;
;; DM/RAL  2024/10/16 14:55:37 UTC
;; ----------------------------------

(defpackage #:units
  (:use #:common-lisp #:um)
  (:shadow #:+ #:- #:* #:/ #:expt #:sqrt))

(in-package #:units)

;; --------------------------------------------
;; An augmentation that holds the units [MLT] for value.

(defgeneric units-of (x)
  (:method (x)
   '(0 0 0)))

(defclass units (augmented-value)
  ((units  :reader units-of  :initarg :units) ;; '(M L T)
   ))

(defun units (x units)
  (if (every #'zerop units)
      (nfmt x)
    (make-instance 'units
                   :val   (val-of x)
                   :units units)
    ))

(defmethod display-form ((obj units))
  `(,(nfmt (val-of obj)) ,(units-of obj)))

;; --------------------------------------------
;; Units arithmetic

(defun * (&rest args)
  (reduce (lambda (a b)
            (let* ((aval (val-of a))
                   (bval (val-of b))
                   (aun  (units-of a))
                   (bun  (units-of b))
                   (xval (cl:* aval bval))
                   (xun  (mapcar #'cl:+ aun bun)))
              (units xval xun)))
          args
          :initial-value 1))

(defun / (x &rest args)
  (if args
      (reduce (lambda (a b)
                (let* ((aval (val-of a))
                       (bval (val-of b))
                       (aun  (units-of a))
                       (bun  (units-of b))
                       (xval (cl:/ aval bval))
                       (xun  (mapcar #'cl:- aun bun)))
                  (units xval xun)))
              args
              :initial-value x)
    ;; else
    (/ 1 x)))
          
(defun expt (a b)
  (check-type b real)
  (let* ((aval  (val-of a))
         (xval  (cl:expt aval b))
         (aun   (units-of a))
         (xun   (mapcar (um:curry #'cl:* b) aun)))
    (units xval xun)))

(defun + (x &rest args)
  (if args
      (reduce (lambda (a b)
                (let* ((aval (val-of a))
                       (bval (val-of b))
                       (aun  (units-of a))
                       (bun  (units-of b)))
                  (unless (equalp aun bun)
                    (error "Need same units"))
                  (units (cl:+ aval bval) aun)))
              args
              :initial-value x)
    ;; else
    x))

(defun - (x &rest args)
  (if args
      (reduce (lambda (a b)
                (let* ((aval (val-of a))
                       (bval (val-of b))
                       (aun  (units-of a))
                       (bun  (units-of b)))
                  (unless (equalp aun bun)
                    (error "Need same units"))
                  (units (cl:- aval bval) aun)))
              args
              :initial-value x)
    ;; else
    (let ((xval (val-of x))
          (xun  (units-of x)))
      (units (cl:- xval) xun))
    ))

(defun sqr (x)
  (expt x 2))

(defun sqrt (x)
  (expt x 1/2))

;; --------------------------------------------
;; Replace the shadowed Listener *

(define-symbol-macro * cl:*)

;; --------------------------------------------

(defun mass (x)
  (units x '(1 0 0)))

(defun len (x)
  (units x '(0 1 0)))

(defun tim (x)
  (units x '(0 0 1)))

;; --------------------------------------------
;; The following are normalized to CGS.
;; --------------------------------------------
;; Length

(defun cm (x)
  (len x))

(defun mm (x)
  (cm (/ x 10.0)))

(defun μm (x)
  (mm (/ x 1000.0)))

(defun nm (x)
  (μm (/ x 1000.0)))

(defun pm (x)
  (nm (/ x 1000.0)))

(defun fm (x)
  (pm (/ x 1000.0)))

(defun m (x)
  (cm (* x 100.0)))

(defun km (x)
  (m (* x 1000.0)))

(defun inch (x)
  (cm (* 2.54 x)))

(defun ft (x)
  (inch (* 12.0 x)))

(defun mi (x)
  (ft (* 5280.0 x)))

;; --------------------------------------------
;; Time

(defun s (x)
  (tim x))

(defun ms (x)
  (s (/ x 1000.0)))

(defun μs (x)
  (ms (/ x 1000.0)))

(defun ns (x)
  (μs (/ x 1000.0)))

(defun ps (x)
  (ns (/ x 1000.0)))

(defun fs (x)
  (ps (/ x 1000.0)))

(defun mins (x)
  (s (* x 60.0)))

(defun hrs (x)
  (mins (* x 60.0)))

;; --------------------------------------------
;; Mass

(defun g (x)
  (mass x))

(defun kg (x)
  (g (* 1000.0 x)))

(defun lbm (x)
  (kg (* 2.2 x)))

;; --------------------------------------------
;; Velocity

(defun cm/s (x)
  (/ (cm x) (s 1)))

(defun m/s (x)
  (/ (m x) (s 1)))

(defun km/s (x)
  (/ (km x) (s 1)))

(defun km/hr (x)
  (/ (km x) (hrs 1)))

(defun mph (x)
  (/ (mi x) (hrs 1)))

(defun fps (x)
  (/ (ft x) (s 1)))

(defvar *clight*
  (cm/s 3e10))

;; --------------------------------------------
;; Acceleration

(defun cm/s^2 (x)
  (/ (cm x) (sqr (s 1))))

(defun m/s^2 (x)
  (/ (m x) (sqr (s 1))))

(defun ft/s^2 (x)
  (/ (ft x) (sqr (s 1))))

(defvar *gravity*
  ;; Earth's surface gravity
  (m/s^2 9.8))

;; --------------------------------------------
;; Force

(defun dyn (x)
  (* (g x) (cm/s^2 1)))

(defun newt (x)
  (* (kg x) (m/s^2 1)))

(defun lbf (x)
  (* (lbm x) *gravity*))

;; --------------------------------------------
;; Energy

(defun erg (x)
  (* (cm x) (dyn 1)))

(defun joule (x)
  (* (m x) (newt 1)))

(defun ev (x)
  (joule (* x 1.602e-19)))

(defun kev (x)
  (ev (* x 1000.0)))

(defun mev (x)
  (kev (* x 1000.0)))

(defun gev (x)
  (mev (* x 1000.0)))

(defun tev (x)
  (gev (* x 1000.0)))

;; --------------------------------------------
;; Power

(defun watts (x)
  (/ (joule x) (s 1)))

(defun kw (x)
  (watts (* x 1000.0)))

(defun mw (x)
  (watts (/ x 1000.0)))

(defun μw (x)
  (mw (/ x 1000.0)))

(defun mgw (x)
  (kw (* x 1000.0)))

(defun gw (x)
  (mgw (* x 1000.0)))

(defun hp (x)
  (watts (* x 745.7)))

#|
(/ *gravity* (ft/s^2 1))
(/ (mph 60) (fps 1))

;; Energy from converting 1 g of mass to energy, in Gj.
(/ (* (g 1) (sqr *clight*))
   (* (gw 1) (s 1)))
|#

;; --------------------------------------------
;; Frequency

(defun hz (x)
  (/ x (s 1)))

(defun milhz (x)
  (hz (/ x 1000.0)))

(defun μhz (x)
  (milhz (/ x 1000.0)))

(defun khz (x)
  (hz (* 1000.0 x)))

(defun mhz (x)
  (khz (* 1000.0 x)))

(defun ghz (x)
  (mhz (* 1000.0 x)))

(defun thz (x)
  (ghz (* 1000.0 x)))

(defvar *hplanck*
  ;; j*s
  (* (joule 6.626e-34) (s 1)))

(defvar *g-grav*
  ;; Big G: N*m^2 / kg
  (/ (* (newt 6.67e-11) (sqr (m 1)))
     (kg 1)))

#|
;; THz for 1ev photon
(/ (ev 1)
   *hplanck*
   (thz 1))

;; λ = h c/E
;; fm for 1 Gev
(/ (* *hplanck* *clight*)
   (gev 1)
   (fm 1))

;; mm Wavelength for 115 GHz
(/ *clight*
   (ghz 115)
   (mm 1))

;; Wavelength for 115 GHz
(/ *clight* (ghz 115))

;; Velocity of ionospheric ionization front
;; for 10 mHz Doppler frequency shift at 10 MHz
(* *clight* (/ (milhz 10) (mhz 10) 2)) ;; 15 cm/s

(/ (milhz 10) (mhz 10))
|#
