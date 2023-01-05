;; jd.lisp
;;
;; DM/RAL  2023/01/04 18:50:00
;; ----------------------------------

(defpackage #:com.ral.useful-macros.jd
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.jd)

;; ----------------------------------
;; Adapted from IAU Standards of Fundamental Astronomy
;; SOFA ANSI C Library 20210512

(defun check-valid-date (yyyy mm dd)
  (check-type yyyy integer)
  (check-type mm   integer)
  (check-type dd   integer)
  (when (< yyyy -4799)
    (error "Invalid year: ~A" yyyy))
  (unless (<= 1 mm 12)
    (error "Invalid month: ~A" mm))
  (let ((mtab  #.#(31 28 31 30 31 30 31 31 30 31 30 31))
        ;; If February in a leap year, 1, else 0
        (ly  (if (and (eql mm 2)
                      (zerop (mod yyyy 4))
                      (or (not (zerop (mod yyyy 100)))
                          (zerop (mod yyyy 400))))
                 1 0)))
    (when (or (< dd 1)
              (> dd (+ ly (aref mtab (1- mm)))))
      (error "Invalid day: ~A" dd))
    ))

(defun date-jd (yyyy mm dd &optional (hh 0) (min 0) (ss 0))
  (check-valid-date yyyy mm dd)
  (locally
    (declare (integer yyyy mm dd))
    (let* ((my    (truncate (- mm 14) 12))
           (iypmy (+ yyyy my)))
      (+ 2400000.5
         (truncate (* 1461 (+ iypmy 4800)) 4)
         (truncate (*  367 (- mm 2 (* 12 my))) 12)
         (truncate (*   -3 (truncate (+ iypmy 4900) 100)) 4)
         dd
         -2432076
         (/ hh  24)
         (/ min 1440)
         (/ ss  86400))
      )))
         
#|
(date-jd 2000 01 01 18 00 00) ;; s.b. 2451545.25
|#
