;; section-3.lisp -- Section 3 of Radul & Sussman - Partial Information
;;
;; DM/RAL 02/22
;; ------------------------------------------------
;; Section 3 - Partial Information
;;
;; Execute these forms one by one and watch the Output Browser for results...

;; ---------------------------------------------------------------
;; Solve for building height using barometer time of flight...

(in-package :propagators)

(defun fall-duration (dt ht)
  (let ((g        (cell (interval 9.789 9.832)))
        (one-half (cell 1/2))
        (t^2      (cell))
        (gt^2     (cell)))
    (squarer dt t^2)
    (multiplier g t^2 gt^2)
    (multiplier one-half gt^2 ht)
    ))

(defcell fall-time)
(defcell building-height)
(fall-duration fall-time building-height)

(add-content fall-time (interval 2.9 3.1))
(content building-height)

;; --------------------------------------------------------
;; Solve for building-height barometer similar triangles...

(defun similar-triangles (s-ba h-ba s h)
  (let ((ratio (cell)))
    (divider h-ba s-ba ratio)
    (multiplier s ratio h)))

(defcell barometer-height)
(defcell barometer-shadow)
(defcell building-height)
(defcell building-shadow)
(similar-triangles barometer-shadow barometer-height building-shadow building-height)

(add-content building-shadow  (interval 54.9 55.1))
(add-content barometer-height (interval 0.3 0.32))
(add-content barometer-shadow (interval 0.36 0.37))
(content building-height)

;; ------------------------------------------------
;; Combine measurements for refined accuracy

(defcell fall-time)
(fall-duration fall-time building-height)

(add-content fall-time (interval 2.9 3.1))
(content building-height)
