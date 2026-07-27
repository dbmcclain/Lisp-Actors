;; section-6.1.lisp -- Section 6.1 of Radul & Sussman - Supported with Provenance
;;
;; DM/RAL 02/22
;; ------------------------------------------------
;; Section 6.1 - Bidirectional Conputation on Supported Measurements with Provenance
;;
;; Execute these forms one by one and watch the Output Browser for results...

(in-package :propagators)

(defcell barometer-height)
(defcell barometer-shadow)
(defcell building-height)
(defcell building-shadow)
(similar-triangles barometer-shadow barometer-height building-shadow building-height)

(add-content building-shadow
             (supported (interval 54.9 55.1) '(shadows)))
(add-content barometer-height
             (supported (interval 0.3 0.32) '(shadows)))
(add-content barometer-shadow
             (supported (interval 0.36 0.37) '(shadows)))
(content building-height)

(defcell fall-time)
(fall-duration fall-time building-height)

(add-content fall-time
             (supported (interval 2.9 3.3) '(lousy-fall-time)))
(content building-height)

(add-content fall-time
             (supported (interval 2.9 3.1) '(better-fall-time)))
(content building-height)

;; -----------------------------------------------
;; Additional feedback refinements...

(content barometer-height)

(content fall-time)

;; --------------------------------------------------
;; Offer another bribe to get better info...

(add-content building-height
             (supported 45 '(superintendent)))
(content building-height)

(content barometer-height)

(content barometer-shadow)

(content building-shadow)

(content fall-time)
