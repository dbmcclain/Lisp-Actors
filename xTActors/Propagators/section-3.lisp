;; section-3.lisp -- Section 3 of Radul & Sussman - Partial Information
;;
;; DM/RAL 02/22
;; ------------------------------------------------
;; Section 3 - Partial Information

(in-package :propagators)

(defun fall-duration (dt ht)
  (compound-propagator
   (lambda ()
     (let ((g        (cell))
           (one-half (cell))
           (t^2      (cell))
           (gt^2     (cell)))
       (funcall (konst (interval 9.789  9.832)) g)
       (funcall (konst (interval 1/2 1/2)) one-half)
       (funcall squarer dt t^2)
       (funcall multiplier g t^2 gt^2)
       (funcall multiplier one-half gt^2 ht)
       ))
   dt))

(defcell fall-time)
(defcell building-height)
(fall-duration fall-time building-height)

(add-content fall-time (interval 2.9 3.1))
(content building-height)

(defun similar-triangles (s-ba h-ba s h)
  (compound-propagator
   (lambda ()
     (let ((ratio (cell)))
       (funcall divider h-ba s-ba ratio)
       (funcall multiplier s ratio h)))
   s-ba h-ba s))

(defcell barometer-height)
(defcell barometer-shadow)
(defcell building-height)
(defcell building-shadow)
(similar-triangles barometer-shadow barometer-height building-shadow building-height)

(add-content building-shadow  (interval 54.9 55.1))
(add-content barometer-height (interval 0.3 0.32))
(add-content barometer-shadow (interval 0.36 0.37))
(content building-height)

(defcell fall-time)
(fall-duration fall-time building-height)

(add-content fall-time (interval 2.9 3.1))
(content building-height)
