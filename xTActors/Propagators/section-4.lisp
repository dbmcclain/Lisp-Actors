;; section-4.lisp -- Section 4 of Radul & Sussman - Bidirectional Computation
;;
;; DM/RAL 02/22
;; ------------------------------------------------
;; Section 4 - Bidirectional Conputation
;;
;; Execute these forms one by one and watch the Output Browser for results...

(in-package :propagators)

(defun product (x y total)
  (funcall multiplier x y total)
  (funcall divider total x y)
  (funcall divider total y x))

(defun quadratic (x x^2)
  (funcall squarer x x^2)
  (funcall sqrter x^2 x))

(defun fall-duration (dt ht)
  (compound-propagator
   (lambda ()
     (let ((g        (cell))
           (one-half (cell))
           (t^2      (cell))
           (gt^2     (cell)))
       (funcall (konst (interval 9.789  9.832)) g)
       (funcall (konst (interval 1/2 1/2)) one-half)
       (quadratic dt t^2)
       (product g t^2 gt^2)
       (product one-half gt^2 ht)
       ))
   dt ht))

(defun similar-triangles (s-ba h-ba s h)
  (compound-propagator
   (lambda ()
     (let ((ratio (cell)))
       (product s-ba ratio h-ba)
       (product s ratio h)))
   s-ba h-ba s h))

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

;; -----------------------------------------------
;; Additional feedback refinements...

(content barometer-height)

(content fall-time)

;; --------------------------------------------------
;; Offer another bribe to get better info...

(add-content building-height 45)
(content barometer-height)

(content barometer-shadow)

(content building-shadow)

(content fall-time)