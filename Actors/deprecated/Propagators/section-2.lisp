;; section-2.lisp -- Section 2 of Radul & Sussman - Basic Propagator Networks
;;
;; DM/RAL 02/22
;; ------------------------------------------------
;; Section 2 - Basic Propagator Networks
;;
;; Execute these forms one by one and watch the Output Browser for results...

;; ---------------------------------------------------------------

(in-package :propagators)

(defun heron-step (x g h)
  (compound-propagator (x g)
    (let ((x/g   (cell))
          (g+x/g (cell))
          (two   (konst 2)))
      (divider x g x/g)
      (adder   g x/g g+x/g)
      (divider g+x/g two h))
    ))

(defcell x)
(defcell guess)
(defcell better-guess)

(heron-step x guess better-guess)

(add-content x 2)
(add-content guess 1.4)
(content better-guess)

;; ---------------------------------------------------

(defun sqrt-network (x answer)
  (compound-propagator (x)
    (let ((one  (konst 1)))
      (sqrt-iter x one answer))
    ))
   
(defun sqrt-iter (x g answer)
  (compound-propagator (x g)
    (let ((done          (cell))
          (not-done      (cell))
          (x-if-not-done (cell))
          (g-if-not-done (cell))
          (new-g         (cell)))
      (good-enuf? g x done)
      (switch done g answer)
      (inverter done not-done)
      (switch not-done x x-if-not-done)
      (switch not-done g g-if-not-done)
      (heron-step x-if-not-done g-if-not-done new-g)
      (sqrt-iter x-if-not-done new-g answer))
    ))

(defun good-enuf? (g x done)
  (compound-propagator (g x)
    (let ((g^2    (cell))
          (eps    (konst 1e-8))
          (x-g^2  (cell))
          (ax-g^2 (cell)))
      (multiplier g g g^2)
      (subtractor x g^2 x-g^2)
      (absolute-value x-g^2 ax-g^2)
      (<? ax-g^2 eps done))
    ))

(defcell x)
(defcell answer)

(sqrt-network x answer)

(add-content x 2)
(content answer)
(float (content answer) 1e0)
