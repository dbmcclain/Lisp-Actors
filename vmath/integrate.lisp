
(in-package #:com.ral.integrate)

(defclass <integration-state> ()
  ((npts  :accessor npts   :initarg :n)
   (sum   :accessor sum    :initarg :sum)
   (dx    :accessor dx     :initarg :dx)
   ))

(defmethod print-object ((obj <integration-state>) stream)
  (format stream "#<integration-state> (npts: ~A, sum: ~A, dx: ~A)"
          (npts obj)
          (sum obj)
          (dx obj)))

;; --------------------------------------------------------------
;; Trapezoid Rule

(defun trapm (fn a b &optional state)
  (if state
      (let* ((dx2 (dx state))
             (dx  (* 0.5d0 dx2))
             (n   (npts state))
             (sum (+ (sum state)
                     (loop for ix from 1 to n
                           for x from (+ a dx) by dx2
                           sum
                           (funcall fn x)
                           ))))
        (setf (npts state) (* 2 n)
              (sum state)  sum
              (dx state)   dx)
        (values (* dx sum) state))

    ;; else - first time
    (let ((dx  (- b a))
          (sum (* 0.5 (+ (funcall fn a)
                         (funcall fn b)))))
      (values (* dx sum) (make-instance '<integration-state>
                                        :n    1
                                        :sum  sum
                                        :dx   dx))
      )))
                
#|
(defun inv-exp (x)
  (/ (exp x)))

(multiple-value-bind (int state) (trapm #'inv-exp 0d0 1d0)
  (print int)
  (dotimes (ix 10)
    (print (trapm #'inv-exp 0d0 1d0 state))))

|#
;; ---------------------------------------------------------------------
;; Mid-point Integration with interval tripling

(defun mtriple (fn a b &optional state)
  (if state
      (let* ((dx3 (dx state))
             (dx  (/ dx3 3d0))
             (dx2 (+ dx dx))
             (n   (npts state))
             (sum (+ (sum state)
                     (loop for ix from 1 to n
                           for x from (+ a (* 0.5d0 dx)) by dx3
                           sum
                           (+ (funcall fn x)
                              (funcall fn (+ x dx2)))
                           ))))
        (setf (dx   state) dx
              (npts state) (* 3 n)
              (sum  state) sum)
        (values (* dx sum) state))

    ;; else -- first time
    (let* ((dx  (- b a))
           (sum (funcall fn (+ a (* 0.5d0 dx)))))
      (values (* dx sum)
              (make-instance '<integration-state>
                             :n   1
                             :dx  dx
                             :sum sum)))
    ))


#|

(multiple-value-bind (int state) (mtriple #'inv-exp 0d0 1d0)
  (print int)
  (dotimes (ix 10)
    (print (mtriple #'inv-exp 0d0 1d0 state))))

|#

;; ----------------------------------------------------------------------
;; Romberb Integration -- Richardson's extrapolation

(defun sub-array (arr start nel)
  (make-array nel
              :element-type (array-element-type arr)
              :displaced-to arr
              :displaced-index-offset start))

(defun qromb (fn a b &key (eps 1d-8) (ord 5) (nmax 20))
  ;; Rombert integration using trapezoid estimates
  (let ((s (make-array nmax))
        (h (make-array nmax)))
    (setf (aref h 0) 1d0)
    (multiple-value-bind (int state) (trapm fn a b)
      (setf (aref s 0) int)
      (loop for jx from 1 below nmax do
            (setf (aref h jx) (* 0.25d0 (aref h (1- jx)))
                  (aref s jx) (trapm fn a b state))
            (if (>= jx ord)
                (let ((xs (sub-array h (- jx ord) (1+ ord)))
                      (ys (sub-array s (- jx ord) (1+ ord))))
                  (multiple-value-bind (ss dss)
                      (interp:polint xs ys 0d0)
                    ;; (print (list ss dss))
                    (if (<= (abs dss) (* eps (abs ss)))
                        (return-from qromb (values ss dss)))
                    )))) 
      (error "Too many iterations in qromb")
      )))

#|
(qromb #'inv-exp 0d0 1d0)
|#

(defun qrombm (fn a b &key (eps 1d-8) (ord 5) (nmax 20))
  ;; Romberg integration using mid-point estimates
  (let ((s (make-array nmax))
        (h (make-array nmax)))
    (setf (aref h 0) 1d0)
    (multiple-value-bind (int state) (mtriple fn a b)
      (setf (aref s 0) int)
      (loop for jx from 1 below nmax do
            (setf (aref h jx) (/ (aref h (1- jx)) 9d0)
                  (aref s jx) (mtriple fn a b state))
            (if (>= jx ord)
                (let ((xs (sub-array h (- jx ord) (1+ ord)))
                      (ys (sub-array s (- jx ord) (1+ ord))))
                  (multiple-value-bind (ss dss)
                      (interp:polint xs ys 0d0)
                    ;; (print (list ss dss))
                    (if (<= (abs dss) (* eps (abs ss)))
                        (return-from qrombm (values ss dss)))
                    )))) 
      (error "Too many iterations in qrombn")
      )))

#|
(qrombm #'inv-exp 0d0 1d0)
(- 1d0 (inv-exp 1d0))
|#
