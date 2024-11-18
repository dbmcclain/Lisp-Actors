;; kalman.lisp -- simple first order Kalman filtering
;;
;; DM/RAL  11/10
;; ---------------------------------------------------

(defpackage #:com.ral.kalman
  (:use #:common-lisp)
  (:export
   #:smooth
   #:smooth-fb
   #:filter
   #:make-filter
   #:predict
   #:update
   ))

(in-package #:com.ral.kalman)

;; -----------------------------------------------------------------
;; Kalman Filtering

(defun smooth (v &key (q 0.002) (p 1) (r 0.1))
  ;; q is process noise covariance
  ;; r is observation noise covariance
  ;; p is a posteriori error covariance
  ;;   p << 1 --> assume v[0] represents exact position
  ;;   p ~ 1  --> allow filter to adapt more readily
  (let* ((x  (aref v 0))
         (ps (copy-seq v))
         (ks (copy-seq v))
         (xx (loop for xm across v
                   for ix from 0
                   collect
                   (let* ((xk   x)
                          (pk   (+ p (* q q)))
                          (kk   (/ pk (+ pk (* r r))))
                          (xnew (+ xk (* kk (- xm xk))))
                          (pnew (* pk (- 1 kk))))
                     (setf (aref ps ix) (log p 10))
                     (setf (aref ks ix) (log kk 10))
                     (setf p pnew)
                     (setf x xnew))) ))
    #|
    ;; (plt:plot 'plt '(0 1000) '(0.67 0.67) :color :red :thick 2 :clear t)
    (plt:plot 'plt v  :clear t)
    (plt:plot 'plt xx :thick 2 :color :red)
    (plt:plot 'pp ps :clear t)
    (plt:plot 'kk ks :clear t)
    |#
    (print (list p (sqrt p)))
    (matrix:as-vector xx)
    ))

(defun smooth-fb (v &rest args &key (q 0.002) (p 1) (r 0.1))
  ;; forward-backward average of KF's
  (declare (ignore q p r))
  (vops:vscale 0.5 (vops:vadd
                    (reverse (apply #'smooth (reverse v) args))
                    (apply #'smooth v args))))


(defclass filter ()
  ((p   :accessor kf-p  :initarg :p  :initform 1)
   (q   :accessor kf-q  :initarg :q  :initform 0.002)
   (r   :accessor kf-r  :initarg :r  :initform 0.1)
   (xk  :accessor kf-xk :initarg :x0 :initform nil)
   ))

(defun make-filter (&key (p 1) (q 0.002) (r 0.1) x0)
  (make-instance 'filter
                 :p p
                 :q q
                 :r r
                 :x0 x0))

(defun predict (kf)
  (kf-xk kf))

(defun sq (x)
  (* x x))

(defun update (kf x)
  (let* ((pk    (+ (kf-p kf) (sq (kf-q kf))))
         (kk    (/ pk (+ pk (sq (kf-r kf)))))
         (xpred (kf-xk kf)))
    (setf (kf-xk kf)
          (if xpred
              (+ xpred  (* kk (- x xpred)))
            x)
          (kf-p kf) (* pk (- 1 kk))) ))

