;; linfit.lisp -- Linear regression with Peter Stetson's iteration
;;
;; DM/RAL  11/10
;; ---------------------------------------------------------------

(defpackage #:com.ral.linfit
  (:use #:common-lisp)
  (:export
   #:wmnsd
   #:regression
   #:regress-fixed-slope
   #:regress-fixed-intercept))

(in-package #:com.ral.linfit)

(defun stetson-refinement (wts devs sigma &key (alpha 2) (beta 2))
  ;; take the resids and wts and produce new wts
  (map 'vector (lambda (dev wt)
                 (/ wt (+ 1 (expt (/ (abs dev) alpha sigma) beta))))
       devs wts))

(defun regression (xs ys wts &key (alpha 2) (beta 2) (eps 1d-3))
  ;; return y-weighted mean and slope though data centroid along with stdev of fit.
  ;; iterate until the relative improvement in overall chisq
  ;; is less than eps. Be careful not to set eps below the sqrt machine precision.
  (let* ((wts   (if (numberp wts)
                    (make-array (length ys)
                                :initial-element wts)
                  wts))
         (nel   (length xs))
         (dof   (- nel 2)))
    (um:nlet iter ((swts    wts)
                   (sigprev nil)
                   (niter   0))
      (let* ((twt    (matrix:sum swts))
             (xmn    (/ (matrix:<*> xs swts) twt))
             (xsmrm  (matrix:- xs xmn))
             (ywmn   (/ (matrix:<*> ys swts) twt))
             (ysmrm  (matrix:- ys ywmn))
             (slope  (/ (matrix:<*> ysmrm xsmrm swts)
                        (matrix:<*> xsmrm xsmrm swts)))
             (devs   (matrix:- ysmrm (matrix:* slope xsmrm)))
             (wsigma (sqrt (/ (matrix:<*> swts devs devs)
                              twt (/ dof nel)))))
        (if (and (< niter 100)
                 (plusp wsigma)
                 (or (null sigprev)
                     (> (abs (- wsigma sigprev)) (* eps sigprev))))
            (go-iter (stetson-refinement wts devs wsigma
                                         :alpha alpha
                                         :beta  beta)
                     wsigma
                     (1+ niter))
          ;; else
          (progn
            (format t "~%linfit:regression niter = ~A" niter)
            (values xmn ywmn slope wsigma niter) ))))))
  

(defun regress-fixed-slope (xs ys wts slope &key (alpha 2) (beta 2) (eps 1d-3))
  ;; return weighted best intercept b in y = slope * x + b, along with stdev of fit
  ;; iterate until the relative improvement in overall chisq
  ;; is less than eps. Be careful not to set eps below the sqrt machine precision.
  (let* ((wts   (if (numberp wts)
                    (make-array (length ys)
                                :initial-element wts)
                  wts))
         (diffs  (map 'vector (lambda (x y)
                               (- y (* slope x)))
                     xs ys))
         (nel    (length xs))
         (dof    (1- nel)))
    (um:nlet iter ((swts    wts)
                   (sigprev nil)
                   (niter   0))
      (let* ((twt    (matrix:sum swts))
             (b      (/ (matrix:<*> diffs swts) twt))
             (devs   (matrix:- diffs b))
             (wsigma (sqrt (/ (matrix:<*> swts devs devs)
                              twt (/ dof nel)))))
        (if (and (< niter 100)
                 (plusp wsigma)
                 (or (null sigprev)
                     (> (abs (- wsigma sigprev)) (* eps sigprev))))
            (go-iter (stetson-refinement wts devs wsigma
                                         :alpha alpha
                                         :beta  beta)
                     wsigma
                     (1+ niter))
          ;; else
          (progn
            (format t "~%linfit:regress-fixed-slope niter = ~A" niter)
            (values b wsigma niter) ))))))
  

(defun regress-fixed-intercept (xs ys wts intercept &key (alpha 2) (beta 2) (eps 1d-3))
  ;; return weighted best intercept b in y = slope * x + b, along with stdev of fit
  ;; iterate until the relative improvement in overall chisq
  ;; is less than eps. Be careful not to set eps below the sqrt machine precision.
  (let* ((wts   (if (numberp wts)
                    (make-array (length ys)
                                :initial-element wts)
                  wts))
         (nel   (length xs))
         (dof   (1- nel)))
    (um:nlet iter ((swts    wts)
                   (sigprev nil)
                   (niter   0))
      (let* ((twt    (matrix:sum swts))
             (slope  (/ (matrix:<*> (matrix:- ys intercept) xs swts)
                        (matrix:<*> xs xs swts)))
             (devs   (matrix:- ys (matrix:+ (matrix:* slope xs) intercept)))
             (wsigma (sqrt (/ (matrix:<*> swts devs devs)
                              twt (/ dof nel)))))
        (if (and (< niter 100)
                 (plusp wsigma)
                 (or (null sigprev)
                     (> (abs (- wsigma sigprev)) (* eps sigprev))))
            (go-iter (stetson-refinement wts devs wsigma
                                         :alpha alpha
                                         :beta  beta)
                  wsigma
                  (1+ niter))
          ;; else
          (progn
            (format t "~%linfit:regress-fixed-intercept niter = ~A" niter)
            (values slope wsigma niter) ))))))
  

(defun wmnsd (ys wts &key (alpha 2) (beta 2) (eps 1d-3))
  ;; return weighted mean and estimated standard deviation of data
  ;; iterate until the relative improvement in overall chisq
  ;; is less than eps. Be careful not to set eps below the sqrt machine precision.
  (let* ((wts   (if (numberp wts)
                    (make-array (length ys)
                                :initial-element wts)
                  wts))
         (nel   (length ys))
         (dof   (1- nel)))
    (um:nlet iter ((swts    wts)
                   (sigprev 0)
                   (niter   0))
      (let* ((twt    (matrix:sum swts))
             (ywmn   (/ (matrix:<*> ys swts) twt))
             (devs   (matrix:- ys ywmn))
             (wsigma (sqrt (/ (matrix:<*> swts devs devs)
                              twt (/ dof nel)))))
        (cond ((or (<= (abs (- wsigma sigprev)) (* eps sigprev))
                   (> niter 100))
               (format t "~%linfit:wmnsd niter = ~A" niter)
               (values ywmn wsigma
                       (/ wsigma (sqrt twt)) ;; est S.D. of mean
                       twt
                       niter) )

              (t 
               (go-iter (stetson-refinement wts devs wsigma
                                            :alpha alpha
                                            :beta  beta)
                        wsigma
                        (1+ niter)))
              )))
    ))
  

#|
(let* ((xs #(1 2 3)))
  (matrix:+ 3 xs))

(let* ((ys  (vm:unoise 1000 1.0)))
  (wmnsd ys 1))

(let* ((ys (vm:gnoise 1000 :sd 1.0)))
  (wmnsd ys 1 :alpha 2 :beta 2))

(let* ((ys (vm:gnoise 1000 :sd 1.0)))
  (let ((mn  (vm:mean ys)))
    (list mn (vm:stdev ys mn))))

(let* ((ys (matrix:+ (vm:gnoise 1000) (vm:unoise 1000 2))))
  (wmnsd ys 1))

(let* ((ys (matrix:+ (vm:gnoise 1000) (vm:unoise 1000 2))))
  (let ((mn (vm:mean ys)))
    (list mn (vm:stdev ys mn))))
|#