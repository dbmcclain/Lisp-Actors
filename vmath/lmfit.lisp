;; lmfit.lisp -- Levenberg-Marquardt nonlinear fitting
;;
;; DM/RAL  11/10
;; -------------------------------------------------------

(defpackage #:com.ral.lmfit
  (:use #:common-lisp)
  (:export
   #:fit
   #:fitnd))

(in-package #:com.ral.lmfit)

;; ------------------------------------------------------------
;; Levenberg-Marquardt Nonlinear LSQ Fitting

(defun fit (v errfn derfn &key verbose (verbose-interval 100) stop-chisq iter-limit
              verbose-fn)
  (let* ((nparm  (length v))
         (errs   (funcall errfn v))
         (ndof   (- (length errs) nparm))
         (verbose-count 0))
    (labels ((chisq (v)
               (let ((errs (funcall errfn v)))
                 (/ (matrix:<*> errs errs) ndof)))
             
             (iter (chisq0 v lam)
               ;; (format t "~%iter: chisq = ~g, lam = ~g" chisq0 lam)
               (let* ((malpha  (matrix:make-matrix
                                :rows (matrix:as-vector
                                       (loop repeat nparm collect
                                             (make-array nparm)) )))
                      (vbeta   (make-array nparm)))
                 (let ((err0 (funcall errfn v))
                       (vder (funcall derfn v)))
                   (loop for ix from 0 below nparm do
                         (let ((der (aref vder ix)))
                           (setf (aref vbeta ix) (matrix:<*> der err0)
                                 (matrix:aref malpha ix ix) (matrix:<*> der der))
                           (loop for jx from 0 below ix do
                                 (let ((tot (matrix:<*> der (aref vder jx))))
                                   (setf (matrix:aref malpha ix jx) tot
                                         (matrix:aref malpha jx ix) tot)) ))) )
                 
                 (labels ((solve (lam)
                            
                            (incf verbose-count)
                            (when verbose
                              (when (zerop (mod verbose-count verbose-interval))
                                (format t "~%Solve: lam = ~g, ctr = ~A" lam verbose-count)))

                            (let ((malphax (matrix:copy malpha)))
                              (loop for ix from 0 below nparm do
                                    (setf (matrix:aref malphax ix ix)
                                          (if (zerop (matrix:aref malpha ix ix))
                                              1e-3
                                            (* (matrix:aref malpha ix ix)
                                               (+ 1d0 lam))) ))
                              (let* ((dv (matrix:cholsl malphax vbeta))
                                     (vx (vops:vsub v dv))
                                     (chisqx (chisq vx))
                                     (dchisq (abs (- chisq0 chisqx))))
                                (when (and verbose
                                           (zerop (mod verbose-count verbose-interval)))
                                  (when (functionp verbose-fn)
                                    (funcall verbose-fn vx))
                                  (format t "~%ChiSq = ~g" chisqx)
                                  (format t "~A" vx))
                                (if (or (< dchisq (* 1d-6 chisq0))
                                        (< chisqx 1d-12)
                                        (and (numberp stop-chisq)
                                             (> verbose-count verbose-interval)
                                             (> chisqx stop-chisq))
                                        (and (numberp iter-limit)
                                             (> verbose-count iter-limit)))
                                    (let ((cov (ignore-errors
                                                 (matrix:sqrt
                                                  (matrix:* chisqx
                                                            (matrix:abs
                                                             (matrix:inv malpha)))))))
                                      (if cov
                                          (when verbose
                                            (format t "~%Finished..."))
                                        (print "~%LMFIT: Singular COV"))

                                      (values chisqx vx cov verbose-count lam))
                                  ;; else
                                  (if (> chisqx chisq0)
                                      (solve (* 10d0 lam))
                                    ;; else
                                    (iter chisqx vx (/ lam 10d0))) ))) ))
                   (solve lam)))))
      (iter (chisq v) v 0.001d0))))

;; ---------------------------------------
;; LM Fitting when you have no partial derivatives to offer...

(defun fitnd (v errfn &rest args)
  (labels ((pders (v)
             (coerce
              (loop for x across v
                    for ix from 0
                    collect
                    (labels ((subst-parm (x)
                               (setf (aref v ix) x)
                               v))
                      (let* ((dx (max 0.001 (* 0.01 (abs x))))
                             (errs+ (funcall errfn (subst-parm (+ x dx))))
                             (errs- (funcall errfn (subst-parm (- x dx)))))
                        (subst-parm x)
                        (vops:vscale (/ 0.5 dx)
                                     (vops:vsub errs+ errs-))
                        )))
              'vector)))
    (apply #'fit v errfn #'pders args)))
