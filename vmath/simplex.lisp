
(in-package #:com.ral.vectorized-math)

(defun #1=simplex (errfn v &key (nmax 1000) (tol 1d-6))
  (let* ((dim   (length v))
         (verts (cons v
                      (loop for ix from 0 below dim collect
                            (let* ((vx (copy-seq v)))
                              (setf (aref vx ix) (* 1.1 (aref vx ix)))
                              vx))))
         (errs  (mapcar errfn verts))
         (converged (um:rcurry #'< tol))
         (count 0))
    
    (um:nlet iter ((verts verts)
                   (errs  errs))
      (incf count)
      (when (zerop (mod count 100))
        (format t "~%Count ~d err = ~f" count (reduce #'min errs)))
      (when (and nmax
                 (> count nmax))
        (return-from #1# (values (first verts) (first errs) count)))
      
      ;; order vertices err(x_1) <= err(x_2) <= ... <= err(x_n+1)
      (let* ((pairs (sort (um:zip errs verts) #'<
                          :key #'first))
             (verts  (mapcar #'second pairs))
             (errs   (mapcar #'first pairs)))

        ;; check for convergence
        (if (every converged
                   (mapcar (um:compose #'abs (um:curry #'- (first errs))) (rest errs)))
            (return-from #1# (values (first verts) (first errs) count))

          ;; compute centroid of vertices and reflect the worst vertex in the opposite
          ;; direction from there, starting from the centroid position
          (let* ((x0     (vops:vscale (/ dim)
                                      (apply #'vops:vadd (butlast verts))))
                 (xr     (vops:vadd x0 (vops:vsub x0 (um:last1 verts))))
                 (errr   (funcall errfn xr)))
        
            (cond ((and (<= (first errs) errr)
                        (< errr (um:last1 (butlast errs))))
                   (go-iter (cons xr (butlast verts)) (cons errr (butlast errs))))
                  
                  ((< errr (first errs))
                   ;; reflection is better so expand in same direction
                   (let* ((xe   (vops:vadd x0 (vops:vscale 2 (vops:vsub xr x0))))
                          (erre (funcall errfn xe)))
                     (if (< erre errr) ;; is better?
                         (go-iter (cons xe (butlast verts)) (cons erre (butlast errs)))
                       (go-iter (cons xr (butlast verts)) (cons errr (butlast errs))))
                     ))
                  
                  (t
                   ;; contract in from centroid toward worst vertex
                   (let* ((xc   (vops:vadd x0 (vops:vscale 0.5 (vops:vsub (um:last1 verts) x0))))
                          (errc (funcall errfn xc)))
                     (if (< errc (um:last1 errs)) ;; is better than worst?
                         (go-iter (cons xc (butlast verts)) (cons errc (butlast errs)))
                       ;; shrink the simplex about the best vertex and try again
                       (let* ((x1  (first verts))
                              (e1  (first errs))
                              (xs  (loop for x in (rest verts) collect
                                         (vops:vadd x1 (vops:vscale 0.5 (vops:vsub x x1)))))
                              (errxs (mapcar errfn xs)))
                         (go-iter (cons x1 xs) (cons e1 errxs))
                         ))
                     ))
                  ))
          )))
    ))
                     
               
