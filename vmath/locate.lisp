
(in-package #:com.ral.interpolation)

(defmethod locate ((vec vector) (x real) &optional prev-index) ;; -> j
  ;; Bisection to locate value x in a sorted vector of values vec.
  ;;
  ;; Often used for finding starting location in interpolation tables.
  ;; Values may be in ascending or descending order.
  ;; Return index j such that vec[j] <= x < vec[j+1], or vec[j] >= x > vec[j+1].
  ;;
  ;; By providing the index from a prior lookup the search for nearby similar values
  ;; can be hastened by means of binary expansion ahead of the bisection. Without a
  ;; furnished prior index the entire table is searched.
  ;;
  ;; If the value of x lies beyond either end of the table then the nearest endpoint index
  ;; is returned.
  ;;
  ;; Vector vec must contain at least two values.

  (let* ((nel (let ((nel (array-total-size vec)))
                (assert (> nel 1))
                nel))
         (nm1 (1- nel))
         (vl  (aref vec 0))
         (vu  (aref vec nm1))
         (jl  -1)
         (ju  nel)
         (ascend (>= vu vl)))
    (declare (fixnum nel nm1 jl ju))
    
    (when prev-index
      ;; perform binary hop for possible nearby x relative to previous lookup
      (let ((inc 1))
        (setf jl (cond ((>= prev-index nm1) (1- nm1))
                       ((minusp prev-index) 0)
                       (t                   prev-index))
              ju (1+ jl)
              vl (aref vec jl)
              vu (aref vec ju))
        
           (cond ((= x vu)
                  ;; at the rightmost index j+1
                  (return-from locate ju))

                 ((eq (> x vu)
                      ascend)
                  ;; we are above the upper value in an ascending table
                  ;; or we are below the lower value in a descending table
                  ;; (we are to the right of the rightmost index (j+1))
                  (cond ((>= ju nm1)
                         (return-from locate nm1))
                        
                        (t
                         (loop while (and (< ju nel)
                                          (eq (>= x (aref vec ju))
                                              ascend))
                               do
                               (setf jl ju)
                               (setf ju (min (+ jl inc) nel))
                               (incf inc inc)
                               ;; (print (list jl ju))
                               ))
                        ))

                 ((or (<= vl x vu)
                      (<= vu x vl))
                  ;; we are at j, or in the interval (j, j+1)
                  (return-from locate jl))
                 
                 ((eq (< x vl)
                      ascend)
                  ;; we are below the lower value in an ascending table,
                  ;; or we are above the upper value in a descending table.
                  ;; (we are left of the leftmost index (j))
                  (cond ((zerop jl)
                         (return-from locate 0))
                        
                        (t
                         (loop while (and (not (minusp jl))
                                          (eq (< x (aref vec jl))
                                              ascend))
                               do
                               (setf ju jl)
                               (setf jl (max (- jl inc) -1))
                               (incf inc inc)
                               ;; (print (list jl ju))
                               ))))
                 ))
      (setf vl (aref vec (max jl 0))
            vu (aref vec (min ju nm1))) )
    
    (loop while (plusp (the fixnum (- ju jl 1))) do
          (let* ((jm (ash (the fixnum (+ jl ju)) -1))
                 (vm (aref vec jm)))
            (declare (fixnum jm))
            
            (if (eq (>= x vm)
                    ascend)
                (setf jl jm
                      vl vm)
              (setf ju jm
                    vu vm))
            ))

    (cond ((minusp jl) 0)
          ((>= ju nel) nm1)
          ((= x vu)    ju)
          (t           jl))
    ))

(defmethod locate-subtable ((vec vector) (x real) (sublen fixnum)
                            &optional prev-index) ;; -> jtbl, j
  ;;
  ;; find the starting index jtbl in vec where a sub-table of size sublen would be
  ;; approximately centered, given x. But in no case will this be off the left end,
  ;; nor permit the sub-table to hang off the right end of the vec.
  ;; Also returns the index for the x value in the table from bisection-locate. That
  ;; secondary index value may be useful for repeated searches with correlated values of x.
  
  (let ((nel (array-total-size vec)))
    (assert (<= sublen nel))
    
    (let ((j (locate vec x prev-index)))
      (values (min (max 0
                        (- j (ash (1- sublen) -1)))
                   (- nel sublen))
              j)
      )))

#|
(defun do-test (j ix expr)
  (assert (= j ix) (j ix) "(/= j ix) Expr: ~A" expr))

(defmacro test (expr ix)
  `(do-test ,expr ,ix ',expr))

(let* ((vec  (vm:framp 10))
       (rvec (reverse vec)))
  ;; vec  = 0 1 2 3 4 5 6 7 8 9 
  ;; rvec = 9 8 7 6 5 4 3 2 1 0
  (test (locate vec 1.1) 1)
  (test (locate vec 1)   1)
  (test (locate vec 0)   0)
  (test (locate vec -1)  0)
  (test (locate vec 9)   9)
  (test (locate vec 9.1) 9)
  (test (locate vec 8.9) 8)

  (test (locate rvec 1.1) 7)
  (test (locate rvec 1)   8)
  (test (locate rvec 0)   9)
  (test (locate rvec -1)  9)
  (test (locate rvec 9)   0)
  (test (locate rvec 9.1) 0)
  (test (locate rvec 8.9) 0)

  (test (locate vec 1.1   )  1)
  (test (locate vec 1.5  1)  1)
  (test (locate vec 3    1)  3)
  (test (locate vec -0.1 3)  0)
  (test (locate vec 1.1  0)  1)
  (test (locate vec 0.5  1)  0)

  (test (locate rvec 1.1   )  7)
  (test (locate rvec 1.5  7)  7)
  (test (locate rvec 3    7)  6)
  (test (locate rvec -0.1 6)  9)
  (test (locate rvec 1.1  8)  7)
  (test (locate rvec 0.5  7)  8)

  "Tests succeeded!"
  )
|#