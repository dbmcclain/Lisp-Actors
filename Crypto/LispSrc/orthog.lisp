
(in-package :core-crypto)

(defun poly (coffs x)
  ;; coffs listed hi to lo
  (reduce (lambda (sum coff)
            (m+ coff (m* x sum)))
          coffs
          :initial-value 0))

(defun chkwts (xs)
  (labels ((wt (x1)
             (m/ (reduce (lambda (prod x2)
                           (if (eql x1 x2)
                               prod
                             (m* prod (m- x1 x2))))
                         xs
                         :initial-value 1))))
    (loop for x in xs collect (wt x))))

(defun tst-chk ()
  (with-mod 101
    (let* ((xs     (um:range 1 2 11))
           (chks   (chkwts xs))
           (gcoffs (loop repeat 6 collect (random 10)))
           (gvals  (loop for x in xs collect (poly gcoffs x)))
           (fcoffs (loop repeat 4 collect (random 10)))
           (fvals  (loop for x in xs collect (poly fcoffs x))))
      (reduce #'m+ (mapcar #'m* chks fvals gvals)))))
  

(defun zpoly (coffs x)
  ;; coffs listed hi to lo
  (reduce (lambda (sum coff)
            (+ coff (* x sum)))
          coffs
          :initial-value 0))

(defun zchkwts (xs)
  (labels ((wt (x1)
             (/ (reduce (lambda (prod x2)
                          (if (eql x1 x2)
                              prod
                            (* prod (- x1 x2))))
                        xs
                        :initial-value 1))))
    (loop for x in xs collect (wt x))))

(defun ztst-chk ()
  (let* ((xs     (um:range 1 2 11))
         (chks   (zchkwts xs))
         (gcoffs (loop repeat 6 collect (- (random 100) 50)))
         (gvals  (loop for x in xs collect (zpoly gcoffs x)))
         (fcoffs (loop repeat 4 collect (- (random 100) 50)))
         (fvals  (loop for x in xs collect (zpoly fcoffs x))))
    (reduce #'+ (mapcar #'* chks fvals gvals))))
  
;; WHY?  Because - the formula is effectively solving for coefficient
;; c_(N-1) when the underlying polynomial should only have
;; coefficients up through c_(N-2).
;;
;; If the code words all came from a polynomial of order (t-1), t < N,
;; and the check polynomial is of order (N-t-1), then the highest
;; composite power should be (t-1)+(N-t-1) = (N-2). And so the
;; coefficient, c_(N-1), should be zero.
;;
;; The formula expresses the fitting via the inverse Vandermonde
;; matrix of the X coefficients against N samples of a polynomial
;; derived function with order N-2. The formula represents the
;; solution for the order N-1 coefficient, which is expected to be
;; zero.
#|
 Given abscissa of x_i, 1 <= i <= N, we have

    y_i = c_0 + c_1 * x_i + c_2 * x_i^2 + ... + c_N-1 * x_i^(N-1)

    or
      A . C = Y

    for A = Vandermonde matrix (1 x_1 x_1^2 ... x_1^(N-1) )
                               (1 x_2 x_2^2 ... x_2^(N-1) )
                               (...                       )
                               (1 x_N x_N^2 ... x_N^(N-1) )
    C = [c_0    ]     Y = [y_1]
        [c_1    ]         [y_2]
        [...    ]         [...]
        [c_(N-1)]         [y_N]

    Matrix A has unique factorization as LU matrices, A = L . U, where diagonal
    elements of U are unity. Inverting these becomes

    L^(-1) = [1                            0                          0                         ...]
             [1/(x_1 - x_2)                1/(x_2 - x_1)              0                         ...]
             [1/(x_1 - x_2)/(x_1 - x_3)    1/(x_2 - x_1)/(x_2 - x_3)  1/(x_3 - x_1)/(x_3 - x_2) ...]
             [ ...                         ...                        ...                       ...]

    or
      L^(-1) has elements l_ij where

              l_ij = 0, i < j
              l_11 = 1
              l_ij = Prod(1/(x_j - x_k), k = 1 to i, k /= j)

    Similarly

       U^(-1) has elements u_ij where

              u_ii = 1
              u_i1 = 0
              u_ij = u_(i-1),(j-1) - u_i,(j-1) x_(j-1)
              u_0j = 0
              
       U^(-1) = [1  -x1  x1*x2   -x1*x2*x3         ...]
                [0   1  -(x1+x2) x1*x2+x2*x3+x3*x1 ...]
                [0   0    1      -(x1+x2+x3)       ...]
                [0   0    0      1                 ...]
                [... ...  ...    ...               1  ]

  And so,

      A . C = L . U . C = Y

      and

      C = U^(-1) . L^(-1) . Y

  Examining the last row of L^(-1) and U^(-1) we see that

     c_(N-1) = Sum(d_i * y_i, i = 1..N)

     where
     
       d_i = Prod(1/(x_i - x_j), j = 1..N, j /= i)

     which is what we compute in the code above.

   Since we have

     y_i = (a0 + a1*x_i + ... + a_(t-1)*x_i^(t-1)) * (b0 + b1 * x_i + ... + b_(N-t-1)*x_i^(N-t-1))

   we see that highest order term will have x_i^(N-2), and hence c_(N-1) = 0.

   This result is completely general, holding across fields of Rationals, Reals, Complex,
   Finite Fields, etc. It is not dependent on Finite Field arithmetic.

|#
