;; list-mat.lisp
;;
;; DM/RAL  2024/11/03 12:52:34 UTC
;; ----------------------------------

(defpackage #:arr-mat
  (:use #:common-lisp #:def*))

(in-package #:arr-mat)

;; ----------------------------------

(defun row-vec (m rix)
  (make-array (array-dimension m 1)
              :element-type (array-element-type m)
              :displaced-to m
              :displaced-index-offset (array-row-major-index m rix 0)))

(defun set-row-vec (m rix v)
  (let ((rv (row-vec m rix)))
    (map-into rv #'identity v)
    v))

(defsetf row-vec  set-row-vec)

;; --------------------------------------------

(defun col-vec (m cix)
  (let* ((nr (array-dimension m 0))
         (ans (make-array nr
                         :element-type (array-element-type m))))
    (dotimes (rix nr)
      (setf (aref ans rix) (aref m rix cix)))
    ans))

(defun set-col-vec (m cix v)
  (loop for rix from 0
        for val across v
        do
        (setf (aref m rix cix) val))
  v)

(defsetf col-vec  set-col-vec)

;; --------------------------------------------
    
(defun vdot (v1 v2)
  (loop for a across v1
        for b across v2
        sum
          (* a b)))

(defun trn (m)
  ;; Compute matrix transpose.
  ;; Matrix is a list of 3 element lists representing row vectors.
  ;; For a unitary transform matrix, the transpose is its inverse.
  (let+ (( (nr nc) (array-dimensions m))
         (ans      (make-array `(,nc ,nr)
                               :element-type (array-element-type m))))
    (loop for cix from 0 below nr
            for rv = (row-vec m cix)
            do
            (setf (col-vec ans cix) rv))
    ans))

(defun mat-mulv (m v)
  ;; Multiply a vector by a matrix, M . v
  (let* ((nr  (array-dimension m 0))
         (ans (make-array nr
                         :element-type (array-element-type m))))
    (dotimes (rix nr)
      (setf (aref ans rix) (vdot (row-vec m rix) v)))
    ans))

(defun mat-mulm (m1 m2)
  ;; Multiply two matrices, M1 . M2
  (let+ (( (nr nc1) (array-dimensions m1))
         ( (nr2 nc) (array-dimensions m2))
         (ans (make-array `(,nr ,nc)
                          :element-type (array-element-type m1))))
    (assert (= nc1 nr2))
    (dotimes (cix nc)
      (let ((cv (col-vec m2 cix)))
        (dotimes (rix nr)
          (setf (aref ans rix cix) (vdot cv (row-vec m1 rix)))
          )))
    ans))

;; -----------------------------------------------

(defun matrix-from-list-form (lst)
  (let* ((nr  (length lst))
         (nc  (length (car lst)))
         (ans (make-array `(,nr ,nc))))
    (dotimes (rix nr)
      (setf (row-vec ans rix) (coerce (nth rix lst) 'vector)))
    ans))

(defun R1 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    (matrix-from-list-form
     `((  1    0     0)
       (  0   ,cx   ,sx)
       (  0 ,(- sx) ,cx)))
    ))

(defun R2 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    (matrix-from-list-form
     `((  ,cx    0  ,(- sx))
       (   0     1     0)
       (  ,sx    0   ,cx)))
    ))

(defun R3 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    (matrix-from-list-form
     `((  ,cx    ,sx    0)
       (,(- sx)  ,cx    0)
       (   0       0    1)))
    ))

;; --------------------------------------------

(defun diag (v)
  (let* ((nel (length v))
         (typ (array-element-type v))
         (ans (make-array `(,nel ,nel)
                          :element-type typ
                          :initial-element (coerce 0 typ))))
    (dotimes (ix nel)
      (setf (aref ans ix ix) (aref v ix)))
    ans))

(defun idn (n)
  (diag (make-array n :initial-element 1)))

(defun vscale (sf v)
  (map 'vector (um:curry #'* sf) v))

(defun vadd (v1 &rest vs)
  (apply #'map 'vector #'+ v1 vs))

(defun vsub (v1 &rest vs)
  (apply #'map 'vector #'- v1 vs))

(defun vmul (v1 &rest vs)
  (apply #'map 'vector #'* v1 vs))

(defun vdiv (v1 &rest vs)
  (apply #'map 'vector #'/ v1 vs))

;; --------------------------------------------

(defun copy-matrix (m)
  (let+ (( (nr nc) (array-dimensions m))
         (ans  (make-array `(,nr ,nc)
                           :element-type (array-element-type m))))
    (dotimes (rix nr)
      (setf (row-vec ans rix) (row-vec m rix)))
    ans))

(defun mscale (sf m)
  (let ((ans (copy-matrix m))
        (fn  (um:curry #'* sf)))
    (dotimes (ix (array-total-size m))
      (setf (row-major-aref ans ix) (funcall fn (row-major-aref m ix))))
    ans))

(defun madd1 (m1 m2)
  (let ((ans  (copy-matrix m1)))
    (dotimes (ix (array-total-size m1))
      (setf (row-major-aref ans ix) (+ (row-major-aref m1 ix) (row-major-aref m2 ix))))
    ans))

(defun madd (m &rest ms)
  (if ms
      (madd (madd1 m (car ms)) (cdr ms))
    m))

(defun msub1 (m1 m2)
  (let ((ans (copy-matrix m1)))
    (dotimes (ix (array-total-size m1))
      (setf (row-major-aref ans ix) (- (row-major-aref m1 ix) (row-major-aref m2 ix))))
    ))

(defun mneg (m)
  (let ((ans (copy-matrix m)))
    (dotimes (ix (array-total-size m))
      (setf (row-major-aref ans ix) (- (row-major-aref m ix))))
    ans))

(defun %msub (m &rest ms)
  (if ms
      (%msub (msub1 m (car ms)) (cdr ms))
    m))

(defun msub (m &rest ms)
  (if ms
      (apply #'%msub m ms)
    (mneg m)))

;; --------------------------------------------

(define-condition singular-matrix (error)
  ())

(defun singular-matrix ()
  (error 'singular-matrix))

(defvar *in-lsq*  nil)

(defun swap-rows (m r1 r2)
  ;; !!Does not copy m, destructrively mutates m.
  ;;
  ;; A permutation matrix operator:
  ;;
  ;;   P = ((1 0 0)
  ;;        (0 0 1)
  ;;        (0 1 0))
  ;;
  ;; applied from the left, interchanges the 2nd and 3rd rows.
  ;;
  (unless (= r1 r2)
    (let ((v (copy-seq (row-vec m r1))))
      (setf (row-vec m r1) (row-vec m r2)
            (row-vec m r2) v)))
  m)

(defun find-pivot (m row-start)
  (let ((maxv (aref m row-start row-start))
        (mix  row-start)
        (nr   (array-dimension m 0)))
    (loop for rx from (1+ row-start) below nr do
            (let ((val (aref m rx row-start)))
              (when (> (abs val) (abs maxv))
                (setf maxv val
                      mix  rx))))
    (values mix maxv)))

(defun solve-by-gaussian-elimination (m v)
  ;; Simultaneously compute solution to M . c = v, for c,
  ;; the inverse matrix minv of m, and determinant det(m).
  (let* ((mwrk  (copy-matrix m))
         (vwrk  (copy-seq v))
         (nr    (array-dimension m 0))
         (idn   (idn nr))
         (det   1))
    (loop for rx from 0 below nr
          do
          (multiple-value-bind (rix maxv)
              (find-pivot mwrk rx)
            (setf det (* det maxv))
            (cond ((zerop maxv)
                   (unless *in-lsq*
                     (singular-matrix))
                   (let ((row (row-vec mwrk rx)))
                     (unless (every #'zerop row)
                       (singular-matrix))
                     (setf (row-vec idn rx) (copy-seq row)
                           (aref vwrk rx)   0)
                     ))

                  (t
                   ;; swap row to get largest remaining pivot into rx'th row
                   (swap-rows mwrk rx rix)
                   (swap-rows idn  rx rix)
                   (rotatef (aref vwrk rx) (aref vwrk rix))
                   (let* ((mrow  (vscale (/ maxv) (row-vec mwrk rx)))
                          (irow  (vscale (/ maxv) (row-vec idn rx)))
                          (val   (/ (aref vwrk rx) maxv)))
                     (setf (row-vec mwrk rx) mrow
                           (row-vec idn  rx) irow
                           (aref vwrk rx)    val)
                     (loop for sx from (1+ rx) below nr
                           for submrow = (row-vec mwrk sx)
                           for subirow = (row-vec idn  sx)
                           for subval  = (aref vwrk sx)
                           for sf = (aref submrow rx)
                           do
                             ;; Subtract out scaled rx'th row from all below
                             ;; it, such that all later rows become zero in
                             ;; the rx'th column.
                             (setf (row-vec mwrk sx) 
                                   (vsub submrow
                                         (vscale sf mrow))
                                   (row-vec idn sx)
                                   (vsub subirow
                                         (vscale sf irow))
                                   (aref vwrk sx)
                                   (- subval (* sf val)))
                           )))
                  )))
    (loop for rx from (1- nr) downto 1 do
            (let* ((mrow  (row-vec mwrk rx))
                   (irow  (row-vec idn  rx))
                   (val   (aref vwrk rx)))
              (when (zerop (aref mrow rx))
                ;; Ensure that check matrix becomes purely diagonal.
                (setf mrow (copy-seq mrow)
                      (aref mrow rx) 1))
              (loop for sx from (1- rx) downto 0
                      for submrow = (row-vec mwrk sx)
                      for subirow = (row-vec idn  sx)
                      for subval  = (aref vwrk sx)
                      for sf = (aref submrow rx)
                      do
                      ;; Subtract out scaled rx'th row from all above
                      ;; it, such that all earlier rows become zero in
                      ;; the rx'th column.
                      (setf (row-vec mwrk sx)
                            (vsub submrow
                                  (vscale sf mrow))
                            (row-vec idn sx)
                            (vsub subirow
                                  (vscale sf irow))
                            (aref vwrk sx)
                            (- subval (* sf val)))
                      )))
    ;; vwrk has the solution vector
    ;; idn should be the inverse of m
    ;; mwrk should now be an IDN matrix
    (values vwrk idn det mwrk)
    ))

;; --------------------------------------------
#|
(defun to-upper-tri (m)
  (let* ((mwrk  (copy-matrix m))
         (nr    (length m))
         (idn   (idn nr)))
    ;;
    ;; Work in parallel on an identity matrix.
    ;;
    ;; Initially, if we have,
    ;;   M . c = v,
    ;; then also,
    ;;   M . c = I . v.
    ;; And at every step, Q . M . c = Q . I . v,
    ;; for some matrix operator Q.
    ;;
    ;; Row swaps are permutation operators. And row scaled
    ;; subtractions can be written as matrix operators:
    ;;
    ;;   Operator Q = ((  1   0  0)
    ;;                 (-sf2  1  0)
    ;;                 (-sf3  0  1))
    ;;
    ;; subtracts sf2 scaled row 1 from row 2, and sf3 scaled row 1
    ;; from row 3.
    ;;
    ;; By working in parallel on an Identity Matrix, the resulting
    ;; transformed Identity matrix becomes a record of the
    ;; accumulation of these individual operator applications. The
    ;; original matrix becomes upper triangular.
    ;;
    (loop for rx from 0 below (1- nr)
          do
          (multiple-value-bind (pos maxv)
              (find-pivot mwrk rx)
            (cond ((zerop maxv)
                   (unless *in-lsq*
                     (singular-matrix))
                   (let ((row (nth rx mwrk)))
                     (unless (every #'zerop row)
                       (singular-matrix))
                     ;; Ensure that IDN matrix will produce a 0/0 situation
                     ;; in UPPER-TRI-SOLVE.
                     (setf (nth rx idn) (copy-list row))
                     ))

                  (t
                   ;; swap row to get largest remaining pivot in rx'th row
                   (swap-rows mwrk rx pos)
                   (swap-rows idn  rx pos)
                   ;; we don't bother normalizing the rows here
                   (let ((mrow  (nth rx mwrk))
                         (irow  (nth rx idn)))
                     (loop for sx from (1+ rx) below nr
                           for submrow = (nth sx mwrk)
                           for subirow = (nth sx idn)
                           for sf = (/ (nth rx submrow) maxv)
                           do
                             ;; Subtract out scaled rx'th row from all below
                             ;; it, such that all later rows become zero in
                             ;; the rx'th column.
                             (setf (nth sx mwrk)
                                   (vsub submrow
                                         (vscale sf mrow))
                                   (nth sx idn)
                                   (vsub subirow
                                         (vscale sf irow))))
                     ))
                  )))
    (values mwrk idn)
    ))

(defun extract-diagonal (m)
  (loop for rx from 0
        for row in m
        collect
          (nth rx row)))

(defun upper-tri-solve (mtri maux v)
  (unless *in-lsq*
    (when (some #'zerop (extract-diagonal mtri))
      (singular-matrix)))
  (let* ((nr  (length mtri))
         (coffs nil))
    (loop for rx from (1- nr) downto 0
          for mrow = (nth rx mtri)
          for irow = (nth rx maux)
          do
            (let ((den (nth rx mrow))
                  (sum (- (vdot irow v)
                          (vdot (nthcdr (1+ rx) mrow) coffs))))
              (push (if (zerop den)
                        0
                      (/ sum den))
                    coffs)))
    coffs))

(defun invert-upper-tri (mtri maux)
  (loop for rx from (1- (length mtri)) downto 0 do
          (let* ((mrow (nth rx mtri))
                 (irow (nth rx maux))
                 (den  (nth rx mrow)))
            (unless (zerop den)
              ;; normalize the rx'th row
              (setf (nth rx mtri)
                    (setf mrow (vscale (/ den) mrow))
                    (nth rx maux)
                    (setf irow (vscale (/ den) irow)))
              (loop for sx from 0 below rx
                    for smrow in mtri
                    for sirow in maux
                    do
                      ;; Subtract out scaled rx'th row from
                      ;; all rows above, such that all higher rows
                      ;; have zero in the rx'th column.
                      (let ((sf (nth rx smrow))) 
                        (setf (nth sx mtri)
                              (vsub smrow
                                    (vscale sf mrow))
                              (nth sx maux)
                              (vsub sirow
                                    (vscale sf irow)))
                        )))
            ))
  (values maux mtri)) ;; mtri is mow IDN
|#
;; --------------------------------------------

(defun simple-minv (m)
  (let ((vdum (col-vec m 0)))
    (multiple-value-bind (vsol minv det mchk)
        (solve-by-gaussian-elimination m vdum)
      (declare (ignore vsol mchk))
      (unless *in-lsq*
        (when (zerop det)
          (singular-matrix)))
      minv)
    ))

(defun pseudo-minv (m)
  (let* ((*in-lsq* t)
         (mt  (trn m))
         (mtm (mat-mulm mt m)))
    (mat-mulm (simple-minv mtm) mt)))
  
(defun is-square-matrix? (m)
  (let+ (( (nr nc) (array-dimensions m)))
    (= nr nc)))

(defun minv (m)
  (if (is-square-matrix? m)
      (handler-case
          (simple-minv m)
        (singular-matrix ()
          (pseudo-minv m)))
    ;; else
    (pseudo-minv m)
    ))

(defun det (m)
  (handler-case
      (let ((vdum (col-vec m 0)))
        (multiple-value-bind (vsol minv det)
            (solve-by-gaussian-elimination m vdum)
          (declare (ignore vsol minv))
          det))
    (singular-matrix ()
      0)))

;; --------------------------------------------

(defun random-vector (n scale)
  (let ((ans (make-array n)))
    (dotimes (ix n)
      (setf (aref ans ix) (lw:mt-random scale)))
    ans))

(defun random-matrix (n m scale)
  (let ((ans (make-array `(,n ,m))))
    (dotimes (ix (* n m))
      (setf (row-major-aref ans ix) (random-vector m scale)))
    ans))

;; --------------------------------------------
;; Solution by Extended Gaussian Elimination
;;
;; Gaussian Elimination, alone, only works on square matrices of full
;; rank. We have Extended Gaussian Elmination, which recovers to
;; provide a least-squares solution in the case of non-square
;; matrices, and which offers least-squares solutions of reduced order
;; for matrices of deficient rank.
;;
;; Extended Gaussian Elimination returns the highest order solution,
;; in the least-squares sense, consistent with the rank of the system.
;; (Ord = Rank-1)
;;
;; DGESVD returns the requested order of solution, even if Ord >=
;; Rank.  This will be a minimum norm solution, which is also good in
;; the least squares sense.
;;
;; A system is full rank, provided that at least N+1 points have
;; unique abscissae for an Nth order solution.
;;
;; For a full-rank system both solutions agree. When the rank of the
;; system is deficient, both solutions agree at the abscissae of the
;; data, but depart elsewhere.
;;

(defun solve-lsq (m v)
  (let* ((*in-lsq*  t)
         (mt   (trn m))
         (mtv  (mat-mulv mt v))
         (mtm  (mat-mulm mt m))) ;; Trn(M) . M now square
    (solve-by-gaussian-elimination mtm mtv)
    ))

(defun solve (m v)
  ;; Solve matrix equation M . c = v, for vector c.
  (if (is-square-matrix? m)
      (handler-case
          (solve-by-gaussian-elimination m v)
        (singular-matrix ()
          (solve-lsq m v)))
    ;; Else - not square matrix, so solve in least-squares sense.
    (solve-lsq m v)
    ))

(defun expt-matrix (xs ord)
  ;; Form the Experiment Matrix for a collection of abscissae. This is
  ;; a purely geometrical factor, independent of actual measured
  ;; values at those abscissae.
  (let* ((nc  (length xs))
         (nr  (1+ ord))
         (m   (make-array `(,nr ,nc))))
    (dotimes (cix nc)
      (dotimes (rix nr)
        (setf (aref m rix cix) (expt (aref xs rix) cix))
        ))
    m))

(defun lsq (xs ys ord)
  ;; xs, ys should be lists
  (solve-lsq (expt-matrix xs ord) ys))

(defun poly-eval (coffs x)
  ;; Horner's evaluation of Polynomials, for coffs stored in ascending
  ;; ord order
  (if coffs
      (+ (car coffs)
         (* x (poly-eval (cdr coffs) x)))
    0
    ))

;; --------------------------------------------
#|
(let ((m  '((1 2 4)
            (1 3 9)
            (1 5 25)
            (1 6 36)))
      (ys '(32 37 13 44)))
  (destructuring-bind (a b c)
      (solve m ys)
    (let ((fn  (lambda (x)
                 (+ a (* x (+ b (* x c)))))
               ))
      (plt:fplot 'plt '(0 10)
                 fn
                 :clear t)
      (plt:plot 'plt (mapcar #'cadr m) ys
                :symbol :circle)
      )))

(let ((m  '((1 2)
            (1 3)
            (1 5)
            (1 6)))
      (ys '(32 37 13 44)))
  (destructuring-bind (a b)
      (solve m ys)
    (let ((fn  (lambda (x)
                 (+ a (* x b)))
               ))
      (plt:fplot 'plt '(0 10)
                 fn
                 :clear t
                 :yrange '(10 50))
      (plt:plot 'plt (mapcar #'cadr m) ys
                :symbol :circle)
      (values)
      )))

(let ((m  '((1 2)
            (1 3)))
      (ys '(32 37)))
  (destructuring-bind (a b)
      (solve m ys)
    (let ((fn  (lambda (x)
                 (+ a (* x b)))
               ))
      (plt:fplot 'plt '(0 10)
                 fn
                 :clear t
                 :yrange '(10 50))
      (plt:plot 'plt (mapcar #'cadr m) ys
                :symbol :circle)
      (values)
      )))

(let ((m  '((1 2 4)
            (1 3 9)))
      (ys '(32 37)))
  (destructuring-bind (a b c)
      (solve m ys)
    (let ((fn  (lambda (x)
                 (+ a (* x (+ b (* c x)))))
               ))
      (plt:fplot 'plt '(0 10)
                 fn
                 :clear t
                 :yrange '(10 50))
      (plt:plot 'plt (mapcar #'cadr m) ys
                :symbol :circle)
      (values)
      )))

(let ((m '((1 2 3) (4 5 6) (10 19 18)))
      (v '(19 20 21)))
  (multiple-value-bind (mtri maux)
      (to-upper-tri m)
    (let ((ans (upper-tri-solve mtri maux v)))
      (assert (equalp (mat-mulv m ans) v))
      ans
      )))

(let ((m (r1 0.5)))
  (list m (trn (minv m))))

;; --------------------------------------------
;; Showing the difference between Extended Gaussian Elimination and DGESVD.
;;
;; Extended Gaussian Elimination in RED curve.
;; DGESVD in PUPRPLE curve.
;;

(defun dfloat (x)
  (float x 1d0))

(defun highlight-duplicate-abscissae (xs ys)
  ;; draw a dashed line joining the extreme ordinates for duplicate abscissa points
  (let* ((xs   (coerce xs 'list))
         (ys   (coerce ys 'list))
         (nel  (length xs))
         (ixs  (um:range 0 nel)))
    (setf ixs (sort ixs #'<
                    :key (um:rcurry #'nth xs)))
    (um:nlet iter ((ixs   ixs)
                   (lastx nil)
                   (miny  nil)
                   (maxy  nil))
      (flet ((draw ()
               (plt:plot 'plt `(,lastx ,lastx) `(,miny ,maxy)
                         :color :black
                         :thick 2
                         :plot-joined t
                         :line-dashing '(4 4))))
        (multiple-value-bind (x y)
            (when ixs
              (values (nth (car ixs) xs)
                      (nth (car ixs) ys)))
          (cond ((null ixs)
                 (unless (and lastx
                              (= miny maxy))
                   (draw) ))
                
                (lastx
                 (cond ((= lastx x)
                        (go-iter (cdr ixs) x (min y miny) (max y maxy)))
                       (t
                        (unless (= miny maxy)
                          (draw))
                        (go-iter (cdr ixs) x y y))
                       ))
                (t
                 (go-iter (cdr ixs) x y y))
                )))
      )))

(let* ((nel     4)
       (max-ord 3)
       (xs      (random-vector nel 10))
       (ys      (random-vector nel 10))
       (m       (expt-matrix xs max-ord)))
  (multiple-value-bind (coffs minv det mchk)
      (solve m ys)
    (print minv)
    (print det)
    (print (list m ys coffs))
    (print mchk)
    ;;
    ;; Show the Extended Gaussian Elimination solution
    ;;
    (let ((fn  (alexandria:curry #'poly-eval (coerce coffs 'list))))
      (plt:fplot 'plt '(-2 12)
                 fn
                 :clear t
                 :color :red
                 :thick 2
                 :yrange '(-2 12))
      
      (when (zerop det)
        (highlight-duplicate-abscissae xs ys))
      (plt:plot 'plt xs ys
                :symbol :circle))
    ;;
    ;; Show the DGESVD solution
    ;;
    (let* ((nr  (1+ max-ord))
           (ma  (make-array `(,nr ,nel)
                           :element-type 'double-float))
          (va   (make-array nr
                            :element-type 'double-float
                            :initial-contents (map 'list #'dfloat ys))))
      (dotimes (rix nr)
        (map-into (row-vec ma rix) #'dfloat (row-vec m rix)))
      (let* ((coffs (vm:dgesvd-solve ma va))
             (fn    (alexandria:curry #'poly-eval (coerce coffs 'list))))
        (plt:fplot 'plt '(-2 12)
                   fn
                   :color :purple
                   :thick 2))
      xs)))
|#
