;; list-mat.lisp
;;
;; DM/RAL  2024/11/03 12:52:34 UTC
;; ----------------------------------

(defpackage #:list-mat
  (:use #:common-lisp))

(in-package #:list-mat)

;; ----------------------------------

(defun vdot (v1 v2)
  (loop for a in v1
        for b in v2
        sum
          (* a b)))

(defun trn (m)
  ;; Compute matrix transpose.
  ;; Matrix is a list of 3 element lists representing row vectors.
  ;; For a unitary transform matrix, the transpose is its inverse.
  (apply #'mapcar #'list m))

(defun mat-mulv (m v)
  ;; Multiply a vector by a matrix, M . v
  (mapcar (alexandria:curry #'vdot v) m))

(defun mat-mulm (m1 m2)
  ;; Multiply two matrices, M1 . M2
  (trn (mapcar (alexandria:curry #'mat-mulv m1) (trn m2))))

;; -----------------------------------------------

(defun R1 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  1    0     0)
      (  0   ,cx   ,sx)
      (  0 ,(- sx) ,cx))
    ))

(defun R2 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  ,cx    0  ,(- sx))
      (   0     1     0)
      (  ,sx    0   ,cx))
    ))

(defun R3 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  ,cx    ,sx    0)
      (,(- sx)  ,cx    0)
      (   0       0    1))
    ))

;; --------------------------------------------

(defun select-row (m nrow)
  (nth nrow m))

(defun select-col (m ncol)
  (mapcar (alexandria:curry #'nth ncol) m))

(defun diag (v)
  (let ((nel (length v)))
    (loop for ix from 0
          for val in v
          collect
            (let ((lst (make-list nel :initial-element 0)))
              (setf (nth ix lst) val)
              lst))
    ))

(defun idn (n)
  (diag (make-list n :initial-element 1)))

(defun copy-matrix (m)
  (mapcar #'copy-list m))

(defun vscale (sf v)
  (mapcar (alexandria:curry #'* sf) v))

(defun vadd (v1 &rest vs)
  (apply #'mapcar #'+ v1 vs))

(defun vsub (v1 &rest vs)
  (apply #'mapcar #'- v1 vs))

(defun vmul (v1 &rest vs)
  (apply #'mapcar #'* v1 vs))

(defun vdiv (v1 &rest vs)
  (apply #'mapcar #'/ v1 vs))

(defun madd (m1 &rest ms)
  (apply #'mapcar #'vadd m1 ms))

(defun msub (m1 &rest ms)
  (apply #'mapcar #'vsub m1 ms))

(defun mscale (sf m)
  (mapcar (alexandria:curry 'vscale sf) m))

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
  (rotatef (nth r1 m) (nth r2 m))
  m)

(defun find-pivot (m row-start)
  (let* ((maxv (nth row-start (nth row-start m)))
         (pos  row-start))
    (loop for rx from (1+ row-start)
          for lst in (nthcdr rx m)
          do
            (let ((val (nth row-start lst)))
              (when (> (abs val) (abs maxv))
                (setf maxv val
                      pos  rx))))
    (values pos maxv)))

(defun solve-by-gaussian-elimination (m v)
  ;; Simultaneously compute solution to M . c = v, for c,
  ;; the inverse matrix minv of m, and determinant det(m).
  (let* ((mwrk  (copy-matrix m))
         (vwrk  (copy-list v))
         (nr    (length m))
         (idn   (idn nr))
         (det   1))
    (loop for rx from 0 below nr
          do
          (multiple-value-bind (pos maxv)
              (find-pivot mwrk rx)
            (setf det (* det maxv))
            (cond ((zerop maxv)
                   (unless *in-lsq*
                     (singular-matrix))
                   (let ((row (nth rx mwrk)))
                     (unless (every #'zerop row)
                       (singular-matrix))
                     (setf (nth rx idn)  (copy-list row)
                           (nth rx vwrk) 0)
                     ))

                  (t
                   ;; swap row to get largest remaining pivot into rx'th row
                   (swap-rows mwrk rx pos)
                   (swap-rows idn  rx pos)
                   (rotatef (nth rx vwrk) (nth pos vwrk))
                   (let* ((mrow  (vscale (/ maxv) (nth rx mwrk)))
                          (irow  (vscale (/ maxv) (nth rx idn)))
                          (val   (/ (nth rx vwrk) maxv)))
                     (setf (nth rx mwrk) mrow
                           (nth rx idn)  irow
                           (nth rx vwrk) val)
                     (loop for sx from (1+ rx) below nr
                           for submrow = (nth sx mwrk)
                           for subirow = (nth sx idn)
                           for subval  = (nth sx vwrk)
                           for sf = (nth rx submrow)
                           do
                             ;; Subtract out scaled rx'th row from all below
                             ;; it, such that all later rows become zero in
                             ;; the rx'th column.
                             (setf (nth sx mwrk)
                                   (vsub submrow
                                         (vscale sf mrow))
                                   (nth sx idn)
                                   (vsub subirow
                                         (vscale sf irow))
                                   (nth sx vwrk)
                                   (- subval (* sf val)))
                           )))
                  )))
    (loop for rx from (1- nr) downto 1 do
            (let* ((mrow  (nth rx mwrk))
                   (irow  (nth rx idn))
                   (val   (nth rx vwrk)))
              (when (zerop (nth rx mrow))
                ;; Ensure that check matrix becomes purely diagonal.
                (setf mrow (copy-list mrow)
                      (nth rx mrow) 1))
              (loop for sx from (1- rx) downto 0
                      for submrow = (nth sx mwrk)
                      for subirow = (nth sx idn)
                      for subval  = (nth sx vwrk)
                      for sf = (nth rx submrow)
                      do
                      ;; Subtract out scaled rx'th row from all above
                      ;; it, such that all earlier rows become zero in
                      ;; the rx'th column.
                      (setf (nth sx mwrk)
                            (vsub submrow
                                  (vscale sf mrow))
                            (nth sx idn)
                            (vsub subirow
                                  (vscale sf irow))
                            (nth sx vwrk)
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
  (let ((vdum (select-col m 0)))
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
  (let ((nr  (length m)))
    (every (lambda (row)
             (= (length row) nr))
           m)))

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
      (let ((vdum (select-col m 0)))
        (multiple-value-bind (vsol minv det)
            (solve-by-gaussian-elimination m vdum)
          (declare (ignore vsol minv))
          det))
    (singular-matrix ()
      0)))

;; --------------------------------------------

(defun random-vector (n scale)
  (loop for ix from 0 below n collect
          (lw:mt-random scale)))

(defun random-matrix (n m scale)
  (loop for rx from 0 below n collect
          (random-vector m scale)))

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
  (let ((m (loop for ix from 0 to ord collect
                   (mapcar (alexandria:rcurry #'expt ix) xs))))
    (trn m)))

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
  (let* ((nel  (length xs))
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
    (let ((fn  (alexandria:curry #'poly-eval coffs)))
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
    (let ((ma  (make-array `(,(length m) ,(length (car m)))
                           :element-type 'double-float))
          (va  (make-array (length ys)
                           :element-type 'double-float
                           :initial-contents (mapcar #'dfloat ys))))
      (loop for rx from 0
            for row in m
            do
              (loop for cx from 0
                    for val in row
                    do
                      (setf (aref ma rx cx) (dfloat val))))
      (let* ((coffs (vm:dgesvd-solve ma va))
             (fn    (alexandria:curry #'poly-eval (coerce coffs 'list))))
        (plt:fplot 'plt '(-2 12)
                   fn
                   :color :purple
                   :thick 2))
      xs)))
|#
