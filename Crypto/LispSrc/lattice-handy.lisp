;; lattice-handy.lisp -- Matrix Operations
;; DM/RAL 01/24
;; ----------------------------------------------------

(in-package :com.ral.crypto.lattice-crypto)

;; -----------------------------------------------------
(defun idn (nrows)
  (let ((ans (make-array nrows)))
    (loop for ix from 0 below nrows do
          (let ((row (make-array nrows
                                 :initial-element 0)))
            (setf (aref row ix) 1
                  (aref ans ix) row)))
    ans))

(defun zero-mat (nrows ncols)
  (let ((ans (make-array nrows)))
    (loop for rix from 0 below nrows do
            (setf (aref ans rix) (make-array ncols
                                             :iniitial-element 0)))
    ans))
            

(defun noise-mat (nbits nrows ncols)
  (let ((ans  (make-array nrows)))
    (loop for ix from 0 below nrows do
            (let ((row (make-array ncols)))
              (loop for cix from 0 below ncols do
                      (setf (aref row cix)
                            (m- (prng:ctr-drbg-int nbits)
                                (ash 1 (1- nbits)))))
              (setf (aref ans ix) row)))
    ans))

(defun random-mat (nrows ncols)
  (let ((nbits (integer-length (mod-base)))
        (ans   (make-array nrows)))
    (loop for ix from 0 below nrows do
            (let ((row (make-array ncols)))
              (loop for cix from 0 below ncols do
                      (setf (aref row cix)
                            (mmod (prng:ctr-drbg-int nbits))))
              (setf (aref ans ix) row)))
    ans))

(defun copy-matrix (mat)
  (let ((ans (copy-seq mat)))
    (loop for v across mat
          for ix from 0
          do
          (setf (aref ans ix) (copy-seq v)))
    ans))

(defun mat-inv (mat &optional mextra)
  (let* ((nrows  (length mat))
         (ncols  (length (aref mat 0)))
         (idn    (idn nrows))
         (mat    (copy-matrix mat))
         (invdet 1)
         (mextra (and mextra
                      (copy-matrix mextra))))
    (labels ((sub-scaled-row (dst sf row)
               (map-into dst (lambda (a b)
                               (m- a (m* sf b)))
                         dst row)))
      (loop for rix from 0 below (min nrows ncols) do
              (let* ((row    (aref mat rix))
                     (irow   (aref idn rix))
                     (xrow   (and mextra
                                  (aref mextra rix)))
                     (cix    (position-if (complement #'zerop) row)))
                (cond (cix
                       (let* ((rinv   (m/ (aref row cix)))
                              (scaler (um:rcurry #'m* rinv)))
                         (setf invdet (m* invdet rinv))
                         (map-into row scaler row)
                         (map-into irow scaler irow)
                         (when mextra
                           (map-into xrow scaler xrow)
                           (setf (aref mextra rix) xrow))
                         (setf (aref mat rix) row
                               (aref idn rix) irow)
                         (loop for rrix from 0 below nrows do
                                 (unless (eql rrix rix)
                                   (let ((rr  (aref (aref mat rrix) cix)))
                                     (unless (zerop rr)
                                       (sub-scaled-row (aref mat rrix) rr row)
                                       (sub-scaled-row (aref idn rrix) rr irow)
                                       (when mextra
                                         (sub-scaled-row (aref mextra rrix) rr xrow)))
                                     )))
                         ))
                      (t
                       (setf invdet 0))
                      )))
      (values idn mat (m/ invdet) mextra)
      )))

(defun trn (a)
  (let* ((nrows (length a))
         (ncols (length (aref a 0)))
         (ans   (make-array ncols)))
    (loop for ix from 0 below ncols do
          (let ((v (make-array nrows)))
            (loop for jx from 0 below nrows do
                  (setf (aref v jx) (aref (aref a jx) ix)))
            (setf (aref ans ix) v)))
    ans))

(defun mat-mul (a b)
  (let* ((nrows  (length a))
         (bt     (trn b))
         (ncols  (length bt))
         (ans    (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let ((v (make-array ncols))
                  (row (aref a rix)))
              (loop for cix from 0 below ncols do
                      (setf (aref v cix)
                            (mmod
                             (fvdot row (aref bt cix)))))
              (setf (aref ans rix) v)))
    ans))
              

(defun mat-left (m ncols)
  (let* ((nrows (length m))
         (ans   (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let* ((v     (aref m rix))
                   (row   (make-array ncols
                                      :initial-element 0)))
              (replace row v)
              (setf (aref ans rix) row)))
    ans))

(defun mat-right (m ncols)
  (let* ((nrows (length m))
         (ans   (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let* ((v      (aref m rix))
                   (vcols  (length v))
                   (start2 (max 0 (- vcols ncols)))
                   (row    (make-array ncols
                                       :initial-element 0)))
              (replace row v :start2 start2)
              (setf (aref ans rix) row)))
    ans))

(defun mat-top (m nrows)
  (let* ((ans   (make-array nrows))
         (mrows (length m))
         (ncols (length (aref m 0))))
    (loop for rix from 0 below (min nrows mrows) do
            (setf (aref ans rix) (copy-seq (aref m rix))))
    (loop for rix from (min nrows mrows) below nrows do
            (setf (aref ans rix) (make-array ncols
                                             :initial-element 0)))
    ans))

(defun mat-bottom (m nrows)
  (let* ((ans   (make-array nrows))
         (mrows (length m))
         (ncols (length (aref m 0)))
         (start (max 0 (- mrows nrows))))
    (loop for rix from 0 below (min nrows mrows) do
            (setf (aref ans rix) (copy-seq (aref m (+ start rix)))))
    (loop for rix from (min nrows mrows) below nrows do
            (setf (aref ans rix) (make-array ncols
                                             :initial-element 0)))
    ans))

(defun normalize-matrix (m)
  (let* ((nrows (length m))
         (ncols (reduce #'max (map 'vector #'length m)))
         (ans   (make-array nrows))
         (m     (coerce m 'vector)))
    (loop for rix from 0 below nrows do
          (let ((v (make-array ncols
                               :initial-element 0))
                (mv (aref m rix)))
            (replace v (coerce mv 'vector))
            (setf (aref ans rix) v)))
    ans))

(defun matrix-section (m &key
                         (start-row 0)
                         (start-col 0)
                         nrows ncols)
  (let* ((nrows (or nrows
                    (length m)))
         (ncols (or ncols
                    (length (aref m 0))))
         (ans   (make-array nrows)))
    (loop for rix from 0 below (min nrows (length m)) do
          (let ((v  (make-array ncols
                                :initial-element 0)))
            (replace v (aref m (+ start-row rix)) :start2 start-col)
            (setf (aref ans rix) v)))
    (loop for rix from (length m) below nrows do
          (setf (aref ans rix) (make-array ncols
                                           :initial-element 0)))
    ans))

(defun adjust-matrix (m nrows ncols)
  (let ((mrows  (length m))
        (ans    (make-array nrows)))
    (loop for rix from 0 below (min nrows mrows) do
          (let ((v  (make-array ncols
                                :initial-element 0)))
            (replace v (aref m rix))
            (setf (aref ans rix) v)))
    (loop for rix from mrows below nrows do
          (setf (aref ans rix) (make-array ncols
                                           :initial-element 0)))
    ans))

(defun adjoin-rows (m1 m2)
  (let* ((nrows1  (length m1))
         (nrows2  (length m2))
         (ncols1  (length (aref m1 0)))
         (ncols2  (length (aref m2 0)))
         (ans     (make-array (+ nrows1 nrows2))))
    (loop for rix from 0 below nrows1 do
          (let ((v (make-array (max ncols1 ncols2)
                               :initial-element 0)))
            (replace v (aref m1 rix))
            (setf (aref ans rix) v)))
    (loop for rix from 0 below nrows2 do
          (let ((v (make-array (max ncols1 ncols2)
                               :initial-element 0)))
            (replace v (aref m2 rix))
            (setf (aref ans (+ rix nrows1)) v)))
    ans))

(defun adjoin-cols (m1 m2)
  (let* ((nrows1 (length m1))
         (nrows2 (length m2))
         (ncols1 (length (aref m1 0)))
         (ncols2 (length (aref m2 0)))
         (ans    (make-array (max nrows1 nrows2))))
    (loop for rix from 0 below (min nrows1 nrows2) do
          (let ((v  (make-array (+ ncols1 ncols2))))
            (replace v (aref m1 rix))
            (replace v (aref m2 rix) :start1 ncols1)
            (setf (aref ans rix) v)))
    (cond ((> nrows1 nrows2)
           (loop for rix from nrows2 below nrows1 do
                 (let ((v  (make-array (+ ncols1 ncols2)
                                       :initial-element 0)))
                   (replace v (aref m1 rix))
                   (setf (aref ans rix) v))))
          ((< nrows1 nrows2)
           (loop for rix from nrows1 below nrows2 do
                 (let ((v  (make-array (+ ncols1 ncols2)
                                       :initial-element 0)))
                   (replace v (aref m1 rix) :start1 ncols1)
                   (setf (aref ans rix) v)))) )
    ans))
          
(defun sw-cols (m col1 col2)
  (let* ((ans   (copy-matrix m))
         (nrows (length ans)))
    (loop for rix from 0 below nrows do
            (let* ((row (aref ans rix))
                   (x (aref row col1)))
              (setf (aref row col1) (aref row col2)
                    (aref row col2) x
                    (aref ans rix) row)
              ))
    ans))

(defun sw-rows (m row1 row2)
  (let* ((ans (copy-matrix m))
         (v   (aref ans row1)))
    (setf (aref ans row1) (aref ans row2)
          (aref ans row2) v)
    ans))

(defun mat+ (m1 m2)
  (map 'vector (lambda (row1 row2)
                 (map 'vector #'m+ row1 row2))
       m1 m2))
          
(defun mat- (m1 m2)
  (map 'vector (lambda (row1 row2)
                 (map 'vector #'m- row1 row2))
       m1 m2))

(defun mat-neg (m)
  (map 'vector (lambda (row)
                 (map 'vector #'m- row))
       m))

(defun mat-scale (m sf)
  (map 'vector (lambda (row)
                 (map 'vector (um:rcurry #'m* sf) row))
       m))
          
(defun mat-abs (m)
  (map 'vector
       (lambda (row)
         (map 'vector #'mabs row))
       m))

(defun mat-rows (m start end)
  (subseq m start end))

(defun selection-mat (nrows)
  (let ((ans (make-array 1))
        (v   (make-array nrows
                         :initial-element 0))
        (rand (prng:ctr-drbg-int nrows)))
    (setf (aref ans 0) v)
    (loop for cix from 0 below nrows do
            (when (logbitp cix rand)
              (setf (aref v cix) 1)))
    ans))

(defun ones-mat (nrows ncols)
  (let ((ans (make-array nrows)))
    (loop for rix from 0 below nrows do
          (setf (aref ans rix) (make-array ncols
                                           :initial-element 1)))
    ans))

;; -----------------------------------------------------------
#|
(let* ((base  (- (ash 1 10) 3))
       (m     (noise-mat 3 3 base)))
  (list m (mat-left (mat-top m 2) 2)))

(let* ((base  (- (ash 1 10) 3))
       (mat   (noise-mat 3 4 base))
       (v     #(#(1) #(2) #(3))))
  (multiple-value-bind (_ inv det vinv)
      (mat-inv mat base v)
    (list
     :mat mat
     :v   v
     :inv inv
     :vinv vinv
     :det det
     :mod base
     :prod (mat-mul mat inv base))))

(normalize-matrix '((1 2) (2 3 4)))

(let* ((base (- (ash 1 10) 3))
       (m    (noise-mat 4 4 base))
       (mx   (matrix-section m
                             :start-row 1
                             :start-col 1
                             :nrows     2
                             :ncols     2)))
  (list m mx))

(let* ((base  (- (ash 1 10) 3))
       (m     (noise-mat 2 2 base)))
  (adjust-matrix m 3 4))

(let* ((m1 #(#(1 2 3)))
       (m2 #(#(4 6))))
  (adjoin-cols (trn m1) (trn m2)))

(adjust-matrix (trn #(#(1 1 2)))  3 3)

(let* ((base  (- (ash 1 10) 3))
       (mat-a (noise-mat 2 3 base))
       (skey  #(#(1) #( 2) #( 3)))
       (pkey  (mat-mul mat-a skey base)))
  (multiple-value-bind (inv vinv det mxtra)
      (mat-inv mat-a base (adjoin-cols (mat-right mat-a 1) pkey))
    (list :mat-a mat-a
          :skey skey
          :pkey pkey
          :inv  inv
          :vinv vinv
          :det  det
          :mxtra mxtra)))

(mat-mul #(#(1) #(2) #(3)) #(#(4 5 6)) (- (ash 1 10) 3))
(mat-mul #(#(1 2 3)) #(#(4) #( 5) #( 6)) (- (ash 1 10) 3))

((a)                ((ad ae af)
 (b)  ((d e f)) =>   (bd be bf)
 (c))                (cd ce cf))

((1)            ((1 2 3)
 (_) ((1 2 3))   (_ _ _)
 (_))            (_ _ _))

V = V_0 + (V - V_0)
A.V = A.V_0 + A.(V-V_0)
IF B = A.V_0 then A.V = B + A.(V-V0)

b = A.s
Trn(A).b = Trn(A).A.s
Inv(Trn(A).A).Trn(A).b = s
(let* ((base  (- (ash 1 10) 3))
       ;; (mat-a (noise-mat 2 3 base))
       (mat-a #(#(2 3 5) #(1 4 7)))
       (skey  #(#(1) #( 2) #( 3)))
       (pkey  (mat-mul mat-a skey base))

       (augm  (adjoin-cols mat-a pkey)))
  (multiple-value-bind (auginv inv det)
      (mat-inv augm base)
    (list :mat-a mat-a
          :pkey  pkey
          :augm auginv)))

(let* ((base  (- (ash 1 10) 3)))
  (with-mod base
    (let* (;; (mat-a (noise-mat 2 3 base))
           (mat-a #(#(3 6 6 3 9)
                    #(6 12 13 0 3))))
      (mat-inv mat-a))))

(vm:dgesvd #2A((3 6 6 3 9) (6 12 13 0 3)))


(with-mod (- (ash 1 10) 3)
  (let* ((a  #(#(2 3 5)
             #(1 4 7)))
       (s  (trn #(#(1 2 3))))
       (p  (mat-mul a s))
       ;; (noise (noise-mat 8 2 1))
       (noise #(#(106) #(84)))
       (pn (mat+ p noise))
       (r  #(#(1 2))))
    (flet ((valias (x3)
             (trn (vector
                   (vector (m- 239 (m* x3 204))
                           (m- 224 (m* x3 206))
                           x3))))
           (pr (title val)
             (format t "~%~A = ~A" title val)))
      (print "----")
      (pr "a" a)
      (pr "s" s)
      (pr "p" p)
      (pr "pn" pn)
      (pr "noise" noise)
      (pr "r.pn" (mat-mul r pn))
      (pr "r.a"  (mat-mul r a))
      (pr "mat-inv" (multiple-value-list (mat-inv a pn)))
      (pr "v3" (valias 3))
      (list :skey-resid
            (mat- (mat-mul r pn) (mat-mul (mat-mul r a) s))
            :v0-resid
            (mat- (mat-mul r pn) (mat-mul (mat-mul r a) (valias 0))))
      )))

(with-mod (- (ash 1 11) 9)
  (let* ((m #(#(2 3)
              #(5 1)
              #(4 7)))
         (s #(#(2)
              #(3)))
         (p (mat-mul m s))
         (dx #(#(1) #(0) #(0)))
         (dy #(#(0) #(1) #(0))))
    (print "----------------")
    ;; (list (mat-mul m dx) (mat-mul m dy) (mat-mul m dz))
    (let* ((noise (noise-mat 5 3 1))
           (noise #(#(2035) #(2036) #(6)))
           (np    (mat+ noise p))
           (mt    (trn m))
           (mtm   (mat-mul mt m))
           (mtp   (mat-mul mt np)))
      ;; (pr "noise" noise)
      (multiple-value-bind (_ mtm-inv det x)
          (mat-inv mtm mtp)
        (pr "mtmI.mtp" (mat-mul mtm-inv mtp))
        (pr "mtmI.mt"  (mat-mul mtm-inv mt))
        (pr "p" p)
        (pr "np" np)
        (pr "mt.np" (mat-mul mt np))
        (pr "zz" (mat-mul mtm-inv (mat-mul mt p)))
        (let ((ss (mat-mul mtm-inv mtp)))
          (pr "ss" ss)
          (pr "pp"  (mat-mul mtm ss)))
        (plt:axes 'plt
                  :clear t
                  :xrange '(-100 100)
                  :yrange '(-100 100))
        (plt:draw-circle 'plt
                         (aref (aref p 1) 0)
                         (aref (aref p 2) 0)
                         32
                         :color :gray50
                         :alpha 0.4)
        (loop for n from 0 to 8 do
                (loop for k from 0 to 10 do
                        (let* ((v (mat+ (mat+ s (mat-scale dx n))
                                        (mat-scale dy k)))
                               (u (mat-mul m v)))
                  (plt:plot 'plt (aref u 1) (aref u 2)
                            :symbol :dot))))
        ))))

(with-mod (getf *sys* :base)
  (let* ((m   (getf *sys* :mat-a))
         (mt  (trn m))
         (mtm (mat-mul mt m))
         (s   (getf *sys* :s))
         (p   (getf *sys* :p)))
    (multiple-value-bind (minv _ det)
        (mat-inv mtm)
      det
      )))

|#
;; ----------------------------------------------------------
;; show true skey resid vs matrix computed skey

(defvar *sys*
  (let* ((base    (- (ash 1 11) 9))
         (nrows   400)
         (ncols   32))
    (with-mod base
      (let* ((mat-a  (random-mat nrows ncols))
             (s      (random-mat ncols 1))
             (p      (mat-mul mat-a s)))
        (list :nrows  nrows
              :ncols  ncols
              :base   base
              :mat-a  mat-a
              :s      s
              :p      p)))
    ))
#|
(progn
  (inspect *sys*)
  (values))
|#

(defun solve (sys rowix)
  (let* ((ncols  (getf sys :ncols))
         (mat-a  (getf sys :mat-a))
         (pn     (getf sys :pn))
         (base   (getf sys :base)))
    (with-mod base
      (let ((subm  (mat-rows mat-a rowix (+ rowix ncols)))
            (subp  (mat-rows pn rowix (+ rowix ncols))))
        (multiple-value-bind (minv inv det x)
            (mat-inv subm subp)
          (declare (ignore minv inv det))
          x)))
    ))

#|
(time (solve *sys* 0))
 |#

(defun get-sols (sys)
  (let* ((nrows  (getf sys :nrows))
         (ncols  (getf sys :ncols)))
    (loop for rowix from 0 to (min 100 (- nrows ncols)) collect (solve sys rowix))
    ))

(defun renoise (nbits)
  (let* ((nrows (getf *sys* :nrows))
         (p     (getf *sys* :p))
         (base  (getf *sys* :base)))
    (with-mod base
      (let* ((noise (noise-mat nbits nrows 1))
             (pn    (mat+ p noise)))
        (setf (getf *sys* :nnoise) nbits
              (getf *sys* :noise)  noise
              (getf *sys* :pn)     pn
              (getf *sys* :sols)   (get-sols *sys*))
        :done
        ))
    ))
#|
(progn
  (renoise 0)
  (let* ((s (getf *sys* :s))
         (sols (getf *sys* :sols)))
    (assert (every (um:curry #'equalp s) sols))))

(time (renoise 6))

;; Prefer 2*log2(NCols) < log2(q) < 1+2*log2(NCols)
;;        NRows ≈ (1+eps)*(1+NCols)*log2(q)
;; for modulus q.

|#

(defun pr (title val)
  (format t "~%~A = ~A" title val))

(defun ilog2 (x)
  (integer-length x))

(defun mat-int (m)
  (aref (aref m 0) 0))

(defun tst (ntrials)
  (with-mod (getf *sys* :base)
    (let* ((nrows  (getf *sys* :nrows))
           ;; (ncols  (getf *sys* :ncols))
           (mat-a  (getf *sys* :mat-a))
           (s      (getf *sys* :s))
           (pn     (getf *sys* :pn))
           (ones   (ones-mat 1 nrows))
           (bsum   (mat-abs (mat-mul ones pn)))
           (asum   (mat-mul ones mat-a))
           (ctr    0)
           (found  nil)
           (intervals nil))
      (labels (
               #|
               (solve (nrow)
                 (multiple-value-bind (minv)
                     (mat-inv (mat-rows (adjoin-cols mat-a pn) nrow (+ nrow ncols)))
                   (values (mat-right minv 1) minv)))
               |#
               (pr (title val)
                 (declare (ignore title val)))
               (to-hex (sel &optional (ix 0) (acc 0))
                 (if (>= ix nrows)
                     acc
                   (to-hex sel (1+ ix) (+ acc acc (aref (aref sel 0) ix)))))
               (ilog2m (m)
                 (ilog2 (mat-int m)))
               (check (sx)
                 (mat-abs (mat- bsum (mat-mul asum sx))) )
               (decrypt (sx r)
                 (let ((rp     (mat-mul r pn))
                       (ra     (mat-mul r mat-a)))
                   (mat-abs (mat- rp (mat-mul ra sx))) )))
      (loop repeat ntrials do
              (incf ctr)
              (setf found nil)
              (let* ((r  (selection-mat nrows)))
                (pr "s"  (list (ilog2m (check s)) (ilog2m (decrypt s r))))
                (loop for rix from 0
                      for x in (getf *sys* :sols)
                      do
                        (incf ctr)
                        (let* ((chk  (check x))
                               (dec  (decrypt x r))
                               (lchk (ilog2m chk))
                               (ldec (ilog2m dec)))
                          (pr (format nil "s~d" rix) (list lchk ldec))
                          (when (zerop ldec)
                            (pr "minv" (list rix chk dec))
                            (pr "sel"  (format nil "#x~x" (to-hex r)))
                            (unless found
                              (setf found t)
                              (push ctr intervals)
                              (setf ctr 0))
                            )))))
      ;; Intervals have mean ≈ stdev --> Poisson?
      (plt:histogram 'histo intervals
                     :clear t)
      (list :freq (float (/ (length intervals) ntrials))
            :mn   (float (vm:mean intervals))
            :sd   (float (vm:stdev intervals)))
        #|
        (loop for rix from 0 to (- nrows ncols) do
                (multiple-value-bind (x minv)
                    (solve rix)
                  (let* ((chk  (check x))
                         (dec  (decrypt x))
                         (lchk (log2m chk))
                         (ldec (log2m dec)))
                    (pr (format nil "s~d" rix) (list lchk ldec))
                    (when (zerop ldec)
                      (pr "minv" (list rix chk dec minv x))
                      (pr "sel"  (format nil "#x~x" (to-hex r))))
                  )))
        |#
        ))))
#|
(time (tst 4000))


Mark Taylor 520-206-9585
|#

;; -----------------------------------------------------
