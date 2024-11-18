;; matrix-extralisp -- Matrix Operations
;; DM/RAL 01/24
;; ----------------------------------------------------

(defpackage #:xmatrix
  (:use :common-lisp)
  (:export
   ))

(in-package :xmatrix)

;; -----------------------------------------------------
(defun idn (nrows)
  (let ((ans (make-array nrows)))
    (loop for ix from 0 below nrows do
          (let ((row (make-array nrows
                                 :initial-element 0)))
            (setf (cl:aref row ix) 1
                  (cl:aref ans ix) row)))
    ans))

(defun zero-mat (nrows ncols)
  (let ((ans (make-array nrows)))
    (loop for rix from 0 below nrows do
            (setf (aref ans rix) (make-array ncols
                                             :initial-element 0)))
    ans))
            
(defun noise-mat (nrows ncols &key (mean 0.0) (sd 1.0))
  (let ((ans  (make-array nrows)))
    (loop for ix from 0 below nrows do
            (setf (cl:aref ans ix) (vm:gnoise ncols :mean mean :sd sd)))
    ans))

(defun copy-matrix (mat)
  (let ((ans (copy-seq mat)))
    (loop for v across mat
          for ix from 0
          do
          (setf (cl:aref ans ix) (copy-seq v)))
    ans))

(defun mat-inv (mat &optional mextra)
  (let* ((nrows  (length mat))
         (ncols  (length (cl:aref mat 0)))
         (idn    (idn nrows))
         (mat    (copy-matrix mat))
         (invdet 1)
         (mextra (and mextra
                      (copy-matrix mextra))))
    (labels ((sub-scaled-row (dst sf row)
               (map-into dst (lambda (a b)
                               (cl:- a (* sf b)))
                         dst row)))
      (loop for rix from 0 below (min nrows ncols) do
              (let* ((row  (aref mat rix))
                     (cix  (position-if (complement #'zerop) row)))
                (when cix
                  (let* ((irow   (cl:aref idn rix))
                         (xrow   (and mextra
                                      (cl:aref mextra rix)))
                         (rinv   (/ (aref row cix)))
                         (scaler (um:rcurry #'* rinv)))
                    (setf invdet (* invdet rinv))
                    (map-into row scaler row)
                    (map-into irow scaler irow)
                    (when mextra
                      (map-into xrow scaler xrow)
                      (setf (cl:aref mextra rix) xrow))
                    (setf (cl:aref mat rix) row
                          (cl:aref idn rix) irow)
                    (loop for rrix from 0 below nrows do
                            (unless (eql rrix rix)
                              (let ((rr  (cl:aref (cl:aref mat rrix) cix)))
                                (sub-scaled-row (cl:aref mat rrix) rr row)
                                (sub-scaled-row (cl:aref idn rrix) rr irow)
                                (when mextra
                                  (sub-scaled-row (cl:aref mextra rrix) rr xrow))
                                )))
                    ))
                )))
    (values mat idn (/ invdet) mextra)
    ))

(defun trn (a)
  (let* ((nrows (length a))
         (ncols (length (cl:aref a 0)))
         (ans   (make-array ncols)))
    (loop for ix from 0 below ncols do
          (let ((v (make-array nrows)))
            (loop for jx from 0 below nrows do
                  (setf (cl:aref v jx) (cl:aref (cl:aref a jx) ix)))
            (setf (cl:aref ans ix) v)))
    ans))

(defun dot-prod (a b)
  (reduce #'cl:+ (map 'vector #'* a b)))

(defun mat-mul (a b)
  (let* ((nrows  (length a))
         (bt     (trn b))
         (ncols  (length bt))
         (ans    (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let ((v (make-array ncols))
                  (row (cl:aref a rix)))
              (loop for cix from 0 below ncols do
                      (setf (cl:aref v cix)
                            (dot-prod row (cl:aref bt cix))))
              (setf (cl:aref ans rix) v)))
    ans))
              

(defun mat-left (m ncols)
  (let* ((nrows (length m))
         (ans   (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let* ((v     (cl:aref m rix))
                   (row   (make-array ncols
                                      :initial-element 0)))
              (replace row v)
              (setf (cl:aref ans rix) row)))
    ans))

(defun mat-right (m ncols)
  (let* ((nrows (length m))
         (ans   (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let* ((v      (cl:aref m rix))
                   (vcols  (length v))
                   (start2 (max 0 (cl:- vcols ncols)))
                   (row    (make-array ncols
                                       :initial-element 0)))
              (replace row v :start2 start2)
              (setf (cl:aref ans rix) row)))
    ans))

(defun mat-top (m nrows)
  (let* ((ans   (make-array nrows))
         (mrows (length m))
         (ncols (length (cl:aref m 0))))
    (loop for rix from 0 below (min nrows mrows) do
            (setf (cl:aref ans rix) (copy-seq (cl:aref m rix))))
    (loop for rix from (min nrows mrows) below nrows do
            (setf (cl:aref ans rix) (make-array ncols
                                             :initial-element 0)))
    ans))

(defun mat-bottom (m nrows)
  (let* ((ans   (make-array nrows))
         (mrows (length m))
         (ncols (length (cl:aref m 0)))
         (start (max 0 (cl:- mrows nrows))))
    (loop for rix from 0 below (min nrows mrows) do
            (setf (cl:aref ans rix) (copy-seq (cl:aref m (cl:+ start rix)))))
    (loop for rix from (min nrows mrows) below nrows do
            (setf (cl:aref ans rix) (make-array ncols
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
                (mv (cl:aref m rix)))
            (replace v (coerce mv 'vector))
            (setf (cl:aref ans rix) v)))
    ans))

(defun matrix-section (m &key
                         (start-row 0)
                         (start-col 0)
                         nrows ncols)
  (let* ((nrows (or nrows
                    (length m)))
         (ncols (or ncols
                    (length (cl:aref m 0))))
         (ans   (make-array nrows)))
    (loop for rix from 0 below (min nrows (length m)) do
          (let ((v  (make-array ncols
                                :initial-element 0)))
            (replace v (cl:aref m (cl:+ start-row rix)) :start2 start-col)
            (setf (cl:aref ans rix) v)))
    (loop for rix from (length m) below nrows do
          (setf (cl:aref ans rix) (make-array ncols
                                           :initial-element 0)))
    ans))

(defun adjust-matrix (m nrows ncols)
  (let ((mrows  (length m))
        (ans    (make-array nrows)))
    (loop for rix from 0 below (min nrows mrows) do
          (let ((v  (make-array ncols
                                :initial-element 0)))
            (replace v (cl:aref m rix))
            (setf (cl:aref ans rix) v)))
    (loop for rix from mrows below nrows do
          (setf (cl:aref ans rix) (make-array ncols
                                           :initial-element 0)))
    ans))

(defun adjoin-rows (m1 m2)
  (let* ((nrows1  (length m1))
         (nrows2  (length m2))
         (ncols1  (length (cl:aref m1 0)))
         (ncols2  (length (cl:aref m2 0)))
         (ans     (make-array (cl:+ nrows1 nrows2))))
    (loop for rix from 0 below nrows1 do
          (let ((v (make-array (max ncols1 ncols2)
                               :initial-element 0)))
            (replace v (cl:aref m1 rix))
            (setf (cl:aref ans rix) v)))
    (loop for rix from 0 below nrows2 do
          (let ((v (make-array (max ncols1 ncols2)
                               :initial-element 0)))
            (replace v (cl:aref m2 rix))
            (setf (cl:aref ans (cl:+ rix nrows1)) v)))
    ans))

(defun adjoin-cols (m1 m2)
  (let* ((nrows1 (length m1))
         (nrows2 (length m2))
         (ncols1 (length (cl:aref m1 0)))
         (ncols2 (length (cl:aref m2 0)))
         (ans    (make-array (max nrows1 nrows2))))
    (loop for rix from 0 below (min nrows1 nrows2) do
          (let ((v  (make-array (cl:+ ncols1 ncols2))))
            (replace v (cl:aref m1 rix))
            (replace v (cl:aref m2 rix) :start1 ncols1)
            (setf (cl:aref ans rix) v)))
    (cond ((> nrows1 nrows2)
           (loop for rix from nrows2 below nrows1 do
                 (let ((v  (make-array (cl:+ ncols1 ncols2)
                                       :initial-element 0)))
                   (replace v (cl:aref m1 rix))
                   (setf (cl:aref ans rix) v))))
          ((< nrows1 nrows2)
           (loop for rix from nrows1 below nrows2 do
                 (let ((v  (make-array (cl:+ ncols1 ncols2)
                                       :initial-element 0)))
                   (replace v (cl:aref m1 rix) :start1 ncols1)
                   (setf (cl:aref ans rix) v)))) )
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
                 (map 'vector #'+ row1 row2))
       m1 m2))
          
(defun mat- (m1 m2)
  (map 'vector (lambda (row1 row2)
                 (map 'vector #'- row1 row2))
       m1 m2))

(defun mat-neg (m)
  (map 'vector (lambda (row)
                 (map 'vector #'- row))
       m))

(defun mat-scale (m sf)
  (map 'vector (lambda (row)
                 (map 'vector (um:rcurry #'* sf) row))
       m))
          
#|
(let* ((m     (noise-mat 3 3)))
  (list m (mat-left (mat-top m 2) 2)))

(let* ((mat   (noise-mat 3 4))
       (v     (vector 1 2 3)))
  (multiple-value-bind (_ vinv inv det)
      (mat-inv mat v)
    (list
     :mat mat
     :v   v
     :inv inv
     :vinv vinv
     :det det
     :prod (mat-mul mat inv))))

(normalize-matrix '((1 2) (2 3 4)))

(let* ((m    (noise-mat 4 4))
       (mx   (matrix-section m
                             :start-row 1
                             :start-col 1
                             :nrows     2
                             :ncols     2)))
  (list m mx))

(let* ((m     (noise-mat 2 2)))
  (adjust-matrix m 3 4))

(let* ((m1 #(#(1 2 3)))
       (m2 #(#(4 6))))
  (adjoin-cols (trn m1) (trn m2)))

(adjust-matrix (trn #(#(1 1 2)))  3 3)

(let* ((mat-a #(#(3 6 6 3 9) #(6 12 13 0 3))))
  (mat-inv mat-a))

(let* ((a #(#(1  0 -3  0  2 -8)
            #(0  1  5  0 -1  4)
            #(0  0  0  1  7 -9)
            #(0  0  0  0  0  0))))
  (mat-mul (trn a) #(#(0 0 1 0 0 0)
                     #(0 0 0 0 1 0)
                     #(0 0 0 0 0 1)))))

x1 =  3 x3 - 2 x5 + 8 x6
x2 = -5 x3 +   x5 - 4 x6
x3
x4 =        -7 x5 + 9 x6
x5
x6

(let* ((a #(#(1  0 -3  0  2 -8)
            #(0  1  5  0 -1  4)
            #(0  0  0  1  7 -9)
            #(0  0  0  0  0  0))))
  (mat-mul a (trn #(#( 3 -5  1  0  0  0)
                    #(-2  1  0 -7  1  0)
                    #( 8 -4  0  9  0  1)))))
  
(let* ((a #(#(1  0 -3  0  2 -8)
            #(0  1  5  0 -1  4)
            #(0  0  0  1  7 -9)
            #(0  0  0  0  0  0))))
  (mat-mul a (trn #(#(0 0 0 0 0 1)))))

(let* ((a  #(#(2 3 5)
             #(1 4 7))))
  (mat-inv (adjoin-cols a (trn #(#(23 30))))))

(let* ((a  #(#(2 3 5)
             #(1 4 7))))
  (mat-mul a (trn #(#(1 2 3)))))

(let* ((a  #(#(2 3 5)
             #(1 4 7)))
       (s  (trn #(#(1 2 3))))
       (p  (mat-mul a s))
       (noise (trn #(#(10 -23))))
       (pn (mat+ p noise))
       (r  #(#(2 1))))
  (print a)
  (print s)
  (print p)
  (print (mat-mul r pn))
  (print (mat-mul r a))
  (mat- (mat-mul r pn) (mat-mul (mat-mul r a) (valias 0))))

(defun valias (x3)
  (trn (vector
        (vector (+ 2/5 (* 1/5 x3))
                (- 37/5 (* 9/5 x3))
                x3))))
(valias 0)
|#
;; -----------------------------------------------------
