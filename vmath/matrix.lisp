;; matrix.lisp -- matrix operators
;;
;; DM/RAL  11/10
;; ------------------------------------------
;; Supports scalars, vectors, 2-D arrays, and matix objects
;; A matrix object is a vector of vectors.

(defpackage #:com.ral.matrix-ops
  (:use #:common-lisp)
  (:shadow
   #:abs #:trace #:sqrt
   #:+ #:* #:- #:/
   #:aref
   #:conjugate #:phase
   #:sin   #:cos   #:tan
   #:asin  #:acos  #:atan
   #:sinh  #:cosh  #:tanh
   #:asinh #:acosh #:atanh
   #:exp   #:log
   #:floor #:ceiling #:round #:truncate
   )
  (:export
   #:matrix
   #:make-matrix
   #:matrix-rows
   #:nrows
   #:ncols
   #:aref
   #:sum
   #:prod
   #:as-vector
   #:<*>
   #:>*<
   #:abs
   #:neg
   #:trace
   #:sqrt
   #:+
   #:*
   #:-
   #:/
   #:copy
   #:copyi
   #:idn
   #:diag
   #:trn
   #:swap-rows
   #:get-row
   #:get-col
   #:solve
   #:det
   #:inv
   #:cholsl
   #:msolve

   #:floor
   #:ceiling
   #:round
   #:truncate
   
   #:maxabs
   #:sq

   #:conjugate
   #:phase
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:sinh
   #:cosh
   #:tanh
   #:asinh
   #:acosh
   #:atanh
   #:exp
   #:log
   ))

(in-package #:com.ral.matrix-ops)

;; ----------------------------------------------------------
;; Matrix and associated operations

(defclass matrix ()
  ((rows :accessor matrix-rows :initarg :rows)))

(defun make-matrix (&key rows)
  (make-instance 'matrix
                 :rows rows))


(defmethod nrows ((m matrix))
  (length (matrix-rows m)))

(defmethod nrows ((v vector))
  ;; vectors are considered row-vectors
  1)

(defmethod nrows ((a array))
  (array-dimension a 0))


(defmethod ncols ((m matrix))
  (length (cl:aref (matrix-rows m) 0)))

(defmethod ncols ((v vector))
  (length v))

(defmethod ncols ((a array))
  (array-dimension a 1))


(defmethod aref ((m matrix) row col)
  (cl:aref (cl:aref (matrix-rows m) row) col))

(defmethod aref ((a array) row col)
  (cl:aref a row col))

(defmethod set-aref ((m matrix) row col x)
  (setf (cl:aref (cl:aref (matrix-rows m) row) col) x))

(defmethod set-aref ((a array) row col x)
  (setf (cl:aref a row col) x))

(defsetf aref (m row col) (v)
  `(set-aref ,m ,row ,col ,v))

(defmethod matrix-rows ((a array))
  (let ((nrows (nrows a)))
    (as-vector (loop for row from 0 below nrows collect
                     (get-row a row)))))

;; -----------------------

(defun sum (v)
  (reduce #'cl:+ v))

(defun prod (v)
  (reduce #'cl:* v))


(defun as-vector (lst)
  (coerce lst 'vector))


(defun <*> (&rest vs)
  ;; vector dot product
  (sum (apply #'map 'vector #'cl:* vs)))

(defun >*< (v1 v2)
  ;; vector outer product
  (make-matrix
   :rows (as-vector (loop for rx across v1 collect
                          (as-vector (loop for cx across v2 collect
                                           (cl:* rx cx))) ))))

;; ---------------------

(defmacro defunop (name op)
  (let ((x (gensym)))
    `(defun ,name (,x)
       (copy ,x :fn #',op))))

(defmacro define-unary-operators ()
  `(progn
     ,@(mapcar
        (um:lambda* ((name op))
          `(defunop ,name ,op))
        '((abs       cl:abs)
          (neg       cl:-)
          ;; (inv       cl:/)
          (sq        (lambda (x) (* (conjugate x) x)))
          (sqrt      cl:sqrt)
          (conjugate cl:conjugate)
          (phase     cl:phase)
          (sin       cl:sin)
          (cos       cl:cos)
          (tan       cl:tan)
          (asin      cl:asin)
          (acos      cl:acos)
          (atan      cl:atan)
          (sinh      cl:sinh)
          (cosh      cl:cosh)
          (tanh      cl:tanh)
          (asinh     cl:asinh)
          (acosh     cl:acosh)
          (atanh     cl:atanh)
          (exp       cl:exp)
          (log       cl:log)
          (floor     cl:floor)
          (ceiling   cl:ceiling)
          (round     cl:round)
          (truncate  cl:truncate)
          )) ))

(define-unary-operators)

;; -------------------------------------

(defgeneric copy (x &key fn))

(defmethod copy (x &key (fn #'identity))
  (funcall fn x))

(defmethod copy ((v sequence) &key (fn #'identity))
  ;; SEQUENCE - works on both vectors and lists
  (map 'vector fn v))

(defmethod copy ((m matrix) &key (fn #'identity))
  (make-matrix
   :rows (map 'vector (lambda (row)
                        (map 'vector fn row))
              (matrix-rows m))))


(defun make-array-like (a &rest args)
  (apply #'make-array (array-dimensions a)
         :element-type (array-element-type a)
         args))

(defun overlay-vec (a)
  (make-array (array-total-size a)
              :element-type (array-element-type a)
              :displaced-to a
              :displaced-index-offset 0))

(defmethod #1=copy ((a array) &key (fn #'identity))
  (let* ((ans       (make-array-like a))
         (hist      nil)  ;; past array-element-type history
         (val       ans)) ;; discriminator bucket
    (tagbody
     again
     (handler-bind
         ((type-error (lambda (c)
                        (declare (ignore c))
                        (unless (eq val ans)
                          ;; When val is ans, we had problems in the
                          ;; function fn, not in the array SETF. Since
                          ;; we just created ans, locally, there is no
                          ;; way that fn could possibly know about it.
                          ;;
                          ;; Allow progressive element-type upgrades,
                          ;; but prevent us from bouncing between two
                          ;; alternate types.
                          ;;
                          ;; If we see that we have already been a
                          ;; suitable type, then just go fully general
                          ;; with boxed type T. That should end our
                          ;; getting called here.
                          ;;
                          (let ((new-type (upgraded-array-element-type (type-of val))))
                            (if (find-if (um:curry #'subtypep new-type) hist)
                                ;; keep us from oscillating forever
                                (setf new-type t)
                              (push (array-element-type ans) hist))
                            (setf ans (make-array (array-dimensions a)
                                                  :element-type new-type))
                            (go again)
                            )))
                      ))
       (map-into (overlay-vec ans)
                 (lambda (x)
                   (setf val ans  ;; set up discriminator between fn and setf
                         val (funcall fn x)))
                 (overlay-vec a))
       (return-from #1# ans)
       ))
    ))

#|
(let* ((vec (make-array 5
                        :element-type 'double-float)))
  (setf (cl:aref vec 0) 256))

(let* ((vec (vm:framp 10))
       (ans (copy vec :fn #'evenp)))
  (values (type-of ans) ans))

(let* ((vec (vm:framp 10))
       (ans (copy vec :fn #'zerop)))
  (values (type-of ans) ans))

(let* ((vec (vm:framp 10))
       (ans (copy vec :fn #'round)))
  (values (type-of ans) ans))

(let* ((vec (vm:framp 10))
       (ans (copy vec :fn (lambda (x)
                            (if (> x 4)
                                (float x 1f0)
                              (float x 1d0))))))
  (values (type-of ans) ans))

(let* ((vec (vm:framp 10))
       (ans (copy vec :fn (constantly pi))))
  (values (type-of ans) ans))

(let* ((vec (vm:dramp 5))
       (ans (copy vec :fn (constantly 1f0))))
  (values (type-of ans) ans))

(compute-applicable-methods #'copy (list (make-array 5 :element-type 'single-float)))
(compute-applicable-methods #'copy (list (vector 1 2)))
(compute-applicable-methods #'copy (list '(1 2 3)))
(clos:class-precedence-list (list 1 2 3))

 |#
;; -------------------------------------

(defun identityi (x &rest ixs)
  (declare (ignore ixs))
  x)

(defmethod copyi ((v vector) &key (fn #'identityi))
  (as-vector (loop for x across v
                   for ix from 0
                   collect (funcall fn x ix))))

(defmethod copyi ((m matrix) &key (fn #'identityi))
  (make-matrix
   :rows (as-vector (loop for iy from 0
                          for row across (matrix-rows m)
                          collect
                          (as-vector (loop for ix from 0
                                           for v across row
                                           collect
                                           (funcall fn v iy ix))) )) ))

(defmethod copyi ((a array) &key (fn #'identityi))
  (let ((ans   (make-array-like a))
        (nrows (nrows a))
        (ncols (ncols a)))
    (loop for iy from 0 below nrows do
          (loop for ix from 0 below ncols do
                (setf (cl:aref ans iy ix)
                      (funcall fn (cl:aref a iy ix) iy ix)) ))
    ans))

          
;; -------------------------------------

(defmethod + (x m)
  (x+nm m x))

(defmethod + ((x cons) m)
  (x+c m x)) 

(defmethod + ((v vector) x)
  (x+v x v))

(defmethod + ((m matrix) x)
  (x+m x m))

(defmethod + ((a array) x)
  (x+a x a))

;; -------------------------------------

(defun - (a b)
  (+ a (neg b)))

;; -------------------------------------

(defmethod / (a b)
  (a*invb b a))

(defmethod a*invb ((b cons) a)
  (map 'list (um:curry 'cl:/ a) b))

(defmethod a*invb ((b vector) a)
  (map 'vector (um:curry #'cl:/ a) b))

(defmethod a*invb (b a)
  (* a (inv b)))


(defmethod / ((a cons) b)
  (c/b b a))

(defmethod c/b (b a)
  (map 'list (um:rcurry 'cl:/ b) a))

(defmethod c/b ((b cons) a)
  (map 'list 'cl:/ a b))

(defmethod c/b ((b vector) a)
  (map 'vector 'cl:/ a b))


(defmethod / ((a vector) b)
  (v/b b a))

(defmethod v/b ((b vector) v)
  (vops:vdiv v b))

(defmethod v/b (b v)
  (vops:vscale (cl:/ b) v))


;; -------------------------------------

(defun idn (n)
  (make-matrix
   :rows (as-vector (loop for row from 0 below n collect
                          (let ((vrow (make-array n :initial-element 0)))
                            (setf (cl:aref vrow row) 1)
                            vrow)) )))

;; -------------------------------------

(defmethod diag (x)
  x)

(defmethod diag ((v vector))
  (let ((nrows (length v)))
    (make-matrix
     :rows (as-vector (loop for row from 0
                            for x across v
                            collect
                            (let ((vrow (make-array nrows :initial-element 0)))
                              (setf (cl:aref vrow row) x)
                              vrow)) ))))

(defmethod diag ((m matrix))
  (diag-m-or-a m))

(defmethod diag ((a array))
  (diag-m-or-a a))

(defmethod diag ((c cons))
  (diag (coerce c 'vector)))

(defun diag-m-or-a (m)
  ;; m can be matrix or array
  (copyi m :fn (lambda (v iy ix)
                        (if (= ix iy)
                            v
                          (coerce 0 (type-of v)))) ))


;; -------------------------------------

(defmethod * (x m)
  (x*nm m x))

(defmethod * ((x cons) m)
  (x*c m x))

(defmethod * ((v vector) x)
  (x*v x v))

(defmethod * ((m matrix) x)
  (x*m x m))

(defmethod * ((a array) x)
  (x*a x a))


;; -------------------------------------

(defmethod trn (x)
  x)

(defmethod trn ((v vector))
  ;; a vector is a row-vector or a column vector
  ;; depending on local context
  ;; Generally: Left = row, Right = column
  v)

(defmethod trn ((a array))
  (let* ((nrows (array-dimension a 0))
         (ncols (array-dimension a 1))
         (mt (make-array `(,ncols ,nrows)
                         :element-type (array-element-type a))))
    (loop for iy from 0 below ncols do
          (loop for ix from 0 below nrows do
                (setf (cl:aref mt iy ix) (cl:aref a ix iy)) ))
    mt))

(defmethod trn ((m matrix))
  (let* ((ncols (ncols m))
         (rows  (matrix-rows m)))
    (make-matrix
     :rows (coerce (loop for iy from 0 below ncols collect
                         (coerce (loop for row across rows collect
                                       (cl:aref row iy))
                                 'vector))
                   'vector)) ))

;; -------------------------------------

(defmethod trace (x)
  x)

(defmethod trace ((m matrix))
  (trace-m-or-a m))

(defmethod trace ((a array))
  (trace-m-or-a a))

(defun trace-m-or-a (a)  
  (loop for ix from 0 below (nrows a) sum
        (aref a ix ix)))


;; -------------------------------------

(defun maxabs (&rest args)
  (reduce #'max (mapcar #'cl:abs args)))

;; -------------------------------------

(defmethod swap-rows ((m matrix) row1 row2)
  (let ((mrows (matrix-rows m)))
    (rotatef (cl:aref mrows row1) (cl:aref mrows row2))))

(defmethod swap-rows ((v vector) row1 row2)
  (rotatef (cl:aref v row1) (cl:aref v row2)))

(defmethod swap-rows ((a array) row1 row2)
  (let ((ncols (ncols a)))
    (loop for ix from 0 below ncols do
          (rotatef (cl:aref a row1 ix) (cl:aref a row2 ix)))))

;; -------------------------------------

(defmethod get-row ((m matrix) row)
  (cl:aref (matrix-rows m) row))

(defmethod get-row ((v vector) row)
  (cl:aref v row))

(defmethod get-row ((a array) row)
  (as-vector
   (loop for ix from 0 below (ncols a) collect
         (cl:aref a row ix))))

;; -------------------------------------

(defmethod put-row ((v vector) row x)
  (setf (cl:aref v row) x))

(defmethod put-row ((m matrix) row v)
  (setf (cl:aref (matrix-rows m) row) v))

(defmethod put-row ((a array) row v)
  (loop for x across v
        for ix from 0
        do
        (setf (cl:aref a row ix) x)))

(defsetf get-row (m row) (v)
  `(put-row ,m ,row ,v))

;; -------------------------------------

(defmethod get-col ((v vector) col)
  (cl:aref v col))

(defmethod get-col ((m matrix) col)
  (as-vector (loop for rowv in (matrix-rows m) collect
                   (cl:aref rowv col))))

(defmethod get-col ((a array) col)
  (as-vector (loop for row from 0 below (nrows a) collect
                   (cl:aref a row col))))

;; -------------------------------------

(defmethod put-col ((v vector) col x)
  (set (cl:aref v col) x))

(defmethod put-col ((m matrix) col v)
  (loop for rowv in (matrix-rows m)
        for x across v
        do
        (setf (cl:aref rowv col) x)))

(defmethod put-col ((a array) col v)
  (loop for row from 0
        for x across v
        do
        (setf (cl:aref a row col) x)))

(defsetf get-col (m col) (v)
  `(put-col ,m ,col ,v))

;; -------------------------------------

(defmethod msolve ((m matrix) v)
  (msolve-m-or-a m v))

(defmethod msolve ((a array) v)
  (msolve-m-or-a a v))

(defun compute-upper-triangular-form (mwrk vwrk)
  ;; uses Gaussian elimination and pivoting. Pivots in matrix mwrk, but
  ;; works across both mwrk and aux vector/matrix vwrk
  ;; NOTE: destructive operations
  (let ((nrows (nrows mwrk)))
    
    (labels ((find-pivot (col)
               (let* ((leading (loop for row from col below nrows collect
                                     (aref mwrk row col)))
                      (maxv    (reduce #'maxabs leading)))
                 (when (zerop maxv)
                     (error "Singular matrix"))
                 (cl:+ col (position maxv leading
                                     :test (lambda (a b)
                                             (= a (abs b)))))))
             
             (pivot (prow col)
               (unless (= prow col)
                 (swap-rows mwrk prow col)
                 (swap-rows vwrk prow col))
               (let* ((pivrow  (get-row mwrk col))
                      (vpivrow (get-row vwrk col))
                      (piv     (cl:aref pivrow col)))
                 (loop for row from (1+ col) below nrows do
                       (let* ((mrowv (get-row mwrk row))
                              (num   (cl:aref mrowv col))
                              (vrowv (get-row vwrk row)))
                         (unless (zerop num)
                           (let ((sf (cl:/ num piv)))
                             (setf (get-row mwrk row) (- mrowv (* sf pivrow))
                                   (get-row vwrk row) (- vrowv (* sf vpivrow)))) ))) )))
      
      (loop for col from 0 below (1- nrows) do
            (let ((prow (find-pivot col)))
              (pivot prow col)))

      (values mwrk vwrk))))

(defun compute-diagonal-form (mwrk vwrk)
  ;; NOTE: destructive operations
  (compute-upper-triangular-form mwrk vwrk)
  (let ((nrows (nrows mwrk)))
    (loop for row from (1- nrows) downto 0 do
          (let* ((pivrow  (get-row mwrk row))
                 (vpivrow (get-row vwrk row))
                 (piv     (cl:aref pivrow row)))
            (loop for iy from 0 below row do
                  (let* ((mrow (get-row mwrk iy))
                         (vrow (get-row vwrk iy))
                         (num  (cl:aref mrow row))
                         (sf   (cl:/ num piv)))
                    (unless (zerop num)
                      (setf (get-row mwrk iy) (- mrow (* sf pivrow))
                            (get-row vwrk iy) (- vrow (* sf vpivrow)))) ))
            )))
  (values mwrk vwrk))

(defun msolve-m-or-a (m v)
  ;; solve M . x = v
  ;; uses Gaussian elimination and pivoting
  ;; non-destructive
  (let* ((mwrk  (copy m))
         (vwrk  (copy v))
         (nrows (nrows mwrk)))
    (compute-diagonal-form mwrk vwrk)
    (loop for row from 0 below nrows do
          (setf (get-row vwrk row)
                (* (cl:/ (aref mwrk row row))
                    (get-row vwrk row))))
    (values vwrk mwrk)))


;; -------------------------------------

(defmethod det (x)
  x)

(defmethod det ((m matrix))
  (det-m-or-a m))

(defmethod det ((a array))
  (det-m-or-a a))

(defun det-m-or-a (a)
  ;; non-destructive
  (handler-case
      (multiple-value-bind (vwrk awrk)
          (msolve-m-or-a a (idn (nrows a)))
        (declare (ignore vwrk))
        (let ((det 1))
          (loop for ix from 0 below (nrows awrk) do
                (setf det (cl:* det (aref awrk ix ix))))
          det))
    (error ()
      0)))

;; -------------------------------------

(defmethod inv (x)
  (cl:/ x))

(defmethod inv ((x cons))
  (map 'list 'cl:/ x))

(defmethod inv ((x vector))
  (map 'vector 'cl:/ x))

(defmethod inv ((a array))
  (inv-m-or-a a))

(defmethod inv ((m matrix))
  (inv-m-or-a m))

(defun inv-m-or-a (m)
  ;; non-destructive
  (multiple-value-bind (vwrk mwrk)
      (msolve-m-or-a m (idn (nrows m)))
    (declare (ignore mwrk))
    vwrk))

;; -------------------------------------

#|

(setf xm (diag #(1 2 3)))
(setf yv #(4 5 6))
(msolve xm yv)
(det xm)

(setf am #2A((1 0 0) (0 2 0) (0 0 3)))
(msolve am yv)
(det am)
(matrix-rows (inv am))

(let* ((m (make-matrix
           :rows #(#(1 1 1)
                   #(1 2 1)
                   #(2 2 3))))
       (inv (inv m)))
  (values m (matrix-rows inv)
          (matrix-rows (* inv m))
          (matrix-rows (* m inv))))

(let* ((m #2A((1 1 1)
              (1 2 1)
              (2 2 3)))
       (inv (inv m)))
  (values m (matrix-rows inv)
          (matrix-rows (* inv m))
          (matrix-rows (* m inv))))
|#

;; -------------------------------------

(defun choldc (m)
  (let* ((a     (matrix-rows (copy m)))
         (nrows (length a))
         (p     (make-array nrows)))
    (loop for rv across a
          for ix from 0
          do
          (let ((sum (cl:aref rv ix)))
            (loop for kx from 0 below ix do
                  (decf sum (sq (cl:aref rv kx))))
            (unless (plusp sum)
              (error "choldc failed"))
            (let ((sf (cl:/ (cl:sqrt sum))))
              (setf (cl:aref p ix) sf)
              (loop for jx from (1+ ix) below nrows do
                    (let ((sum (cl:aref rv jx)))
                      (loop for kx from 0 below ix do
                            (decf sum (cl:* (cl:aref rv kx) (cl:aref (cl:aref a jx) kx))))
                      (setf (cl:aref (cl:aref a jx) ix) (cl:* sf sum)) ))) ))
    (values a p)))
                    
(defun cholsl (m v)
  (multiple-value-bind (a p)
      (choldc m)
    (let* ((nrows (length v))
           (x     (make-array nrows)))
      (loop for ix from 0
            for rv across a
            for px across p
            do
            (let ((sum (cl:aref v ix)))
              (loop for kx from (1- ix) downto 0 do
                    (decf sum (cl:* (cl:aref rv kx) (cl:aref v kx))))
              (setf (cl:aref x ix) (cl:* sum px))))
      (loop for ix from (1- nrows) downto 0 do
            (let ((sum (cl:aref x ix)))
              (loop for kx from (1+ ix) below nrows do
                    (decf sum (cl:* (cl:aref (cl:aref a kx) ix) (cl:aref x kx))))
              (setf (cl:aref x ix) (cl:* sum (cl:aref p ix)))))
      x)))

;; --------------------------------------------
;; second arg, m, now known to be a matrix

(defmethod x+m (x m)
  (copy m :fn (um:curry #'cl:+ x)))

(defmethod x+m ((x matrix) m)
  (make-matrix
   :rows (map 'vector #'vops:vadd (matrix-rows x) (matrix-rows m))))

(defmethod x+m ((x array) m)
  (make-matrix
   :rows (as-vector (loop for row from 0 below (nrows m) collect
                          (+ (get-row m row)
                             (get-row x row))))))
                              
(defmethod x*m (x m)
  (copy m :fn (um:curry #'cl:* x)))

(defmethod x*m ((v vector) m)
  (map 'vector (um:rcurry #'<*> v) (matrix-rows m)))

(defmethod x*m ((x matrix) m)
  (x*m-m-or-a x m))

(defmethod x*m ((x array) m)
  (x*m-m-or-a x m))


(defun x*m-m-or-a (x m)
  (make-matrix
   :rows (let* ((nrows  (nrows m))
                (xt     (trn x))
                (ncols  (nrows xt))
                (mrows  (matrix-rows m))
                (xtrows (matrix-rows xt)))
           (as-vector (loop for row from 0 below nrows collect
                            (as-vector (loop for col from 0 below ncols collect
                                             (<*> (cl:aref mrows row) (cl:aref xtrows col))) ))))))

;; --------------------------------------------
;; special for row-vector * matrix

(defmethod x*ml (x m)
  (x*m x m))

(defmethod x*ml ((v vector) m)
  (as-vector
   (loop for row across (matrix-rows (trn m)) collect
         (<*> v row)) ))

;; --------------------------------------------
;; second arg, x, known not to be a matrix

(defmethod x+nm (m x)
  (cl:+ m x))

(defmethod x+nm ((v vector) x)
  (x+v x v))

(defmethod x+nm ((m matrix) x)
  (x+m x m))

(defmethod x*nm (m x)
  (cl:* m x))

(defmethod x*nm ((v vector) x)
  (x*v x v))

(defmethod x*nm ((m matrix) x)
  (x*ml x m))

;; -----------------------------------------
;; second arg c, known to be a cons

(defmethod x+c (x c)
  (map 'list (um:curry 'cl:+ x) c))

(defmethod x+c ((x cons) c)
  (map 'list 'cl:+ x c))

(defmethod x+c ((x vector) c)
  (map 'vector 'cl:+ x c))


(defmethod x*c (x c)
  (map 'list (um:curry 'cl:* x) c))

(defmethod x*c ((x cons) c)
  (map 'list 'cl:* x c))

(defmethod x*c ((x vector) c)
  (map 'vector 'cl:* x c))

;; ------------------------------------------
;; second arg v, known to be a vector

(defmethod x+v (x v)
  (vops:voffset x v))

(defmethod x+v ((x cons) v)
  (map 'vector 'cl:+ x v))

(defmethod x+v ((x vector) v)
  (vops:vadd x v))

(defmethod x*v (x v)
  (vops:vscale x v))

(defmethod x*v ((x cons) v)
  (map 'vector 'cl:* x v))

(defmethod x*v ((x vector) v)
  (vops:vmul x v))

(defmethod x*v ((m matrix) v)
  (x*ml v m))

;; ----------------------------------------
;; second arg a, known to be an array

(defmethod x+a (x a)
  (copy a :fn (lambda (v)
                (cl:+ x v))))

(defmethod x+a ((x array) a)
  (copyi a :fn (lambda (v iy ix)
                 (cl:+ v (cl:aref x iy ix))) ))


(defmethod x*a (x a)
  (copy a :fn (lambda (v)
                (cl:* x v))))

(defmethod x*a ((m matrix) a)
  (x*m-m-or-a m a))

(defmethod x*a ((x array) a)
  (x*m-m-or-a x a))

