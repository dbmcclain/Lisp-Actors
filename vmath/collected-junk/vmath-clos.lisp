;; vmath.lisp -- Vectorized math in Lisp
;;
;; DM/MCFA  08/99
;; ---------------------------------------------------------------

(defpackage "VECTORIZED-MATH"
  (:use "COMMON-LISP")
  (:nicknames "VMATH" "VM")
  (:export
   "IRAMP"
   "FRAMP"
   "DRAMP"
   "BIPOLAR-FRAMP"
   "UNOISE"
   "HYPOT"
   "SINC"
   "LOGABS"
   "VECTOR-OF"
   "GASDEV"
   "GNOISE"
   "MEDIAN"
   "MAD"
   "NEGMAD"
   "TOTAL"
   "MEAN"
   "STDEV"
   "PERCENTILE"
   "PERCENTILES"
   "COPY"
   "HISTOGRAM"

   "FZEROS"
   "FONES"
   "IZEROS"
   "IONES"
   
   "INNER-PROD"
   "GENERAL-INNER-PROD"
   "OUTER-PROD"
   "GENERAL-OUTER-PROD"
   "DIST"
   "SHIFT"
   "SHIFTH"
   "SLICE"
   "RESHAPE"
   "MAP-DIMENSION"
   "REDUCE-DIMENSION"
   "VROTL"
   "VROTR"
   "VSHIFTH"
   "SPLIT"
   "TRANSPOSE"
   "ACANON"
   "XPLANE"
   "YPLANE"
   "BIPOLAR-XPLANE"
   "BIPOLAR-YPLANE"
   "SHIFTED-BIPOLAR-FRAMP"
   "SHIFTED-BIPOLAR-XPLANE"
   "SHIFTED-BIPOLAR-YPLANE"

   "VECTORWISE"
   "ELEMENTWISE"
   "ELEMENTWISE-REDUCE"
   "GENSYMS-FOR-ARGS"
   "IXSORT"

   "REQUIRE-SAME-SHAPE"
   "REQUIRE-SAME-SIZE"
   "DEFUN-FFP"
   ))
   
(in-package "VECTORIZED-MATH")

;; ---------------------------------------------------------------
(defmethod varray-dimensions ((arr array))
  (array-dimensions arr))

(defmethod varray-dimensions ((arr ca:<carray>))
  (ca:carray-dimensions arr))


;; ----------------
(defmethod varray-dimension ((arr array) index)
  (array-dimension arr index))

(defmethod varray-dimension ((arr ca:<carray>) index)
  (ca:carray-dimension arr index))


;; ----------------
(defmethod varray-total-size ((arr array))
  (array-total-size arr))

(defmethod varray-total-size ((arr ca:<carray>))
  (ca:carray-total-size arr))


;; ----------------
(defmethod varray-element-type ((arr array))
  (array-element-type arr))

(defmethod varray-element-type ((arr ca:<carray>))
  (ca:carray-element-type arr))


;; ----------------
(defmethod varray-rank ((arr array))
  (array-rank arr))

(defmethod varray-rank ((arr ca:<carray>))
  (ca:carray-rank arr))


;; ----------------
(defmethod varray-row-major-index ((arr array))
  (array-row-major-index arr))

(defmethod varray-row-major-index ((arr ca:<carray>))
  (ca:carray-row-major-index arr))

;; ----------------
(defmethod aref-closure ((arr array))
  #'(lambda (&rest indices)
      (apply #'aref arr indices)))

(defmethod aref-closure ((arr ca:<carray>))
  #'(lambda (&rest indices)
      (apply #'ca:caref arr indices)))


;; ----------------
(defmethod vref ((arr array) &rest indices)
  (apply #'aref arr indices))

(defmethod vref ((arr ca:<carray>) &rest indices)
  (apply #'ca:caref arr indices))


;; ----------------
(defmethod set-vref-closure ((arr array))
  #'(lambda (v &rest indices)
      (setf (apply #'aref arr indices) v)))

(defmethod set-vref-closure ((arr ca:<carray>))
  #'(lambda (v &rest indices)
      (setf (apply #'ca:caref arr indices) v)))


;; ----------------
(defmethod (setf vref) (val (arr array) &rest indices)
  (setf (apply #'aref arr indices) val))

(defmethod (setf vref) (val (arr ca:<carray>) &rest indices)
  (setf (apply #'ca:caref arr indices) val))


;; ----------------
(defmethod varray-row-major-aref ((arr array) index)
  (funcall #'row-major-aref arr index))

(defmethod varray-row-major-aref ((arr ca:<carray>) index)
  (funcall #'ca:row-major-caref arr index))


;; ----------------
(defmethod (setf varray-row-major-aref) (v (arr array) index)
  (setf (row-major-aref arr index) v))

(defmethod (setf varray-row-major-aref) (v (arr ca:<carray>) index)
  (setf (ca:row-major-caref arr index) v))

;; ----------------
(defmethod varray-row-major-aref-closure ((arr array))
  #'(lambda (index)
      (row-major-aref arr index)))

(defmethod varray-row-major-aref-closure ((arr ca:<carray>))
  #'(lambda (index)
      (ca:row-major-caref arr index)))

;; ---------------------------------------------------------------
(defun require-same-shape (&rest args)
  (unless (apply #'equal (mapcar #'varray-dimensions args))
    (error "arrays not the same shape")))

(defun require-same-size (&rest args)
  (unless (apply #'= (mapcar #'varray-total-size args))
    (error "arrays not the same size")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pair-up (&rest args)
    (apply #'mapcar #'list args)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensyms-for-args (args &optional pref)
    (mapcar #'(lambda (arg)
                (declare (ignore arg))
                (gensym pref))
            args)))

(defun make-overlay-vector (arr)
  (if (vectorp arr)
      arr
    (make-array (varray-total-size arr)
                :displaced-to arr
                :element-type (varray-element-type arr))))

(defun pair-up-gensyms-with-overlay-vectors (gargs args)
  (mapcar #'(lambda (garg arg)
              `(,garg (make-overlay-vector ,arg)))
          gargs args))

(defun make-destination-vector (&rest args)
  (make-array (apply #'min (mapcar #'varray-total-size args))))

;; ---------------------------------------------------------------
;; Macro to encourage the use of inline FPU code sans boxing of
;; intermediate results... The (float 0) optimization appears to have
;; no effect unless stated at the topmost declare level.
;;
#+:lispworks
(defmacro defun-ffp (name args &body body)
  `(defun ,name ,args
     (declare (optimize (speed  3)
                        (safety 0)
                        (debug  0)
                        (float  0)))
     ,@body))

#-:lispworks
(defmacro defun-ffp (name args &body body)
	`(defun ,name ,args ,@body))

;; ---------------------------------------------------------------
;; elementwise on vectors...
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-args-from-bindings (arg-bindings)
    (let ((argnames (mapcar #'(lambda (binding)
                                (if (consp binding)
                                    (first binding)
                                  binding))
                            arg-bindings))
          (argvalues (mapcar #'(lambda (binding)
                                 (if (consp binding)
                                     (second binding)
                                   binding))
                             arg-bindings)))
      (values argnames argvalues))))

(defun indexed-dest-vwise (fn dest &rest args)
  (let ((args  (mapcar #'make-overlay-vector args))
        (vdest (make-overlay-vector dest)))
    (dotimes (ix (length vdest) dest)
      (setf (aref vdest ix)
            (apply fn ix (mapcar #'(lambda (v)
                                     (aref v ix))
                                 args)))
      )))

(defun indexed-vwise (fn &rest args)
  (let ((args  (mapcar #'make-overlay-vector args))
        (vdest (apply #'make-destination-vector args)))
    (dotimes (ix (length vdest) vdest)
      (setf (aref vdest ix)
            (apply fn ix (mapcar #'(lambda (v)
                                     (aref v ix))
                                 args)))
      )))

(defun dest-vwise (fn dest &rest args)
  (let ((args  (mapcar #'make-overlay-vector args))
        (vdest (make-overlay-vector dest)))
    (apply #'map-into vdest fn args)
    dest))

(defun vwise (fn &rest args)
  (let ((args (mapcar #'make-overlay-vector args)))
    (apply #'map 'vector fn args)))

(defun indexed-null-vwise (fn &rest args)
  (let ((args (mapcar #'make-overlay-vector args)))
    (dotimes (ix (apply #'min (mapcar #'length args)))
      (apply fn ix (mapcar #'(lambda (v)
                               (aref v ix))
                           args)))
    ))

(defun null-vwise (fn &rest args)
  (let ((args (mapcar #'make-overlay-vector args)))
    (apply #'map nil fn args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun need-symbolic-index-var (index)
    (unless (symbolp index)
      (error "symbol index var expected, got: ~A" index))))

(defmacro vectorwise (arg-bindings body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
    (if index
        (progn
          (need-symbolic-index-var index)
          (if dest
              `(indexed-dest-vwise #'(lambda (,index ,@argnames) ,body-form)
                                   ,dest ,@argvalues)
            `(indexed-vwise #'(lambda (,index ,@argnames) ,body-form)
                            ,@argvalues)))
      (if dest
          `(dest-vwise #'(lambda ,argnames ,body-form)
                       ,dest ,@argvalues)
        `(vwise #'(lambda ,argnames ,body-form)
                ,@argvalues))
      )))

(defmacro vectorwise-iter (arg-bindings body-form &key index)
  ;; Iterate a function over corresponding elements of the argument arrays.
  ;; No result is stored here, although the function might cause its own
  ;; side effects...
  ;; index is name to use for index so to be accessible in body-form
  (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
    (if index
        (progn
          (need-symbolic-index-var index)
          `(indexed-null-vwise #'(lambda (,index ,@argnames) ,body-form)
                               ,@argvalues))
      `(null-vwise #'(lambda ,argnames ,body-form) ,@argvalues))
    ))

#| -- tests of macro-expansion -- don't try to evaluate these forms... --
(vectorwise (a b c) (+ a (* b (/ c 2))) :dest tmp)
(vectorwise (a b) (+ a b) :dest (make-array 5))  ;; should no longer create error
(vectorwise (a b c) (+ a (* b (/ c 2))))
(vectorwise (a b c) (+ a (* b (/ c 2))) :dest (make-array 5))
(vectorwise (a b c) (+ a (* b (/ c ix))) :index ix)
|#

;; ---------------------------------------------------------------
;; elementwise on N-dimensional arrays...
;;
(defmacro elementwise (arg-bindings body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
    (if dest
        `(vectorwise ,arg-bindings ,body-form :dest ,dest :index ,index)
      (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
        (let ((gdest (gensym "dest-")))
          `(let* (,@(pair-up argnames argvalues)
                  (,gdest (vectorwise ,argnames ,body-form :index ,index)))
             (make-array (varray-dimensions ,(first argnames))
                         :displaced-to ,gdest
                         :element-type (varray-element-type ,gdest)))
          ))
      ))

(defmacro elementwise-iter (arg-bindings body-form &key index)
  `(vectorwise-iter ,arg-bindings ,body-form :index ,index))

(defmacro elementwise-reduce (fn arg &rest keys)
  (let ((garg (gensym "arg-")))
    `(let ((,garg (make-overlay-vector ,arg)))
       (apply #'reduce ,fn ,garg ,keys))
    ))

;; ---------------------------------------------------------------
;;
(defun-ffp iramp (nel)
  (declare (type fixnum nel))
  (let ((v (make-array nel
                       :initial-element 0
                       :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (varray-row-major-aref v ix) ix))
    ))

(defun-ffp framp (nel)
  (declare (type fixnum nel))
  (let ((v (make-array nel
                       :initial-element 0.0s0
                       :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (varray-row-major-aref v ix) (coerce ix 'single-float)))
    ))

(defun-ffp dramp (nel)
  (declare (type fixnum nel))
  (let ((v (make-array nel
                       :initial-element 0.0d0
                       :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (varray-row-major-aref v ix) (coerce ix 'double-float)))
    ))

(defun-ffp bipolar-framp (nel)
  (declare (type fixnum nel))
  (let ((v (make-array nel :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) v))
    (do ((ix 0 (1+ ix))
         (val (- (/ (coerce nel 'float) 2.0))
              (+ val 1.0)))
        ((>= ix nel) v)
      (declare (type fixnum ix)
               (type float val))
      (setf (varray-row-major-aref v ix) val))
    ))

(defun-ffp unoise (nel limit &optional (random-state *random-state*))
  (declare (type fixnum nel))
  (let ((v (make-array nel :element-type 'single-float))
        (flimit (coerce limit 'float)))
    (declare (type float flimit)
             (type (simple-array single-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (varray-row-major-aref v ix) 
				(the float (random flimit random-state))
				))
    ))

(defvar *v-other* nil)
(defun-ffp gasdev (&optional (random-state *random-state*))
  (if *v-other*
      (let ((ans *v-other*))
	(setf *v-other* nil)
	ans)
      (let ((v1 (- (* 2.0 
						(the float (random 1.0 random-state))
						) 1.0))
            (v2 (- (* 2.0 
						(the float (random 1.0 random-state))
						) 1.0)))
        (declare (type float v1 v2))
        (let ((r2 (+ (* v1 v1) (* v2 v2))))
          (declare (type float r2))
          (if (and (plusp r2)
                   (< r2 1.0))
              (let ((fac 
						(the float (sqrt (/ (* -2.0 (the float (log r2))) r2)))
						))
                (declare (type float fac))
                (setf *v-other* (* fac v1))
                (* fac v2))
	      (gasdev random-state))
          ))
  ))

(defun-ffp gnoise (nel &key (mean 0.0) (sd 1.0) (random-state *random-state*))
  (declare (type fixnum nel))
  (let ((v (make-array nel :element-type 'single-float))
        (fmean (coerce mean 'float))
        (fsd   (coerce sd   'float)))
    (declare (type (simple-array single-float (*)) v)
             (type float fmean fsd))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (varray-row-major-aref v ix)
            (+ fmean (* fsd 
						(the float (gasdev random-state))
						))))
    ))

(defun hypot (x y)
  (abs (complex x y)))
  
(defun-ffp sinc (x)
  (if (zerop x)
      1.0
    (let ((fx (coerce x 'float)))
      (declare (type float fx))
      (/ 
				(the float (sin fx)) 
				fx))))

(defun logabs (x &optional base)
  (log (abs x) base))

;; ----------------------------------------------
;;
(defmethod vector-of (x)
  (make-array 1 :initial-element x))

(defmethod vector-of ((lst cons))
  (coerce lst 'vector))

(defmethod vector-of ((v vector))
  v)

(defmethod vector-of ((arr array))
  (make-array (varray-total-size arr)
              :displaced-to arr
              :element-type (varray-element-type arr)))

;; -----------------------------------------------------
(defun general-inner-prod (inner-op arr1 arr2 outer-op)
  (reduce outer-op
          (vector-of
           (elementwise (arr1 arr2)
                        (funcall inner-op arr1 arr2)))
          ))

(defun inner-prod (arr1 arr2)
  (general-inner-prod #'* arr1 arr2 #'+))

(defun blit-array (src offs dst offd nel)
  (let ((vsrc (make-array nel
               :displaced-to src
               :displaced-index-offset
               (apply #'varray-row-major-index src offs)
               :element-type (varray-element-type src)))
        (vdst (make-array nel
               :displaced-to dst
               :displaced-index-offset
               (apply #'varray-row-major-index dst offd)
               :element-type (varray-element-type dst))))
    (map-into vdst #'identity vsrc)))


(defun izero (x)
  (declare (ignore x))
  0)

(defun split (arr)
  (if (> (varray-rank arr) 1)
      (let* ((dims  (varray-dimensions arr))
             (dtail (rest dims))
             (zs    (mapcar #'izero dtail)))
        (map 'vector
             #'(lambda (ix)
                 (make-array dtail
                  :displaced-to arr
                  :displaced-index-offset
                  (apply #'varray-row-major-index arr (cons ix zs))
                  :element-type (varray-element-type arr)
                  ))
             (iramp (first dims))))
    arr))

(defun foldl (op init v)
  (reduce op v :initial-value init))

(defun foldr (op init v)
  (reduce op v :initial-value init :from-end t))

(defun aflat-dimensions (arr)
  (if (arrayp arr)
      (append (varray-dimensions arr)
              (aflat-dimensions (varray-row-major-aref arr 0)))))

(defun aflat (arr)
  (if (arrayp (varray-row-major-aref arr 0))
      (foldr #'(lambda (v rslt)
                 (concatenate 'vector v rslt))
             #() arr)
    (vector-of arr)))

(defun acanon (arr)
  (let ((new-arr (aflat arr)))
    (make-array (aflat-dimensions arr)
                :displaced-to new-arr
                :element-type (varray-element-type new-arr))))

(defun general-outer-prod (op arr1 arr2)
  (let* ((ans (make-array (append (varray-dimensions arr1)
                                  (varray-dimensions arr2))
                          :element-type (varray-element-type arr1)))
         (sz2 (varray-total-size arr2)))
    (elementwise-iter (arr1)
        (elementwise (arr2)
                    (funcall op arr1 arr2)
                    :dest (make-array sz2
                             :displaced-to ans
                             :displaced-index-offset (* ix sz2)
                             :element-type (varray-element-type ans)))
        :index ix)
    ans))

(defun outer-prod (arr1 arr2)
  (general-outer-prod #'* arr1 arr2))

(defun fones (nel)
  (make-array nel :initial-element 1.0
              :element-type 'single-float))

(defun fzeros (nel)
  (make-array nel :initial-element 0.0
              :element-type 'single-float))

(defun iones (nel)
  (make-array nel :initial-element 1
              :element-type 'fixnum))

(defun izeros (nel)
  (make-array nel :initial-element 0
              :element-type 'fixnum))

(defun xplane (ny nx)
  (outer-prod (fones ny) (framp nx)))

(defun yplane (ny nx)
  (outer-prod (framp ny) (fones nx)))

(defun bipolar-xplane (ny nx)
  (outer-prod (fones ny) (bipolar-framp nx)))

(defun bipolar-yplane (ny nx)
  (outer-prod (bipolar-framp ny) (fones nx)))

(defun vrotr (vec nel)
  (let ((pos (- (length vec) nel)))
    (concatenate 'vector
                 (subseq vec pos)
                 (subseq vec 0 pos))
    ))

(defun vrotl (vec nel)
  (concatenate 'vector
               (subseq vec nel)
               (subseq vec 0 nel)))


(defun vshifth (vec)
  (vrotl vec (truncate (length vec) 2)))

(defun shifted-bipolar-framp (nel)
  (vshifth (bipolar-framp nel)))

(defun shifted-bipolar-xplane (ny nx)
  (outer-prod (fones ny) (shifted-bipolar-framp nx)))

(defun shifted-bipolar-yplane (ny nx)
  (outer-prod (shifted-bipolar-framp ny) (fones nx)))

(defun dist (ny nx)
  (general-outer-prod #'hypot
                      (shifted-bipolar-framp ny)
                      (shifted-bipolar-framp nx)))

(defun shift (arr nels)
  (labels ((make-shifter (dims strides nels)
              (let ((nel (car nels))
                    (dim (car dims))
                    (stride (car strides))
                    (mover (if (cdr dims)
                               (make-shifter (cdr dims)
                                             (cdr strides)
                                             (cdr nels))
                             #'(lambda (src srcoff dst dstoff)
                                 (setf (aref dst dstoff)
                                       (aref src srcoff)))
                             )))
                (let ((nel (if (minusp nel)
                               (+ dim nel)
                             nel)))
                  #'(lambda (src srcoff dst dstoff)
                      (dotimes (iy nel)
                        (funcall mover
                                 src
                                 (+ srcoff (* stride (+ iy (- dim nel))))
                                 dst
                                 (+ dstoff (* iy stride))))
                      (dotimes (iy (- dim nel))
                        (funcall mover
                                 src
                                 (+ srcoff (* stride iy))
                                 dst
                                 (+ dstoff (* stride
                                              (+ iy nel))))))
                  ))))
    (let ((dst (make-array (varray-dimensions arr)
                           :element-type (varray-element-type arr))))
      (let ((vsrc (make-array (varray-total-size arr)
                              :displaced-to arr
                              :element-type (varray-element-type arr)))
            (vdst (make-array (varray-total-size dst)
                              :displaced-to dst
                              :element-type (varray-element-type dst)))
            (shifter (make-shifter (varray-dimensions arr)
                                   (varray-strides arr)
                                   nels)))
        (funcall shifter vsrc 0 vdst 0)
        dst))))

(defun varray-strides (arr)
  (reduce #'(lambda (dim rslt)
              (cons (* dim (car rslt))
                    rslt))
          (cdr (varray-dimensions arr))
          :initial-value '(1)
          :from-end t))

(defun shifth (arr)
  (shift arr (mapcar
              #'(lambda (x)
                  (truncate x 2))
              (varray-dimensions arr))))

(defun transpose (arr)
  (unless (= (varray-rank arr) 2)
    (error "Transpose requires a rank 2 array"))
  (destructuring-bind (ny nx) (varray-dimensions arr)
    (declare (type fixnum ny nx))
    (let ((ans (make-array `(,nx ,ny)
                           :element-type (varray-element-type arr))))
      (loop for iy from 0 below ny do
            (loop for ix from 0 below nx do
                  (setf (aref ans ix iy) (aref arr iy ix))
                  ))
      ans)))
#|
;; this version suffers on large arrays...
;; it tries to pass too many arguments to APPLY
(defun transpose (arr)
  (unless (= (varray-rank arr) 2)
    (error "Transpose requires a rank 2 array"))
  (let ((dims (reverse (varray-dimensions arr)))
        (v    (split arr)))
    (make-array
     dims
     :displaced-to           
     (apply #'concatenate
            'vector
            (map 'list
                 #'(lambda (ix)
                     (map 'vector
                          #'(lambda (sub)
                              (aref sub ix))
                          v))
                 (iramp (first dims)))
            ))))
|#

(defun slice (arr dim &rest ixs)
  (let* ((pre  (subseq ixs 0 dim))
         (post (subseq ixs (1+ dim))))
    (map 'vector
         #'(lambda (ix)
             (apply #'aref arr (append pre (list ix) post)))
         (iramp (varray-dimension arr dim)))
    ))

(defun internal-map-dimension (fn arr dim)
  (if (zerop dim)
      (map 'vector fn (split arr))
    (map 'vector #'(lambda (sub)
                     (internal-map-dimension fn sub (1- dim)))
         (split arr))
    ))

(defun map-dimension (fn arr dim)
  (acanon (internal-map-dimension fn arr dim)))

(defun internal-reduce-dimension (fn arr dim)
  (if (zerop dim)
      (reduce fn (split arr))
    (map 'vector
         #'(lambda (sub)
             (internal-reduce-dimension fn sub (1- dim)))
         (split arr))
    ))

(defun reduce-dimension (fn arr dim)
  (acanon (internal-reduce-dimension fn arr dim)))

#|
(setf d (dist 64 32))

(map-dimension #'total d 0)

; -- same as --

(reduce-dimension #'+ d 1) ; -> like IDL: total(d,0)

;;
(defun v+ (a b)
	(elementwise (a b) (+ a b)))
(reduce-dimension #'v+ d 0) ; -> like IDL: total(d,1)

|#

(defun reshape (arr dims)
  (make-array dims
              :displaced-to arr
              :element-type (varray-element-type arr)))

(defun copy (arr)
  (make-array (varray-dimensions arr)
              :displaced-to
              (map-into (make-array (varray-total-size arr)
                                    :element-type (varray-element-type arr))
                        #'identity (vector-of arr))
              :element-type (varray-element-type arr)))

(defun select-kth (vec ixs k left right)
	(declare 
		#+:lispworks 
		(optimize (debug 1) (speed 3) (safety 0)) ;; what is this trying to achieve?
        (type fixnum k left right)
        (type (array fixnum 1) ixs)
        (type (array * 1) arr))
  (labels ((ind-aref (ix)
				;; Corman Lisp does not yet handle DECLARE inside of (LABELS ...) (v/1.5 12/01)
				#-:cormanlisp 
				(declare (type fixnum ix))
				(aref vec (the fixnum (aref ixs ix)))
				))
    (let ((l   left)
          (r   right))
      (declare (type fixnum l r))
      (tagbody
       again-1
       (let ((v (ind-aref k))
             (i l)
             (j r))
         (declare (type fixnum i j))
         (tagbody
          again-2
          (let ((ip (do ((ix i (1+ ix)))
                        ((<= v (ind-aref ix)) ix)
                      (declare (type fixnum ix))
                      ))
                (jp (do ((ix j (1- ix)))
                        ((>= v (ind-aref ix)) ix)
                      (declare (type fixnum ix))
                      )))
            (declare (type fixnum ip jp))
            (if (<= ip jp)
                (let ((ipp (1+ ip))
                      (jpp (1- jp)))
                  (declare (type fixnum ipp jpp))
                  (rotatef (aref ixs ip) (aref ixs jp))
                  (setf i ipp
                        j jpp)
                  (if (<= ipp jpp)
                      (go again-2)))
              (setf i ip
                    j jp))
            ))
         (if (< j k) (setf l i))
         (if (< k i) (setf r j))
         (if (< l r) (go again-1))
         )))
    (values (ind-aref k) ixs)))

(defun percentile (arr pc)
  (let* ((vec   (vector-of arr))
         (len   (length vec))
         (limit (1- len))
         (ixs   (iramp len))
         (index (round (* limit pc) 100)))
    (values (select-kth vec ixs index 0 limit) index)))
      
(defun median (arr)
  (percentile arr 50))

(defun mad (arr &optional (med (median arr)))
  (median (elementwise (arr)
                       (abs (- arr med)))
          ))

;; ---------------------------------------------------------------------
#+:cormanlisp
(progn
(defmethod where (predicate (proseq null) &key key)
  (declare (ignore predicate key))
  nil)

(defmethod where (predicate (proseq cons) &key (key 'identity))
  (let* ((rslt (list nil))
         (tail rslt))
    (do ((l proseq (cdr l))
         (ix 0     (1+ ix)))
        ((endp l) (cdr rslt))
      (if (funcall predicate (funcall key (car l)))
          (setf (cdr tail) (list ix)
                tail       (cdr tail))))
    ))

(defmethod where (predicate (proseq vector) &key (key 'identity))
  (let* ((rslt (list nil))
         (tail rslt))
    (dotimes (ix (length proseq) (cdr rslt))
      (if (funcall predicate (funcall key (aref proseq ix)))
          (setf (cdr tail) (list ix)
                tail       (cdr tail))))
    ))

(defmethod where (predicate (proseq array) &key (key 'identity))
  (where predicate (make-array (varray-total-size proseq)
                               :displaced-to proseq
                               :element-type (varray-element-type proseq))
         :key key))


(defun where-not (predicate &rest rest)
  (apply 'where (complement predicate) rest))


(defmethod subselect (proseq (where null))
  nil)

(defmethod subselect (proseq (where cons))
  (subselect proseq (coerce where 'vector)))

(defmethod subselect ((proseq list) (where vector))
  (subselect (coerce proseq 'vector) where))

(defmethod subselect ((proseq vector) (where vector))
  (let* ((len  (length where))
         (rslt (if (plusp len)
                   (make-array len
                               :element-type (varray-element-type proseq))
                 nil)))
    (when rslt
      (dotimes (ix len rslt)
        (setf (aref rslt ix) (aref proseq (aref where ix)))))
    ))

(defmethod subselect ((proseq array) (where vector))
  (subselect (make-array (varray-total-size proseq)
                         :displaced-to proseq
                         :element-type (varray-element-type proseq))
             where)))
;; -----------------------------------------------------------------------

#-:cormanlisp
(defun negmad (arr)
  (let* ((med  (median arr))
         (npix (coerce
                (um:subselect arr
                              (um:where #'plusp
                                        (elementwise (arr)
                                                     (if (< arr med)
                                                         1.0
                                                       0.0))))
                'vector)))
    (mad npix (median npix))))

#+:cormanlisp
(defun negmad (arr)
  (let* ((med  (median arr))
         (npix (coerce
                (subselect arr
                              (where #'plusp
                                        (elementwise (arr)
                                                     (if (< arr med)
                                                         1.0
                                                       0.0))))
                'vector)))
    (mad npix (median npix))))

(defun percentiles (arr)
  (let* ((v     (vector-of arr))
         (len   (length v))
         (limit (1- len))
         (ixs   (iramp len))
         (n50   (round (* 50 limit) 100))
         (n01   (round limit 100))
         (n05   (round (*  5 limit) 100))
         (n10   (round (* 10 limit) 100))
         (n25   (round (* 25 limit) 100))
         (n75   (round (* 75 limit) 100))
         (n90   (round (* 90 limit) 100))
         (n95   (round (* 95 limit) 100))
         (n99   (round (* 99 limit) 100))
         (pc50  (select-kth v ixs n50 0 limit))
         (pc25  (select-kth v ixs n25 0 n50))
         (pc10  (select-kth v ixs n10 0 n25))
         (pc05  (select-kth v ixs n05 0 n10))
         (pc01  (select-kth v ixs n01 0 n05))
         (pc75  (select-kth v ixs n75 n50 limit))
         (pc90  (select-kth v ixs n90 n75 limit))
         (pc95  (select-kth v ixs n95 n90 limit))
         (pc99  (select-kth v ixs n99 n95 limit)))
    (list
     :pc01 pc01
     :pc05 pc05
     :pc10 pc10
     :pc25 pc25
     :pc50 pc50
     :pc75 pc75
     :pc90 pc90
     :pc95 pc95
     :pc99 pc99)))

(defun ixsort (arr)
  (let ((v (vector-of arr)))
    (sort (iramp (varray-total-size v)) #'<
          :key #'(lambda (ix)
                   (aref v ix)))
    ))

(defun total (arr)
  (elementwise-reduce #'+ arr))

(defun mean (arr)
  (/ (reduce #'+ (vector-of arr)) (varray-total-size arr)))

(defun variance (arr &optional (mn (mean arr)))
  (labels ((square (x) (* x x)))
    (/ (reduce #'(lambda (ans x)
                   (+ ans (square (- x mn))))
               (vector-of arr))
       (1- (varray-total-size arr)))))

(defun stdev (arr &optional (mn (mean arr)))
  (sqrt (variance arr mn)))

(defun histogram (arr &key min max range nbins binwidth)
  (let* ((v (vector-of arr))
         (minv (if range
                   (elt range 0)
                 (or min
                     (reduce #'min v))))
         (maxv (if range
                   (elt range 1)
                 (or max
                     (reduce #'max v))))
         (range (- maxv minv))
         (nbins (or nbins
                    (and binwidth
                         (truncate range binwidth))
                    200))
         (binwidth (or binwidth
                       (/ range nbins)))
         (h (make-array nbins :initial-element 0
                        :element-type 'integer))
         (x (let ((xs (framp nbins)))
              (vectorwise (xs)
                          (+ minv (/ binwidth 2)
                             (* binwidth xs))
                          :dest xs))))
    (dotimes (ix (length v))
      (let ((jx (round (- (aref v ix) minv) binwidth)))
        (unless (or (minusp jx)
                    (>= jx nbins))
          (incf (aref h jx)))))
    (values x h)))

;; -- end of vmath.lisp -- ;;
