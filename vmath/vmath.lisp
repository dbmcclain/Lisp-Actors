;; vmath.lisp -- Vectorized math in Lisp
;;
;; DM/MCFA  08/99
;; ---------------------------------------------------------------

(in-package #:com.ral.vectorized-math)

;; ---------------------------------------------------------------

(defun horner (v x)
  ;; accept of sequence of coeffs in increasing degree
  (reduce (lambda (c acc)
            (+ c (* x acc)))
          v
          :initial-value 0.0
          :from-end t))

(defmethod size-of ((v vector))
  ;; returns only the used size of v when v is possibly
  ;; an adjustable array with a larger total size but where
  ;; the fill pointer is smaller.
  (length v))

(defmethod size-of ((a array))
  (array-total-size a))

(defun require-same-shape (&rest args)
  (unless (apply #'equal (mapcar #'array-dimensions args))
    (error "arrays not the same shape")))

(defun require-same-size (&rest args)
  (unless (apply #'= (mapcar #'size-of args))
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

(defgeneric make-overlay-vector (arr &key start end)
  (:method ((vec vector) &key (start 0 start-present-p) (end (length vec) end-present-p))
   (if (or start-present-p end-present-p)
       (make-array (- end start)
                   :element-type (array-element-type vec)
                   :displaced-to vec
                   :displaced-index-offset start)
     ;; else
     vec))
  (:method ((lst list) &key (start 0) (end nil end-present-p))
   (let ((vec (coerce (nthcdr start lst) 'vector)))
     (if end-present-p
         (make-overlay-vector vec :end end)
       vec)))
  (:method ((arr array) &key (start 0) (end (array-total-size arr)))
   (make-array (- end start)
               :displaced-to arr
               :displaced-index-offset start
               :element-type (array-element-type arr)
               )) )

(defun pair-up-gensyms-with-overlay-vectors (gargs args)
  (mapcar #'(lambda (garg arg)
              `(,garg (make-overlay-vector ,arg)))
          gargs args))

(defun make-destination-vector (&rest args)
  (make-array (apply #'min (mapcar #'size-of args))))

#|
;; ---------------------------------------------------------------
;; Macro to encourage the use of inline FPU code sans boxing of
;; intermediate results... The (float 0) optimization appears to have
;; no effect unless stated at the topmost declare level.
;;
#+(and :lispworks :LISPWORKS3)
(defmacro defun-ffp (name args &body body)
  `(lw:top-level-form (defun ,name)
     (defun ,name ,args
       (declare (optimize (speed  3)
                          (safety 0)
                          (debug  0)
                          (float  0)))
       ,@body)))

#+(not :LISPWORKS3) ;; deprecates lw:top-level-form
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
|#

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
    dest
    ))

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
      (multiple-value-bind (argnames argvalues)
          (get-args-from-bindings arg-bindings)
        (let ((gdest (gensym "dest-")))
          `(let* (,@(pair-up argnames argvalues)
                  (,gdest (vectorwise ,argnames ,body-form :index ,index)))
             (make-array (array-dimensions ,(first argnames))
                         :displaced-to ,gdest
                         :element-type (array-element-type ,gdest)))
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

(defun iramp (nel)
  (declare (type fixnum nel))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel
                       :initial-element 0
                       :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) ix))
    ))

(defun framp (nel)
  (declare (type fixnum nel))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel
                       :initial-element 0.0F0
                       :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) (coerce ix 'single-float)))
    ))

(defun dramp (nel)
  (declare (type fixnum nel))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel
                       :initial-element 0.0d0
                       :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) (coerce ix 'double-float)))
    ))

(defun bipolar-framp (nel)
  (declare (type fixnum nel))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) v))
    (do ((ix 0 (1+ ix))
         (val (- (/ (coerce (1- nel) 'single-float) 2.0F0))
              (+ val 1.0F0)))
        ((>= ix nel) v)
      (declare (type fixnum ix)
               (type single-float val))
      (setf (row-major-aref v ix) val))
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mt-random"))

(defun unoise (nel &key (limit 1.0f0) (random-state lw:*mt-random-state*))
  ;; Uniform variates between 0 and LIMIT
  (declare (type fixnum nel))
  (declare (special lw:*mt-random-state*))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel :element-type 'single-float))
        (flimit (coerce limit 'single-float)))
    (declare (type single-float flimit)
             (type (simple-array single-float (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) 
            (the single-float (lw:mt-random flimit random-state))
            ))
    ))

(defvar *v-other* nil)
(defun gasdev (&optional (random-state lw:*mt-random-state*))
  (declare (special lw:*mt-random-state*))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (if *v-other*
      (let ((ans *v-other*))
	(setf *v-other* nil)
	ans)
    (let ((v1 (- (* 2.0F0 
                    (the single-float (lw:mt-random 1.0F0 random-state))
                    ) 1.0F0))
          (v2 (- (* 2.0F0 
                    (the single-float (lw:mt-random 1.0F0 random-state))
                    ) 1.0F0)))
      (declare (type single-float v1 v2))
      (let ((r2 (+ (* v1 v1) (* v2 v2))))
        (declare (type single-float r2))
        (if (and (plusp r2)
                 (< r2 1.0F0))
            (let ((fac 
                   (the single-float (sqrt (/ (* -2.0F0 (the single-float (log r2))) r2)))
                   ))
              (declare (type single-float fac))
              (setf *v-other* (* fac v1))
              (* fac v2))
          (gasdev random-state))
        ))
    ))

(defun gnoise (nel &key (mean 0.0F0) (sd 1.0F0) (random-state lw:*mt-random-state*))
  (declare (type fixnum nel))
  (declare (special lw:*mt-random-state*))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel :element-type 'single-float))
        (fmean (coerce mean 'single-float))
        (fsd   (coerce sd   'single-float)))
    (declare (type (simple-array single-float (*)) v)
             (type single-float fmean fsd))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix)
            (+ fmean (* fsd 
                        (the single-float (gasdev random-state))
                        ))))
    ))

(defun hypot (x y)
  (abs (complex x y)))

(defun sinc (x)
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug 0)
                     (float  0)))
  (if (zerop x)
      1.0F0
    (let ((fx (coerce x 'single-float)))
      (declare (type single-float fx))
      (/ 
       (the single-float (sin fx)) 
       fx))))

(defun logabs (x &optional base)
  (log (abs x) base))

;; ----------------------------------------------
;;
(defmethod vector-of (x)
  (make-array 1 :initial-element x))

(defmethod vector-of ((lst list))
  (coerce lst 'vector))

(defmethod vector-of ((v vector))
  (if (array-has-fill-pointer-p v)
      (subseq v 0 (length v))
    v))

(defmethod vector-of ((arr array))
  (make-array (size-of arr)
              :displaced-to arr
              :element-type (array-element-type arr)))

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
               (apply #'array-row-major-index src offs)
               :element-type (array-element-type src)))
        (vdst (make-array nel
               :displaced-to dst
               :displaced-index-offset
               (apply #'array-row-major-index dst offd)
               :element-type (array-element-type dst))))
    (map-into vdst #'identity vsrc)))


(defun izero (x)
  (declare (ignore x))
  0)

(defun split (arr)
  (if (> (array-rank arr) 1)
      (let* ((dims  (array-dimensions arr))
             (dtail (rest dims))
             (zs    (mapcar #'izero dtail)))
        (map 'vector
             #'(lambda (ix)
                 (make-array dtail
                  :displaced-to arr
                  :displaced-index-offset
                  (apply #'array-row-major-index arr (cons ix zs))
                  :element-type (array-element-type arr)
                  ))
             (iramp (first dims))))
    arr))

(defun foldl (op init v)
  (reduce (lambda (val rslt)
	    (funcall op rslt val))
	  op v :initial-value init))

(defun foldr (op v init)
  (reduce op v :initial-value init :from-end t))

(defun aflat-dimensions (arr)
  (if (arrayp arr)
      (append (array-dimensions arr)
              (aflat-dimensions (row-major-aref arr 0)))))

(defun aflat (arr)
  (if (arrayp (row-major-aref arr 0))
      (foldr #'(lambda (v rslt)
                 (concatenate 'vector v rslt))
             arr #())
    (vector-of arr)))

(defun acanon (arr)
  (let ((new-arr (aflat arr)))
    (make-array (aflat-dimensions arr)
                :displaced-to new-arr
                :element-type (array-element-type new-arr))))

(defun general-outer-prod (op arr1 arr2)
  (let* ((ans (make-array (append (array-dimensions arr1)
                                  (array-dimensions arr2))
                          :element-type (array-element-type arr1)))
         (sz2 (size-of arr2)))
    (elementwise-iter (arr1)
        (elementwise (arr2)
                    (funcall op arr1 arr2)
                    :dest (make-array sz2
                             :displaced-to ans
                             :displaced-index-offset (* ix sz2)
                             :element-type (array-element-type ans)))
        :index ix)
    ans))

(defun outer-prod (arr1 arr2)
  (general-outer-prod #'* arr1 arr2))

(defun fones (nel)
  (make-array nel :initial-element 1.0F0
              :element-type 'single-float))

(defun fzeros (nel)
  (make-array nel :initial-element 0.0F0
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
    (let ((dst (make-array (array-dimensions arr)
                           :element-type (array-element-type arr))))
      (let ((vsrc (make-array (size-of arr)
                              :displaced-to arr
                              :element-type (array-element-type arr)))
            (vdst (make-array (size-of dst)
                              :displaced-to dst
                              :element-type (array-element-type dst)))
            (shifter (make-shifter (array-dimensions arr)
                                   (array-strides arr)
                                   nels)))
        (funcall shifter vsrc 0 vdst 0)
        dst))))

(defun array-strides (arr)
  (reduce #'(lambda (dim rslt)
              (cons (* dim (car rslt))
                    rslt))
          (cdr (array-dimensions arr))
          :initial-value '(1)
          :from-end t))

(defun shifth (arr)
  (shift arr (mapcar
              #'(lambda (x)
                  (truncate x 2))
              (array-dimensions arr))))

(defun transpose (arr)
  (unless (= (array-rank arr) 2)
    (error "Transpose requires a rank 2 array"))
  (destructuring-bind (ny nx) (array-dimensions arr)
    (declare (type fixnum ny nx))
    (let ((ans (make-array `(,nx ,ny)
                           :element-type (array-element-type arr))))
      (loop for iy from 0 below ny do
            (loop for ix from 0 below nx do
                  (setf (aref ans ix iy) (aref arr iy ix))
                  ))
      ans)))
#|
;; this version suffers on large arrays...
;; it tries to pass too many arguments to APPLY
(defun transpose (arr)
  (unless (= (array-rank arr) 2)
    (error "Transpose requires a rank 2 array"))
  (let ((dims (reverse (array-dimensions arr)))
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
  ;; returns a 1D slice along any dimension, from an array of
  ;; arbitrary rank > 0
  (let* ((pre  (subseq ixs 0 dim))
         (post (subseq ixs (1+ dim))))
    (map 'vector
         #'(lambda (ix)
             (apply #'aref arr (append pre (list ix) post)))
         (iramp (array-dimension arr dim)))
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
              :element-type (array-element-type arr)))

(defun copy (arr)
  (make-array (array-dimensions arr)
              :displaced-to
              (map-into (make-array (size-of arr)
                                    :element-type (array-element-type arr))
                        #'identity (vector-of arr))
              :element-type (array-element-type arr)))

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
         (len   (length vec)))
    (when (plusp len)
      (let* ((limit (1- len))
             (ixs   (iramp len))
             (index (round (* limit pc) 100)))
        (values (select-kth vec ixs index 0 limit) index)
        ))))
      
(defun median (arr)
  (percentile arr 50))

(defun mad (arr &optional (med (median arr)))
  (let ((vec (make-overlay-vector arr)))
    (median (vectorwise (vec)
                        (abs (- vec med)))
            )))

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
  (where predicate (make-array (size-of proseq)
                               :displaced-to proseq
                               :element-type (array-element-type proseq))
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
                               :element-type (array-element-type proseq))
                 nil)))
    (when rslt
      (dotimes (ix len rslt)
        (setf (aref rslt ix) (aref proseq (aref where ix)))))
    ))

(defmethod subselect ((proseq array) (where vector))
  (subselect (make-array (size-of proseq)
                         :displaced-to proseq
                         :element-type (array-element-type proseq))
             where)))

;; -----------------------------------------------------------------------

#-:cormanlisp
(defun negmad (arr)
  (let* ((vec  (um:row-major-vector arr))
         (med  (median vec))
         (npix (remove-if (um:rcurry #'>= med) vec))
         #|
         (npix (coerce
                (um:subselect arr
                              (um:where #'plusp
                                        (elementwise (arr)
                                                     (if (< arr med)
                                                         1.0
                                                       0.0))))
                'vector))
         |#
         )
    (mad npix (median npix))))

#+:cormanlisp
(defun negmad (arr)
  (let* ((med  (median arr))
         (npix (coerce
                (subselect arr
                              (where #'plusp
                                        (elementwise (arr)
                                                     (if (< arr med)
                                                         1.0F0
                                                       0.0F0))))
                'vector)))
    (mad npix (median npix))))

(defun standard-percentiles (arr)
  (let* ((v       (vector-of arr))
         (len     (length v))
         (limit   (1- len))
         (ixs     (iramp len))

         (n01     (round limit 100))
         (n02.5   (round (* 2.5 limit) 100))
         (n05     (round (*  5 limit) 100))
         (n10     (round (* 10 limit) 100))
         (n16.7   (round (* 16.7 limit) 100))
         (n25     (round (* 25 limit) 100))
         (n33.3   (round (* limit 33.3) 100))
         (n50     (round (* 50 limit) 100))
         (n66.7   (round (* limit 66.7) 100))
         (n75     (round (* 75 limit) 100))
         (n83.3   (round (* 83.3 limit) 100))
         (n90     (round (* 90 limit) 100))
         (n95     (round (* 95 limit) 100))
         (n97.5   (round (* 97.5 limit) 100))
         (n99     (round (* 99 limit) 100))

         (pc50    (select-kth v ixs n50 0 limit))
         (pc33.3  (select-kth v ixs n33.3 0 n50))
         (pc25    (select-kth v ixs n25 0 n33.3))
         (pc16.7  (select-kth v ixs n16.7 0 n25))
         (pc10    (select-kth v ixs n10 0 n16.7))
         (pc05    (select-kth v ixs n05 0 n10))
         (pc02.5  (select-kth v ixs n02.5 0 n05))
         (pc01    (select-kth v ixs n01 0 n02.5))

         (pc66.7  (select-kth v ixs n66.7 n50 limit))
         (pc75    (select-kth v ixs n75 n66.7 limit))
         (pc83.3  (select-kth v ixs n83.3 n75 limit))
         (pc90    (select-kth v ixs n90 n83.3 limit))
         (pc95    (select-kth v ixs n95 n90 limit))
         (pc97.5  (select-kth v ixs n97.5 n95 limit))
         (pc99    (select-kth v ixs n99 n95 limit)))
    (list
     :pc01   pc01
     :pc02.5 pc02.5
     :pc05   pc05
     :pc10   pc10
     :pc16.7 pc16.7
     :pc25   pc25
     :pc33.3 pc33.3
     :pc50   pc50
     :pc66.7 pc66.7
     :pc75   pc75
     :pc83.3 pc83.3
     :pc90   pc90
     :pc95   pc95
     :pc97.5 pc97.5
     :pc99   pc99)))

(defun percentiles (pcs arr)
  (let* ((v       (vector-of arr))
         (len     (length v))
         (limit   (1- len))
         (ixs     (iramp len)))

    (mapcar #'(lambda (pc)
                (let ((nx (round (* pc limit))))
                  (select-kth v ixs nx 0 limit)))
            pcs)))

(defun ixsort (arr)
  (let ((v (vector-of arr)))
    (sort (iramp (size-of v)) #'<
          :key #'(lambda (ix)
                   (aref v ix)))
    ))

(defun total (arr)
  (elementwise-reduce #'+ arr))

(defun mean (arr)
  (let ((v (vector-of arr)))
    (/ (reduce #'+ v) (size-of v))))

(defun variance (arr &optional (mn (mean arr)))
  (let* ((v (vector-of arr))
         (n (size-of v)))
    (if (> n 1)
        (let ((s (elementwise ((x v))
                              (- x mn))))
          (/ (inner-prod s s)
             (1- n)))
      0)))

(defun stdev (arr &optional (mn (mean arr)))
  (sqrt (variance arr mn)))

(defun wtmean (wts arr)
  (let ((varr (vector-of arr))
        (vwts (vector-of wts)))
    (/ (vsum (vmul vwts varr)) (vsum vwts))
    ))

(defun wtvariance (wts arr &optional (mn (wtmean wts arr)))
  (let ((varr (vector-of arr))
        (vwts (vector-of wts)))
    (/ (vsum (vmul vwts (vsqr (voffset (- mn) varr)))) (vsum wts))
    ))

(defun wtstdev (wts arr &optional (mn (wtmean wts arr)))
  (sqrt (wtvariance wts arr mn)))
  
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
         (binwidth (let ((bw (or binwidth
                                 (/ range nbins))))
                     (if (zerop bw)
                         1
                       bw)))
         (h (make-array (+ 2 nbins)
                        :initial-element 0
                        :element-type    'integer))
         (x (let ((xs     (framp (1+ nbins))))
              (vectorwise (xs)
                          (coerce (+ minv (* binwidth xs))
                                  'single-float)
                          :dest xs))))
    (dotimes (ix  (length v))
      (let* ((val (aref v ix))
             (jx  (round (- val minv) binwidth)))
        (when (<= minv val maxv)
          (incf (aref h jx)))))
    (values x h binwidth)))

;; --------------------------------------------------------------------

#+(AND :MACOSX (OR :X86 :X86-64))
(defun do-without-denormals (fn)
  (let ((savemxcsr (disable-denormals)))
    (unwind-protect
        (funcall fn)
      (restore-denormals savemxcsr))
    ))

#+(AND :MACOSX (OR :X86 :X86-64))
(defmacro without-denormals (&body body)
  `(do-without-denormals (lambda ()
                           ,@body)))

#-(AND :MACOSX (OR :X86 :X86-64))
(defmacro without-denormals (&body body)
  `(progn
     ,@body))

;; -- end of vmath.lisp -- ;;
