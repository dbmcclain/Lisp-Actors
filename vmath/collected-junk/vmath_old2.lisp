;; vmath.lisp -- Vectorized math in Lisp
;;
;; DM/MCFA  08/99
;; ---------------------------------------------------------------

(defpackage "VECTORIZED-MATH"
  (:use "USEFUL-MACROS" "COMMON-LISP")
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
(defun require-same-shape (&rest args)
  (unless (reduce #'equal (mapcar #'array-dimensions args))
    (error "arrays not the same shape")))

(defun require-same-size (&rest args)
  (unless (reduce #'= (mapcar #'array-total-size args))
    (error "arrays not the same size")))

(defun pair-up (&rest args)
  (apply #'mapcar #'(lambda (&rest args)
                      args)
         args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensyms-for-args (args &optional pref)
    (mapcar #'(lambda (arg)
                (declare (ignore arg))
                (if pref
                    (gensym pref)
                  (gensym)))
            args)))

(defun make-overlay-vector (arr)
  (if (vectorp arr)
      arr
    (make-array (array-total-size arr)
                :displaced-to arr
                :element-type (array-element-type arr))))

(defun pair-up-gensyms-with-overlay-vectors (gargs args)
  (mapcar #'(lambda (garg arg)
              `(,garg (make-overlay-vector ,arg)))
          gargs args))

(defun make-destination-vector (&rest args)
  (make-array (apply #'min (mapcar #'array-total-length args))))

;; ---------------------------------------------------------------
;; Macro to encourage the use of inline FPU code sans boxing of
;; intermediate results... The (float 0) optimization appears to have
;; no effect unless stated at the topmost declare level.
;;
(defmacro defun-ffp (name args &body body)
  `(defun ,name ,args
     (declare (optimize (speed  3)
                        (safety 0)
                        (debug  0)
                        (float  0)))
     ,@body))

;; ---------------------------------------------------------------
;; elementwise on vectors...
;;
#|
(defmacro vectorwise ((&rest args) body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (if dest
      (progn
        (unless (every #'symbolp (cons index args))
          (error "vectorwise requires symbol arguments"))
        (let ((gix  (or index (gensym)))
              (gdst (if (symbolp dest) dest (gensym))))
          `(do* (,@(unless (eq gdst dest)
                     `((,gdst ,dest)))
                 (,gix (1- (length ,gdst)) (1- ,gix)))
               ((minusp ,gix) ,gdst)
             (declare (type fixnum ,gix)
                      (type (vector t (*)) ,gdst ,@args))
             (setf (aref ,gdst ,gix)
                   (funcall #'(lambda ,args ,body-form)
                            ,@(mapcar #'(lambda (arg)
                                          `(aref ,arg ,gix))
                                      args)))
             )))
    `(vectorwise ,args ,body-form
                 :dest (make-array (length ,(first args))
                                   :element-type (array-element-type ,(first args)))
                 :index ,index)
    ))

(defmacro vectorwise-iter ((&rest args) body-form &key index)
  ;; Iterate a function over corresponding elements of the argument arrays.
  ;; No result is stored here, although the function might cause its own
  ;; side effects...
  ;; index is name to use for index so to be accessible in body-form
  (unless (every #'symbolp (cons index args))
    (error "vectorwise-iter requires symbol arguments"))
  (let ((gix  (or index (gensym))))
    `(do ((,gix (1- (length ,(first args))) (1- ,gix)))
         ((minusp ,gix))
       (declare (type fixnum ,gix)
                (type (vector t (*)) ,@args))
       (funcall #'(lambda ,args ,body-form)
                ,@(mapcar #'(lambda (arg)
                              `(aref ,arg ,gix))
                          args)))
       ))
|#
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
    (values argnames argvalues)))

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

(defmacro vectorwise (arg-bindings body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
    (if index
        (progn
          (unless (symbolp index)
            (error "vectorwise requires symbolic index argument"))
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
          (unless (symbolp index)
            (error "vectorwise-iter requires symbolic index arguments"))
          `(indexed-null-vwise #'(lambda (,index ,@argnames) ,body-form)
                               ,@argvalues))
      `(null-vwise #'(lambda ,argnames ,body-form) ,@argvalues))
    ))

#|
(defmacro vectorwise (arg-bindings body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
    (if index
        (progn
          (unless (symbolp index)
            (error "vectorwise requires symbolic index argument"))
          (if dest
              (let ((gdest   (gensym "dest-"))
                    (gdestov (gensym "dest-overlay-")))
                `(let* (,@(pair-up-gensyms-with-overlay-vectors argnames argvalues)
                        (,gdest   ,dest)
                        (,gdestov (make-overlay-vector ,gdest)))
                   (declare (type (vector t (*)) ,gdestov ,@argnames))
                   (dotimes (,index (length ,gdestov) ,gdest)
                     (setf (aref ,gdestov ,index)
                           (funcall #'(lambda ,argnames ,body-form)
                                    ,@(mapcar #'(lambda (arg)
                                                  `(aref ,arg ,index))
                                              argnames)))
                     )))
            (let ((gdest (gensym "dest-")))
              `(let* (,@(pair-up-gensyms-with-overlay-vectors argnames argvalues)
                      (,gdest  (make-destination-vector ,@argnames)))
                 (dotimes (,index (length ,gdest) ,gdest)
                   (declare (type (vector t (*)) ,gdest ,@argnames))
                   (setf (aref ,gdest ,index)
                         (funcall #'(lambda ,argnames ,body-form)
                                  ,@(mapcar #'(lambda (arg)
                                                `(aref ,arg ,index))
                                            argnames)))
                   ))
              )))
      (if dest
          (let ((gdest   (gensym "dest-"))
                (gdestov (gensym "dest-overlay-")))
            `(let* (,@(pair-up-gensyms-with-overlay-vectors argnames argvalues)
                    (,gdest   ,dest)
                    (,gdestov (make-overlay-vector ,gdest)))
               (declare (type (vector t (*)) ,gdestov ,@argnames))
               (map-into ,gdestov
                         #'(lambda ,argnames ,body-form)
                         ,@argnames)
               ,gdest))
        `(let (,@(pair-up-gensyms-with-overlay-vectors argnames argvalues))
           (declare (type (vector t (*)) ,@argnames))
           (map 'vector
                #'(lambda ,argnames ,body-form)
                ,@argnames))
        ))
    ))

(defmacro vectorwise-iter (arg-bindings body-form &key index)
  ;; Iterate a function over corresponding elements of the argument arrays.
  ;; No result is stored here, although the function might cause its own
  ;; side effects...
  ;; index is name to use for index so to be accessible in body-form
  (multiple-value-bind (argnames argvalues) (get-args-from-bindings arg-bindings)
    (if index
        (progn
          (unless (symbolp index)
            (error "vectorwise-iter requires symbolic index arguments"))
          `(let (,@(pair-up-gensyms-with-overlay-vectors argnames argvalues))
             (declare (type (vector t (*)) ,@argnames))
             (dotimes (,index (length ,(first argnames)))
               (funcall #'(lambda ,argnames ,body-form)
                        ,@(mapcar #'(lambda (arg)
                                      `(aref ,arg ,index))
                                  argnames)))
             ))
      `(let (,@(pair-up-gensyms-with-overlay-vectors argnames argvalues))
         (declare (type (vector t (*)) ,@argnames))
         (map nil #'(lambda ,argnames ,body-form) ,@argnames))
      )))
|#

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
#|
(defmacro elementwise ((&rest args) body-form &key dest index)
  ;; dest is name of destination variable. If missing or nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (if dest
      (let ((gargs  (gensyms-for-args args))
            (gdstin (if (symbolp dest) dest (gensym)))
            (gdst   (gensym)))
        `(let* (,@(unless (eq gdstin dest)
                    `((,gdstin ,dest)))
                (,gdst (make-array (array-total-size ,gdstin)
                                   :displaced-to ,gdstin
                                   :element-type (array-element-type ,gdstin)))
                ,@(mapcar
                   #'(lambda (garg arg)
                       `(,garg (make-array (array-total-size ,arg)
                                           :displaced-to ,arg
                                           :element-type (array-element-type ,arg))))
                   gargs args))
           (funcall #'(lambda ,args
                        (vectorwise ,args ,body-form
                                    :dest ,gdst
                                    :index ,index))
                    ,@gargs)
           ,gdstin))
    `(elementwise ,args ,body-form
                  :dest (make-array (array-dimensions ,(first args))
                                    :element-type (array-element-type ,(first args)))
                  :index ,index)
    ))

(defmacro elementwise-iter ((&rest args) body-form &key index)
  (let ((gargs (gensyms-for-args args)))
    `(let ,(mapcar #'(lambda (garg arg)
                       `(,garg (make-array (array-total-size ,arg)
                                           :displaced-to ,arg
                                           :element-type (array-element-type ,arg))))
                   gargs args)
       (funcall #'(lambda ,args
                    (vectorwise-iter ,args ,body-form
                                     :index ,index))
                ,@gargs))
    ))

(defmacro elementwise-reduce (fn arg &rest keys)
  (let ((garg  (gensym))
        (gargv (gensym)))
    `(let* ((,garg ,arg)
            (,gargv (make-array (array-total-size ,garg)
                                :displaced-to ,garg
                                :element-type (array-element-type ,garg))))
       (apply #'reduce ,fn ,gargv ,keys))
    ))
|#
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
(defun-ffp iramp (nel)
  (declare (type fixnum nel))
  (let ((v (make-array nel
                       :initial-element 0
                       :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) ix))
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
      (setf (row-major-aref v ix) (coerce ix 'single-float)))
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
      (setf (row-major-aref v ix) (coerce ix 'double-float)))
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
      (setf (row-major-aref v ix) val))
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
      (setf (row-major-aref v ix) (the float (random flimit random-state))))
    ))

(defvar *v-other* nil)
(defun-ffp gasdev (&optional (random-state *random-state*))
  (if *v-other*
      (let ((ans *v-other*))
	(setf *v-other* nil)
	ans)
      (let ((v1 (- (* 2.0 (the float (random 1.0 random-state))) 1.0))
            (v2 (- (* 2.0 (the float (random 1.0 random-state))) 1.0)))
        (declare (type float v1 v2))
        (let ((r2 (+ (* v1 v1) (* v2 v2))))
          (declare (type float r2))
          (if (and (plusp r2)
                   (< r2 1.0))
              (let ((fac (the float (sqrt (/ (* -2.0 (the float (log r2))) r2)))))
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
      (setf (row-major-aref v ix)
            (+ fmean (* fsd (the float (gasdev random-state))))))
    ))

(defun hypot (x y)
  (abs (complex x y)))
  
(defun-ffp sinc (x)
  (if (zerop x)
      1.0
    (let ((fx (coerce x 'float)))
      (declare (type float fx))
      (/ (the float (sin fx)) fx))))

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
  (make-array (array-total-size arr)
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
  (reduce op v :initial-value init))

(defun foldr (op init v)
  (reduce op v :initial-value init :from-end t))

(defun aflat-dimensions (arr)
  (if (arrayp arr)
      (append (array-dimensions arr)
              (aflat-dimensions (row-major-aref arr 0)))))

(defun aflat (arr)
  (if (arrayp (row-major-aref arr 0))
      (foldr #'(lambda (v rslt)
                 (concatenate 'vector v rslt))
             #() arr)
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
         (sz2 (array-total-size arr2)))
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

(defun vshifth (vec)
  (vrotl vec (truncate (length vec) 2)))

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
      (let ((vsrc (make-array (array-total-size arr)
                              :displaced-to arr
                              :element-type (array-element-type arr)))
            (vdst (make-array (array-total-size dst)
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

(defun slice (arr dim &rest ixs)
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

(map-dimension #'vtotal d 0)

; -- same as --

(reduce-dimension #'v+ d 1) ; -> like IDL: total(d,0)

;;
(reduce-dimension #'v+ d 0) ; -> like IDL: total(d,1)

|#

(defun reshape (arr dims)
  (make-array dims
              :displaced-to arr
              :element-type (array-element-type arr)))

(defun copy (arr)
  (make-array (array-dimensions arr)
              :displaced-to
              (map-into (make-array (array-total-size arr)
                                    :element-type (array-element-type arr))
                        #'identity (vector-of arr))
              :element-type (array-element-type arr)))

(defun select-kth (vec ixs k left right)
  (declare (optimize (debug 1) (speed 3) (safety 0))
           (type fixnum k left right)
           (type (array fixnum 1) ixs)
           (type (array * 1) arr))
  (labels ((ind-aref (ix)
                     (declare (type fixnum ix))
                     (aref vec (the fixnum (aref ixs ix)))))
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

(defun mad (arr med)
  (median (elementwise (arr)
                       (abs (- arr med)))
          ))

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
    (sort (iramp (array-total-size v)) #'<
          :key #'(lambda (ix)
                   (aref v ix)))
    ))

(defun mean (arr)
  (/ (reduce #'+ (vector-of arr)) (array-total-size arr)))

(defun variance (arr &optional (mn (mean arr)))
  (labels ((square (x) (* x x)))
    (/ (reduce #'(lambda (ans x)
                   (+ ans (square (- x mn))))
               (vector-of arr))
       (1- (array-total-size arr)))))

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

