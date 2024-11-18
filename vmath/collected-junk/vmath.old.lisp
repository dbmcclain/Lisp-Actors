;; vmath.lisp -- Vectorized math in Lisp
;;
;; DM/MCFA  08/99
;; ---------------------------------------------------------------

(defpackage "VECTORIZED-MATH"
  (:use "USEFUL-MACROS" "COMMON-LISP")
  (:nicknames "VMATH" "VM")
  (:export
   "RAMP"
   "BIPOLAR-RAMP"
   "UNOISE"
   "HYPOT"
   "SINC"
   "LOGABS"
   "VECTOR-OF"
   "GASDEV"
   "GNOISE"
   "MEDIAN"
   "MEAN"
   "STDEV"
   "PERCENTILES"
   "COPY"
   "HISTOGRAM"

   "PIXELWISE"
   "VBINOP"
   "VMAP"
   "VREDUCE"

   "ZEROS"
   "ONES"

   "V+"
   "V-"
   "V*"
   "V/"
   "VMOD"
   "VREM"
   "VFLOAT"
   "VEQ?"
   "VNE?"
   "VLT?"
   "VLE?"
   "VGE?"
   "VGT?"
   "VTRUNCATE"
   "VROUND"
   "VFLOOR"
   "VCEILING"
   "VMIN"
   "VMAX"
   "VMIN-OF"
   "VMAX-OF"
   "VCLIP"
   "VSIN"
   "VCOS"
   "VTAN"
   "VSINH"
   "VCOSH"
   "VTANH"
   "VASIN"
   "VACOS"
   "VATAN"
   "VASINH"
   "VACOSH"
   "VATANH"
   "VSQRT"
   "VHYPOT"
   "VABS"
   "VNEG"
   "VINV"
   "VEXPT"
   "VEXP"
   "VLOG"
   "VLOG10"
   "VATAN2"
   "VSINC"
   "VRE"
   "VIM"
   "VCOMPLEX"
   "VLOGABS"
   
   "VMAX-IX"
   "VMIN-IX"
   
   ;"WHERE"
   ;"SELECT"
   
   "VTOTAL"
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
   "SHIFTED-BIPOLAR-RAMP"
   "SHIFTED-BIPOLAR-XPLANE"
   "SHIFTED-BIPLOAR-YPLANE"

   "VECTORWISE"
   "ELEMENTWISE"
   "ELEMENTWISE-REDUCE"
   ))
   
(in-package "VECTORIZED-MATH")

;; ---------------------------------------------------------------
;; elementwise on vectors...
;;
(defmacro vectorwise ((&rest args) body-form &key dest index)
  ;; dest is name of destination variable. If missing of nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (if dest
      (progn
        (unless (every #'symbolp (list* index dest args))
          (error "vectorwise requires symbol arguments"))
        (let ((gix  (or index (gensym))))
          `(do ((,gix (1- (length ,dest)) (1- ,gix)))
               ((minusp ,gix) ,dest)       
             (declare (type fixnum ,gix)
                      (type (vector t *) ,dest ,@args))
             (setf (aref ,dest ,gix)
                   (funcall #'(lambda ,args ,body-form)
                            ,@(mapcar #'(lambda (arg)
                                          `(aref ,arg ,gix))
                                      args)))
             )))
    (let ((gdst (gensym)))
      `(let ((,gdst (make-array (length ,(first args)))))
         (vectorwise ,args ,body-form :dest ,gdst :index ,index)))
    ))

#|
(vectorwise (a b c) (+ a (* b (/ c 2))) :dest tmp)
(vectorwise (a b) (+ a b) :dest (make-array 5))  ;; should create error
(vectorwise (a b c) (+ a (* b (/ c 2))))
(vectorwise (a b c) (+ a (* b (/ c ix))) :index ix)
|#

;; ---------------------------------------------------------------
;; elementwise on N-dimensional arrays...
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensyms-for-args (args)
    (mapcar #'(lambda (arg)
                (declare (ignore arg))
                (gensym))
            args)))

(defmacro elementwise ((&rest args) body-form &key dest index)
  ;; dest is name of destination variable. If missing of nil then
  ;; a new vector is created for the result.
  ;; index is name to use for index so to be accessible in body-form
  (if dest
      (let ((gargs (gensyms-for-args args))
            (gdst  (gensym)))
        `(let ((,gdst (make-array (array-total-size ,dest)
                                  :displaced-to ,dest))
               ,@(mapcar #'(lambda (garg arg)
                             `(,garg (make-array (array-total-size ,arg)
                                                 :displaced-to ,arg)))
                         gargs args))
           (vectorwise ,gargs ,body-form :dest ,gdst :index ,index)
           ,dest))
    (let ((gdst  (gensym))
          (gdstv (gensym)))
      `(let* ((,gdst  (make-array (array-dimensions ,(first args))))
              (,gdstv (make-array (array-total-size ,gdst)
                                  :displaced-to ,gdst)))
         (elementwise ,args ,body-form :dest ,gdstv :index ,index)
         ,gdst))
    ))

(defmacro elementwise-reduce (fn arg &rest keys)
  (let ((garg  (gensym))
        (gargv (gensym)))
    `(let* ((,garg ,arg)
            (,gargv (make-array (array-total-size ,garg)
                                :displaced-to ,garg)))
       (apply #'reduce ,fn ,gargv ,keys))
    ))

;; ---------------------------------------------------------------
;;
(defun ramp (nel)
  (let ((v (make-array nel)))
    (dotimes (ix nel v)
      (setf (svref v ix) ix))))

(defun bipolar-ramp (nel)
  (let ((v (make-array nel)))
    (do ((ix 0 (1+ ix))
         (val (- (/ nel 2)) (1+ val)))
        ((>= ix nel) v)
      (setf (svref v ix) val))))

(defun unoise (nel limit &optional (random-state *random-state*))
  (let ((v (make-array nel)))
    (dotimes (ix nel v)
      (setf (svref v ix) (random limit random-state)))
    ))

(let (v-other
      iset)
  (defun gasdev (&optional (random-state *random-state*))
    (if iset
        (progn
          (setf iset nil)
          v-other)
      (let ((v1 (1- (* 2 (random 1.0 random-state))))
            (v2 (1- (* 2 (random 1.0 random-state)))))
        (let ((r2 (+ (* v1 v1) (* v2 v2))))
          (if (and (plusp r2)
                   (< r2 1.0))
              (let ((fac (sqrt (/ (* -2.0 (log r2)) r2))))
                (setf v-other (* fac v1)
                      iset    t)
                (* fac v2))
            (gasdev random-state))
          ))
      )))

(defun gnoise (nel &optional (random-state *random-state*))
  (let ((v (make-array nel)))
    (dotimes (ix nel v)
      (setf (svref v ix) (gasdev random-state)))))

(defun hypot (x y)
  (abs (complex x y)))

(defun sinc (x)
  (if (zerop x)
      1.0
      (/ (sin x) x)))

;; ----------------------------------------------
;;
(defun vector-of (arr)
  (make-array (array-total-size arr) :displaced-to arr))

#|
(defun elementwise (fn &rest arrs)
  (apply #'map 'vector fn (mapcar #'vector-of arrs)))

(defun pixelwise (fn &rest arrs)
  (make-array (array-dimensions (car arrs))
              :displaced-to
              (apply #'elementwise fn arrs)))
|#
#|
(pixelwise #'(lambda (a b c)
               (/ (+ a b) c))
           arr1 arr2 arr3)
(pixelwise #'+ arr1 arr2)
(pixelwise #'sinc arr)
(pixelwise #'(lambda (a b c d)
               (list a b c d))
           arr1 arr2 arr3 arr4)
|#
#|
(defmethod vbinop (op (arr array) (k number))
  (pixelwise #'(lambda (x)
                 (funcall op x k))
             arr))

(defmethod vbinop (op (k number) (arr array))
  (pixelwise #'(lambda (x)
                 (funcall op k x))
             arr))

(defmethod vbinop (op (arr1 array) (arr2 array))
  (pixelwise #'(lambda (a b)
                 (funcall op a b))
             arr1 arr2))

(defmethod vbinop (op (v1 vector) (v2 vector))
  (map 'vector op v1 v2))

(defmethod vbinop (op (v vector) (k number))
  (map 'vector #'(lambda (x)
                   (funcall op x k))
       v))

(defmethod vbinop (op (k number) (v vector))
  (map 'vector #'(lambda (x)
                   (funcall op k x))
       v))

(defmethod vbinop (op (n1 number) (n2 number))
  (funcall op n1 n2))

(defmethod vmap (op (arr array))
  (pixelwise #'(lambda (a)
                 (funcall op a))
             arr))

(defmethod vmap (op (v vector))
  (map 'vector op v))

(defmethod vmap (op (n number))
  (funcall op n))

(defmethod vreduce (op (arr array))
  (reduce op (vector-of arr)))

(defmethod vreduce (op (v vector))
  (reduce op v))
|#

;; ---------------------------------------------
#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun numeric-boolean (op)
    #'(lambda (a b)
        (if (funcall op a b) 1 0)))
  
  (dolist (pair
           '((eq?  . =)
             (ne?  . /=)
             (lt?  . <)
             (le?  . <=)
             (ge?  . >=)
             (gt?  . >)))
    (setf (symbol-function (first pair))
          (numeric-boolean (symbol-function (rest pair))))
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vbinary (op)
    #'(lambda (a b)
        (vbinop op a b)))
  
  (dolist (pair
           '((v+        . +)
             (v-        . -)
             (v*        . *)
             (v/        . /)
             (vlt?      . lt?)
             (vle?      . le?)
             (vge?      . ge?)
             (vgt?      . gt?)
             (veq?      . eq?)
             (vne?      . ne?)
             (vmax      . max)
             (vmin      . min)
             (vatan2    . atan)
             (vhypot    . hypot)
             (vexpt     . expt)
             (vmod      . mod)
             (vrem      . rem)
             (vcomplex  . complex)))           
    (setf (symbol-function (first pair))
          (vbinary (symbol-function (rest pair))))
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vrounder (op)
    #'(lambda (v &optional (dvsr 1))
        (vmap #'(lambda (x)
                  (funcall op x dvsr))
              v)))

  (dolist (pair
           '((vround    . round)
             (vtruncate . truncate)
             (vceiling  . ceiling)
             (vfloor    . floor)))
    (setf (symbol-function (first pair))
          (vrounder (symbol-function (rest pair))))
    ))
  
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vunary (op)
    #'(lambda (v)
        (vmap op v)))
  
  (dolist (pair
           '((vabs   . abs)
             (vneg   . -)
             (vinv   . /)
             (vsin   . sin)
             (vcos   . cos)
             (vtan   . tan)
             (vasin  . asin)
             (vacos  . acos)
             (vatan  . atan)
             (vsinh  . sinh)
             (vcosh  . cosh)
             (vtanh  . tanh)
             (vasinh . asinh)
             (vacosh . acosh)
             (vatanh . atanh)
             (vexp   . exp)
             (vsqrt  . sqrt)
             (vfloat . float)
             (vsinc  . sinc)
             (vcis   . cis)
             (vphase . phase)
             (vre    . realpart)
             (vim    . imagpart)
             ))
    (setf (symbol-function (first pair))
        (vunary (symbol-function (rest pair))))
    ))
|#

(defun logabs (x &optional base)
  (log (abs x) base))

#|
(defun vlogabs (arr &optional base)
  (pixelwise #'(lambda (x)
                 (logabs x base))
             arr))

(defun vlog (v &optional base)
  (vmap #'(lambda (x)
            (log x base))
        v))

(defun vlog10 (v)
  (vlog v 10))

(defun vmax-of (arr)
  (vreduce #'max arr))

(defun vmin-of (arr)
  (vreduce #'min arr))

(defun vclip (arr lo hi)
  (vmap #'(lambda (x)
            (cond ((< x lo) lo)
                  ((> x hi) hi)
                  (t x)))
        arr))

(defun vminmax-ix (test arr)
  (let ((max-ix 0)
	(max-val (row-major-aref arr 0)))
    (dotimes (ix (array-total-size arr) (values max-ix max-val))
      (let ((v (row-major-aref arr ix)))
	(if (funcall test v max-val)
	    (setf max-ix ix
		  max-val v))
	))
    ))
  
(defun vmax-ix (arr)
  (vminmax-ix #'> arr))

(defun vmin-ix (arr)
  (vminmax-ix #'< arr))
|#

;; -----------------------------------------------------
  
;; -----------------------------------------------------
#|
(defmethod where ((arr array))
  (v- (delete-if #'zerop
                 (v* (vector-of arr)
                     (v+ 1
                         (ramp (array-total-size arr)))
                     ))
      1))

(defmethod where ((v vector))
  (v- (delete-if #'zerop
                 (v* v
                     (v+ 1
                         (ramp (length v)))))
      1))

(defun select (arr ixs)
  (let ((v (vector-of arr)))
    (map 'vector
         #'(lambda (ix)
             (aref v ix))
         ixs)))
|#
#|
(defun vtotal (arr)
  (vreduce #'+ arr))
|#
(defun general-inner-prod (inner-op arr1 arr2 outer-op)
  (vreduce outer-op (funcall inner-op arr1 arr2)))

(defun inner-prod (arr1 arr2)
  (general-inner-prod #'* arr1 arr2 #'+))

(defun blit-array (src offs dst offd nel)
  (let ((vsrc (make-array nel
               :displaced-to src
               :displaced-index-offset
               (apply #'array-row-major-index src offs)))
        (vdst (make-array nel
               :displaced-to dst
               :displaced-index-offset
               (apply #'array-row-major-index dst offd))))
    (map-into vdst #'identity vsrc)))


(defun zero (x)
  (declare (ignore x))
  0)

(defun split (arr)
  (if (> (array-rank arr) 1)
      (let* ((dims  (array-dimensions arr))
             (dtail (rest dims))
             (zs    (mapcar #'zero dtail)))
        (map 'vector
             #'(lambda (ix)
                 (make-array
                  dtail
                  :displaced-to arr
                  :displaced-index-offset
                  (apply #'array-row-major-index arr (cons ix zs))
                  ))
             (ramp (first dims))))
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
  (make-array (aflat-dimensions arr)
              :displaced-to (aflat arr)))

(defun general-outer-prod (op arr1 arr2)
  (make-array (list (length arr1) (length arr2))
              :initial-contents
              (pixelwise #'(lambda (a)
                             (vbinop op a arr2))
                         arr1)))

(defun outer-prod (arr1 arr2)
  (general-outer-prod #'* arr1 arr2))

(defun ones (nel)
  (make-array nel :initial-element 1))

(defun zeros (nel)
  (make-array nel :initial-element 0))

(defun xplane (ny nx)
  (outer-prod (ones ny) (ramp nx)))

(defun yplane (ny nx)
  (outer-prod (ramp ny) (ones nx)))

(defun bipolar-xplane (ny nx)
  (outer-prod (ones ny) (bipolar-ramp nx)))

(defun bipolar-yplane (ny nx)
  (outer-prod (bipolar-ramp ny) (ones nx)))

(defun shifted-bipolar-ramp (nel)
  (vshifth (bipolar-ramp nel)))

(defun shifted-bipolar-xplane (ny nx)
  (outer-prod (ones ny) (shifted-bipolar-ramp nx)))

(defun shifted-bipolar-yplane (ny nx)
  (outer-prod (shifted-bipolar-ramp ny) (ones nx)))

(defun dist (ny nx)
  (vbinop #'hypot (shifted-bipolar-xplane nx ny)
          (shifted-bipolar-yplane nx ny)))

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

#|
(defun internal-ashift (arr nels)
  (let* ((tl  (rest nels))
         (nel (first nels))
         (sharr (if tl
                    (map 'vector #'(lambda (sub)
                                     (internal-ashift sub tl))
                         (split arr))
                  arr)))
    (if (minusp nel)
        (vrotl sharr (- nel))
      (vrotr sharr nel))
    ))

(defun ashift (arr nels)
  (acanon (internal-ashift arr nels)))

(defun ashifth (arr)
  (ashift arr (mapcar #'(lambda (x)
                          (- (truncate x 2)))
                      (array-dimensions arr))))
|#

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
    (let ((dst (make-array (array-dimensions arr))))
      (let ((vsrc (make-array (array-total-size arr)
                              :displaced-to arr))
            (vdst (make-array (array-total-size dst)
                              :displaced-to dst))
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
  (if (= (array-rank arr) 2)
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
                     (ramp (first dims)))
                )))
    arr))

(defun slice (arr dim &rest ixs)
  (let* ((pre  (subseq ixs 0 dim))
         (post (subseq ixs (1+ dim))))
    (map 'vector
         #'(lambda (ix)
             (apply #'aref arr (append pre (list ix) post)))
         (ramp (array-dimension arr dim)))
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
              :displaced-to arr))

(defun copy (arr)
  (make-array (array-dimensions arr)
              :displaced-to
              (map 'vector #'identity (vector-of arr))))

(defun median (arr &key nocopy)
  (elt (percentiles arr '(50) :nocopy nocopy) 0))

(defun percentiles (arr pcs &key nocopy)
  (let* ((ans (sort (vector-of (if nocopy arr (copy arr))) #'<))
         (nel (array-total-size arr)))
    (map 'vector #'(lambda (pc)
                     (aref ans (truncate (* pc nel) 100)))
         pcs)))

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
                     (vmin-of v))))
         (maxv (if range
                   (elt range 1)
                 (or max
                     (vmax-of v))))
         (range (- maxv minv))
         (nbins (or nbins
                    (and binwidth
                         (truncate range binwidth))
                    200))
         (binwidth (or binwidth
                       (/ range nbins)))
         (h (make-array nbins :initial-element 0))
         (x (v+ (+ minv (/ binwidth 2)) (v* binwidth (ramp nbins)))))
    (dotimes (ix (length v))
      (let ((jx (round (- (aref v ix) minv) binwidth)))
        (unless (or (minusp jx)
                    (>= jx nbins))
          (incf (aref h jx)))))
    (values x h)))

