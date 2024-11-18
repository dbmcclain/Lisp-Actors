(in-package #:com.ral.vectorized-math)

(defvar *dgesvd-library*
  (translate-logical-pathname
   #+:MAC "PROJECTS:DYLIB;libLispNrUtil.dylib"
   #+:WIN32 "PROJECTS:DYLIB;Lisp_NrUtil.dll"))

(fli:register-module *dgesvd-library*)
#|
(fli:disconnect-module *dgesvd-library*)
|#

(fli:define-foreign-function (lisp_dgesvd
                              "lisp_dgesvd" :source)
    ((a-mat  (:pointer :double))
     (nrow   :int)
     (ncol   :int)
     (u-mat  (:pointer :double)) ;; returned values copied in
     (w-vec  (:pointer :double)) ;; returned values copied in
     (vt-mat (:pointer :double))) ;; returned values copied in
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (lisp_dgesvd_bksb
                              "lisp_dgesvd_bksb" :source)
    ((u-mat  (:pointer :double))
     (w-vec  (:pointer :double))
     (vt-mat (:pointer :double))
     (y-vec  (:pointer :double))
     (x-vec  (:pointer :double)) ;; returned values copied in
     (nrow  :int)
     (ncol  :int)
     (tolerance :double))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (lisp_dgesvd_solve
                              "lisp_dgesvd_solve" :source)
    ((a-mat (:pointer :double))
     (y-vec (:pointer :double))
     (x-vec (:pointer :double)) ;; returned values copied in
     (nrow  :int)
     (ncol  :int)
     (tolerance  :double))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function (lisp_dgesvd_predict
                              "lisp_dgesvd_predict" :source)
    ((v-vec   (:pointer :double))
     (xv-vec  (:pointer :double))
     (nel     :int)
     (ncol    :int)
     (tol     :double)
     (v-pred  (:pointer :double))
     (npred   :int)
     (ll      (:pointer :double))
     (hh      (:pointer :double))
     (sd-pred (:pointer :double)))
  :result-type :int
  :language :ansi-c)

(defun blit-lisp-to-c (l-vec c-vec nel)
  (loop for ix from 0 below nel do
        (setf (fli:dereference c-vec :index ix)
              (coerce (row-major-aref l-vec ix) 'double-float))))

(defun blit-c-to-lisp (c-vec l-vec nel)
  (loop for ix from 0 below nel do
        (setf (row-major-aref l-vec ix)
              (fli:dereference c-vec :index ix))))

(defun dgesvd (arr)
  (destructuring-bind (nrow ncol) (array-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((ca-mat (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems (* nrow ncol)))
	    (cu-mat (fli:allocate-dynamic-foreign-object
		     :type :double
		     :nelems (* nrow ncol)))
            (cw-vec (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems ncol))
            (cv-mat (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems (* ncol ncol))))
        (blit-lisp-to-c arr ca-mat (* nrow ncol))
	(let ((err (lisp_dgesvd ca-mat nrow ncol cu-mat cw-vec cv-mat)))
	  (unless (zerop err)
	    (error "dgesvd: ~A" err)))
        (let ((u-mat (make-array `(,nrow ,ncol)
                                 :element-type 'double-float))
              (w-vec (make-array ncol :element-type 'double-float))
              (v-mat (make-array `(,ncol ,ncol)
                                 :element-type 'double-float)))
          (blit-c-to-lisp ca-mat u-mat (* nrow ncol))
          (blit-c-to-lisp cw-vec w-vec ncol)
          (blit-c-to-lisp cv-mat v-mat (* ncol ncol))
          (list u-mat w-vec v-mat)
          )))
    ))

(defun dgesvd-bksb (u-mat w-vec vt-mat b-vec &key (tolerance 1.0d-8))
  (destructuring-bind (nrow ncol) (array-dimensions u-mat)
    (fli:with-dynamic-foreign-objects ()
      (let ((cu-mat (fli:allocate-dynamic-foreign-object
                     :type :double
                     :nelems (* nrow ncol)))
            (cw-vec (fli:allocate-dynamic-foreign-object
                     :type :double
                     :nelems ncol))
            (cvt-mat (fli:allocate-dynamic-foreign-object
                     :type :double
                     :nelems (* ncol ncol)))
            (cb-vec (fli:allocate-dynamic-foreign-object
                     :type :double
                     :nelems nrow))
            (cx-vec (fli:allocate-dynamic-foreign-object
                     :type :double
                     :nelems ncol)))
        (blit-lisp-to-c w-vec cw-vec ncol)
        (blit-lisp-to-c b-vec cb-vec nrow)
        (blit-lisp-to-c u-mat cu-mat (* nrow ncol))
        (blit-lisp-to-c vt-mat cvt-mat (* ncol ncol))
	(let ((err (lisp_dgesvd_bksb cu-mat cw-vec cvt-mat
				     cb-vec cx-vec nrow ncol
				     (coerce tolerance 'double-float))))
	  (unless (zerop err)
	    (error "dgesvd_bksb: ~A" err)))
        (let ((x-vec (make-array ncol :element-type 'double-float)))
          (blit-c-to-lisp cx-vec x-vec ncol)
          x-vec)))
    ))
        
(defun dgesvd-solve (arr vec &key (tolerance 1.0d-8))
  (destructuring-bind (nrow ncol) (array-dimensions arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((ca-mat (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems (* nrow ncol)))
            (cv-vec (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems nrow))
            (cx-vec (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems ncol)))
        (blit-lisp-to-c arr ca-mat (* nrow ncol))
        (blit-lisp-to-c vec cv-vec nrow)
        (let ((err (lisp_dgesvd_solve ca-mat cv-vec cx-vec nrow ncol
				      (coerce tolerance 'double-float))))
	  (unless (zerop err)
	    (error "dgesvd-solve: ~A" err)))
        (let ((x-vec (make-array ncol :element-type 'double-float)))
          (blit-c-to-lisp cx-vec x-vec ncol)
          x-vec
          )))
    ))

(defun dgesvd-predict (vec xvec ndim npred ll hh &key (tolerance 1.0d-8))
  (let ((nel (min (length vec) (length xvec))))
    (fli:with-dynamic-foreign-objects ()
      (let ((cvec (fli:allocate-dynamic-foreign-object
		   :type :double
		   :nelems nel))
            (cxvec (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems nel))
	    (cpred (fli:allocate-dynamic-foreign-object
		    :type :double
		    :nelems npred))
	    (csd   (fli:allocate-dynamic-foreign-object
		    :type :double
		    :nelems npred))
            (chh   (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems nel))
            (cll   (fli:allocate-dynamic-foreign-object
                    :type :double
                    :nelems nel)))
        (blit-lisp-to-c vec  cvec  nel)
        (blit-lisp-to-c xvec cxvec nel)
        (blit-lisp-to-c hh   chh   nel)
        (blit-lisp-to-c ll   cll   nel)
	(let ((err (lisp_dgesvd_predict cvec cxvec nel ndim
					(coerce tolerance 'double-float)
					cpred npred cll chh csd)))
	  (unless (zerop err)
	    (error "svd-predict: ~A" err)))
        (let ((pred (make-array npred :element-type 'double-float))
	      (sd   (make-array npred :element-type 'double-float)))
          (blit-c-to-lisp cpred pred npred)
	  (blit-c-to-lisp csd   sd   npred)
          (list pred sd)
          )))
    ))

