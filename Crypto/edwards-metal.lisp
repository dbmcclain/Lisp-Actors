
(in-package :edec)

;; -----------------------------------------------------------------------

(cffi:defcfun ("Ed3363_affine_mul" _Ed3363-affine-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

(cffi:defcfun ("Ed3363_projective_mul" _Ed3363-projective-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

(cffi:defcfun ("Ed3363_projective_add" _Ed3363-projective-add) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8)
  (pt2x :pointer :uint8)
  (pt2y :pointer :uint8)
  (pt2z :pointer :uint8))

(cffi:defcfun ("Ed3363_to_affine" _Ed3363-to-affine) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8))

;; -----------------------------------------------------------------------

(cffi:defcfun ("Curve1174_affine_mul" _Curve1174-affine-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

(cffi:defcfun ("Curve1174_projective_mul" _Curve1174-projective-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

(cffi:defcfun ("Curve1174_projective_add" _Curve1174-projective-add) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8)
  (pt2x :pointer :uint8)
  (pt2y :pointer :uint8)
  (pt2z :pointer :uint8))

(cffi:defcfun ("Curve1174_to_affine" _Curve1174-to-affine) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8))

;; -----------------------------------------------------------------------

(cffi:defcfun ("CurveE521_affine_mul" _CurveE521-affine-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

(cffi:defcfun ("CurveE521_projective_mul" _CurveE521-projective-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

(cffi:defcfun ("CurveE521_projective_add" _CurveE521-projective-add) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8)
  (pt2x :pointer :uint8)
  (pt2y :pointer :uint8)
  (pt2z :pointer :uint8))

(cffi:defcfun ("CurveE521_to_affine" _CurveE521-to-affine) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8))

(cffi:defcfun ("g521_mul" _g521_mul) :void
  (src1 :pointer :uint8)
  (src2 :pointer :uint8)
  (dst  :pointer :uint8))

(cffi:defcfun ("g521_inv" _g521_inv) :void
  (src  :pointer :uint8)
  (dst  :pointer :uint8))

(cffi:defcfun ("g521_sqrt" _g521_sqrt) :bool
  (src  :pointer :uint8)
  (dst  :pointer :uint8))

;; -------------------------------------------------
;; Support for C-level Ed3363 curve

(defun xfer-to-c (val cvec)
  ;; transfer val to C vector in 9 8-byte words
  (declare (integer val))
  (setf val (mod val *ed-q*))
  (setf (cffi:mem-aref cvec :uint64 0.) (ldb (byte 64.   0.) val)
        (cffi:mem-aref cvec :uint64 1.) (ldb (byte 64.  64.) val)
        (cffi:mem-aref cvec :uint64 2.) (ldb (byte 64. 128.) val)
        (cffi:mem-aref cvec :uint64 3.) (ldb (byte 64. 192.) val))
  (when (or (eql *edcurve* *curve-ed3363*)
            (eql *edcurve* *curve-e521f*))
    (setf (cffi:mem-aref cvec :uint64 4.) (ldb (byte 64. 256.) val)
          (cffi:mem-aref cvec :uint64 5.) (ldb (byte 64. 320.) val))
    (when (eql *edcurve* *curve-e521f*)
      (setf (cffi:mem-aref cvec :uint64 6.) (ldb (byte 64. 384.) val)
            (cffi:mem-aref cvec :uint64 7.) (ldb (byte 64. 448.) val)
            (cffi:mem-aref cvec :uint64 8.) (ldb (byte 64. 512.) val)))
    ))

(defun xfer-from-c (cvec)
  ;; retrieve val from C vector in 9 8-byte words
  (let ((v 0))
    (declare (integer v))
    (setf v (dpb (cffi:mem-aref cvec :uint64 0.) (byte 64.   0.) v)
          v (dpb (cffi:mem-aref cvec :uint64 1.) (byte 64.  64.) v)
          v (dpb (cffi:mem-aref cvec :uint64 2.) (byte 64. 128.) v)
          v (dpb (cffi:mem-aref cvec :uint64 3.) (byte 64. 192.) v))
    (when (or (eql *edcurve* *curve-ed3363*)
              (eql *edcurve* *curve-e521f*))
      (setf v (dpb (cffi:mem-aref cvec :uint64 4.) (byte 64. 256.) v)
            v (dpb (cffi:mem-aref cvec :uint64 5.) (byte 64. 320.) v))
      (when (eql *edcurve* *curve-e521f*)
        (setf v (dpb (cffi:mem-aref cvec :uint64 6.) (byte 64. 384.) v)
              v (dpb (cffi:mem-aref cvec :uint64 7.) (byte 64. 448.) v)
              v (dpb (cffi:mem-aref cvec :uint64 8.) (byte 64. 512.) v))
        ))
    #|
      ;; libs now handle conversion properly - DM 03/19
    (assert (and (<= 0 v)
                 (< v *ed-q*)))
    (mod v *ed-q*)
    |#
    v
    ))

(defmacro with-fli-buffers (buffers &body body)
  (if (endp buffers)
      `(progn
         ,@body)
    (destructuring-bind (name &optional lisp-val) (car buffers)
      `(cffi:with-foreign-pointer (,name 72.)
         ,@(when lisp-val
             `((xfer-to-c ,lisp-val ,name)))
         (with-fli-buffers ,(cdr buffers) ,@body))
      )))

#+:LISPWORKS
(editor:setup-indent "with-fli-buffers" 1.)

;; -------------------------------------------------------------------

(defmethod %ecc-fast-mul ((pt ecc-proj-pt) n)
  ;; about a 10% speed penalty over using affine points
  (with-fli-buffers ((cptx (ecc-proj-pt-x pt))  ;; projective in...
                     (cpty (ecc-proj-pt-y pt))
                     (cptz (ecc-proj-pt-z pt))
                     (cwv  n))
    
    (funcall (fast-ed-curve-proj-mul *edcurve*)
             cptx cpty cptz cwv)
    
    (make-ecc-proj-pt
              :x (xfer-from-c cptx) ;; projective out...
              :y (xfer-from-c cpty)
              :z (xfer-from-c cptz))
    ))

(defmethod %ecc-fast-mul ((pt ecc-pt) n)
  (with-fli-buffers ((cptx (ecc-pt-x pt))  ;; affine in...
                     (cpty (ecc-pt-y pt))
                     (cptz)
                     (cwv  n))

    (funcall (fast-ed-curve-affine-mul *edcurve*)
             cptx cpty cptz cwv)
    
    (make-ecc-proj-pt
     :x (xfer-from-c cptx) ;; projective out...
     :y (xfer-from-c cpty)
     :z (xfer-from-c cptz))
    ))

(defmethod %ecc-fast-mul ((pt ecc-cmpr-pt) n)
  (%ecc-fast-mul (ed-projective pt) n))

(defun %ecc-fast-add (pt1 pt2)
  (with-fli-buffers ((cpt1xv (ecc-proj-pt-x pt1)) ;; projective in...
                     (cpt1yv (ecc-proj-pt-y pt1))
                     (cpt1zv (ecc-proj-pt-z pt1))
                     (cpt2xv (ecc-proj-pt-x pt2))
                     (cpt2yv (ecc-proj-pt-y pt2))
                     (cpt2zv (ecc-proj-pt-z pt2)))
    
             (funcall (fast-ed-curve-proj-add *edcurve*)
                      cpt1xv cpt1yv cpt1zv
                      cpt2xv cpt2yv cpt2zv)
    
    (make-ecc-proj-pt ;; projective out...
     :x (xfer-from-c cpt1xv)
     :y (xfer-from-c cpt1yv)
     :z (xfer-from-c cpt1zv))
    ))

(defun %ecc-fast-to-affine (pt)
  (with-fli-buffers ((cptx (ecc-proj-pt-x pt)) ;; projective in...
                     (cpty (ecc-proj-pt-y pt))
                     (cptz (ecc-proj-pt-z pt)))
    
    (funcall (fast-ed-curve-to-affine *edcurve*)
             cptx cpty cptz)
    
    (make-ecc-pt
     :x (xfer-from-c cptx) ;; affine out...
     :y (xfer-from-c cpty))
    ))

(defun %ecc-fast-grp-mul (n1 n2)
  (with-fli-buffers ((cn1  n1)
                     (cn2  n2)
                     (cdst 0))
    (funcall '_g521_mul cn1 cn2 cdst)
    (xfer-from-c cdst)))

(defun %ecc-fast-grp-inv (n)
  (with-fli-buffers ((cn   n)
                     (cdst 0))
    (funcall '_g521_inv cn cdst)
    (xfer-from-c cdst)))

(defun %ecc-fast-grp-sqrt (n)
  (with-fli-buffers ((cn   n)
                     (cdst 0))
    (funcall '_g521_sqrt cn cdst)
    (xfer-from-c cdst)))

#|
(with-ed-curve :curve-e521f
  (with-mod *ed-q*
    (let* ((x    22)
           (xinv (m/ 22)))
      (list
       (m* x xinv)
       (%ecc-fast-grp-mul x xinv)))))

(with-ed-curve :curve-e521f
  (with-mod *ed-q*
    (let* ((x    22)
           (xinv (m/ 22)))
      (list
       xinv
       (%ecc-fast-grp-inv x)))))

(with-ed-curve :curve-e521f
  (with-mod *ed-q*
    (let* ((x    23)
           (xrt (msqrt x)))
      (list
       xrt
       (%ecc-fast-grp-sqrt x)))))

(let* ((pt (with-ed-curve :curve-e521
             (ed-affine (ed-mul *ed-gen* 15))))
       (pt2 (with-ed-curve :curve-e521f
              (ed-affine (ed-mul *ed-gen* 15)))))
  (list pt pt2))
|#
