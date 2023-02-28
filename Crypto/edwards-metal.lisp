
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

;; -------------------------------------------------
;; Support for C-level Ed3363 curve

(defun xfer-to-c (val cvec)
  ;; transfer val to C vector in 6 8-byte words
  (declare (integer val))
  (setf val (mod val *ed-q*))
  (setf (cffi:mem-aref cvec :uint64 0.) (ldb (byte 64.   0.) val)
        (cffi:mem-aref cvec :uint64 1.) (ldb (byte 64.  64.) val)
        (cffi:mem-aref cvec :uint64 2.) (ldb (byte 64. 128.) val)
        (cffi:mem-aref cvec :uint64 3.) (ldb (byte 64. 192.) val))
  (when (eql *edcurve* *curve-ed3363*)
    (setf (cffi:mem-aref cvec :uint64 4.) (ldb (byte 64. 256.) val)
          (cffi:mem-aref cvec :uint64 5.) (ldb (byte 64. 320.) val))
    ))

(defun xfer-from-c (cvec)
  ;; retrieve val from C vector in 6 8-byte words
  (let ((v 0))
    (declare (integer v))
    (setf v (dpb (cffi:mem-aref cvec :uint64 0.) (byte 64.   0.) v)
          v (dpb (cffi:mem-aref cvec :uint64 1.) (byte 64.  64.) v)
          v (dpb (cffi:mem-aref cvec :uint64 2.) (byte 64. 128.) v)
          v (dpb (cffi:mem-aref cvec :uint64 3.) (byte 64. 192.) v))
    (when (eql *edcurve* *curve-ed3363*)
      (setf v (dpb (cffi:mem-aref cvec :uint64 4.) (byte 64. 256.) v)
            v (dpb (cffi:mem-aref cvec :uint64 5.) (byte 64. 320.) v)))
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
      `(cffi:with-foreign-pointer (,name 48.)
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

