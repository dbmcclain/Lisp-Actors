
(in-package :edec-ff)

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
            (eql *edcurve* *curve-e521*))
    (setf (cffi:mem-aref cvec :uint64 4.) (ldb (byte 64. 256.) val)
          (cffi:mem-aref cvec :uint64 5.) (ldb (byte 64. 320.) val))
    (when (eql *edcurve* *curve-e521*)
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
              (eql *edcurve* *curve-e521*))
      (setf v (dpb (cffi:mem-aref cvec :uint64 4.) (byte 64. 256.) v)
            v (dpb (cffi:mem-aref cvec :uint64 5.) (byte 64. 320.) v))
      (when (eql *edcurve* *curve-e521*)
        (setf v (dpb (cffi:mem-aref cvec :uint64 6.) (byte 64. 384.) v)
              v (dpb (cffi:mem-aref cvec :uint64 7.) (byte 64. 448.) v)
              v (dpb (cffi:mem-aref cvec :uint64 8.) (byte 64. 512.) v))
        ))
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
  (with-embedding-field
    (with-fli-buffers ((cptx (normalize (ecc-proj-pt-x pt)))  ;; projective in...
                       (cpty (normalize (ecc-proj-pt-y pt)))
                       (cptz (normalize (ecc-proj-pt-z pt)))
                       (cwv  (with-curve-field
                               (normalize n))))
      
      (funcall (fast-ed-curve-proj-mul *edcurve*)
               cptx cpty cptz cwv)
      
      (make-ecc-proj-pt
       :x (xfer-from-c cptx) ;; projective out...
       :y (xfer-from-c cpty)
       :z (xfer-from-c cptz))
      )))

(defmethod %ecc-fast-mul ((pt ecc-pt) n)
  (with-embedding-field
    (with-fli-buffers ((cptx (normalize (ecc-pt-x pt)))  ;; affine in...
                       (cpty (normalize (ecc-pt-y pt)))
                       (cptz)
                       (cwv  (with-curve-field
                               (normalize n))))
      
      (funcall (fast-ed-curve-affine-mul *edcurve*)
               cptx cpty cptz cwv)
      
      (make-ecc-proj-pt
       :x (xfer-from-c cptx) ;; projective out...
       :y (xfer-from-c cpty)
       :z (xfer-from-c cptz))
      )))

(defmethod %ecc-fast-mul ((pt ecc-cmpr-pt) n)
  (%ecc-fast-mul (ed-projective pt) n))

(defun %ecc-fast-add (pt1 pt2)
  (with-embedding-field
    (with-fli-buffers ((cpt1xv (normalize (ecc-proj-pt-x pt1))) ;; projective in...
                       (cpt1yv (normalize (ecc-proj-pt-y pt1)))
                       (cpt1zv (normalize (ecc-proj-pt-z pt1)))
                       (cpt2xv (normalize (ecc-proj-pt-x pt2)))
                       (cpt2yv (normalize (ecc-proj-pt-y pt2)))
                       (cpt2zv (normalize (ecc-proj-pt-z pt2))))
      
      (funcall (fast-ed-curve-proj-add *edcurve*)
               cpt1xv cpt1yv cpt1zv
               cpt2xv cpt2yv cpt2zv)
      
      (make-ecc-proj-pt ;; projective out...
                        :x (xfer-from-c cpt1xv)
                        :y (xfer-from-c cpt1yv)
                        :z (xfer-from-c cpt1zv))
      )))

(defun %ecc-fast-to-affine (pt)
  (with-embedding-field
    (with-fli-buffers ((cptx (normalize (ecc-proj-pt-x pt))) ;; projective in...
                       (cpty (normalize (ecc-proj-pt-y pt)))
                       (cptz (normalize (ecc-proj-pt-z pt))))
      
      (funcall (fast-ed-curve-to-affine *edcurve*)
               cptx cpty cptz)
      
      (make-ecc-pt
       :x (xfer-from-c cptx) ;; affine out...
       :y (xfer-from-c cpty))
      )))

;; ---------------------------------------------------

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
(with-ed-curve :curve-e521
  (with-mod *ed-q*
    (let* ((x    22)
           (xinv (m/ 22)))
      (list
       (m* x xinv)
       (%ecc-fast-grp-mul x xinv)))))

(with-ed-curve :curve-e521
  (with-mod *ed-q*
    (let* ((x    22)
           (xinv (m/ 22)))
      (list
       xinv
       (%ecc-fast-grp-inv x)))))

(with-ed-curve :curve-e521
  (with-mod *ed-q*
    (let* ((x    23)
           (xrt (msqrt x)))
      (list
       xrt
       (%ecc-fast-grp-sqrt x)))))

(let* ((pt (with-ed-curve :curve-e521
             (ed-affine (ed-mul *ed-gen* 15))))
       (pt2 (with-ed-curve :curve-e521
              (ed-affine (ed-mul *ed-gen* 15)))))
  (list pt pt2))

(with-ed-curve :curve-e521
  (compute-deterministic-elligator-skey :test (random *ed-r*)))

(defun tst ()
 (with-ed-curve :curve-e521 ;; 5.6x faster than :curve-e521
   (let ((h  (hash/256 :hello (uuid:make-v1-uuid))))
     (loop for ix from 0 below 1000 do
             (compute-deterministic-elligator-skey :test h)
             (setf h (hash/256 h))
           ))))
(time (tst))

(defun tst ()
 (with-ed-curve :curve-e521 ;; 5.6x faster than :curve-e521
   (loop for ix from 0 below 1000 do
           (compute-deterministic-elligator-skey :test (random *ed-r*))
           )))
(time (tst))

(defun tst ()
  (loop for ix from 0 below 1000 do
        (let* ((r (random *ed-r*))
               (pt1 (with-ed-curve :curve-e521
                      (ed-nth-pt r)))
               (pt2 (with-ed-curve :curve-e521
                      (ed-nth-pt r))))
          (assert (ed-pt= pt1 pt2))
          )))
(time (tst))

;; --------------------------------------------

(defun tst (niter)
  (with-ed-curve :curve-e521
    (loop repeat niter do
          (let* ((r1 (random *ed-q*))
                 (r2 (random *ed-q*))
                 (p1 (with-mod *ed-q*
                       (m* r1 r2)))
                 (p2 (%ecc-fast-grp-mul r1 r2)))
            (assert (= p1 p2))
            ))))
(time (tst 1_000_000)))

(defun tst (niter)
  (with-ed-curve :curve-e521
    (loop repeat niter do
          (let* ((r1 (random *ed-q*))
                 (p1 (with-mod *ed-q*
                       (m/ r1)))
                 (p2 (%ecc-fast-grp-inv r1)))
            (assert (= p1 p2))
            ))))
(time (tst 100_000)))

(defun tst (niter)
  (with-ed-curve :curve-e521
    (loop repeat niter do
          (let* ((r1 (random *ed-q*))
                 (p1 (with-mod *ed-q*
                       (ignore-errors
                         (msqrt r1))))
                 (p2 (when p1
                       (%ecc-fast-grp-sqrt r1))))
            (when p1
              (assert (= p1 p2)))
            ))))
(time (tst 10_000))

;; ---------------------------------------------------

(defun tst (niter)
  (with-ed-curve :curve-e521
    (loop repeat niter do
            (let* ((r1 (random *ed-r*))
                   (p1 (with-ed-curve :curve-e521
                         (ed-nth-pt r1)))
                   (p2 (ed-nth-pt r1)))
              (assert (and (= (ecc-pt-x p1)
                              (ecc-pt-x p2))
                           (= (ecc-pt-y p1)
                              (ecc-pt-y p2))))
              ))))

(time (tst 1_000))

;; -------------------------------------------------

(defun tst (curve-id niter)
  (with-ed-curve curve-id
    (loop repeat niter do
          (multiple-value-bind (skey pkey)
              (ed-random-pair)
            (ed-validate-point pkey))
          )))
(time (tst :curve1174  10_000))  ;; 540 µs/iter
(time (tst :curve-e521 1_000))   ;;  20 ms/iter, 40x slower
(time (tst :curve-e521 10_000))  ;;  840 µs/iter, 1.5x slower
|#
