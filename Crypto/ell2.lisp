
(in-package :edec)

;; --------------------------------------------------------------------
;; Investigate Decaf encodings

(defstruct (ecc-projt-pt (:include ecc-proj-pt)
                         (:conc-name ecc-pt-))
  tt)

(defmethod ed-projt-pt ((pt ecc-projt-pt))
  pt)

(defmethod ed-projt-pt ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    (with-mod *ed-q*
      (make-ecc-projt-pt
       :x  x
       :y  y
       :z  1
       :tt (m* x y))
      )))

(defmethod ed-projt-pt ((pt ecc-proj-pt))
  (ed-projt-pt (ed-affine pt)))

;; ---------------------------------------------------------------------------

(defun mnegp (x)
  (declare (integer x))
  (< (m- x) x))

(defun mabs (x)
  (declare (integer x))
  (min x (m- x)))

(defun ed-decaf-encode (pt)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ecc-pt ((x x) (y y) (z z) (tt tt)) (ed-projt-pt pt))
               (declare (integer x y z tt)))
        (let* ((z+y (m+ z y))
               (z-y (m- z y))
               (a-d (m- 1 *ed-d*))
               (r   (cond ((zerop z+y) 0)
                          ((zerop z-y) 0)
                          (t (m/ (msqrt (m* a-d z+y z-y))))
                          ))
               (u  (m* a-d r))
               (xx (m* -2 u))
               (r  (if (mnegp xx)
                       (m- r)
                     r))
               (s  (m*  u
                        (m+ y
                            (m* r
                                (m- x
                                    (m* *ed-d* y tt)))))))
          (mabs s)
          ))))

(defun ed-decaf-decode (s)
  (declare (integer s))
  (with-mod *ed-q*
    (unless (= s (mabs s))
      (error "Invalid encoding"))
    (let* ((x  (m* 2 s))
           (ss (m* s s))
           (z  (m+ 1 ss))
           (u  (m- (m* z z) (m* 4 *ed-d* ss)))
           (xx (m* u ss))
           (v  (cond ((zerop xx)  0)
                     ((quadratic-residue-p xx)
                      (m/ (msqrt xx)))
                     (t
                      (error "Invalid encoding"))
                     ))
           (v   (if (mnegp (m* u v))
                    (m- v)
                  v))
           (w   (m* v s (m- 2 z)))
           (w   (if (zerop w)
                    1
                  w))
           (y   (m* w z))
           (tt  (m* w x)))
      (make-ecc-projt-pt
       :x  x
       :y  y
       :z  z
       :tt tt)
      )))

(defun #1=decaf-dec (s)
  (when (<= 0 s (ash *ed-q* -1))
    (if (zerop s)
        (ed-neutral-point)
      (with-mod *ed-q*
        (let ((den (m+ (m* s s s s)
                       (m* s s (m- 2 (m* 4 *ed-d*)))
                       1)))
          (unless (quadratic-residue-p den)
            (return-from #1# nil))
          (let* ((x (m/ (m* 2 s) (m+ 1 (m* s s))))
                 (y (m/ (m- 1 (m* s s))
                        (msqrt den)))
                 (pt (make-ecc-pt
                      :x x
                      :y y)))
            (when (ed-satisfies-curve pt)
              pt))))
      )))

(defun decaf= (pt1 pt2)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) (ed-projt-pt pt1))
             (declare (integer x1 y1))
             (:struct-accessors ecc-pt ((x2 x) (y2 y)) (ed-projt-pt pt2))
             (declare (integer x2 y2)))
    (with-mod *ed-q*
      (= (m* x1 y2)
         (m* x2 y1))
      )))
                        
(defun tst ()
  (with-mod *ed-q*
    (loop repeat 1000 do
          (let* ((g   (ed-nth-pt (field-random *ed-r*)))
                 (s   (ed-decaf-encode g))
                 (gs  (ed-affine (ed-decaf-decode s)))
                 (s2  (ed-decaf-encode gs))
                 (gs2 (ed-affine (ed-decaf-decode s2))))
            (assert (= s s2))
            (assert (ed-pt= gs2 gs))
            ))))
