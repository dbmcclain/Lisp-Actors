
(in-package :edec)

          
(defun make-coord ()
  (make-array 5
              :element-type 'fixnum
              :initial-element 0))

(defun fetch (val &optional ans)
  (let ((ans (or ans (make-coord))))
    (setf (aref ans 0) (ldb (byte 51   0) val)
          (aref ans 1) (ldb (byte 51  51) val)
          (aref ans 2) (ldb (byte 51 102) val)
          (aref ans 3) (ldb (byte 51 153) val)
          (aref ans 4) (ldb (byte 47 204) val))
    ans))

(defun lobits (x n)
  (ldb (byte n 0) x))

(defun hibits (x n)
  (ash x (- n)))

(defun scr (vec &optional ans)
  (let (w
        (ans (or ans (make-coord))))
    (setf w (aref vec 4)
          (aref ans 4) (lobits w 47)
          w (+ (aref vec 0) (* 9 (hibits w 47)))
          (aref ans 0) (lobits w 51)
          w (+ (aref vec 1) (hibits w 51))
          (aref ans 1) (lobits w 51)
          w (+ (aref vec 2) (hibits w 51))
          (aref ans 2) (lobits w 51)
          w (+ (aref vec 3) (hibits w 51))
          (aref ans 3) (lobits w 51)
          w (+ (aref ans 4) (hibits w 51))
          (aref ans 4) (lobits w 47)
          (aref ans 0) (+ (aref ans 0) (* 9 (hibits w 47))))
    ans))

(defun gneg (vec &optional ans)
  (let ((ans (or ans (make-coord))))
    (setf (aref ans 0) (- (aref vec 0))
          (aref ans 1) (- (aref vec 1))
          (aref ans 2) (- (aref vec 2))
          (aref ans 3) (- (aref vec 3))
          (aref ans 4) (- (aref vec 4)))
    ans))

(defvar *qvec* (fetch *ed-q*))

(defun gmod (x &optional ans)
  (let ((ans (or ans (make-coord))))
    (scr (scr x) ans)
    (when (and (eql (aref ans 4) (aref *qvec* 4))
               (eql (aref ans 3) (aref *qvec* 3))
               (eql (aref ans 2) (aref *qvec* 2))
               (eql (aref ans 1) (aref *qvec* 1))
               (>=  (aref ans 0) (aref *qvec* 0)))
      (setf (aref ans 0) (- (aref ans 0) (aref *qvec* 0))
            (aref ans 1) 0
            (aref ans 2) 0
            (aref ans 3) 0
            (aref ans 4) 0))
    ans))

(defun gnorm (vec &optional ans)
  (let ((ans (or ans (make-coord))))
    (gmod vec ans)
    ans))

(defun store (vec)
  (let ((ans 0)
        w)
    (setf w (gnorm vec)
          (ldb (byte 51   0) ans) (aref w 0)
          (ldb (byte 51  51) ans) (aref w 1)
          (ldb (byte 51 102) ans) (aref w 2)
          (ldb (byte 51 153) ans) (aref w 3)
          (ldb (byte 57 204) ans) (aref w 4))
    ans))

(defun gadd (x y &optional ans)
  (let ((ans (or ans (make-coord))))
    (setf (aref ans 0) (+ (aref x 0) (aref y 0))
          (aref ans 1) (+ (aref x 1) (aref y 1))
          (aref ans 2) (+ (aref x 2) (aref y 2))
          (aref ans 3) (+ (aref x 3) (aref y 3))
          (aref ans 4) (+ (aref x 4) (aref y 4)))
    ans))

(defun gsub (x y &optional ans)
  (let ((ans (or ans (make-coord))))
    (setf (aref ans 0) (- (aref x 0) (aref y 0))
          (aref ans 1) (- (aref x 1) (aref y 1))
          (aref ans 2) (- (aref x 2) (aref y 2))
          (aref ans 3) (- (aref x 3) (aref y 3))
          (aref ans 4) (- (aref x 4) (aref y 4)))
    ans))

(defun gcopy (vec &optional ans)
  (let ((ans (or ans (make-coord))))
    (setf (aref ans 0) (aref vec 0)
          (aref ans 1) (aref vec 1)
          (aref ans 2) (aref vec 2)
          (aref ans 3) (aref vec 3)
          (aref ans 4) (aref vec 4))
    ans))

(defun geq (x y)
  (let ((tmp (make-coord)))
    (setf tmp (gsub x y)
          tmp (gnorm tmp))
    (and (eql 0 (aref tmp 0))
         (eql 0 (aref tmp 1))
         (eql 0 (aref tmp 2))
         (eql 0 (aref tmp 3))
         (eql 0 (aref tmp 4))
         )))

(defun gmul (x y &optional ans)
  (let ((ans (or ans (make-coord)))
        w)
    (flet ((prd (ix iy)
             (* (aref x ix) (aref y iy))))
      (setf w  (+ (prd 0 4) (prd 1 3) (prd 2 2) (prd 3 1) (prd 4 0))
            (aref ans 4) (lobits w 47)
            w  (+ (prd 0 0)
                  (* 144 (+ (prd 1 4) (prd 2 3) (prd 3 2) (prd 4 1)))
                  (* 9 (hibits w 47)))
            (aref ans 0) (lobits w 51)
            w  (+ (prd 0 1) (prd 1 0)
                  (* 144 (+ (prd 2 4) (prd 3 3) (prd 4 2)))
                  (hibits w 51))
            (aref ans 1) (lobits w 51)
            w  (+ (prd 0 2) (prd 1 1) (prd 2 0)
                  (* 144 (+ (prd 3 4) (prd 4 3)))
                  (hibits w 51))
            (aref ans 2) (lobits w 51)
            w  (+ (prd 0 3) (prd 1 2) (prd 2 1) (prd 3 0)
                  (* 144 (prd 4 4))
                  (hibits w 51))
            (aref ans 3) (lobits w 51)
            w  (+ (aref ans 4) (hibits w 51))
            (aref ans 4) (lobits w 47)
            (aref ans 0) (+ (aref ans 0) (* 9 (hibits w 47))))
      ans)))

      
                            
                 
#|          
(loop repeat 10_000 do
      (let ((r (random *ed-q*)))
        (assert (eql r (store (fetch r))))))

(loop repeat 10_000 do
      (let ((r (random *ed-q*)))
        (assert (eql r (store (gnorm (fetch r)))))
        ))

(loop repeat 1_000_000 do
      (let* ((x  (random *ed-q*))
             (y  (random *ed-q*))
             (chk (mod (* x y) *ed-q*))
             (xv (fetch x))
             (yv (fetch y))
             (zv (gmul xv yv))
             (z  (store zv)))
        (assert (eql chk z))
        ))

(let* ((x (+ *ed-q* 5)))
  (gmod (fetch x)))


(fetch -5)
(gneg (fetch 5))
(scr (gneg (fetch 5)))

(fetch (+ *ed-q* 5))
(scr (fetch (+ *ed-q* 5)))
(gmod (fetch (+ *ed-q* 5)))

(fetch (- (+ *ed-q* 5)))                 

(let* ((bits (1- (ash 1 60)))
       (vec  (vector bits bits bits bits bits)))
  (scr (scr vec)))

(let* ((bits (1- (ash 1 51)))
       (vec (vector bits bits bits bits bits)))
  (scr vec))

(loop repeat 1_000_000 do
        (let ((x (random *ed-q*))
              
          (

(let* ((bits (1- (ash 1 60)))
       (vec  (vector bits bits bits bits (- bits))))
  (scr (gneg (scr (gneg vec)))))

(let* ((bits (- (1- (ash 1 60))))
       (vec  (vector bits bits bits bits bits)))
  (scr vec))
  (scr (gneg (scr (gneg vec)))))

(store (vector 0 -1 0 0 0))
(scr (vector 0 -1 0 0 0))))
(scr (scr (vector 0 -1 0 0 0)))))
(scr (scr (vector 0 0 0 -1 0)))
(scr (vector -1 -1 -1 -1 -1))

(progn
  *qvec*)
(gmod *qvec*)
(gmod (vector 0 0 0 -1 0))
(fetch -1)
(gmod (fetch -1))

(fetch 1)
(gneg (fetch 1))
(scr (gneg (fetch 1)))
(gmod (gneg (fetch 1)))

(with-fast-ed-impl
  (ed-affine (ed-mul *ed-gen* -1)))
(with-slow-ed-impl
  (ed-affine (ed-mul *ed-gen* -1)))
(with-ed-curve :curve-e521f
  (print *ed-gen*)
  (with-fast-ed-impl (print (ed-affine (ed-mul *ed-gen* -1))))
  (with-slow-ed-impl (print (ed-affine (ed-mul *ed-gen* -1))))
  (values))
(with-ed-curve :curve-e521f
  (print *ed-gen*)
  (with-fast-ed-impl (print (ed-affine (ed-add *ed-gen* (ed-mul *ed-gen* -1)))))
  (with-slow-ed-impl (print (ed-affine (ed-add *ed-gen* (ed-mul *ed-gen* -1)))))
  (values))

(time
 (loop repeat 200_000 do
         (random *ed-r*)))
(time
 (with-slow-ed-impl
   (loop repeat 2_000 do
           (let ((r (random *ed-r*)))
             (ed-mul *ed-gen* r)))))
(time
 (with-fast-ed-impl
   (loop repeat 200_000 do
           (let ((r (random *ed-r*)))
             (ed-mul *ed-gen* r)))))

(loop repeat 2_000 do
        (let* ((r (random *ed-r*))
               (pf (with-fast-ed-impl
                     (ed-mul *ed-gen* r)))
               (ps (with-slow-ed-impl
                     (ed-mul *ed-gen* r))))
          (assert (ed-pt= pf ps))
          ))

(with-ed-curve :curve-e521f
  (time
   (loop repeat 50_000 do
           (random *ed-r*))))
  
(with-ed-curve :curve-e521f
  (time
   (with-slow-ed-impl
     (loop repeat 1_000 do
             (let ((r (random *ed-r*)))
               (ed-mul *ed-gen* r))))))
(with-ed-curve :curve-e521f
  (time
   (with-fast-ed-impl
     (loop repeat 50_000 do
             (let ((r (random *ed-r*)))
               (ed-mul *ed-gen* r))))))

(with-ed-curve :curve-e521f
  (loop repeat 1_000 do
          (let* ((r (random *ed-r*))
                 (pf (with-fast-ed-impl
                       (ed-mul *ed-gen* r)))
                 (ps (with-slow-ed-impl
                       (ed-mul *ed-gen* r))))
            (assert (ed-pt= pf ps))
            )))
  

(ed-affine (ed-mul *ed-gen* 15))
(with-slow-impl
  (ed-affine (ed-mul *ed-gen* 15)))

(progn
  *ed-gen*)

(with-slow-impl
  (ed-affine (ed-mul (ed-projective *ed-gen*) 2)))

(with-fast-ed-impl
  (ed-affine (ed-mul *ed-gen* 2)))

(ed-affine (ed-mul *ed-gen* 1))

(let ((x (1- (ash 1 521))))
  (write
   (vector
    (ldb (byte 53 (* 0 53)) x)
    (ldb (byte 53 (* 1 53)) x)
    (ldb (byte 53 (* 2 53)) x)
    (ldb (byte 53 (* 3 53)) x)
    (ldb (byte 53 (* 4 53)) x)
    (ldb (byte 53 (* 5 53)) x)
    (ldb (byte 53 (* 6 53)) x)
    (ldb (byte 53 (* 7 53)) x)
    (ldb (byte 53 (* 8 53)) x)
    (ldb (byte 44 (* 9 53)) x))
   :base 16))


;; show roughly 50% of x values in field have corresponding curve points
(with-ed-curve :curve-ed3363 ;; :curve-ed448 ;; :curve41417 ;; :curve-e521f
  (let* ((ct 0)
         (niter 10_000))
    (loop repeat niter
          for x from (random *ed-r*)
          do
            (when (ignore-errors
                    (ed-solve-y x))
              (incf ct)))
    (float (/ ct niter))))

|#
