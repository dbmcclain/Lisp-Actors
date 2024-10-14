
(defpackage :pbc/x
  (:use :cl :pbc :ro :crypto/modular-arith :vec-repr))

(in-package :pbc/x)

(defparameter k (make-key-pair :dave))
(defparameter pkey (keying-triple-pkey k))
(defparameter skey (keying-triple-skey k))

(defparameter beta   76600213043964638334639432839350561620586998450651561245322304548751832163977)
(defparameter alpha0 82889197335545133675228720470117632986673257748779594473736828145653330099944)
(defparameter alpha1 66367173116409392252217737940259038242793962715127129791931788032832987594232)
(defparameter q      115792089237314936872688561244471742058375878355761205198700409522629664518163)
(defparameter r      115792089237314936872688561244471742058035595988840268584488757999429535617037)

#|
(defvar *field* (inherit-from =top=
                              :add    'field-add
                              :sub    'field-sub
                              :mul    'field-mul
                              :div    'field-div
                              :cmp    'field-cmp
                              :neg    'field-neg
                              :inv    'field-inv
                              :expt   'field-expt
                              :sqrt   'field-sqrt
                              :is-sqr 'field-is-sqr
                              ))

(defvar *fld-q* (inherit-from *field*
                              :ord 115792089237314936872688561244471742058375878355761205198700409522629664518163))

(defvar *fld-r* (inherit-from *field*
                              :ord 115792089237314936872688561244471742058035595988840268584488757999429535617037))

(defun field-add (x y)
  (assert (eq (parent x) (parent y)))
  (with-mod (prop x :ord)
    (copy-obj x :val (e+ (prop x :val) (prop y :val)))))

(defun field-sub (x y)
  (assert (eq (parent x) (parent y)))
  (with-mod (prop x :ord)
    (copy-obj x :val (e- (prop x :val) (prop y :val)))))

(defun field-mul (x y)
  (assert (eq (parent x) (parent y)))
  (with-mod (prop x :ord)
    (copy-obj x :val (e* (prop x :val) (prop y :val)))))

(defun field-div (x y)
  (assert (eq (parent x) (parent y)))
  (with-mod (prop x :ord)
    (copy-obj x :val (e/ (prop x :val) (prop y :val)))))

(defun field-expt (x y)
  (assert (eq (parent x) (parent y)))
  (with-mod (prop x :ord)
    (copy-obj x :val (e^ (prop x :val) (prop y :val)))))

(defun field-cmp (x y)
  (assert (eq (parent x) (parent y)))
  (with-mod (prop x :ord)
    (e- (prop x :val) (prop y :val))))

(defun field-neg (x)
  (with-mod (prop x :ord)
    (copy-obj x :val (eneg (prop x :val)))))

(defun field-inv (x)
  (with-mod (prop x :ord)
    (copy-obj x :val (einv (prop x :val)))))

(defun field-sqrt (x)
  (with-mod (prop x :ord)
    (copy-obj x :val (esqrt (prop x :val)))))

(defun field-is-sqr (x)
  (with-mod (prop x :ord)
    (e-quadratic-residue-p (prop x :val))))

;; ------------------------------------------

(defun make-field-element (fld val)
  (inherit-from fld :val (mod val (ord fld))))

;; ------------------------------------------

(defun ord (x)
  (prop x :ord))

(defun element-add (x y)
  (funcall (prop x :add) x y))

(defun element-sub (x y)
  (funcall (prop x :sub) x y))

(defun element-mul (x y)
  (funcall (prop x :mul) x y))

(defun element-div (x y)
  (funcall (prop x :div) x y))

(defun element-cmp (x y)
  (funcall (prop x :cmp) x y))

(defun element-expt (x y)
  (funcall (prop x :expt) x y))

(defun element-neg (x)
  (funcall (prop x :neg) x))

(defun element-inv (x)
  (funcall (prop x :inv) x))

(defun element-sqrt (x)
  (funcall (prop x :sqrt) x))

(defun element-is-sqr (x)
  (funcall (prop x :is-sqr) x))

;; ---------------------------------------------

(defvar *curve* (inherit-from =top=
                              :add    'curve-add
                              :sub    'curve-sub
                              :mul    'field-mul
                              :div    'field-div
                              :cmp    'curve-cmp
                              :neg    'curve-neg
                              :inv    'field-inv
                              :expt   'field-expt
                              :sqrt   'field-sqrt
                              :is-sqr 'field-is-sqr
                              ))
|#
;; ----------------------------------------------

(defclass field ()
  ((ord   :reader field-ord
          :initarg :ord)))

(defmethod print-object ((obj field) out-stream)
  (format out-stream "#<FIELD ORD = ~A>" (field-ord obj)))

(defclass field-int ()
  ((fld   :reader field-int-fld
          :initarg :fld)
   (val   :reader field-int-val
          :initarg :val)))

(defmethod print-object ((obj field-int) out-stream)
  (format out-stream "#<FIELD-INT VAL = ~A>" (field-int-val obj)))

(defmethod make-field-int ((fld field) (v integer))
  (make-instance 'field-int
                 :fld fld
                 :val (with-mod (field-ord fld)
                        (mmod v))))

(defmethod int ((x field-int))
  (field-int-val x))

(defmethod element-binop (op (x field-int) (y field-int))
  (let ((fld (field-int-fld x)))
    (assert (eq fld (field-int-fld y)))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (funcall op (field-int-val x) (field-int-val y))))
    ))

(defmethod element-binop (op (x field-int) (y integer))
  (let ((fld (field-int-fld x)))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (funcall op (field-int-val x) y)))))
  
(defmethod element-add (x y)
  (element-binop 'm+ x y))

(defmethod element-sub (x y)
  (element-binop 'm- x y))

(defmethod element-mul (x y)
  (element-binop 'm* x y))

(defmethod element-div (x y)
  (element-binop 'm/ x y))

(defmethod element-expt (x y)
  (element-binop 'm^ x y))

(defmethod element-rnd (x)
  (let* ((fld (field-int-fld x))
         (ord (field-ord fld)))
    (make-field-int fld (ecc-crypto-b571:random-between 1 ord))))

(defmethod element-is-sqr ((x field-int))
  (let ((fld (field-int-fld x)))
    (with-mod (field-ord fld)
      (quadratic-residue-p (field-int-val x)))))

(defmethod element-sqrt ((x field-int))
  (let ((fld (field-int-fld x)))
    (make-instance 'field-int
                   :fld  fld
                   :val  (with-mod (field-ord fld)
                           (msqrt (field-int-val x)))
                   )))

(defmethod element-inv ((x field-int))
  (let ((fld (field-int-fld x))
        (v   (field-int-val x)))
    (assert (plusp v))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (m/ v)))
    ))

(defmethod element-neg ((x field-int))
  (let ((fld  (field-int-fld x)))
    (make-instance 'field-int
                   :fld fld
                   :val (with-mod (field-ord fld)
                          (m- (field-int-val x))))
    ))

(defmethod element-cmp ((x field-int) (y field-int))
  (let ((fld (field-int-fld x)))
    (assert (eq fld (field-int-fld y)))
    (element-cmp x (field-int-val y))))

(defmethod element-cmp ((x field-int) (y integer))
  (let ((ans (- (field-int-val x) y)))
    (cond ((zerop ans)   0)
          ((minusp ans) -1)
          (t             1)
          )))

(defmethod element-0p ((x field-int))
  (zerop (field-int-val x)))

(defmethod element-1p ((x field-int))
  (= 1 (field-int-val x)))

(defmethod element-0 ((x field-int))
  (make-instance 'field-int
                 :fld (field-int-fld x)
                 :val 0))

(defmethod element-1 ((x field-int))
  (make-instance 'field-int
                 :fld  (field-int-fld x)
                 :val  1))

;; ------------------------------------------------------------

(defclass ext-field ()
  ((fld   :reader ext-field-fld
          :initarg :fld)
   (deg   :reader ext-field-deg
          :initarg :deg)
   (coffs :reader ext-field-coffs
          :initarg :coffs)))

(defmethod print-object ((obj ext-field) out-stream)
  (format out-stream "#<EXT-FIELD DEG = ~A  COFFS = ~A>"
          (ext-field-deg obj)
          (concatenate 'vector (ext-field-coffs obj) #(1))))

(defclass poly ()
  ((ext   :reader poly-ext
          :initarg :ext)
   (coffs :reader poly-coffs
          :initarg :coffs)))

(defmethod print-object ((obj poly) out-stream)
  (format out-stream "#<POLY COFFS = ~A>" (poly-coffs obj)))

(defmethod element-binop (op (x poly) (y poly))
  (let ((ext (poly-ext x)))
    (assert (eq ext (poly-ext y)))
    (make-instance 'poly
                   :ext   ext
                   :coffs (with-mod (field-ord (ext-field-fld ext))
                            (let ((xcoffs (poly-coffs x))
                                  (ycoffs (poly-coffs y)))
                              (ecase op
                                ((m+ m-) (map 'vector op xcoffs ycoffs))
                                (m* (poly-mul ext xcoffs ycoffs))
                                )))
                   )))

(defun poly-mul (ext xcoffs ycoffs)
  (let* ((deg (ext-field-deg ext))
         (ans (make-array deg
                          :initial-element 0))
         (hi  (make-array deg
                          :initial-element 0)))
    (loop for ix from 0 below deg
          for x across xcoffs
          do
          (loop for iy from 0 below deg
                for y across ycoffs
                do
                (let ((z  (m* x y))
                      (iz (+ ix iy)))
                  (if (< iz deg)
                      (setf (aref ans iz) (m+ (aref ans iz) z))
                    (let ((iz (- iz deg)))
                      (setf (aref hi iz) (m+ (aref hi iz) z))))
                  )))
    (loop for ix from (1- deg) downto 0
          do
          (let ((x (aref hi ix)))
            (loop for iy from 0 below deg
                  for y across (ext-field-coffs ext)
                  do
                  (let ((z  (m* x y))
                        (iz (+ ix iy)))
                    (if (< iz deg)
                        (setf (aref ans iz) (m- (aref ans iz) z))
                      (let ((iz (- iz deg)))
                        (setf (aref hi iz) (m- (aref hi iz) z)))
                      )))))
    ans))

(defmethod element-div ((pn poly) (pd poly))
  (let ((ext  (poly-ext pn)))
    (assert (eq ext (poly-ext pd)))
    (let* ((fld    (ext-field-fld ext))
           (ord    (field-ord fld))
           (ncoffs (poly-coffs pn))
           (dcoffs (poly-coffs pd))
           (n      (position-if (complement 'zerop) ncoffs
                                :from-end t))
           (d      (position-if (complement 'zerop) dcoffs
                                :from-end t)))
      (if (< n d)
          (values (element-0 pn) pn)
        (let ((quot   (make-poly ext '()))
              (ncoffs (copy-seq ncoffs)))
          (with-mod ord
            (um:nlet iter ((n  n))
              (if (< n d)
                  (values quot
                          (make-poly ext ncoffs))
                (let ((r  (m/ (aref ncoffs n)
                              (aref dcoffs d))))
                  (element-set quot (- n d) r)
                  (loop for dx from d downto 0
                        for nx from n by -1
                        do
                        (setf (aref ncoffs nx) (m- (aref ncoffs nx)
                                                   (m* r (aref dcoffs dx)))))
                  (gp-iter (1- n)))
                )))))
      )))

(defmethod element-cmp ((p1 poly) (p2 poly))
  (let ((ext  (poly-ext p1)))
    (assert (eq ext (poly-ext p2)))
    (let* ((coffs1  (poly-coffs p1))
           (coffs2  (poly-coffs p2))
           (ms1     (position-if (complement 'zerop) coffs1
                                 :from-end t))
           (ms2     (position-if (complement 'zerop) coffs2
                                 :from-end t)))
      (cond ((< ms1 ms2)  -1)
            ((> ms1 ms2)   1)
            (t
             (um:nlet iter ((ix  ms1))
               (if (< ix 0)
                   0
                 (let ((v1  (aref coffs1 ix))
                       (v2  (aref coffs2 ix)))
                   (cond ((< v1 v2) -1)
                         ((> v1 v2)  1)
                         (t  (go-iter (1- ix)))
                         )))))
            ))))

(defmethod element-inv ((p poly))
  (let* ((ext  (poly-ext p))
         (fld  (ext-field-fld ext))
         (ord  (field-ord fld)))
    (element-expt p (1- ord))))

#|
(let* ((ext (make-extension-field *fld-q* '(1 0 0 0 0 0 1)))
       (pn  (make-poly ext '(1 2 3 4 5 6)))
       (pd  (make-poly ext '(1 1))))
  (multiple-value-bind (q r) (element-div pn pd)
    (let ((xn  (element-add r
                            (element-mul pd q))))
      (element-cmp xn pn))))

(let* ((ext (make-extension-field *fld-q* '(3 0 1)))
       (pn  (make-poly ext '(3 2)))
       (pd  (make-poly ext '(1 1)))
       (pwr (element-expt pn (1- (field-ord *fld-q*)))))
  (inspect (element-mul pn pwr)))
|#
                          
(defmethod element-mul ((p poly) (x integer))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext))
         (ord (field-ord fld)))
    (with-mod ord
      (make-poly ext (reverse
                      (map 'vector (um:curry 'm* x) (poly-coffs p))))
      )))

(defmethod element-mul ((p poly) (x field-int))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext)))
    (assert (eq fld (field-int-fld x)))
    (element-mul p (field-int-val x))))

(defmethod element-div ((p poly) (x integer))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext))
         (ord (field-ord fld)))
    (with-mod ord
      (element-mul p (m/ x)))
    ))

(defmethod element-div ((p poly) (x field-int))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext)))
    (assert (eq fld (field-int-fld x)))
    (element-div p (field-int-val x))))
           
(defmethod element-set ((p poly) (ix integer) (val integer))
  (let* ((coffs (poly-coffs p))
         (ext   (poly-ext p))
         (fld   (ext-field-fld ext)))
    (assert (and (<= 0 ix)
                 (< ix (length coffs))))
    (with-mod (field-ord fld)
      (setf (aref coffs ix) (mmod val)))
    ))

(defmethod element-neg ((p poly))
  (let* ((ext  (poly-ext p))
         (fld  (ext-field-fld ext)))
    (make-instance 'poly
                   :ext   ext
                   :coffs (with-mod (field-ord fld)
                            (map 'vector 'm- (poly-coffs p)))
                   )))

(defmethod element-expt ((p poly) (x integer))
  (let ((ans (element-1 p)))
    (loop for ix from 0 below (integer-length x)
          for q = p then (element-mul q q)
          do
          (when (logbitp ix x)
            (setf ans (element-mul ans q))))
    ans))

(defmethod element-expt ((p poly) (x field-int))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext)))
    (assert (eq fld (field-int-fld x)))
    (element-expt p (field-int-val x))))
           
(defmethod element-0p ((p poly))
  (every 'zerop (poly-coffs p)))

(defmethod element-1p ((p poly))
  (and (= 1 (aref (poly-coffs p) 0))
       (every 'zerop (subseq (poly-coffs p) 1))))

(defmethod element-0 ((p poly))
  (make-poly (poly-ext p) '()))

(defmethod element-1 ((p poly))
  (make-poly (poly-ext p) '(1)))

(defmethod element-rnd ((p poly))
  (let* ((ext  (poly-ext p))
         (fld  (ext-field-fld ext))
         (ord  (field-ord fld)))
    (make-poly ext (map 'vector (lambda (x)
                                  (declare (ignore x))
                                  (ecc-crypto-b571:random-between 1 ord))
                        (poly-coffs p)))
    ))
                       

(defmethod element-is-sqr ((p poly))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext))
         (ord (field-ord fld)))
    (element-1p (element-expt p (truncate (1- ord) 2)))))

(defmethod element-sqrt ((p poly))
  (let* ((ext (poly-ext p))
         (fld (ext-field-fld ext))
         (ord (field-ord fld)))
    (cond ((= 3 (mod ord 4))
           (element-expt p (truncate (1+ ord) 4)))
          (t
           (poly-cipolla p))
          )))

(defun poly-cipolla (p)
  (declare (ignore p))
  (error "Not yet implemented"))
  

(defmethod make-extension-field ((fld field) (coffs sequence))
  "coffs presented and stored in ascending power order"
  (let* ((ord  (field-ord fld))
         (deg  (1- (length coffs))))
    (assert (every 'integerp coffs))
    (make-instance 'ext-field
                   :fld   fld
                   :deg   deg
                   :coffs (make-array deg
                                      :initial-contents
                                      (with-mod ord
                                        (let ((lst (coerce coffs 'list)))
                                          (mapcar (um:rcurry 'm* (m/ (um:last1 lst)))
                                                  (butlast lst)))))
                   )))

(defmethod make-poly ((ext ext-field) (coffs sequence))
  "coffs presented and stored in ascending power order"
  (let* ((deg  (ext-field-deg ext))
         (fld  (ext-field-fld ext))
         (ord  (field-ord fld)))
    (assert (<= (length coffs) deg))
    (assert (every 'integerp coffs))
    (let ((init (concatenate 'vector
                             (map 'list (um:rcurry 'mod ord) coffs)
                             (make-list (- deg (length coffs))
                                        :initial-element 0))))
      (make-instance 'poly
                     :ext  ext
                     :coffs (make-array deg
                                        :initial-contents init)
                     ))))

;; ------------------------------------------------------------------------------

(defvar *fld-q* (make-instance 'field
                               :ord 115792089237314936872688561244471742058375878355761205198700409522629664518163))

(defvar *fld-r* (make-instance 'field
                               :ord 115792089237314936872688561244471742058035595988840268584488757999429535617037))

;; ---------------------------------------------------------------------

(defclass curve ()
  ((qfld :reader curve-qfld
         :initarg :qfld)
   (rfld :reader curve-rfld
         :initarg :rfld)
   (a    :reader curve-a
         :initform 0
         :initarg :a)
   (b    :reader curve-b
         :initarg :b)
   (h    :reader curve-h
         :initarg :h
         :initform 1))
  (:documentation "Curve: y^2 = x^3 + a*x + b
Curve define over field qfld, order = h*ord(rfld).
X, Y are elements of qfld."))

(defclass pt ()
  ((curve :reader pt-curve
          :initarg :curve)
   (x     :reader pt-x
          :initarg :x)
   (y     :reader pt-y
          :initarg :y)))

(defmethod print-object ((obj pt) out-stream)
  (format out-stream "#<PT X: ~A :Y ~A>"
          (pt-x obj) (pt-y obj)))

(defmethod element-0p ((p pt))
  (element-0p (pt-y p)))

(defmethod element-0 ((p pt))
  (make-instance 'pt
                 :curve (pt-curve p)
                 :x     (element-0 (pt-x p))
                 :y     (element-0 (pt-y p))))

(defmethod element-neg ((p pt))
  (make-instance 'pt
                 :curve (pt-curve p)
                 :x     (pt-x p)
                 :y     (element-neg (pt-y p))))

(defmethod affine-double ((pt pt))
  (with-accessors ((curve  pt-curve)
                   (x      pt-x)
                   (y      pt-y)) pt
    (let* ((s  (element-div
                (element-add
                 (element-mul (element-1 x)
                              (curve-a curve))
                 (element-mul (element-mul x x)
                              3))
                (element-mul y 2)))
           (x2 (element-sub (element-mul s s)
                            (element-add x x)))
           (y2 (element-sub (element-mul s
                                         (element-sub x x2))
                            y)))
      (make-instance 'pt
                     :curve curve
                     :x     x2
                     :y     y2))))

(defmethod element-add ((pt1 pt) (pt2 pt))
  (with-accessors ((curve1  pt-curve)
                   (x1      pt-x)
                   (y1      pt-y)) pt1
    (with-accessors ((curve2 pt-curve)
                     (x2     pt-x)
                     (y2     pt-y)) pt2
      (assert (eq curve1 curve2))
      (cond
       ((element-0p pt1) pt2)
       ((element-0p pt2) pt1)
       ((element-0p (element-sub x1 x2))
        (cond ((element-0p (element-sub y1 y2))
               (affine-double pt1))
              ((element-0p (element-add y1 y2))
               (element-0 pt1))
              (t (error "affine-add: points not on curve"))
              ))
       (t
        (let* ((s   (element-div (element-sub y2 y1)
                                 (element-sub x2 x1)))
               (x3  (element-sub (element-mul s s)
                                 (element-add x1 x2)))
               (y3  (element-sub (element-mul s
                                              (element-sub x1 x3))
                                 y1)))
          (make-instance 'pt
                         :curve curve1
                         :x     x3
                         :y     y3)))
       ))))

(defmethod element-sub ((p1 pt) (p2 pt))
  (element-add p1 (element-neg p2)))

(defmethod element-mul ((p pt) (x integer))
  (let ((ans  (element-0 p)))
    (loop for ix from 0 below (integer-length x)
          for q = p then (element-add q q)
          do
          (when (logbitp ix x)
            (setf ans (element-add ans q))))
    ans))

(defmethod pt-on-curve-p ((p pt))
  (with-accessors ((curve  pt-curve)
                   (x      pt-x)
                   (y      pt-y)) p
    (cond ((element-0p p) nil)
          (t (let* ((yy  (element-mul y y))
                    (one (element-1 x))
                    (gx  (element-add
                          (element-mul x
                                       (element-add
                                        (element-mul x x)
                                        (element-mul one (curve-a curve))))
                          (element-mul one (curve-b curve)))))
               (element-0p (element-sub yy gx))))
          )))

(defmethod validate-pt ((p pt))
  (and (not (element-0p p))
       (pt-on-curve-p p)
       (let* ((curve (pt-curve p))
              (rfld  (curve-rfld curve))
              (h     (curve-h    curve)))
         (element-0p (element-mul p (* h (field-ord rfld)))))
       ))

(defmethod hash-to-curve ((curve curve) (h hash:hash))
  (let* ((x    (hash-to-field (curve-qfld curve) h))
         (one  (element-1 x)))
    (um:nlet iter ((x x))
      (let ((gx (element-add
                 (element-mul x
                              (element-add
                               (element-mul x x)
                               (element-mul one (curve-a curve))))
                 (element-mul one (curve-b curve)))))
        (if (element-is-sqr gx)
            (make-instance 'pt
                           :curve curve
                           :x     x
                           :y     (element-sqrt gx))
          (go-iter (element-add one
                                (element-mul x x)))
          )))
    ))

(defmethod hash-to-vec ((h hash:hash) nb)
  "short hashes -> H | 0 | H | 1 | H ..."
  (let* ((nel   (hash:hash-length h))
         (bytes (hash:hash-bytes h)))
    (cond ((= nel nb)  bytes)
          ((> nel nb)  (subseq bytes 0 nb))
          (t 
           (let ((vec (make-ub8-vector nb)))
             (um:nlet iter ((pos 0)
                            (ctr 0))
               (if (>= pos nb)
                   vec
                 (progn
                   (replace vec bytes :start1 pos)
                   (incf pos nel)
                   (when (< pos nb)
                     (setf (aref vec pos) ctr)
                     (incf pos))
                   (go-iters pos (1+ ctr)))
                 ))))
          )))

(defun shift-to-fit (vec ord)
  (um:nlet iter ((x  (int (bev vec))))
    (if (>= x ord)
        (go-iter (ash x -1))
      x)))

(defmethod hash-to-field ((qfld field) (h hash:hash))
  (let* ((ord   (field-ord qfld))
         (nb    (ceiling (integer-length ord) 8))
         (xval  (shift-to-fit (hash-to-vec h nb) ord)))
    (make-field-int qfld xval)))

(defmethod hash-to-field ((qfld ext-field) (h hash:hash))
  (let* ((ord    (field-ord (ext-field-fld qfld)))
         (deg    (ext-field-deg qfld))
         (nbpc   (ceiling (integer-length ord) 8))
         (nb     (* deg nbpc))
         (xbytes (hash-to-vec h nb))
         (xcoffs (loop repeat deg
                       for pos from 0 by nbpc
                       collect
                       (shift-to-fit (subseq xbytes pos (+ pos nbpc)) ord))))
    (make-poly qfld xcoffs)))

    
(defvar *curve* (make-instance 'curve
                               :qfld *fld-q*
                               :rfld *fld-r*
                               :b    3))

(defvar *ext-curve* (let ((ext (make-extension-field *fld-q* '(3 0 1))))
                      (make-instance 'curve
                                     :qfld ext
                                     :rfld *fld-r*
                                     :b    (make-poly ext '(-3 3)))))

;; --------------------------------------------------------------------
#|
(let* ((ext (make-extension-field *fld-q* '(3 0 1)))
       (x   (make-poly ext '(3)))
       (h   (hash-to-field ext (hash:hash/256 :test)))
       (xx  (element-sqrt x)))
  ;; (inspect (list xx (element-mul xx xx)))
  ;; (inspect (list h (element-expt h (field-ord *fld-q*))))
  (inspect (element-expt x (1- (expt (field-ord *fld-q*) 2))))
  ;; (inspect (element-expt x 2))
  )
|#