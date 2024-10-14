
(in-package :pbc/x)

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
            (um:nlet-tail iter ((n  n))
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
                  (iter (1- n)))
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
             (um:nlet-tail iter ((ix  ms1))
               (if (< ix 0)
                   0
                 (let ((v1  (aref coffs1 ix))
                       (v2  (aref coffs2 ix)))
                   (cond ((< v1 v2) -1)
                         ((> v1 v2)  1)
                         (t  (iter (1- ix)))
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

(defmethod element-conj ((p poly))
  (make-instance 'poly
                 :ext (poly-ext p)
                 :coffs (map 'vector 'element-conj (poly-coffs p))))
