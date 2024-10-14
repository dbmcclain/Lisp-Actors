
(in-package :core-crypto)

(defun fast-mod (x)
  #F
  (declare (integer x))
  (let* ((m     (mod-base))
         (nbits (integer-length (1- m)))
         (wrap  (- (ash 1 nbits) m)))
    (declare (integer m wrap)
             (fixnum nbits))
    (um:nlet iter ((x  x)
                   (ct 1))
      (declare (integer x))
      (let ((hi  (ash x (- nbits))))
        (declare (integer hi))
        (if (zerop hi)
            (let ((nx (- x m)))
              (declare (integer nx))
              (values (if (minusp nx) x nx) ct))
          (go-iter (+ (ldb (byte nbits 0) x)
                      (* wrap hi))
                   (1+ ct))
          )))
    ))

(defun fast-mul (a b)
  #F
  (declare (integer a b))
  (let* ((m     (mod-base))
         (nbits (integer-length (1- m)))
         (nbh   (ash nbits -1))
         (wrap  (- (ash 1 nbits) m))
         (a     (fast-mod a))
         (al    (ldb (byte nbh 0) a))
         (ah    (ash a (- nbh)))
         (b     (fast-mod b))
         (bl    (ldb (byte nbh 0) b))
         (bh    (ash b (- nbh)))
         (p0    (* al bl))
         (p1    (+ (* al bh) (* ah bl)))
         (p1l   (ash (ldb (byte (- nbits nbh) 0) p1) nbh))
         (p1h   (ash p1 (- nbh nbits)))
         (p2    (* ah bh))
         (p2l   (ash (ldb (byte (- nbits nbh nbh) 0) p2) (+ nbh nbh)))
         (p2h   (ash p2 (- (+ nbh nbh) nbits)))
         (pl    (+ p0 p1l p2l))
         (ph    (+ p1h p2h)))
    (declare (integer m wrap a al ah b bl bh p0 p1 p1l p1h p2 p2l p2h)
             (fixnum nbits nbh))
    (fast-mod (+ pl (* wrap ph)))
    ))

#|
(modr (m* (1- *ed-r*) (1- *ed-r*)))
(modr (fast-mul (1- *ed-r*) (1- *ed-r*)))
(let* ((m (1+ (ash 1 128)))
       (n (- m (ash 1 126))))
  (with-mod m
    (fast-mul n n)))

;; WORST CASE IS VERY BAD... N iterations for N-bit modulus
;; worst case is Modulus = 2^N+1, for product 2^N * 2^N
|#
