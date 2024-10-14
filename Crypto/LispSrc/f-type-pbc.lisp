
(in-package :pbc)

(defstruct ecc-pt
  x y)

(defun ecc-infinity ()
  (make-ecc-pt
   :x 1
   :y 0))

(defun ecc-infinite-p (pt)
  (zerop (ecc-pt-y pt)))

(defun ecc-affine-double (pt)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x)
                                        (y1 y)) pt)
             (s  (m/ (m* 3 x1 x1) (m* 2 y1)))
             (x2 (m- (m* s s) x1 x1))
             (y2 (m- (m* s (m- x1 x2)) y1)))
    (make-ecc-pt :x x2 :y y2)) )


(defun ecc-affine-add (pt1 pt2)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x)
                                        (y1 y)) pt1)
             (:struct-accessors ecc-pt ((x2 x)
                                        (y2 y)) pt2))
    (cond
     ((zerop y1) pt2)
     ((zerop y2) pt1)
     ((= x1 x2)
      (cond ((= y1 y2)
             (ecc-affine-double pt1))
            ((= y1 (m- y2))
             (ecc-infinity))
            (t (error "ecc-affine-add: points not on curve"))
            ))
     (t
        (let* ((s      (m/ (m- y2 y1) (m- x2 x1)))
               (x3     (m- (m* s s) x1 x2))
               (y3     (m- (m* s (m- x1 x3)) y1)))
          (make-ecc-pt :x x3 :y y3)))
       )))

(defun ecc-mul (pt n)
  ;; left-to-right algorithm
  ;; assumes n has been wrapped modulo *ecc-r*
  ;; input pt is in affine coords, result is in affine coords
  (cond ((zerop n)  (ecc-infinity))
        
        ((or (= n 1)
             (ecc-infinite-p pt))  pt)
        
        (t  (let* ((r0  pt)
                   (l   (1- (integer-length n)))
                   (v   (vector r0
                                r0)))
              (loop repeat l do
                    (decf l)
                    (setf (aref v 1)                  (ecc-affine-double (aref v 1))
                          (aref v (ldb (byte 1 l) n)) (ecc-affine-add (aref v 1) r0)))
              (aref v 1)))
        ))

(defun horner (coffs x)
  (um:nlet iter ((coffs coffs)
                 (ans   0))
    (if (endp coffs)
        ans
      (go-iter (cdr coffs) (+ (car coffs)
                           (* x ans))))))

(defun gen-f (mbits)
  "Generate curve of embedding order k = 12, from
    'Pairing-Friendly Elliptic Curves of Prime Order'
      by Barreto and Naehrig -
   Curve has equation: y^2 = x^3 + b"
  (labels ((poly (x)
             (horner '(36 36 24 6 1) x)))
    (let* ((nb  (truncate mbits 4))
           (limit (ash 1 mbits))
           (x   (um:nlet iter ((x    0)
                                    (pos  nb))
                  (setf x (dpb 1 (byte 1 pos) x))
                  (when (< mbits (integer-length (poly (- x))))
                    (setf x (dpb 0 (byte 1 pos) x)))
                  (decf pos)
                  (if (minusp pos)
                      x
                    (go-iter x pos))))
           (tt nil)
           (p  nil)
           (n  nil))
      #|
           ;; ----------------------------------------
           ;; code to search for any suitable curve
           
      (um:nlet iter ()
        (setf tt (1+ (* 6 x x))
              p (poly (- x))
              n (- (1+ p) tt))
          (unless (and (primes:is-prime? p)
                       (primes:is-prime? n)
                       (= 3 (mod p 4))
                       (= 4 (mod p 9)))
            (setf p (poly x)
                  n (- (1+ p) tt))
            (unless (and (primes:is-prime? p)
                         (primes:is-prime? n)
                         (= 3 (mod p 4))
                         (= 4 (mod p 9)))
              (incf x)
              (go-iter))))
      (with-mod p
        (um:nlet iter ((b  0))
          (let* ((new-b (um:nlet iterb ((b b))
                          (let ((b+1 (1+ b)))
                            (if (quadratic-residue-p b+1)
                                b
                              (go-iterb b+1)))))
                 (y     (msqrt (1+ new-b)))
                 (pt    (make-ecc-pt
                         :x 1
                         :y y)))
            (if (ecc-infinite-p (ecc-mul pt n))
                (list :n   n
                      :p   p
                      :b   new-b
                      :y   y)
              (go-iter (1+ new-b))))))
      |#

      ;; ---------------------------------------------
      ;; get as close as we can to 2^mbits-1 with p,n
      ;; making sure than p mod 4 = 3 and p mod 9 = 4
      ;; for b = 3 => point (1,2) on curve
      
      (um:nlet outer ()
        (um:nlet iter ()
          (setf tt (1+ (* 6 x x))
                p (poly x)
                n (- (1+ p) tt))
          (unless (and (< p limit)
                       (primes:is-prime? p)
                       (primes:is-prime? n)
                       (= 3 (mod p 4))
                       (= 4 (mod p 9)))
            (setf p (poly (- x))
                  n (- (1+ p) tt))
            (unless (and (primes:is-prime? p)
                         (primes:is-prime? n)
                         (= 3 (mod p 4))
                         (= 4 (mod p 9)))
              (decf x)
              (go-iter))))
        (with-mod p
          (let ((pt  (make-ecc-pt
                      :x 1
                      :y 2)))
            (if (ecc-infinite-p (ecc-mul pt n))
                (pprint (list :n   n
                              :p   p
                              :b   3
                              :y   2
                              :xgen x
                              :tt   tt))
              (progn
                (decf x)
                (go-outer))
              )))
        ))))
      
    