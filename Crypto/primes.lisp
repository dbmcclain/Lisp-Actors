;; primes.lisp -- generation and testing of prime numbers
;; --------------------------------------------------------------------------------------
;; DAM Files -- direct access disk-based hash tables.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

;; -------------------------------------------
(in-package #:primes)
;; -------------------------------------------
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0))
          (inline empty singleton create))

;; -----------------------------------------------------------------------------
;;

(defun mt-random (ix)
  (declare (integer ix))
  ;; return a random integer 0 <= val < ix
  #+:LISPWORKS (ecc-crypto-b571:basic-random ix)
  #+:ALLEGRO   (random ix);; Allegro uses Mersenne Twister already
  #+sbcl (random ix)   ;; <http://www.sbcl.org/manual/#Random-Number-Generation>
  #-(OR :LISPWORKS :ALLEGRO :SBCL) (error "Not-yet-implemented"))

(defun random-between (lower upper)
  ;; generate random integer in [lower, upper)
  (ecc-crypto-b571:basic-random-between lower upper))

;; -----------------------------------------------------------------------------

(declaim (inline 2^ divides? add-mod sub-mod mult-mod div-mod))

(defun 2^ (n)
  (declare (fixnum n))
  (ash 1 n))

(defun divides? (n d)
  (declare (integer n d))
  (and (<= d n)
       (zerop (mod n d))))

(defun add-mod  (a b m)
  (declare (integer a b m))
  (mod (+ a b) m))

(defun sub-mod  (a b m)
  (declare (integer a b m))
  (mod (- a b) m))

(defun mult-mod (a b m)
  (declare (integer a b m))
  (mod (* a b) m))

(defun inv-mod (a n)
  ;; 1/a mod n
  ;; assumes n is prime
  ;; solve Bezouts identity: a*x + b*y = gcd(a,b)
  ;; for (x, y) => x is the modular multiplicative inverse of a modulo b,
  ;; when a coprime to b.
  (declare (integer a n))
  (unless (< a n)
    (error "INV-MOD A not less than Modulus"))
  (um:nlet iter ((i  n)
                 (j  a)
                 (y1 1)
                 (y2 0))
    (declare (integer i j y2 y1))
    (multiple-value-bind (quo rem) (truncate i j)
      (declare (integer quo rem))
      (cond  ((zerop rem)
              (if (= j 1)
                  (mod y1 n)
                (error "INV-MOD: no inverse exists")))

             (t (go-iter j rem (- y2 (* y1 quo)) y1))
             ))))

(defun div-mod (n d m)
  (declare (integer n d m))
  (mult-mod n (inv-mod d m) m))

(defun expt-mod (b n m)
  ;; b^n mod m
  (declare (integer b n m))
  (let* ((bb  (if (minusp n)
                  (inv-mod b m)
                (mod b m)))
         (np   (abs n))
         (nsize (integer-length np)))
    (declare (integer bb np)
             (fixnum nsize))
    (um:nlet iter ((b  bb)
                   (p  1)
                   (ix 0))
      (declare (integer b p)
               (fixnum ix))
      (if (< ix nsize)
          (go-iter (mult-mod b b m)
                   (if (logbitp ix n)
                       (mult-mod p b m)
                     p)
                   (1+ ix))
        ;; else
        p))))

;; ----------------------------------------------------------

#|
(defvar *primes*
  ;; first 168 primes, good for testing up to 1,000,000.
  (do ((s   '(19 17 13 11 7 5 3 2))
       (ix   23 (+ ix 2)))
      ((>= ix 1000) (nreverse s))
    (unless (member ix s
                    :test #'divides?)
      (push ix s))
    ))
|#

(defvar *primes*
  ;; first 168 primes, good for testing up to 1,000,000.
  (do ((s   '(3 2)) ;; all other primes are (6*k +/- 1)
       (ix   6 (+ 6 ix)))
      ((>= ix 1000) (nreverse s))
    (declare (fixnum ix)
             (cons s))
    (let ((p (1- ix)))
      (declare (fixnum p))
      (unless (member p s
                      :test #'divides?)
      (push p s)))
    (let ((q (1+ ix)))
      (declare (fixnum q))
      (unless (member q s
                      :test #'divides?)
        (push q s))) ))

;; ----------------------------------------------------------

(defun make-prime (k &optional (mr-iters 50))
  "(make-prime k) - return a prime p, k < p < 2*k-2"
  ;; By Bertrand's Postulate, there is always at least one prime, p,
  ;; where k < p < 2*k-2, for k > 3
  (declare (integer k)
           (fixnum mr-iters))
  (cond ((< k 997)
         (let* ((start (position-if (um:curry '< k) *primes*))
                (end   (position-if (um:curry '< (* 2 (1- k))) *primes* :start start))
                (ps    (subseq *primes* start end)))
           (values (elt ps (mt-random (length ps)))
                   1)))

        (t
         (let* ((lower (1+ k))
                (upper (* 2 (1- k)))
                (niter 0)
                (n     (let ((x (random-between lower upper)))
                         (- x (mod x 6) 1))))
           
           ;; Since fundamental theorem states that pi(n) ~ n/ln(n),
           ;; or density of primes is inversely proportional to number of decimal digits,
           ;; on average about ln(k)/2 trials will be needed
           (labels ((tst1 (n)
                      (incf niter)
                      (and (is-prime? n mr-iters)
                           n))
                    (tst (n)
                      (or (tst1 n)
                          (tst1 (+ n 2)))))
             (values (or (um:nlet iter> ((n n))
                           (when (< n upper)
                             (or (tst n)
                                 (go-iter> (+ n 6)))))
                         (um:nlet iter< ((n (- n 6)))
                           (when (> n lower)
                             (or (tst n)
                                 (go-iter< (- n 6))))))
                     niter))))
        ))

#|
(defun collect-stats (n k)
  (let ((trials (loop repeat n collect (second (multiple-value-list (make-prime k))))))
    (plt:histogram 'plt2 trials
                   :clear t
                   :thick 2
                   :title  "Number of Probes to Find a 100-digit Prime"
                   :ytitle "PDF Density"
                   :xtitle "Number of PRNG Probes")))


(defun to-rrs (n ps)
  (um:nlet iter ((n  n)
                 (ps ps)
                 (ans nil))
    (if (zerop n)
        (nreverse ans)
      (multiple-value-bind (q r) (truncate n (car ps))
        (go-iter (- n (* r (car ps))) (cdr ps) (cons r ans)))
      )))

;; collect a bunch of primes around 2^32
(let* ((ps (um:nlet iter ((n (2^ 32))
                          (ans nil)
                          (ix 1))
             (if (< ix 16)
                 (let ((p (make-prime n)))
                   (go-iter (1+ p) (cons p ans) (1+ ix)))
               (nreverse ans))))
       (x  3618502788666131106986593281521497120414687020801267626233049500247285301239))

(um:nlet iter ((ix 1)
               (ans nil)
               (n 1))
  (if (> ix 20)
      (nreverse ans)
    (if (is-prime? (- (2^ 32) n))
        (go-iter (1+ ix) (cons n ans) (+ n 2))
      (go-iter ix ans (+ n 2)))))

(let ((b32 (2^ 32)))
  (um:nlet iter ((ix 1)
                 (ans nil)
                 (n 1))
    (if (> ix 20)
        (nreverse ans)
      (if (every (lambda (nn)
                   (= 1 (gcd (- b32 n) (- b32 nn))))
                 ans)
          (go-iter (1+ ix) (cons n ans) (+ n 2))
        (go-iter ix ans (+ n 2))))))

;; cs (5 17 65 99 107 135 153 185 209 267 299 315 353 369 387 419 467 483 527 629 635 639 645 657 677 705 713 743 819 849 855 869 923 929)
;; s.t. (2^32 - c) prime

(let* ((cs  '(5 17 65 99 107 135 153 185))
       (ms  (mapcar (lambda (c)
                      (- (2^ 32) c))
                    cs))
       (x  3618502788666131106986593281521497120414687020801267626233049500247285301239)
       (xs (um:nlet iter ((x  x)
                          (ms ms)
                          (ans nil))
             (if (endp ms)
                 ans
               (multiple-value-bind (q r) (truncate x (car ms))
                 (go-iter q (cdr ms) (cons (list r (car ms)) ans))))))
       (y  (um:nlet iter ((xs xs)
                          (ans 0))
             (if (endp xs)
                 ans
               (destructuring-bind (r m) (car xs)
                 (go-iter (cdr xs) (+ r (* m ans))))
               ))))
  (inspect xs)
  (= y x))

(defvar *mrs-cs*
  '(5 17 65 99 107
      135 153 185 209 267
      299 315 353 369 387
      419 467 483 527 629
      635 639 645 657 677
      705 713 743 819 849
      855 869 923 929))

(defvar *mrs-ms*
  (mapcar (lambda (c)
            (- #.(ash 1 32) c))
          (um:take 8 *mrs-cs*)))

(defvar *mrs-bigm*
  (reduce '* *mrs-ms*))

(defvar *mrs-ims*
  (let* ((mks (mapcar (um:curry 'truncate *mrs-bigm*) *mrs-ms*)))
    (mapcar (lambda (mk m)
              (* mk (inv-mod (mod mk m) m)))
            mks *mrs-ms*)))

(defun to-rns (bignum)
  (mapcar (lambda (m)
            (list (mod bignum m) m))
          *mrs-ms*))

(defun from-rns (rns)
  (let* ((as (mapcar 'car rns)))
    (mod (reduce '+ (mapcar '* as *mrs-ims*)) *mrs-bigm*)))

(defun to-mrs (bignum)
  (um:nlet iter ((x  bignum)
                 (ms *mrs-ms*)
                 (ans nil))
    (if (endp ms)
        ans
      (multiple-value-bind (q r) (truncate x (car ms))
        (go-iter q (cdr ms) (cons (list r (car ms)) ans)))
      )))

(defun from-mrs (mrs)
  (um:nlet iter ((xs mrs)
                 (ans 0))
    (if (endp xs)
        ans
      (destructuring-bind (r m) (car xs)
        (go-iter (cdr xs) (+ r (* m ans)))
        ))))

(defun rns-mul (a b)
  (mapcar (lambda (ax bx)
            (destructuring-bind (ar am) ax
              (destructuring-bind (br bm) bx
                (list (mod (* ar br) am) am))))
          a b))

(defun rns-add (a b)
  (mapcar (lambda (ax bx)
            (destructuring-bind (ar am) ax
              (destructuring-bind (br bm) bx
                (list (mod (+ ar br) am) am))))
          a b))

(defun rns-sub (a b)
  (mapcar (lambda (ax bx)
            (destructuring-bind (ar am) ax
              (destructuring-bind (br bm) bx
                (list (mod (- ar br) am) am))))
          a b))
|#

;; ----------------------------------------------------------
#|
(defun is-prime? (p)
  (let ((p-1 (1- p)))
    (labels ((witness? (witness)
               (/= 1 (expt-mod witness p-1 p))))
      
      (cond ((<= p 1000000)
             (or (member p *primes*)
                 (not (member p *primes*
                              :test #'divides?))
                 ))

            ((evenp p) nil) ;; no higher primes are even
          
            ((zerop (mod p 5)) nil) ;; no higher primes end in 5
        
            ((some #'witness? *primes*) nil) ;; lower primes are highly likely witnesses
            
            (t
             ;; use probabilistic Fermat's test for certainty < 1/2^100
             ;; if p prime then a^(p-1) mod p = 1, for 1 <= a < p
             ;;
             ;; NOTE: this fails if p is a Charmichael number = a composite
             ;; number for which a^(p-1) mod p = 1 whenever gcd(a,p) = 1
             (um:nlet iter ((ntests 100)
                            (seen   *primes*))
               (if (zerop ntests)
                   t ;; maybe...
                 (let ((a (+ 4 (mt-random (- p 5)))))
                   (if (member a seen)
                       (go-iter ntests seen)
                     (if (witness? a)
                         nil
                       (go-iter (1- ntests) (cons a seen))) )))))
            ))))
                              
;; --------------------------------------------------------------

(defun jacobi-symbol (m n &optional (prod 1))
  ;; requires n odd
  (assert (oddp n))
  (if (> m n)
      (jacobi-symbol (mod m n) n prod)
    
    (cond
     ((= 1 n) prod)
     
     ((= 1 m)
      (if (logbitp 1 n)
          (- prod)
        prod))
     
     ((= 2 m)
      (if (member (mod n 8) '(1 7))
          prod
        (- prod)))
     
     ((evenp m)
      (jacobi-symbol (ash m -1) n (jacobi-symbol 2 n prod)))
     
     ((/= 1 (gcd m n)) 0)
     
     (t 
      (jacobi-symbol n m
                     (if (and (= 3 (mod n 4))
                              (= 3 (mod m 4)))
                         (- prod)
                       prod)))
     )))

;; --------------------------------------------------------------------

(defun solovay-strassen-prime? (p &optional (k 100))
  ;; false positive < 1/2^k
  ;; require p odd > 2
  (assert (oddp p))
  (assert (> p 2))
  (um:nlet iter ((niter k))
    (if (zerop niter)
        t
      (let ((a (+ 2 (mt-random (- p 2)))))
        (when (= 1 (gcd a p))
          (let ((jac  (jacobi-symbol a p))
                (pwr  (expt-mod a (ash p -1) p)))
            (when (= jac pwr)
              (go-iter (1- niter))) ))) )))
|#

#|
(defun random-in-range (min max)
  "Return a random integer in the interval [min .. max]"
  (declare (integer min max))
  (+ min (mt-random (1+ (- max min)))))
|#

#|
(defun make-unique-random-in-range-fn (min max)
  (declare (integer min max))
  (let ((seen  nil)
        (limit (1+ (- max min))))
    (declare (integer limit))
    (lambda ()
      (um:nlet iter ()
        (let ((n (mt-random limit)))
          (declare (integer n))
          (if (member n seen :test #'=)
              (go-iter)
            (progn
              (push n seen)
              (+ min n))) ))) ))
|#

(defun factor-out (number divisor)
  "Return two values R and E such that NUMBER = DIVISOR^E * R,
  and R is not divisible by DIVISOR."
  (declare (integer number divisor))
  (do ((e 0 (1+ e))
       (r number (/ r divisor)))
      ((/= (mod r divisor) 0) (values r e))
    (declare (integer e r))
    (values r e)))

(defun perfect-square? (c)
  (declare (integer c))
  (let ((r (isqrt c)))
    (= c (* r r))))

#|
(defun perfect-square? (c)
  ;; even if c really is a perfect square, this can often fail
  (let* ((n   (integer-length c))
         (m   (ceiling n 2))
         (lim (2^ m))
         (ix  0))
    (labels ((guess (&optional x)
               (if x
                   (let ((x1 (truncate (+ c (* x x)) (ash x 1))))
                     (print (list (incf ix) x1))
                     x1)
                 (let ((x1 (random-between (ash lim -1) lim)))
                   (print (list (incf ix) x1))
                   x1)
                 )))
      (do* ((x-2 nil x-1)
            (x-1 nil x)
            (x  (guess) (guess x)))
          ((or (eql x x-1)
               (eql x x-2))
           (values (= c (* x x)) x))
        ))))
|#
(defun jacobi-symbol (a n)
  (declare (integer a n))
  (let ((a (mod a n)))
    (cond
     ((or (= 1 a)
          (= 1 n))
      1)
     
     ((zerop a) 0)
     
     (t (multiple-value-bind (a1 e) (factor-out a 2)
          (let* ((rem (mod n 8))
                 (s 0))
            (declare (integer rem s))

            (cond
              ((evenp e) 
               (setq s 1))
              ((or (= 1 rem)
                   (= 7 rem)) 
               (setq s 1))
              ((or (= 3 rem)
                   (= 5 rem)) 
               (setq s -1)))

            (when (and (= 3 (mod n 4))
                       (= 3 (mod a1 4)))
              (setf s (- s)))

            (* s (jacobi-symbol (mod n a1) a1))))))))
                 
(defun probabilistic-lucas-test (c)
  (declare (integer c))
  (if (or (evenp c)
          (perfect-square? c))
      nil
    (let* ((d (loop for sign = 1 then (- sign)
                    for d = 5 then (* sign (+ 2 (abs d)))
                    for jac = (jacobi-symbol d c)
                    do
                    (cond ((zerop jac) (return-from probabilistic-lucas-test nil))
                          ((= -1 jac)  (return d)))))
           (k  (1+ c)))
      (declare (integer d k))
      
      (labels ((half-mod (x)
                 (declare (integer x))
                 (mod (ash (if (oddp x)
                               (+ x c)
                             x)
                           -1)
                      c)))
        
        (um:nlet iter ((u  1)
                       (v  1)
                       (ix (- (integer-length k) 2)))
          (declare (integer u v ix))
          (if (minusp ix)
              (zerop u)
            (let ((utmp (mult-mod u v c))
                  (vtmp (half-mod (+ (* v v) (* d u u)))))
              (declare (integer utmp vtmp))
              (if (logbitp ix k)
                  (go-iter (half-mod (+ utmp vtmp))
                           (half-mod (+ vtmp (* d utmp)))
                           (1- ix))
                (go-iter utmp vtmp (1- ix))) ))) ))))
                
      
#|
(defun strong-miller-rabin-liar? (d s p p-1 a)
  ;; return true if a not not witness a composite value for p
  ;; p-1 = d * 2^s
  (let ((x (expt-mod a d p)))
    (or (= x 1)
        (loop repeat s
              for y = x then (mod (* y y) p)
              thereis (= y p-1))) ))
|#

(defun make-strong-miller-rabin-liar-test (p)
  (declare (integer p))
  (let ((p-1 (1- p)))
    (declare (integer p-1))
    (multiple-value-bind (d s) (factor-out p-1 2)
      ;; p-1 = d * 2^s, d odd
      (declare (integer d s))
      
      (lambda (a)
        ;; return true if a not not witness a composite value for p
        (declare (integer a))
        (let ((x (expt-mod a d p)))
          (declare (integer x))
          (or (= x 1)
              (loop repeat s
                    for y of-type integer = x then (the integer (mod (the integer (* y y)) p))
                    thereis (= y p-1))) )))))

(defun probabilistic-miller-rabin-prime? (p &optional (k (min 50 (- p 2))))
  ;; false positive < 1/4^k
  ;; return NIL if p found to be composite
  ;; assumes p odd > 2
  (declare (integer p k))
  (let ((p-1 (1- p)))
    (declare (integer p-1))
    (assert (> p-1 k))
    (let ((strong-liar? (make-strong-miller-rabin-liar-test p)))
      (loop repeat k
            for witness of-type integer = (random-between 2 p)
            always (funcall strong-liar? witness)) )))

(defun deterministic-miller-rabin-prime? (p witnesses)
  (declare (integer p)
           (list witnesses))
  ;; return NIL if p found to be composite on the basis of the listed witnesses
  ;; assumes p odd > 2
  (let ((strong-liar? (make-strong-miller-rabin-liar-test p)))
    (loop for witness of-type integer in witnesses
          always (funcall strong-liar? witness)) ))

(defun miller-rabin-prime? (p &optional (mr-iters 50))
  ;; assumes p odd > 3
  (declare (integer p)
           (fixnum mr-iters))
  (or (and (< p 14)
           (fast-locate p '(2 3 5 7 11 13)))
      (and (= 1 (gcd p 30030))
           (cond
            ((< p         1373653) (deterministic-miller-rabin-prime? p '(2 3)))
            ((< p         9080191) (deterministic-miller-rabin-prime? p '(31 73)))
            ((< p      4759123141) (deterministic-miller-rabin-prime? p '(2 7 61)))
            ((< p   2152302898747) (deterministic-miller-rabin-prime? p '(2 3 5 7 11)))
            ((< p   3474749660383) (deterministic-miller-rabin-prime? p '(2 3 5 7 11 13)))
            ((< p 341550071728321) (deterministic-miller-rabin-prime? p '(2 3 5 7 11 13 17)))
            (t                     (and (probabilistic-miller-rabin-prime? p mr-iters)
                                        (probabilistic-lucas-test p)))
            ))))


(defun fast-locate (p tbl)
  ;; assumes tbl is in ascending order
  (um:nlet iter ((tbl tbl))
    (when tbl
      (destructuring-bind (hd &rest tl) tbl
        (let ((diff (- p hd)))
          (or (zerop diff)
              (and (plusp diff)
                   (go-iter tl))))
        ))))

(defun is-prime? (p &optional (mr-iters 50))
  (declare (integer p)
           (fixnum mr-iters))
  (cond
   ((< p 2)   nil)
   ((< p 4)   t)
   ((evenp p) nil)
   ((/= 1 (mod (* p p) 24)) nil)
   ((< p 1000)
    (fast-locate p *primes*))
   (t         (miller-rabin-prime? p mr-iters))
   ))

(defun find-generator (p)
  (assert (is-prime? p))
  (let* ((p-1 (1- p)))
    (multiple-value-bind (fs q) (factors-of p-1)
      (um:nlet outer ((g  (random-between 2 p)))
        (if (= 1 (expt-mod g (/ p-1 q) p))
            (go-outer (random-between 2 p))
          (um:nlet inner ((s fs))
            (if (endp s)
                g
              (let ((q (caar s)))
                (if (= 1 (expt-mod g (/ p-1 q) p))
                    (go-outer (random-between 2 p))
                  (go-inner (cdr s))
                  ))))
          )))))

(defun find-small-generator (p)
  (assert (is-prime? p))
  (let* ((p-1 (1- p)))
    (multiple-value-bind (fs q) (factors-of p-1)
      (um:nlet outer ((gs  *primes*))
        (if (endp gs)
            (error "can't find small generator")
          (let ((g  (car gs)))
            (if (= 1 (expt-mod g (/ p-1 q) p))
                (go-outer (cdr gs))
              (um:nlet inner ((s fs))
                (if (endp s)
                    g
                  (let ((q (caar s)))
                    (if (= 1 (expt-mod g (/ p-1 q) p))
                        (go-outer (cdr gs))
                      (go-inner (cdr s))
                      ))))
              )))))))

(defun find-small-generator-with-factors (p factors)
  (assert (is-prime? p))
  (let* ((p-1 (1- p)))
    (um:nlet outer ((gs  *primes*))
      (if (endp gs)
          (error "can't find small generator")
        (let ((g  (car gs)))
          (um:nlet inner ((s factors))
            (if (endp s)
                g
              (let ((q (car s)))
                (if (= 1 (expt-mod g (/ p-1 q) p))
                    (go-outer (cdr gs))
                  (go-inner (cdr s))
                  ))))
          )))))
#|
  (1- *ed-r*) curve1174
  2^4 3^2 5^2 7 17 5662229 53308853141 60171084739669153 152435752726607681 762707485068427817
(find-small-generator-with-factors edec:*ed-r* '(2 3 5 7 17 5662229 53308853141 60171084739669153 152435752726607681 762707485068427817))
(find-small-generator-with-factors edec:*ed-q* '(2 19 3121 30510656070643106182115999270826633842178510774222732476374386585332681))

  |#
;; ------------------------------------------------------------

(defun extended-gcd (a b)
  ;; solve Bezout's identity: a*x + b*y = gcd(a,b)
  ;; for (x, y) => x is the modular multiplicative inverse of a modulo b,
  ;; when a coprime to b.
  (declare (integer a b))
  (if (zerop (mod a b))
      (values 0 1)
    (multiple-value-bind (x y) (extended-gcd b (mod a b))
      (declare (integer x y))
      (values y
              (- x (* y (truncate a b)))) )))

(defun compute-modulo-inverse (a m)
  (declare (integer a m))
  (assert (= 1 (gcd a m))) ;; ensure A coprime with M
  (mod (extended-gcd a m) m))

;; ---------------------------------------------------------------
#|
(defun tst (n)
  (let ((u (expt 65537 10)))
    (loop repeat n
          for ix from 1
          do
          (let ((y (1+ (* 2 ix u))))
            (when (is-prime? y)
              (print y)
              (force-output))))))
|#
;; ---------------------------------------------------------------
;; (defvar *niter* 0)

(defun test-primitive-root (n factors x)
  (declare (integer n x)
           (list factors))
  ;; check to see if x is a primitive root of prime n
  ;; where factors are the prime factors of phi(n) = (n-1)
  (and
   (= 1 (gcd x n))                  ;; - coprime test S.B. true for all X - REDUNDANT
   (= 1 (expt-mod x (1- n) n))      ;; - Fermat's test S.B. true for all X
   (every (lambda (factor) ;; - Lucas' test S.B. true for some X
            (declare (cons factor))
            (/= 1 (expt-mod x (truncate (1- n) (the integer (first factor))) n)))
          factors)
   x)) ;; return the witness as true
  
(defun lucas-lehmer-test (n factors &optional (limit 20))
  (declare (integer n)
           (fixnum limit)
           (list factors))
  ;; Lucas-Lehmer test for N = prod(factors)+1 as prime,
  ;; for FACTORS a list of prime factors of (N-1).
  ;; If passes test, then N is absolutely prime and we return a witness number.
  ;; Otherwise it is probably composite and we return NIL.
  ;;
  ;; This test can give a false positive on composite numbers if
  ;; the list of factors of (N-1) include composite numbers.
  ;; So be certain that factors are all prime numbers.

  ;; (incf *niter*)
  (or ;; (test-primitive-root n factors 2)
      ;; (test-primitive-root n factors 5)
      (um:nlet iter ((limit limit))
        (declare (fixnum limit))
        (and (plusp limit)                    ;; - true if not exhausted the search
             (let ((x (random-between 2 (1- n))))
               (declare (integer x))
               ;; no need to look over whole range, since the upper half mirrors the lower half
               (or (test-primitive-root n factors x) ;; - Lucas' test S.B. true for some X
                   (go-iter (1- limit)))) ))))

(defun provably-prime? (n &optional absolute-proof)
  (declare (integer n))
  (lucas-lehmer-test n (factors-of (1- n) absolute-proof)))

#|
(defun collect-stats (n k)
  (let ((trials (loop repeat n collect
                      (let ((*niter* 0))
                        (provably-prime? (make-prime k))
                        *niter*)) ))
    (plt:histogram 'plt3 trials
                   :clear t
                   :thick 2
                   :cum    nil
                   :title  "Number of Probes in Lucas-Lehmer Test"
                   :ytitle "PDF Density"
                   :xtitle "Number of PRNG Probes")
    (values (float (vm:mean trials))
            (float (vm:stdev trials))) ))


(defun collect-stats (n k)
  (labels ((hit-test (&optional (niter 1))
               (if (divides? (mt-random k) 3)
                   niter
                 (hit-test (1+ niter)))))
    (let ((trials (loop repeat n collect
                        (hit-test))))
      (plt:histogram 'plt3 trials
                     :clear t
                     :thick 2
                     :cum    nil
                     :title  "Number of Probes in 1/3 Hit-Test"
                     :ytitle "PDF Density"
                     :xtitle "Number of PRNG Probes")
      (values (float (vm:mean trials))
              (float (vm:stdev trials))) )))
|#

#|
(defun small-composite? (n)
  ;; test for absolute prime N, for N < 1,000,000
  ;; by using direct division tests for primes in *PRIMES*
  (assert (< n 1000000))
  (let ((limit (isqrt n)))
    (some (lambda (x)
            (and (< x limit)
                 (divides? n x)))
          *primes*)))
|#

(defun factors-of (n &optional complete-factorization-p)
  (declare (integer n))
  ;; good for some N -- return a list of conses: ((factor1 . exponent1) (factor2 . exponent2) ...)
  ;;
  ;; It is possible that after partial factoring a residue (> 1) will remain.
  ;; 
  ;; If the residue IS NOT a probable prime, then it is definitely composite, and its factors
  ;; are larger than we can handle from *PRIMES*. So give up with an error report.
  ;; 
  ;; If the residue IS a probable prime then:
  ;;   - If the user asked for a complete factorization, attempt to recursively
  ;;       prove that the residue is prime, and then append that residue to the list of factors.
  ;;       If we are unable to prove that the residue is prime then it is too large for this routine.
  ;;
  ;;   - If the user did not ask for a complete factorization then just accept it as a
  ;;     probable prime factor, and append it to the list of factors.
  ;;
  (um:nlet iter ((n  n)
                 (ps *primes*)
                 (factors nil))
    (declare (integer n)
             (list factors ps))
    (if ps
        (let ((p (first ps)))
          (declare (integer p))
          (if (< n p)
              (nreverse factors)
            (if (divides? n p)
                (multiple-value-bind (d r) (factor-out n p)
                  (declare (integer d r))
                  (go-iter d (rest ps) (cons (cons p r) factors)))
              (go-iter n (rest ps) factors))))

      (labels ((residue-too-large ()
                 (values (nreverse factors) n nil))) ;; (error "Residue too large in FACTORS-OF: ~A ~A" n factors)))
    
        (cond
         ((= n 1) (nreverse factors))
         
         ((is-prime? n)
          (if complete-factorization-p
              ;; user asked for complete factorization. Try if possible.
              (if (provably-prime? n t)
                  (nreverse (cons (cons n 1) factors))
                ;; else - couldn't prove it, the residue is probably composite
                ;; and it must be too large for this routine
                (residue-too-large))
            (nreverse (cons (cons n 1) factors))))

         (t (let ((r (isqrt n)))
              (declare (integer r))
              (if (and (= n (* r r))
                       (is-prime? r))
                  (nreverse (cons (cons r 2) factors))
                (residue-too-large)))) ;; non-prime leftover -- routine can't handle it
         )))))

(defun totient (n)
  ;; Euler totient function: phi(n)
  ;; For primitive roots modulo n, there are phi(phi(n)) of them
  ;; For n prime, P, phi(P) = (P-1), and the primitive roots appear
  ;; more or less uniformly distributed over the interval 1 < x < P.
  (declare (integer n))
  (reduce (lambda (tot factor)
            (destructuring-bind (p . e) factor
              (* tot (1- p) (expt p (1- e)))))
          (factors-of n)
          :initial-value 1))

#|
(plt:plot 'plt
          *primes*
          (mapcar (lambda (p)
                    (float (/ (totient (1- p)) (1- p))))
                  *primes*)
          :clear t)
|#

;; ---------------------------------------------------------------
#|
(defun generate-strong-prime (ndigits &optional (nlevels 2))
  ;; a strong prime N has a large prime factor in N-1
  ;; NLEVELS indicates how many component (P-1) should be strong
  ;;  NLEVELS: T   = all levels should be strong and proven prime
  ;;           NIL = none should be strong -> probabilistic prime
  ;;           <integer> = nbr of strong component levels - proven or probabilistic result
  ;;                 If the printout shows: Make Base Prime: Absolute
  ;;                    then we will have a proven prime result.
  ;;                 Otherwise, we will have a probabilistic prime result.
  (labels ((find-incremental-prime (u)
             ;; start with large prime U, and compute 2*k*U+1 until prime
             (let ((incr (* 2 u u)))
               (loop for p from (1+ incr) by incr
                     for k from 1
                     until (and (is-prime? p)
                                (provably-prime? p)
                                (< (1- ndigits) (* 0.301 (integer-length p))))
                     ;; return a provably prime number, assuming u were prime
                     finally (return p))))
           
           (base-prime (ndigits)
             (format t "~&Make Base Prime: ")
             (loop for u = (make-prime (random-between (expt 10 ndigits) (* 5 (expt 10 ndigits))))
                   until (handler-case
                             (when (provably-prime? u t)
                               (format t "Absolute") ;; proven prime
                               t) ;; if no error, but not proven prime, then try again
                           (error (cx)
                             (declare (ignore cx))
                             ;; could not completely factor
                             (when (or (null nlevels)
                                       (integerp nlevels))
                               (format t "Probabilistic") ;; accept on probabilistic faith
                               t))) ;; otherwise, return nil to generate another trial prime
                   finally (return u))) )

    ;; get starting large prime
    (let ((u (if (or (< ndigits 32)
                     (and nlevels
                          (integerp nlevels)
                          (< nlevels 2))
                     (null nlevels)) ;; Null levels - accept base prime as U
                 (base-prime (truncate ndigits 2))
               ;; else, recursively generate strong prime for U
               (generate-strong-prime (truncate ndigits 2) (or (and (integerp nlevels)
                                                                    (1- nlevels))
                                                               nlevels))) ))
      (format t "~&Using: ~A" u)
      (find-incremental-prime u)) ))
|#

(defun generate-strong-prime (nbits &optional (nlevels 2))
  ;; a strong prime N has a large prime factor in N-1
  ;; NLEVELS indicates how many component (P-1) should be strong
  ;;  NLEVELS: T   = all levels should be strong and proven prime
  ;;           NIL = none should be strong -> probabilistic prime
  ;;           <integer> = nbr of strong component levels - proven or probabilistic result
  ;;                 If the printout shows: Make Base Prime: Absolute
  ;;                    then we will have a proven prime result.
  ;;                 Otherwise, we will have a probabilistic prime result.
  (declare (fixnum nbits))
  (labels ((find-incremental-prime (u)
             (declare (integer u))
             ;; start with large prime U, and compute 2*k*U+1 until prime
             (let ((incr (* 2 u u)))
               (declare (integer incr))
               (loop for p of-type integer from (1+ incr) by incr
                     for k of-type integer from 1
                     until (and (is-prime? p)
                                (provably-prime? p)
                                (< (1- nbits) (integer-length p)))
                     ;; return a provably prime number, assuming u were prime
                     finally (return p))))
           
           (base-prime (nbits)
             (declare (fixnum nbits))
             (format t "~&Make Base Prime: ")
             (loop for u of-type integer = (make-prime (let ((b (2^ nbits)))
                                                         (declare (integer b))
                                                         (random-between b (+ b b))))
                   until (handler-case
                             (when (provably-prime? u t)
                               (format t "Absolute") ;; proven prime
                               t) ;; if no error, but not proven prime, then try again
                           (error (cx)
                             (declare (ignore cx))
                             ;; could not completely factor
                             (when (or (null nlevels)
                                       (integerp nlevels))
                               (format t "Probabilistic") ;; accept on probabilistic faith
                               t))) ;; otherwise, return nil to generate another trial prime
                   finally (return u))) )

    ;; get starting large prime
    (let ((u (if (or (< nbits 107)
                     (and nlevels
                          (integerp nlevels)
                          (< nlevels 2))
                     (null nlevels)) ;; Null levels - accept base prime as U
                 (base-prime (- (truncate nbits 2) 3))
               ;; else, recursively generate strong prime for U
               (generate-strong-prime (- (truncate nbits 2) 3) (or (and (integerp nlevels)
                                                                        (1- nlevels))
                                                                   nlevels))) ))
      (format t "~&Using: (~A) ~A" (integer-length u) u)
      (find-incremental-prime u)) ))

;; ----------------------------------------------------------------------------------           

(defun compute-diffie-hellman (nbits)
  (declare (fixnum nbits))
  (let* ((p       (generate-strong-prime nbits t))
         (factors (factors-of (1- p)))
         (g       (lucas-lehmer-test p factors))) ;; returns a primitive root if successful
    (declare (integer p))
    (if g ;; if lucas-lehmer failed then try again
        (list :DH-P p :DH-G g)
      (compute-diffie-hellman nbits))))

;; ----------------------------------------------------------------------------------           

(defun compute-rsa-d&e (p q)
  (declare (integer p q))
  (let ((phi    (* (1- p) (1- q)))
        (thresh (integer-length (* p q)))) ;; threshold for e greater than this for good mixing
    (declare (integer phi)
             (fixnum thresh))
    (loop for d of-type integer = (make-prime (1+ (max p q)))
          for e of-type integer = (inv-mod d phi)
          do
          (when (> e thresh)
            (return (values d e)))
          (when (> d thresh)
            (return (values e d)) ))))

(defun generate-rsa-base (nbits)
  (declare (fixnum nbits))
  (let* ((nd1 (- (floor nbits 2) 8))
         (p   (generate-strong-prime nd1))
         (nd2 (+ (ceiling nbits 2) 8))
         (q   (generate-strong-prime nd2)))
    (declare (integer p q)
             (fixnum nd1 nd2))
    (multiple-value-bind (d e)
        (compute-rsa-d&e p q)
      (declare (integer d e))
      ;; perform trial encryptions/decryptions to verify goodness
      (loop repeat 100 do
            (let ((msg (random-between 2 (* p q))))
              (declare (integer msg))
              (assert (= msg (expt-mod (expt-mod msg e (* p q)) d (* p q))))))
      (list :N-Key (* p q) ;; Public N-Key
            :E-Key e ;; Public E-Key
            :D-Key d ;; Private D-Key
            :Primes  (list p q) ;; Private prime factors of N-Key
            :Phi-GCD (gcd (1- p) (1- q)) ;; verify that GCD is small
            ))))

(defvar *daves-N-Key* ;; public
  2264622332100115635046544861457968154487858357629655981804341141586114228298294029783844324050989940210772053439566053045896814644059794725879563686363912167075068867395096157661687366233697816122565111)

(defvar *daves-E-Key* ;; public
  246217118187169985272570746886853299629231075233304175666349360972368752967752016255671063892504959532124058155571361367065153880715972807546628534121821786783732922864646413843902219977232097825366741)

;; ------------------------------------

(defvar *daves-D-Key* ;; private
  3336134262755358265069226579401091710115226108761764767081277708392567434516013871621774832824438208861)

(defvar *daves-P-prime* ;; private
  1213152860237679366017990949504570398999532153649335816658883894008526264696809742265586934057363591)

(defvar *daves-Q-prime* ;; private
  1866724636544510727941144178801761797219686359320662768042557145048836731901008435492740832105185576721)

;; --------------------------------------------------------------------------------
#|
(um:nlet iter ((ps *primes*)
               (coll nil))
  (if (rest ps)
      (let ((p (first ps)))
        (do ((qs (rest ps) (cdr qs)))
            ((null qs))
          (let* ((q (first qs))
                 (n (1+ (* p q))))
            (if (is-prime? n)
                (push n coll)) )))
        (go-iter (rest ps) coll))
    coll))
|#

;; ----------------------------------------------------------------------------------

(defun generate-safe-prime (nbits)
  (declare (fixnum nbits))
  ;; a safe prime is of the form (2*U+1) where U is a large prime.
  (let* ((q0  (random-between (2^ (- nbits 2)) (2^ (- nbits 1))))
         (q  (+ q0 (- 5 (mod q0 6))))
         (p  (+ 1 q q)))
    (declare (integer p q0 q))
    (do  ((q  q  (+ q 6))
          (p  p  (+ p 12)))
        ((and (is-prime? q)
              (is-prime? p)) p)
        (declare (integer p q))
      )))

#+(AND :COM.RAL :LISPWORKS :ACTORS)
(defun par-attack (fn &optional timeout)
  (let ((mbox  (mp:make-mailbox))
        (procs nil))
    (flet ((trial ()
             (mp:mailbox-send mbox (funcall fn))))
      (dotimes (ix 16)
        (push (ac:spawn-worker #'trial) procs))
      (prog1
          (mp:mailbox-read mbox nil timeout)
        (dolist (proc procs)
          (ac:terminate-actor proc)))
      )))

#+(AND :COM.RAL :LISPWORKS :ACTORS)
(defun par-gen-safe-prime (nbits)
  (declare (fixnum nbits))
  (par-attack (lambda ()
                (generate-safe-prime nbits))))

#+(AND :COM.RAL :LISPWORKS :ACTORS)
(defun par-gen-safe-primes (nbits nprimes)
  (loop repeat nprimes collect (print (par-gen-safe-prime nbits))))

#|
(setf x
      (loop repeat 100 collect
            (let ((ans (generate-safe-prime 512)))
              (format t "~&~128,'0x" ans)
              ans)))

(defun gen-bunch (&optional (nbr 100))
  (let ((xv (make-array nbr)))
    (loop for ix from 0 below nbr do
          (setf (aref xv ix) ix))
    (um:npvmap (lambda (ix)
                 (let ((ans (generate-safe-prime 512)))
                   (format t "~&~3D ~128,'0x" ix ans)
                   ans))
               xv
               :grp (truncate nbr 4))
    xv))

(defvar *xv* (gen-bunch))

(setf xs (append x
                 (coerce *xv* 'list)
                 (bfly:!? "eval@Magnetite.local" '(coerce primes::*xv* 'list))
                 (bfly:!? "eval@Citrine.local"   '(coerce primes::*xv* 'list))
                 (bfly:!? "eval@Malachite.local" '(coerce primes::*xv* 'list))))

 |#
;; -----------------------------------------------------------------------------------

(defun show-group (n)
  (declare (integer n))
  (loop for ix from 2 below (1- n) ;; to (truncate (1- n) 2)
        collect
        (loop for k from 1 below n collect (expt-mod ix k n))))


(defun show-gens (n)
  (declare (integer n))
  (let ((factors (factors-of (1- n))))
    (loop for ix of-type integer from 2 below (ceiling n 2)
          for g of-type integer = (test-primitive-root n factors ix)
          when g collect g)))
        
;; -----------------------------------------------------------------------------------

(defun convert-bytes-to-integer (v)
  (let* ((v  (coerce v 'vector))
         (nb (length v))
         (n  0))
    (declare (integer n)
             (fixnum nb))
    (loop for iy of-type fixnum from (* 8 (1- nb)) downto 0 by 8
          for ix of-type fixnum from 0
          do
          (setf (ldb (byte 8 iy) n) (aref v ix)))
    n))

(defun convert-integer-to-vector (x)
  (declare (integer x))
  (let* ((nb (ceiling (integer-length x) 8))
         (v  (make-array nb :element-type '(unsigned-byte 8))))
    (declare (fixnum nb)
             (vector v))
    (loop for ix of-type fixnum from (* 8 (1- nb)) downto 0 by 8
          for iy of-type fixnum from 0
          do
          (setf (aref v iy) (ldb (byte 8 ix) x)))
    v))

;; -----------------------------------------------

(defmethod integer-of ((x integer))
  x)

(defmethod integer-of ((v vector))
  (convert-bytes-to-integer v))

(defmethod integer-of ((u uuid:uuid))
  (uuid:uuid-to-integer u))

(defmethod integer-of ((s string))
  (integer-of (vector-of s)))

;; ------------

(defmethod vector-of ((v vector))
  v)

(deftype vector-ub8 ()
  `(simple-array (unsigned-byte 8) (*)))

(defun string-to-vector (str)
  (map 'vector-ub8 'char-code str))

(defmethod vector-of ((s string))
  (string-to-vector s))

(defmethod vector-of ((u uuid:uuid))
  (uuid:uuid-to-byte-array u))

(defmethod vector-of ((x integer))
  (convert-integer-to-vector x))

;; -----------------------------------------------

#|
(defun make-nbit-prime (nbits hash-type)
  (declare (fixnum nbits))
  (assert (>= (* 8 (ironclad:digest-length hash-type)) nbits))
  (let* ((lower (2^ nbits))
         (upper (+ lower lower))
         (mask  (1- (ash lower -1)))
         (base  (+ mask 2)))
    (loop for seed of-type integer = (random-between lower upper)
          for u of-type integer = (let ((dig (ironclad:make-digest hash-type)))
                                    (ironclad:update-digest dig (vector-of seed))
                                    (logand (integer-of (ironclad:produce-digest dig)) mask))
          for q of-type integer = (- (+ base u) (logand u 1))
          when (is-prime? q)
          do (return (values q seed))) ))
|#

(defun gen-dsa-primes (nbits lbits hash-type)
  (declare (fixnum nbits lbits))
  (let* ((hbits (* 8 (ironclad:digest-length hash-type)))
         (hfact (2^ hbits))
         (n     (1- (ceiling lbits hbits)))
         (b     (- lbits 1 (* n hbits)))
         (bmask (1- (2^ b)))
         (2^lm1 (2^ (1- lbits)))
         (hmask (1- (2^ nbits))))
    (declare (fixnum hbits n b)
             (integer hfact bmask 2^lm1 hmask))
    (loop for (q seed) = (multiple-value-list (make-nbit-prime nbits hash-type))
          do
          (format t "~&Q = ~A" q)
          (loop for counter of-type fixnum from 0 below (* 4 lbits)
                for offset of-type fixnum from 1 by (1+ n)
                for v = (make-array (1+ n))
                do
                (loop for j of-type fixnum from 0 to n do
                      (setf (aref v j) (let ((dig (ironclad:make-digest hash-type)))
                                         (ironclad:update-digest dig
                                                                 (vector-of
                                                                  (logand (+ seed offset j)
                                                                          hmask)))
                                         (integer-of (ironclad:produce-digest dig)) )))
                (let* ((w (loop for ix from 0 to n
                                for k = 1 then (* k hfact)
                                sum (* k (let ((x (aref v ix)))
                                           (if (= ix n)
                                               (logand x bmask)
                                             x)))))
                       (x (+ w 2^lm1))
                       (c (mod x (* 2 q)))
                       (p (- x (1- c))))
                  (declare (integer w x c p))
                  (when (and (>= p 2^lm1)
                             (is-prime? p))
                    (return-from gen-dsa-primes (values p q)) ))) )))
                  

;; -------------------------------------------------------

(defun make-2kp+1-prime (nbits &optional (mr-iters 50))
  (declare (fixnum nbits mr-iters))
  ;; a safe prime is of the form (2*k*U+1) where U is a large prime, and k is a small multiplier.
  (loop for base of-type integer = (random-between (2^ (- nbits 8)) (2^ (- nbits 2)))
        for u of-type integer = (make-prime base mr-iters)
        do
        (let ((incr (+ u u)))
          (declare (integer incr))
          (um:nlet iter ((p  (1+ incr))
                         (ct 1))
            (declare (integer p)
                     (fixnum ct))
            (let ((nb (integer-length p)))
              (declare (fixnum nb))
              (cond ((> ct 100) nil)
                    ((< nb nbits) (go-iter (+ p incr) (1+ ct)))
                    ((= nb nbits) (if (is-prime? p mr-iters)
                                      (return-from make-2kp+1-prime p)
                                    (go-iter (+ p incr) (1+ ct))))
                    (t nil)) )) )) )

#+(AND :COM.RAL :LISPWORKS)
(defun par-make-2kp+1-prime (nbits &optional (mr-iters 50))
  (declare (fixnum nbits mr-iters))
  (par-attack (lambda ()
                (make-2kp+1-prime nbits mr-iters))
              300))

(defun make-nbit-random (nbits)
  (random-between (2^ (1- nbits)) (1- (2^ nbits))))

#|
(defun make-rigid-prime (nbits &key (mr-iters 50) deterministic)
  (declare (fixnum nbits mr-iters))
  ;; a rigid prime is of the form p = (2*u+1) where u is also prime
  (let* ((p  (if deterministic
                 (vec-repr:int (hash:get-hash-nbits (1- nbits) deterministic))
               (make-nbit-random (1- nbits))))
         (p  (+ p (- 5 (mod p 6)))))
    (um:nlet iter ((p  p))
      (if (is-prime? p mr-iters)
          (let ((2p+1 (1+ (ash p 1))))
            (if (is-prime? 2p+1 mr-iters)
                2p+1
              (go-iter (+ p 6))))
        (go-iter (+ p 6))))
    ))
|#

(defun make-strong-prime (nbits &key (mr-iters 50) deterministic)
  (declare (fixnum nbits mr-iters))
  ;; a strong prime is of the form p = (2*u+1) where u is also prime
  ;; the product n = p*q of two strong primes produces a rigid modulus
  (let* ((p  (if deterministic
                 (vec-repr:int (hash:get-hash-nbits (1- nbits) deterministic))
               (make-nbit-random (1- nbits))))
         (p  (- p (mod p 6) 1)))
    (labels ((chk (x)
               (when (is-prime? x mr-iters)
                 (let ((2x+1 (1+ (ash x 1))))
                   (and (is-prime? 2x+1 mr-iters)
                        2x+1)))))
      (um:nlet iter ((p  p)
                     (q  (- p 6)))
        (or (chk p)
            (chk q)
            (go-iter (+ p 6) (- q 6))))
      )))

(defun make-nbit-prime (nbits &optional (start 1))
  (let ((dn (ash 1 14)))
    (um:nlet iter ((n  (1+ (ash 1 nbits)))
                   (ix start))
      (if (is-prime? n)
          (values n ix)
        (go-iter (- n (* ix dn)) (1+ ix))))))

;; ------------------------------------------------------------------------
;;
#|
;; 1024-bit primes of the form 2*q+1, q prime

110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131953372219

110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131953542247

110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131954440903

110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131954912347

110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131955850507

121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318901362059

121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318901390307

121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318901683443

121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318903543947

121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318903574907

121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318904076759

125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701859684739

125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701859958483

125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860004983

125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860519219

125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860543159

125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860928467
 
125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701862024703

126109499319730975072643891561817777090078234159970968064380371675130087731049251176894058682887645606400141915814997634886187326048858326430138948012633141618386065743242120967800116033859745494421234716111317867182827099082997052278782304518545867425519912626164985878601696040793159811247884048409248818959

128491880968628397295439786826090448071799216337734462740114772282882719015462829336458309749315736156755660134536485137110659061247296862257126145128366477583072943793395168181329513531307158492226048781300219481587675392558457213959505574314676368201921410594701570918097602501976047163949823423891177344307

141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755291790299

141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755292134987

141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755293540883

141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755293652723

141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755294155379

148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789856351283
 
148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789856714019

148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789859728239

148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789859801019

171050948217267985673436464661659295399250022438505679154384395855169504304692006719499500515962744962166121514204840502682644185949107664427757974606706183917926195232484819082843189438596224733882477303185478371183408054236233026031072171840648622216400655791611595902476601403117316676079943273588450078823

171050948217267985673436464661659295399250022438505679154384395855169504304692006719499500515962744962166121514204840502682644185949107664427757974606706183917926195232484819082843189438596224733882477303185478371183408054236233026031072171840648622216400655791611595902476601403117316676079943273588450196183

171050948217267985673436464661659295399250022438505679154384395855169504304692006719499500515962744962166121514204840502682644185949107664427757974606706183917926195232484819082843189438596224733882477303185478371183408054236233026031072171840648622216400655791611595902476601403117316676079943273588451879867

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645108962583

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645111589323

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645111835839

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112131423

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112476843

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112921323

172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112941687

94151763009122605199016046651212124826776428399875868337139174973411137492045166520124166776862436148366239370758238172290409368422703069135426261530234619164046611105339201384911749696986955053718391402729253242954270262493136526039898838280505289291728617563351299061826021401294993431392010748869152068839

;; ------------------------------------------
;; 3072-bit primes
3831312298414591100618585476831656081302761106445333367964445766101605673062226600081798851596800064959373358447986085212005152872491415633440971408310613696416816247131251191127605769187849632722625692595830232912120509411459227561219422278588388521279190819787228640437741365583664399535445890926433594017171015279994876446437819329266951171495721820598600803760517032910294611120426233980568547605244805692942542173452077092113363027552652352478710408874217449160078586013212918771714470031143005776430786677042471224235686920661387334363033446494987716085736076096310420090635775820390682772337164190737858010311492171925311511494388679285585265392735735001153369804939762264859453754474981785662042445364307068663290064088038175751118437060927313568593020052845029608309540759305204515401915036636381115496655888777888350958428841118551928804350704193370950059587452549447387500168231728657073005226795752366621721650043
 
3831312298414591100618585476831656081302761106445333367964445766101605673062226600081798851596800064959373358447986085212005152872491415633440971408310613696416816247131251191127605769187849632722625692595830232912120509411459227561219422278588388521279190819787228640437741365583664399535445890926433594017171015279994876446437819329266951171495721820598600803760517032910294611120426233980568547605244805692942542173452077092113363027552652352478710408874217449160078586013212918771714470031143005776430786677042471224235686920661387334363033446494987716085736076096310420090635775820390682772337164190737858010311492171925311511494388679285585265392735735001153369804939762264859453754474981785662042445364307068663290064088038175751118437060927313568593020052845029608309540759305204515401915036636381115496655888777888350958428841118551928804350704193370950059587452549447387500168231728657073005226795752366621722201983


4958785161367786818650506991217893560732160987155560166147355256318343730672007000617972370099971962494700856552987951937218247097059454941629097569639188036827211147105738076734511317020078487276286491358372632084332882695850067691398348189906787233169219752059424030467571339174639350733762588153818101491313128431544377463490330085235988821840800868743767765574584051564101839230517392257253335131309484284694143823340149541779335482773415887622206258688531561916037338908636837083115429203108131090935827916738395997365880322515121312964782143228810034285376087076317654664015150046265987030969619227677703418063968963098558420966734477221970070843218349274881138321331257168869525821381696480724905193809829613249838623332872950546090941864063816507225702124570173612604409545843018398456301621339452868044785522603462621441423561331069806338876113287105372571110488493829754039066151606455764564313406778878957833765827

|#

(defun make-2q+1-prime (kbits &optional (mr-iters 50))
  ;; If P = 2*Q+1, for Q prime, and all higher primes are 6*k+/-1, then
  ;;   if Q = 6*k-1 then P = 12*k-1
  ;;   if Q = 6*k+1 then P = 12*k+3, but that is divisible by 3, and so cannot be.
  ;; Therefore we want to look only for Q = 6*k-1, Q prime, and P = 2*Q+1 prime
  (declare (fixnum kbits mr-iters))
  (let* ((lower (1+ (2^ (- kbits 2))))
         (upper (1- (2^ (1- kbits))))
         (niter 0)
         (range kbits))
    (declare (integer lower upper)
             (fixnum niter range))
    
    ;; Since fundamental theorem states that pi(n) ~ n/ln(n),
    ;; or density of primes is inversely proportional to number of decimal digits,
    ;; on average about ln(k)/2 trials will be needed

    ;; If P = 2*Q+1, for Q prime, and all higher primes are 6*k+/-1, then
    ;;   if P = 6*k-1 = 2*Q+1 ==> Q = 3*k-1, and Q prime
    ;;   if P = 6*k+1 = 2*Q+1 ==> Q = 3*k, so Q can't be prime
    ;; Therefore, examine only the candidates q = 3*k-1.

    (labels ((test (n)
               (declare (integer n))
               (and (is-prime? n mr-iters)
                    (let ((p (1+ (ash n 1))))
                      (and (is-prime? p mr-iters)
                           p)))))

      (loop for p =
            (let* ((x  (2^ (- kbits 2)))
                   (y  (logior x (mt-random x)))
                   (r  (mod (- 5 y) 6))
                   (n  (+ y r)))
              (declare (integer n x y)
                       (fixnum r))
              (print n)
              (or
               (um:nlet iter> ((n  n)
                               (ix 0))
                 (declare (integer n)
                          (fixnum ix))
                 (when (< ix range)
                   (incf niter)
                   (when (< n upper)
                     (um:if-let (p (test n))
                         p
                       (go-iter> (+ n 6) (1+ ix))) )))

               (um:nlet iter< ((n (- n 6))
                               (ix 0))
                 (declare (integer n)
                          (fixnum ix))
                 (when (< ix range)
                   (incf niter)
                   (when (> n lower)
                     (um:if-let (p (test n))
                         p
                       (go-iter< (- n 6) (1+ ix)) ))))))
              when p do (format t "~&niter = ~A" niter)
              when p return p) )))

;; -------------------------------------------------------------------

(declaim (inline sqr big-log pdens))

(defun sqr (n)
  (declare (integer n))
  (* n n))

(defun big-log (n)
  ;; log for BigNums
  (declare (integer n))
  (* (integer-length n) #.(log 2)))

(defun pdens (n)
  (declare (integer n))
  ;; approx local density of primes near N
  ;; from Bertrand's postulate
  (/ (big-log n)))


#+(AND :COM.RAL :LISPWORKS :ACTORS)
(defun sieve-2q+1 (nbits &optional (mr-iters 50))
  ;; If P = 2*Q+1, for Q prime, and all higher primes are 6*k+/-1, then
  ;;   if Q = 6*k-1 then P = 12*k-1
  ;;   if Q = 6*k+1 then P = 12*k+3, but that is divisible by 3, and so cannot be.
  ;; Therefore we want to look only for Q = 6*k-1, Q prime, and P = 2*Q+1 prime
  (declare (fixnum nbits mr-iters))
  (let* ((tbits 18) ;; big enough to search 1.5M candidates
         (ns    (2^ tbits))
         (arr   (make-array ns
                            :element-type    'bit
                            :initial-element 0)))
    (declare (fixnum ns tbits)
             ((array bit (*)) arr))
    
    (um:nlet iter ()
      (let* ((base  (random-between (2^ (- nbits 2))
                                    (2^ (- nbits 1))))
             (lower (+ base (- 5 (mod base 6)))))
        (declare (integer base lower))

        (fill arr 0)
         
        ;; We have lower = -1 mod 6 = 5 mod 6, or lower = 6*m-1
        ;; each bit in arr represents (lower + 6*k) = (6*(k+m)-1)
        ;; First mark the ones that are known composite numbers from our *primes* table.
        
        ;; (print "Filling bit array")
        (dolist (p (cddr *primes*))
          (declare (fixnum p))
          (do ((offs (the fixnum
                          (let ((h (mod 6 p)))
                            (declare (fixnum h))
                            (mod (- (* (mod lower p) (inv-mod h p))) p)))
                     (the fixnum (+ offs p))))
              ((>= offs ns))
            (declare (fixnum offs))
            (setf (aref arr offs) 1)))
        ;; (print arr)
        (format t "~&~D Potential Primes"
                (count-if #'zerop arr))
        
        ;; Now scan for possible primes -- not marked composite
        ;; Form q = (lower + 6*k) and test for q prime, then test for p = 2*q+1 prime
        ;; we start out with q mod 6 = 5, and every bit represents an additional increment of 6
        ;; p will therefore always be of the form p mod 12 = 11.
        ;; (print "Scanning for primes")
        (labels ((scan (grpix)
                   (declare (fixnum grpix))
                   (let* ((wd     (truncate ns 4))
                          (kstart (* grpix wd))
                          (kstop  (+ kstart wd)))
                     (declare (fixnum wd kstart kstop))
                     (do ((k  kstart  (1+ k)))
                         ((or (>= k kstop)
                              (and (zerop (aref arr k))
                                   (let* ((q (+ lower (* 6 k)))
                                          (p (+ 1 q q)))
                                     (declare (integer p q))
                                     (and (is-prime? q mr-iters)
                                          (is-prime? p mr-iters)
                                          (return p)))
                                   )))
                       ;; this *could* return nil, but unlikely
                       (declare (fixnum k))
                       ))))

          (let ((mbox  (mp:make-mailbox))
                (procs nil))
            (dotimes (ix 4)
              (push (ac:spawn-worker #'scan ix) procs))
            (or
             (prog1
                 (mp:mailbox-read mbox nil 300)
               (dolist (proc procs)
                 (ac:terminate-actor proc)))
             (go-iter)))                                       
          )))))

(defun find-primitive-root (n)
  (declare (integer n))
  (let* ((factors (factors-of (1- n))))
    (loop for k of-type integer from 2 do
          (when (test-primitive-root n factors k)
            (return k)))
    ))

#+(AND :COM.RAL :LISPWORKS)
(defun collect-safe-primes (nprimes nbits &optional (mr-iters 50))
  (declare (fixnum nprimes nbits mr-iters))
  (um:nlet iter ((primes nil))
    (if (>= (length primes) nprimes)
        primes
      (go-iter (append (sieve-2q+1 nbits mr-iters) primes))
      )))

;; ----------------------------------------

(defun decompose (n)
  ;; decompose a number n into a collection of prime factors:
  ;; n = (* factors (1+ (* factors (1+ (* factors ....)))))
  (declare (integer n))
  (multiple-value-bind (factors residue) 
      (factors-of n)
    (labels ((expt-form (factor)
               (if (= 1 (cdr factor))
                   (car factor)
                 (expt (car factor) (cdr factor)))))
      (cond (residue
             (if factors
                 `(* ;; ,@(mapcar #'expt-form factors)
                     ,(reduce #'* (mapcar #'expt-form factors))
                     (1+ ,(decompose (1- residue))))
               `(1+ ,(decompose (1- residue)))))
            ((> (length factors) 1)
             ;; `(* ,@(mapcar #'expt-form factors))
             (reduce #'* (mapcar #'expt-form factors)))
            (factors (expt-form (car factors))) ))))


;; ----------------------------------------------------------------
;; FFT in finite field...

(defun evens (a)
  (when a
    (cons (car a) (odds (cdr a)))))

(defun odds (a)
  (when a
    (evens (cdr a))))

(defun fft (k a w m)
  ;; k is a power of 2
  ;; a is a sequence of length k
  ;; w is k'th root of unity in the field
  ;; m is mod base of field
  ;;
  ;; there is a 2^r root of unity in a field Zm (m prime)
  ;; if m = 1 + c*2^r
  ;;
  (if (= 1 k)
      a
    (let* ((k/2  (ash k -1))
           (w^2  (mult-mod w w m))
           (b    (fft k/2 (evens a) w^2 m))
           (c    (fft k/2 (odds  a) w^2 m))
           (sums (loop for bi in b
                       for ci in c
                       for wi = 1 then (mult-mod wi w m)
                       collect
                       (add-mod bi
                            (mult-mod wi ci m)
                            m)))
           (diffs (loop for bi in b
                        for ci in c
                        for wi = 1 then (mult-mod wi w m)
                        collect
                        (sub-mod bi
                                 (mult-mod wi ci m)
                                 m))))
      (append sums diffs))
    ))

(defun ifft (k a w m)
  (fft k
       (mapcar (um:rcurry 'mult-mod (inv-mod k m) m) a)
       (inv-mod w m)
       m))

(defun find-fft-prime (n)
  ;; find a prime of the form c*2^n+1
  (let* ((2^n  (expt 2 n)))
    (um:nlet iter ((c 1))
      (when (< c 100)
        (let* ((p (1+ (* c 2^n))))
          (if (is-prime? p)
              p
            (go-iter (1+ c))))) )))

(defun shuffle (x)
  ;; bit reversal addressing shuffle - idempotent
  (if (cdr x)
      (let* ((a (evens x))
             (b (odds  x)))
        (append (shuffle a) (shuffle b)))
    ;; else
    x))

;; e.g., for n = 24, 16 M pt FFT: mod base m = 167772161, w = 2,
;;  w^(2^24) = 1.
;; w^(2^n) != 1 for all n < 24.
;;
;; then for n = 10, 1024 pt FFT
;;   m = same base 167772161
;;   w = 2^(24-10) mod m = 37665517

#|
(defvar *p20s* nil)
|#

(defun chkroot (n r b)
  (let ((pwr (2^ n)))
    (and (= 1 (expt-mod r pwr b))
         (um:nlet iter ((pwr pwr))
           (if (> pwr 2)
               (let ((pwr/2 (ash pwr -1)))
                 (unless (= 1 (expt-mod r pwr/2 b))
                   (go-iter pwr/2)))
             r))
         )))

(defun try-find-field-for-fft (nbits)
  (format t "~%Trying ~A bits" nbits)
  (let* ((b   (find-fft-prime nbits))
         (xs  (when b
                (um:nlet iter ((ix 2))
                  (when (< ix 1000)
                    (if (chkroot nbits ix b)
                        ix
                      (go-iter (1+ ix)))) ))))
    (when b
      (list (1- (integer-length b)) b xs))))

(defun find-field-for-fft (nbits &optional (ntries 100))
  (um:nlet iter ((n 0)
                 (nbits nbits))
    (when (< n ntries)
      (let ((f (try-find-field-for-fft nbits)))
        (if f
            f
          (go-iter (1+ n) (1+ nbits))))
      )))

;; -----------------------------------------------------------------------------
;;

(defun make-lagrange-interpolator (m shares)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (mult-mod prod (sub-mod x x2 m) m))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum share)
                 (destructuring-bind (x y) share
                   (let ((xs (mapcar 'car (remove share shares))))
                     (add-mod sum
                              (mult-mod y
                                        (div-mod (lprod x0 xs)
                                                 (lprod x xs)
                                                 m)
                                        m)
                              m) ))) )
        (reduce #'term shares
                :initial-value 0))) ))

(defun solve-lagrange (m x0 &rest shares)
  (let ((fn (make-lagrange-interpolator m shares)))
    (funcall fn x0)))

;; -----------------------------------------------------------------------------
