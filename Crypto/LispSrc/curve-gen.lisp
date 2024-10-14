;; curve-gen.lisp -- Elliptic Curve Generation
;; DM/Acudora  06/12
;; ----------------------------------------------------------------
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

(in-package :ecc-crypto-b571)

;; -----------------------------------------------------------

(defun do-with-repl-fn (fnsym newfn fnbody)
  (let ((oldfn (shiftf (symbol-function fnsym) newfn)))
    (unwind-protect
        (funcall fnbody)
      (setf (symbol-function fnsym) oldfn))))

(defmacro with-repl-fn ((fnsym newfn) &body body)
  `(do-with-repl-fn ',fnsym ,newfn (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-repl-fn" 1)

(defun do-with-lisp-ecc (fn)
  (with-repl-fn (ecc-basic-mul #'ecc-projective-mul)
    (with-repl-fn (ecc-add #'ecc-affine-add)
      (with-repl-fn (ecc-sub #'ecc-affine-sub)
        (funcall fn)))))

(defmacro with-lisp-ecc (&body body)
  ;; eval body with only the Lisp implementations
  ;; of ECC math ops
  `(do-with-lisp-ecc (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-lisp-ecc" 1)

;; ---------------------------------------------------------------------------------
;; Choose q = 2^m for bit length m
;; Choose large prime for r
;; Choose cofactor h = 2
;; Choose a = 1
;; Number of points on curve E = h*r
;; Repeatedly choose random (x, y)
;; Solve for b = y^2 + x*y - x^3 - a*x^2
;; Verify (x,y)*r = inf
;; ----------------------------------------

(defun ecc-pt-for-x-alt (x)
  ;; find a y value so that (x,y) is on the curve (a,b)
  ;; also... if (x, y) is a solution, then so is (x, y+x)
  ;; Uses: s = x + y/x in s^2 + s = x^2 + a + b/x^2, solve for s
  (let* ((xsq (gf* x x))
         (s   (handler-case
                  (gf-solve-quadratic
                   (gf+ xsq *ecc-a*
                        (gf/ *ecc-b* xsq)))
                (error ()
                  (error "X not on curve")))))
    (ecc-validate-public-key
     (make-ecc-pt
      :x x
      :y (gf* x (gf+ x s)))) ))

(defun ecc-pt-for-x (x)
  ;; find a y value so that (x,y) is on the curve (a,b)
  ;; also... if (x, y) is a solution, then so is (x, y+x)
  ;; Uses: s = y/x in s^2 + s = x + a + b/x^2, solve for s
  (let* ((xsq (gf* x x))
         (s   (handler-case
                  (gf-solve-quadratic
                   (gf+ x *ecc-a*
                        (gf/ *ecc-b* xsq)))
                (error ()
                  (error "X not on curve")))))
    (ecc-validate-public-key
     (make-ecc-pt
      :x x
      :y (gf* x s)))))

(defun ecc-isomorph (gamma)
  ;; translate the curve by gamma C(a,b) -> C(a+gamma+gamma^2,b)
  (um:bind* ((:struct-accessors ecc-pt (x y) *ecc-gen*))
    (make-ecc-curve
     :a   (gf+ *ecc-a* (gf* gamma (gf+ 1 gamma)))
     :s   gamma
     :gen (make-ecc-pt
           :x x
           :y (gf+ y (gf* gamma x)))) ))

(defun ecc-frobenius-curve (n)
  (let ((expon (ash 1 n)))
    (labels ((rotate (x)
               (gf^ x expon)))
      (make-ecc-curve
       :a  (rotate *ecc-a*)
       :b  (rotate *ecc-b*)
       :e  n
       :gen (make-ecc-pt
             :x (rotate (ecc-pt-x *ecc-gen*))
             :y (rotate (ecc-pt-y *ecc-gen*))) ))))
    
(defun ecc-find-curve-for-x (x)
  ;; Find a Frobenius isomorph to the mother curve
  ;; (incl the mother curve) that can host this x value.
  ;; Return the new curve with x as its generator x value.
  ;;
  ;; Must have Tr(x) = Tr(a) for a solution to exist
  ;; Known failure from B-571 is x=1, even though Tr(a) = 1
  ;;
  ;; NOTE: there could be more than one Frobenius isomorph that could
  ;; host x. Return the first one counting 2^N exponents upward from N
  ;; zero, the mother curve.
  ;;
  (unless (= (gf-trace x) (gf-trace *ecc-a*))
    (error "No solution unless Tr(x) = Tr(a)"))
  (um:nlet iter ((pwr 0))
    (if (< pwr *nbits*)
        (or (with-ecc-curve (ecc-frobenius-curve pwr)
              (ignore-errors
                (make-ecc-curve
                 :gen (ecc-pt-for-x-alt x))))
            (go-iter (1+ pwr)))
      ;; else
      (error "Can't find a curve suitable for X = ~A" x))))

(defun ecc-find-all-curves-for-x (x)
  (unless (= (gf-trace x) (gf-trace *ecc-a*))
    (error "No solution unless Tr(x) = Tr(a)"))
  (um:nlet iter ((pwr 0)
                 (lst nil))
    (if (< pwr *nbits*)
        (um:if-let (c (ignore-errors
                        (with-ecc-curve (ecc-frobenius-curve pwr)
                          (make-ecc-curve
                           :gen (ecc-pt-for-x x)))))
            (go-iter (1+ pwr) (cons c lst))
          ;; else
          (go-iter (1+ pwr) lst))
      ;; else
      lst)))

(defun ecc-find-curve-for-y (y &optional (pt *ecc-gen*))
  ;; y is on some curve. If (x',y') on C(a,b) then
  ;; y = y' + x' * s, and so (x',y) is on C(a+gamma,b)
  ;; where gamma = s^2 + s
  ;; So, s = (y+y')/x'
  ;; Choose P as (x',y') on C(a,b)
  ;; NOTE: new curve has same Trace as old curve
  ;; This works for any arbitrary point along the mother curve.
  ;; So by default pt = *ecc-gen* we get a derivative of the generator.
  ;; But there are *ecc-r* (= prime) possibilities.
  (um:bind* ((:struct-accessors ecc-pt ((xp x)
                                        (yp y)) pt)
             (s       (gf/ (gf+ y yp) xp))
             (a+gamma (gf+ *ecc-a* s (gf* s s))))
    ;; return the curve, and have the new point as its generator
    (make-ecc-curve
     :a    a+gamma
     :s    s
     :gen  (with-ecc-curve (:a a+gamma
                            :s s)
             (ecc-validate-public-key (make-ecc-pt
                                       :x xp
                                       :y y)))) ))

(defun ecc-find-curve-for-xy (x y)
  ;; Finding a curve to host x entails a Frobenius rotation
  (with-ecc-curve (ecc-find-curve-for-x x)
    ;; Finding a curve to host y entails a translation
    (ecc-find-curve-for-y y)))

#|
 ;; on average about 50% of the Frobenius x's are also
 ;; valid on the mother curve.
(let ((y/n (make-array *nbits*
                       :initial-element 0)))
  (loop for ix from 0 below *nbits* do
        (if (ignore-errors
              (ecc-pt-for-x (gf^ (ecc-pt-x *ecc-gen*) (ash 1 ix))))
            (setf (aref y/n ix) 1)))
  (plt:plot 'y/n y/n :clear t :symbol :circle)
  (plt:histogram 'histo y/n :clear t))

;; on average about 50% of the points along the mother curve
;; are also valid x's along a Frobenius isomorph
(let ((y/n (make-array 100
                       :initial-element 0))
      (pt  *ecc-gen*))
  (loop for ix from 0 below 100 do
        (if (ignore-errors
              (ecc-pt-for-x-alt (gf^ (ecc-pt-x pt) 64)))
            (setf (aref y/n ix) 1))
        (setf pt (ecc-add pt *ecc-gen*)))
  (plt:histogram 'histo y/n :clear t))

;; is there any x value (apart from 0) that is never on
;; any of the Frobenius isomorphs, including the mother curve?
;; ... Answer: We haven't found one yet... (apart from x = 0, x = 1)
;; which leads us to speculate that the entire GF plane is covered
;; by some curve. We know that for any valid x, all values of y are valid,
;; including 0. And the only known invalid x values are 0 and 1.
(um:nlet iter ((ct 0))
  (if (< ct 1000)
      (let ((x (random-between 2 (ash 1 *nbits*))))
        (if (null (ignore-errors
                    (ecc-find-curve-for-x x)))
            'yes-found-an-x-not-on-any-curve ;; -- we found an x value that is not on any curve!!
          ;; else try again
          (go-iter (1+ ct))))
    ;; else -- out of all the looks, every x was on at least one of the Frobenius isomorphs !!
    'no-every-x-examined-was-on-some-curve))
 |#

#|
(defun nearest-ecc-pt-for-x (x)
  ;; look for an ECC point near x, stepping by 2x until we find one.
  (if (zerop x)
      (ecc-infinity)
    (let ((tr (gf-trace *ecc-a*)))
      (um:nlet iter ((x x))
        (unless (= (gf-trace x) tr)
          (setf x (gf+ 1 x)))
        ;; (format t "~&Trying: ~A" x)
        (handler-case
            (return-from nearest-ecc-pt-for-x (ecc-pt-for-x x))
          (error () ))
        (go-iter (gf* 2 x))))))
|#

(defun ecc-find-a-valid-x-from (x &optional (ntries 7))
  (labels ((try-this (x)
             (and
              (ignore-errors
                (ecc-pt-for-x x))
              x)))

    (um:nlet iter ((x  x)
                   (n  ntries))
      (when (plusp n)
        (or (try-this x)
            (go-iter (1+ x) (1- n)))))
    ))
        
(defun ecc-find-a-valid-x ()
  ;; on average you have a roughly 50% chance of landing on a valid x
  ;; and about 90% chance of being within 1 or 2 steps away from a valid x
  (labels ((try-this (x)
             (ignore-errors
               (ecc-pt-for-x x))))
    
    (um:nlet iter ()
      (let ((x (gf-random-k*)))
        (or (try-this x)
            (try-this (1+ x))
            (try-this (+ x 2))
            (go-iter)) ))))

#|
 ;; on average you have a roughly 50% chance of landing on a valid x
 ;; and about 90% chance of being within 1 or 2 steps away from a valid x
(let ((ns nil))
  (labels ((scanner (x)
             (um:nlet iter ((ix 0))
                 (let ((s (ignore-errors
                            (ecc-pt-for-x (+ x ix)))))
                   (if s
                       (push ix ns)
                     (go-iter (1+ ix)))) )))
    (loop repeat 1000 do
          (let ((x (random-between 1 (ash 1 571))))
            (scanner x)))
    (plt:histogram 'histo ns :clear t :cum t)))

(let ((xs nil))
  (loop for ix from 1 below 100 do
        (let ((s (ignore-errors
                   (ecc-pt-for-x ix))))
          (when s
            (push ix xs))))
  (plt:plot 'xs (nreverse xs) :clear t :symbol :circle))
|#

#|
(defun random-ecc-point-on-curve (a)
  (unless (= 1 (gf-trace a))
    (error "No solution"))
  (let* ((n  (mod (convert-bytes-to-int (ctr-drbg *nbits*))
                  *ecc-r*))
         (pt (ecc-mul *ecc-gen* n))
         (x  (ecc-pt-x pt)))
    (with-ecc-curve (:a a)
      (nearest-ecc-pt-for-x n))))
|#

#|
;; y^2 + x*y = x^3 + a*x^2 + b
;; (y/x)^2 + (y/x) = x + a + b/x^2
;; (x+y/x)^2 + (x+y/x) = x^2 + a + b/x^2
(defun lhs-rhs (pt)
  (with-accessors ((x ecc-pt-x)
                   (y ecc-pt-y)) pt
    (let ((xsq (gf* x x))
          (lam (gf+ x (gf/ y x))))
      (list (gf+ (gf* lam lam) lam)
            (gf+ (gf/ *ecc-b* xsq)
                 *ecc-a*
                 xsq)) )))
|#

;; --------------------------------------
;; NOTES:
;;
;; For (x,y) on E: y^2 + x*y = x^3 + a*x^2 + b
;; ------------------------------
;; 1) little-Frobenius automorphism:
;;
;;    E -> E'  (x,y) -> (x^2, y^2)
;;    E': y^2 + x*y = x^3 + a^2*x^2 + b^2
;; ======
;; 2) In general: E -> E'  (x,y) -> (x^(2^n), y^(2^n)), n in (-inf,+inf)
;;    E': y^2 + x*y = x^3 + a^(2^n) * x^2 + b^(2^n)
;; any power-of-two exponent, even negative, which means power-of-two
;; roots too.
;; ------------------------------
;; 3)  (b^(1/4),b^(1/2)) is pt of O(4)
;;     E: y^2 + x*y = x^3 + b


(defun gen-pts (x y)
  (progn ;; with-gf2^7
    (let* ((a  1)
           (b  (gf+ (gf^2 y) (gf* y x) (gf* a (gf^2 x)) (gf^ x 3)))
           (pt (make-ecc-pt :x x :y y)))
      (with-ecc-curve (:a a :b b)
        (when (ecc-solution-p pt)
          (let ((ct  1)
                (pts (list pt)))
            (um:nlet iter ()
              (let ((ptn (ecc-add pt (car pts))))
                (incf ct)
                (push ptn pts)
                (unless (ecc-infinite-p ptn)
                  (go-iter))))
            #|
            (let ((*ecc-r* ct))
              (dolist (pt (cdr pts))
                (validate-public-key pt)))
            |#
            (list (make-ecc-curve
                   :gen pt
                   :r   ct)
                  pts) ))))))
#|
(let ((lst nil))
  (loop for ix from 1 to 127 do
        (when (= 1 (logxor (ldb (byte 1 0) ix)
                           (ldb (byte 1 9) ix)))
          (let* ((info (gen-pts ix 3))
                 (r    (getf info :r)))
            (when (primes:is-prime? r)
              (push (list ix r) lst)) )))
  (setf lst (remove-duplicates (sort lst #'<
                                     :key #'cadr)
                               :key 'cadr)))
|#
#|
(defun gen-npts (n x)
  (with-gf2^7
    (let* ((y  1)
           (b  (gf+ 1 x (gf^2 x) (gf^ x 3)))
           (pt (make-ecc-pt :x x :y y)))
      (with-ecc-curve (:a 1 :b b)
        (when (ecc-solution-p pt)
          (let ((pts (list pt)))
            (loop repeat n do
              (let ((ptn (ecc-add pt (car pts))))
                (push ptn pts)))
            (list :gen pt :a 1 :b b :pts pts) ))))))

(with-gf2^7
  (with-ecc-curve (:a 1 :b 104)
    (let ((*ecc-r* 73))
      (validate-public-key (make-ecc-pt
                            :x 10 :y 1)))))
|#

#|
;; E over GF(2^7)
(:GEN
 #S(ECC-PT :X 9 :Y 3)
 :A
 1
 :B
 26
 :R
 73
 :PTS
 (#S(ECC-PT :X 0 :Y 0)
  #S(ECC-PT :X 9 :Y 10)
  #S(ECC-PT :X 117 :Y 104)
  #S(ECC-PT :X 29 :Y 85)
  #S(ECC-PT :X 57 :Y 100)
  #S(ECC-PT :X 77 :Y 27)
  #S(ECC-PT :X 93 :Y 66)
  #S(ECC-PT :X 105 :Y 121)
  #S(ECC-PT :X 71 :Y 117)
  #S(ECC-PT :X 25 :Y 36)
  #S(ECC-PT :X 33 :Y 96)
  #S(ECC-PT :X 99 :Y 41)
  #S(ECC-PT :X 87 :Y 24)
  #S(ECC-PT :X 39 :Y 127)
  #S(ECC-PT :X 109 :Y 110)
  #S(ECC-PT :X 101 :Y 3)
  #S(ECC-PT :X 1 :Y 116)
  #S(ECC-PT :X 107 :Y 59)
  #S(ECC-PT :X 67 :Y 108)
  #S(ECC-PT :X 91 :Y 26)
  #S(ECC-PT :X 125 :Y 118)
  #S(ECC-PT :X 41 :Y 38)
  #S(ECC-PT :X 55 :Y 106)
  #S(ECC-PT :X 103 :Y 63)
  #S(ECC-PT :X 59 :Y 108)
  #S(ECC-PT :X 11 :Y 67)
  #S(ECC-PT :X 15 :Y 93)
  #S(ECC-PT :X 45 :Y 103)
  #S(ECC-PT :X 23 :Y 72)
  #S(ECC-PT :X 123 :Y 65)
  #S(ECC-PT :X 83 :Y 46)
  #S(ECC-PT :X 121 :Y 108)
  #S(ECC-PT :X 27 :Y 101)
  #S(ECC-PT :X 89 :Y 84)
  #S(ECC-PT :X 49 :Y 45)
  #S(ECC-PT :X 79 :Y 5)
  #S(ECC-PT :X 65 :Y 88)
  #S(ECC-PT :X 65 :Y 25)
  #S(ECC-PT :X 79 :Y 74)
  #S(ECC-PT :X 49 :Y 28)
  #S(ECC-PT :X 89 :Y 13)
  #S(ECC-PT :X 27 :Y 126)
  #S(ECC-PT :X 121 :Y 21)
  #S(ECC-PT :X 83 :Y 125)
  #S(ECC-PT :X 123 :Y 58)
  #S(ECC-PT :X 23 :Y 95)
  #S(ECC-PT :X 45 :Y 74)
  #S(ECC-PT :X 15 :Y 82)
  #S(ECC-PT :X 11 :Y 72)
  #S(ECC-PT :X 59 :Y 87)
  #S(ECC-PT :X 103 :Y 88)
  #S(ECC-PT :X 55 :Y 93)
  #S(ECC-PT :X 41 :Y 15)
  #S(ECC-PT :X 125 :Y 11)
  #S(ECC-PT :X 91 :Y 65)
  #S(ECC-PT :X 67 :Y 47)
  #S(ECC-PT :X 107 :Y 80)
  #S(ECC-PT :X 1 :Y 117)
  #S(ECC-PT :X 101 :Y 102)
  #S(ECC-PT :X 109 :Y 3)
  #S(ECC-PT :X 39 :Y 88)
  #S(ECC-PT :X 87 :Y 79)
  #S(ECC-PT :X 99 :Y 74)
  #S(ECC-PT :X 33 :Y 65)
  #S(ECC-PT :X 25 :Y 61)
  #S(ECC-PT :X 71 :Y 50)
  #S(ECC-PT :X 105 :Y 16)
  #S(ECC-PT :X 93 :Y 31)
  #S(ECC-PT :X 77 :Y 86)
  #S(ECC-PT :X 57 :Y 93)
  #S(ECC-PT :X 29 :Y 72)
  #S(ECC-PT :X 117 :Y 29)
  #S(ECC-PT :X 9 :Y 3)))
|#
#|
(defun tst ()
  (let* ((curve (car (gen-pts 9 3)))
         (arr   (make-array '(128 128) :initial-element 0)))
    (plt:window 'pltc :clear t
                :background :black)
    (with-ecc-curve curve
      ;; translations
      (loop for aix from 0 below 128 by 2 do
            (with-accessors ((x ecc-pt-x)
                             (y ecc-pt-y)) *ecc-gen*
              (with-ecc-curve (:a (gf+ *ecc-a* aix (gf^2 aix))
                               :s aix
                               :gen (make-ecc-pt
                                     :x x
                                     :y (gf+ y (gf* aix x))))
                
                (loop for bix from 0 below 7 do
                      ;; rotations
                      (let ((expon (ash 1 bix)))
                        (with-accessors ((x ecc-pt-x)
                                         (y ecc-pt-y)) *ecc-gen*
                          (with-ecc-curve (:a (gf^ *ecc-a* expon)
                                           :b (gf^ *ecc-b* expon)
                                           :e bix
                                           :gen (make-ecc-pt
                                                 :x (gf^ x expon)
                                                 :y (gf^ y expon)))
                            
                            ;; collect points
                            (let ((pts (list *ecc-gen*)))
                              (loop for ix from 2 to *ecc-r* do
                                    (push (ecc-add *ecc-gen* (car pts)) pts))

                              (loop for pt in pts do
                                    (with-accessors ((x ecc-pt-x)
                                                     (y ecc-pt-y)) pt
                                      (incf (aref arr x y))))
                              
                              ;; show the points
                              (let ((dx (aref #(-1 1 -2 2 -2  2  0) bix))
                                    (dy (aref #( 1 1  0 0 -1 -1  0) bix)))
                                (plt:plot 'pltc
                                          (mapcar (lambda (x)
                                                    (+ x (/ dx 6)))
                                                  (mapcar #'ecc-pt-x pts))
                                          (mapcar (lambda (y)
                                                    (+ y (/ dy 6)))
                                                  (mapcar #'ecc-pt-y pts))
                                          ;; :clear (shiftf clr nil)
                                          :fullgrid nil
                                          :symbol :dot
                                          :alpha  1
                                          :color  (aref #(:red :orange :yellow :green :blue :violet :white)
                                                        bix))))
                            )))))
              )))
    (plt:tvscl 'img arr :magn 2 :clear t :zrange '(0 7))
    (plt:plot 'plt (loop for ix from 1 below 128 by 2 collect (aref arr ix 1))
              :yrange '(0 10)
              :symbol :circle
              :plot-joined t
              :clear t)
    (format t "~&Sum = ~A"
            (loop for ix from 0 below 128 sum (aref arr ix 1)))
    ))

(defun tst ()
  (let ((curve (car (gen-pts 9 3)))
        (seed  10)
        (ctr   3))
    (with-ecc-curve curve
      (let ((x (coerce
                (loop for ix from 1 to 16384 collect
                      (progn
                        (incf ctr)
                        (let* ((pt (ecc-mul *ecc-gen* (+ ctr seed)))
                               (x  (ecc-pt-y pt)))
                          (setf seed x))))
                'vector)))
        (plt:plot 'plt (fft:fwd-magnitude-db x) :clear t)
        (plt:plot 'plt2 (map 'vector #'realpart
                             (fft:inv (map 'vector (lambda (x)
                                                     (* x (conjugate x)))
                                           (fft:fwd x))))
                  :clear t
                  :xrange '(0 400))
        (subseq x 0 500)
        ))))
                  
|#

(defun find-root-curve (&optional (curve *curve*))
  (um:if-let (parent (ecc-curve-parent curve))
      (find-root-curve parent)
    curve))

(defmacro with-root-ecc-curve (&body body)
  `(with-ecc-curve (find-root-curve)
     ,@body))

(defun gen-random-ecc-curve (d s e)
  #F
  (declare (type integer d s e))
  (assert (integerp d))
  (assert (integerp s))
  (assert (integerp e))
  (with-root-ecc-curve
   ;; the only reasonable way of doing this with invertible results
   ;; and without tracking the provenance of points (i.e., history of
   ;; parent curves) is to always revert back to the reference mother
   ;; curve before randomizing anew.
   (um:bind* ((d  (mod d *ecc-r*))  ;; d in [1,*ecc-r*)
              (s  (ldb (byte *nbits* 0) s))
              (e  (mod e *nbits*))
              (:struct-accessors ecc-pt (x y) (ecc-basic-mul *ecc-gen* d))
              ;; *ecc-r* is prime, so d*G != Infinity, since d < *ecc-r*
              (expon (ash 1 e))
              ;; rotate first, then translate
              ;;
              ;; rotation = random Frobenius isomorphism
              ;;   (x,y,a,b) -> (x^(2^k), y^(2^k), a^(2^k), b^(2^k)) for random k in [0, nbits)
              ;; translation = random additive isomorphism
              ;;   (x,y,a,b) -> (x, y+s*x, a+s+s^2, b), for random s in [0,2^nbits)
              ;;
              ;; Since (x,y) on C(a,b) and (x,y+x) = -(x,y) also on C(a,b)
              ;; the choices of translation for s and s+1 produce the same curve.
              ;;
              ;; Total of (r * nbits * 2^(nbits-1)) possible points in GF^2 covered
              ;; For nbits = 571 this is > 2*10^346
              ;; All x values, for all curves, must have Tr(x) = Tr(a) and so only half
              ;; the possible x values can be used.
              ;; There are only 1/2*(2^nbits)^2 possible points in the GF plane ~ 3*10^343
              ;; so we cover this plane by more than 570 times.(!!)
              ;;
              (a     (gf^ *ecc-a* expon))
              (b     (gf^ *ecc-b* expon))
              (x     (gf^ x expon))
              (y     (gf^ y expon))
              (a     (gf+ a s (gf^2 s)))
              (y     (gf+ y (gf* s x)))
              (pt    (make-ecc-pt :x x :y y))
              (curve (make-ecc-curve
                      :a   a
                      :b   b
                      :gen pt
                      :d   (mod (* d *ecc-d*) *ecc-r*)
                      :s   (gf+ s *ecc-s*)
                      :e   (mod (+ e *ecc-e*) *nbits*))))
         (with-ecc-curve curve
           (ecc-validate-public-key pt))
         curve)))

(defun choose-random-ecc-curve ()
  ;; *ecc-r* is prime, so d*G != Infinity for d < *ecc-r*
  (let ((d (random-between 1 *ecc-r*))   ;; d in (0,*ecc-r*)
        (s (ctr-drbg-int *nbits*))       ;; s in [0,2^n)
        (e (random-between 0 *nbits*)))  ;; e in [0,n)
    (gen-random-ecc-curve d s e)))

(defmacro with-random-ecc-curve (&body body)
  `(with-ecc-curve (choose-random-ecc-curve)
     ,@body))

(defmethod normalize-pt ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt (x y) pt)
             (e      *ecc-e*)
             (s      *ecc-s*)
             (parent (find-root-curve))
             (epar   (ecc-curve-e parent))
             (spar   (ecc-curve-s parent))
             (expon  (ash 1 (mod (- epar e) *nbits*))))
    (make-ecc-pt
     :x (gf^ x expon)
     :y (gf^ (gf+ y (gf* x (gf+ s spar))) expon))
    ))

(defmethod to-random-curve ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt (x y) pt)
             (expon  (ash 1 *ecc-e*))
             (xnew   (gf^ x expon)))
    (make-ecc-pt
     :x xnew
     :y (gf+ (gf^ y expon)
             (gf* xnew *ecc-s*)))
    ))

(defmethod to-random-curve ((pt ecc-projective-pt))
  (ecc-projective
   (to-random-curve
    (ecc-affine pt))))

(defmethod normalize-pt ((pt ecc-projective-pt))
  (ecc-projective
   (normalize-pt
    (ecc-affine pt))))

#|
(defun gen-pairs (n &optional (pwr 0))
  (loop for ix from 1 below n collect
        (list ix
              (gf-trace ix)
              (gf-trace (gf/ (gf^ *ecc-b* (ash 1 pwr))
                             (gf^2 ix))))))

(defun simplest-b ()
  (loop for ix from 0 below *nbits* minimize
        (integer-length (gf^ *ecc-b* (ash 1 ix)))))

;; show that any random x abscissa is hosted by
;; about 50% of the Frobenius rotations of the mother curve
;; -- over 10,000 trials --
;; looks very Gaussian
;; mean/stdev: 0.499 +/- 0.021
;; median/mad: 0.501 +/- 0.014
;; IF Gaussian, then to have no rotations hosting an arbitrary x
;; would be a roughly 25 sigma event, or about 1 in 6*10^272 trials
;; (vanishingly small...)
(defun tst-xs (&optional (n 1000))
  (let* ((b0  (random-between 1 (ash 1 *nbits*)))
         (bs (loop for ix from 0 below *nbits*
                   collect
                   (gf^ b0 (ash 1 ix))))
         (trials (make-array n)))
    (loop for ix from 0 below n do
          (let* ((x     (random-between 2 (ash 1 *nbits*)))
                 (trx   (gf-trace x))
                 (1/x^2 (gf^ x -2)))
            (setf (aref trials ix)
                  (float
                   (/ (loop for b in bs count
                            (/= trx (gf-trace (gf* b 1/x^2))))
                      *nbits*)))))
    (plt:histogram 'histo trials :clear t)
    (list :mean  (vm:mean trials)
          :stdev (vm:stdev trials)
          :med   (vm:median trials)
          :mad   (vm:mad trials))))

|#

(defun gen-random-primitive (n)
  ;; find an irreducible polynomial of degree n
  (with-ecc-curve (:nbits n)
    (let ((1<<n    (ash 1 n)))
      (um:nlet iter ((ct 0))
        (if (< ct 100)
            (with-ecc-curve (:gf (logior 1<<n
                                         1
                                         (random-between 2 1<<n )))
              (or
               (um:nlet inner ((ix 0))
                 (if (> ix 50)
                     $prim
                   (let ((x (random-between 2 1<<n )))
                     (and (= x (lisp-gf^ x 1<<n ))
                          (go-inner (1+ ix))))))
               (go-iter (1+ ct))))
          ;;
          (error "Can't find one")) ))))

(defun gen-primitive-trinomial (n)
  ;; find an irreducible trinomial of degree n
  (with-ecc-curve (:nbits n)
    (let ((1<<n    (ash 1 n)))
      (um:nlet iter ((ix 1))
        (if (< ix *nbits*)
            (with-ecc-curve (:gf (logior 1<<n
                                         1
                                         (ash 1 ix)))
              (or
               (um:nlet inner ((ct 0))
                 (if (>= ct 100)
                     $prim
                   (let ((x (random-between 2 1<<n )))
                     (and (= x (lisp-gf^ x 1<<n ))
                          (go-inner (1+ ct))))))
               (go-iter (1+ ix))))
          ;;
          (error "Can't find one")) ))))

(defun gen-primitive-pentanomial (n)
  ;; find an irreducible pentanomial of degree n
  (with-ecc-curve (:nbits n)
    (let ((1<<n    (ash 1 n)))
      (um:nlet iter ((ix 3))
        (if (< ix *nbits*)
            (let ((pskel3 (logior 1<<n
                                  1
                                  (ash 1 ix)
                                  )))
              (um:nlet iter2 ((jx 2))
                (if (< jx ix)
                    (let ((pskel4 (logior pskel3
                                          (ash 1 jx))))
                      (um:nlet iter3 ((kx 1))
                        (if (< kx jx)
                            (with-ecc-curve (:gf (logior pskel4
                                                         (ash 1 kx)))
                              (or
                               (um:nlet inner ((ct 0))
                                 (if (>= ct 100)
                                     $prim
                                   ;; else
                                   (let ((x (random-between 2 1<<n )))
                                     (and (= x (lisp-gf^ x 1<<n ))
                                          (go-inner (1+ ct))))))
                               (go-iter3 (1+ kx))))
                          ;; else
                          (go-iter2 (1+ jx)))))
                  ;; else
                  (go-iter (1+ ix)))))
          ;; else
          (error "Can't find one")) ))))

(defun gen-primitive (n)
  (or (ignore-errors
        (gen-primitive-trinomial n))
      (gen-primitive-pentanomial n)))

#|
(let ((*nbits* 5) ($prim 37))
  (destructuring-bind (c pts) (gen-pts 2 2)
    (with-ecc-curve c
      (plt:plot 'plt
                (mapcar #'ecc-pt-x pts)
                (mapcar #'ecc-pt-y pts)
                :clear t
                :symbol :circle)
      (print (gf-trace (ecc-curve-b c))))
    c))

(defun ecc-pt-for-x (x)
  ;; find a y value so that (x,y) is on the curve (a,b)
  ;; also... if (x, y) is a solution, then so is (x, y+x)
  ;; Uses: s = y/x in s^2 + s = x + a + b/x^2, solve for s
  (let* ((xsq (gf* x x))
         (s   (handler-case
                  (gf-solve-quadratic
                   (gf+ x *ecc-a*
                        (gf/ *ecc-b* xsq)))
                (error ()
                  (error "X not on curve")))))
    (progn ;; validate-public-key
     (make-ecc-pt
      :x x
      :y (gf* x s)))))

(setf *pts5*
      (let ((*nbits* 5)
            ($prim   37)
            (pts     nil))
        (destructuring-bind (c cpts)
            ;; (gen-pts 31 4)
            (gen-pts 2 2)
          (with-ecc-curve c
            (assert (= 1 (gf-trace *ecc-b*)))
            (loop for ix from 0 below 32 do
                  (um:when-let (pt (ignore-errors (ecc-pt-for-x ix)))
                    (push pt pts)
                    (push (make-ecc-pt
                           :x (ecc-pt-x pt)
                           :y (gf+ (ecc-pt-x pt)
                                   (ecc-pt-y pt)))
                          pts))))
          (plt:plot 'plt
                    (mapcar #'ecc-pt-x pts)
                    (mapcar #'ecc-pt-y pts)
                    :clear t
                    :symbol :square)
          (plt:plot 'plt
                    (mapcar #'ecc-pt-x cpts)
                    (mapcar #'ecc-pt-y cpts)
                    :color :red
                    :symbol :circle)
          #|
          (loop for pt in cpts do
                (assert (member pt pts
                                :test #'equalp)))
          |#
          (let* ((n 16)
                 (cn (with-ecc-curve (:b (gf^ *ecc-b* n))
                       (gen-pts (gf^ (ecc-pt-x (ecc-curve-gen c)) n)
                                (gf^ (ecc-pt-y (ecc-curve-gen c)) n)))))
            (list c cpts pts cn)) )))

;; 521 is prime
;; and (2^521 - 1) is prime
(defvar $prim521 #x20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000001)
|#

(defun make-ecc-random-bit-generator ()
  ;; not suitable for high-volume production of random numbers
  ;; (too slow!)
  ;; But interesting nonetheless...
  (let* ((ctr   (convert-bytes-to-int (make-nonce)))
         (curve (make-ecc-curve)))
    (lambda ()
      (with-ecc-curve curve
        (let ((pt (ecc-mul *ecc-gen* (incf ctr))))
          (with-ecc-curve (:gen pt)
            (with-accessors ((x ecc-pt-x)
                             (y ecc-pt-y)) pt
              (let* ((rot  (mod (ldb (byte 10 173) x) *nbits*))
                     (xlat (gf* x y)))
                (with-ecc-curve (ecc-frobenius-curve rot)
                  (setf curve (ecc-isomorph xlat)))
                y))))))
     ))



;; ------------------------------------------------------

