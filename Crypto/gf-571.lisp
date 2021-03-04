;; gf-571.lisp -- Galois Polynomial Field of order 571
;; DM/Acudora  11/11
;; ----------------------------------------------------
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

;;------------------------------------------------
;; ECC over the Field F(2^571)
;; based on underlying polynomial field GF(2^571)

(defun gf+ (&rest args)
  (apply #'logxor args))

(defun gf- (&rest args)
  (apply #'gf+ args))

;; (defvar *nbits* 571)
;; (defvar *gf-order* (1- (ash 1 *nbits*)))

(defun make-prim-poly-from-taps (order taps)
  ;; taps stated in a list, like this one for GF(2^8): '(6 5 4)
  ;; assumes first and final taps are 0 and Order, so we really have
  ;;   P(8,x) = x^8 + x^6 + x^5 + x^4 + 1
  (reduce (lambda (sum n)
            (logior sum
                    (ash 1 n)))
          taps
          :initial-value (logior (ash 1 order) 1)))

(defun bitrev (n)
  (um:nlet iter ((r 0)
                      (n n))
    (if (zerop n)
        r
      (go-iter (+ r r (logand n 1))
               (ash n -1)) )))

(defvar $prim-571
  ;; B571: t^571 + t^10 + t^5 + t^2 + 1
  (logior (ash 1   571)
          (ash 1   10)
          (ash 1   5)
          (ash 1   2)
          1))

(defvar $prim-163
  ;; B163: t^163 + t^7 + t^6 + t^3 + 1
  (logior (ash 1   163)
          (ash 1   7)
          (ash 1   6)
          (ash 1   3)
          1))

(defvar $prim-128
  ;; B128: t^128 + t^7 + t^2 + t^1 + 1
  (logior (ash 1   128)
          (ash 1   7)
          (ash 1   2)
          (ash 1   1)
          1))

(defvar $prim-256
  ;; B256:
  (logior (ash 1 256)
          (ash 1 254)
          (ash 1 251)
          (ash 1 246)
          1))

(defmacro with-gf (nbits prim &body body)
  `(with-ecc-curve (:nbits ,nbits
                    :gf    ,prim)
     ,@body))

(defmacro with-gf2^571 (&body body)
  `(with-gf 571 $prim-571
     ,@body))

(defmacro with-gf2^128 (&body body)
  `(with-gf 128 $prim-128
     ,@body))

(defmacro with-gf2^163 (&body body)
  `(with-gf 163 $prim-163
     ,@body))

(defmacro with-gf2^256 (&body body)
  `(with-gf 256 $prim-256
     ,@body))

(defvar $prim-8
  ;; B8: t^8 + t^4 + t^3 + t^2 + 1
  (logior (ash 1   8)
          (ash 1   4)
          (ash 1   3)
          (ash 1   2)
          1))

(defvar $prim-7
  ;; B7: t^7 + t^1 + 1
  (logior (ash 1   7)
          (ash 1   1)
          1))

(defvar $prim-11
  ;; B11: t^11 + t^2 + 1
  (logior (ash 1 11)
          (ash 1  2)
          1))

(defvar $prim-16
  ;; B16: #x150d7
  #x150d7)

(defvar $prim-24
  ;; B24:
  #x1b00001)

(defvar $prim-32
  ;; B32:
  #x165656565)

(defvar $prim-64
  ;; B64:
  #x1b000000000000001)

(defmacro with-gf2^8 (&body body)
  `(with-gf 8 $prim-8
     ,@body))

(defmacro with-gf2^7 (&body body)
  `(with-gf 7 $prim-7
     ,@body))

(defmacro with-gf2^11 (&body body)
  `(with-gf 11 $prim-11
     ,@body))

#|
(defun inhale (x)
  (do ((ans  0)
       (ix   (1- (integer-length x)) (1- ix)))
      ((minusp ix) ans)
    (setf ans (logxor (step-state ans)
                      (ldb (byte 1 ix) x)))
    ))

(defvar *x*
 (let ((arr (make-array 256 :initial-element nil))) 
   (loop for ix from 0 below 255 do
         (let ((jx (with-gf2^8 (gf^ 2 ix))))
          (setf (aref arr jx) t)))
   (loop for ix from 0 below 256
         unless (aref arr ix) collect ix)))
(inspect
 (loop for ix from 0 below 256 collect (with-gf2^8 (gf^ 214 ix))))
(inspect
 (loop for ix from 0 below 128 collect (with-gf2^7 (gf^ 4 ix))))
(inspect
 (let ((arr (make-array 256)))
   (loop for ix from 2 below 256 do
         (um:nlet iter ((exp 2))
           (if (= 1 (with-gf2^8 (gf^ ix exp)))
               (setf (aref arr ix) exp)
             (go-iter (1+ exp))) ))
   arr))

(inspect
 (let ((n 10))
   (loop for ix from 0 below n collect (expt-mod n 3 ix))))

(inspect
 (let ((arr (make-array 256)))
   (loop for ix from 2 below 256 do
         (um:nlet iter ((exp 2))
           (if (= 1 (with-gf 8 (make-prim-poly-from-taps 8 '(6 5 4))
                      (gf^ ix exp)))
               (setf (aref arr ix) exp)
             (go-iter (1+ exp))) ))
   arr))

|#

(defun step-state (state)
  (let ((new-state (ash state 1)))
    (if (logbitp *nbits* new-state)
        (logxor new-state $prim)
      new-state)))

#|
(defun gf* (a b)
  (do ((ans  0)
       (x    a   (step-state x))
       (limit (integer-length b))
       (mask 0   (1+ mask)))
      ((>= mask limit) ans)
    (when (logbitp mask b)
      (setf ans (logxor ans x))) ))
|#

#|
(with-gf2^571
  (let* ((n    4096)
         (xs (let ((vals nil))
               (do ((ix  0   (1+ ix))
                    (g   1   (step-state g)))
                   ((>= ix n) (coerce vals 'vector))
                 (push (if (evenp g) 1.0 -1.0) vals)
                 )))
         (ys (fft:fwd xs)))
    (setf (aref ys 0) 0)
    (plt:plot 'rplt (map 'vector #'abs ys)
              :clear t
              ;; :xrange '(0 10)
              )
    xs))

;; ----------------------------------------------------------------
;; let's look at packing 32 harmonics of a signal into one stream
;; using randomized phase shifts between each harmonic, to prevent
;; excessive amplitude excursions in the composite signal...

(with-gf2^571
  (let* ((n    4096)
         (nosc 10)
         (freq 20/48000)
         (xs   (coerce (um:range 0 4096) 'vector)))
    (labels ((osc (n &optional (phs 0))
               (let ((ans (map 'vector (lambda (x)
                                         (sin (* 2 pi (+ phs (* n x freq)))))
                               xs)))
                 ;; (fill ans 0.0 :start 480)
                 ans))
             (db20 (x)
               (* 20 (log (max 1d-6 (abs x)) 10))))
      (let* ((ys (apply #'map 'vector #'+
                        (loop for ix from 1 to nosc
                              collect
                              (osc ix))))
             (zs (apply #'map 'vector #'+
                        (loop for ix from 1 to nosc
                              for state = (lw:mt-random (ash 1 571)) then (step-state state)
                              collect
                              (osc ix (/ state (ash 1 571)))))))
        (plt:plot 'sound ys
                  :clear t)
        (plt:plot 'sound zs
                  :color :red)
        (let* ((fys (map 'vector #'db20 (fft:fwd ys)))
               ;; (fys (map 'vector (um:rcurry #'- (reduce #'max fys)) fys))
               (fzs (map 'vector #'db20 (fft:fwd zs)))
               ;; (fzs (map 'vector (um:rcurry #'- (reduce #'max fzs)) fzs))
               (frqs (map 'vector (um:curry #'* (/ 48000 n)) xs)))
          (plt:plot 'spec frqs fys
                    :clear t
                    :xrange '(0 24000))
          (plt:plot 'spec frqs fzs
                    :color :red))
        ))))

(with-gf2^571
  (let* ((n    4096)
         (nosc 3)
         (phs  (vector 0.0  0.0  0.5  0.0   0.0  0.0 0.0 0.0))
         (freq 20/48000)
         (xs   (coerce (um:range 0 4096) 'vector)))
    (labels ((osc (n &optional (phs 0))
               (let ((ans (map 'vector (lambda (x)
                                         (sin (* 2 pi (+ phs (* n x freq)))))
                               xs)))
                 ;; (fill ans 0.0 :start 480)
                 ans))
             (db20 (x)
               (* 20 (log (max 1d-6 (abs x)) 10))))
      (let ((arr (make-array '(10 10)))
            (ph  (um:range 0 0.1 1))
            (minv (list 0 0 -100)))
        (loop for j1 from 0
              for phi1 from 0 below 1 by 0.1
              do
              (setf (aref phs 1) phi1)
              (loop for j2 from 0
                    for phi2 from 0 below 1 by 0.1 do
                    (setf (aref phs 2) phi2)
                    (let* ((ys (apply #'map 'vector #'+
                                      (loop for ix from 1 to nosc
                                            collect
                                            (osc ix (aref phs (1- ix)))))))
                      (let ((y (- (reduce #'min ys) (reduce #'max ys))))
                        (setf (aref arr j2 j1) y)
                        (when (> y (third minv))
                          (setf minv (list phi1 phi2 y))))
                      )))
        ;; (plt:plot-image 'phs ph ph arr)
        (plt:tvscl 'phs arr :magn 16)
        minv
        ))))

;; optimal phases from Genetic Algorithm search
(map 'vector (um:rcurry #'/ (* 2 pi))
     #( 0.0
        0.429515
        3.16
        0.137262
        0.470874
        2.566408
        1.168097
        4.706253
        5.680273
        4.289010))

|#

(defun lisp-bin-gf* (a b)
  ;; right-to-left
  (when (< a b)  ;; get smaller arg in b
    (rotatef a b))
  (cond ((zerop b) 0)
        ((= 1 b)   a)
        (t  (do ((ans  a)
                 (mask (- (integer-length b) 2) (1- mask)))
                ((minusp mask) ans)
              (setf ans (step-state ans))
              (when (logbitp mask b)
                (setf ans (logxor ans a)))) )
        ))


;; #-:COM.RAL
(defstub c-gf571-mul)
#-:COM.RAL
(defstub c-gf128-mul)

(defun bin-gf* (a b)
  (cond
   ((or (zerop a)
        (zerop b))  0)
   ((= 1 a)  b)
   ((= 1 b)  a)
   (t  (case *nbits*
         (571 (with-fast-impl (c-gf571-mul a b)
                              (lisp-bin-gf* a b)))
         
         (128 (with-fast-impl (c-gf128-mul a b)
                              (lisp-bin-gf* a b)))
         
         (t   (lisp-bin-gf* a b)) ))
   ))
    
(defun gf* (a &rest args)
  (reduce 'bin-gf* args :initial-value a))

#|
(defun gf^ (x n)
  (labels ((expt (n)
             (do ((ans   1)
                  (limit (integer-length n))
                  (expon 0  (1+ expon))
                  (mul   x  (gf* mul mul)))
                 ((>= expon limit) ans)
               (when (logbitp expon n)
                 (setf ans (gf* ans mul))))))
    (if (minusp n)
        (gfinv (expt (- n)))
      (expt n))))
|#

(defun gf^ (x n)
  (cond
   ((zerop n)  1)
   ((= 1 n)    x)
   ((zerop x)  0)
   ((= 1 x)    1)
   (t          ;; right-to-left
               (labels ((local-expt (n)
                          (cond ((zerop n) 1)
                                ((= 1 n)   x)
                                (t  (do ((ans   x)
                                         (mask (- (integer-length n) 2) (1- mask)))
                                        ((minusp mask) ans)
                                      (setf ans (gf* ans ans))
                                      (when (logbitp mask n)
                                        (setf ans (gf* ans x))) ))
                                )))
                 (if (minusp n)
                     (gfinv (local-expt (- n)))
                   (local-expt n))))
   ))


(defun lisp-gf^ (x n)
  (cond
   ((zerop n)  1)
   ((= 1 n)    x)
   ((zerop x)  0)
   ((= 1 x)    1)
   (t          ;; right-to-left
               (labels ((local-expt (n)
                          (cond ((zerop n) 1)
                                ((= 1 n)   x)
                                (t  (do ((ans   x)
                                         (mask (- (integer-length n) 2) (1- mask)))
                                        ((minusp mask) ans)
                                      (setf ans (lisp-bin-gf* ans ans))
                                      (when (logbitp mask n)
                                        (setf ans (lisp-bin-gf* ans x))) ))
                                )))
                 (if (minusp n)
                     (lisp-gfinv (local-expt (- n)))
                   (local-expt n))))
   ))

(defun gfdeg (x)
  (1- (integer-length x)))

(defun lisp-gfinv (x)
  ;; extended Euclidean algorithm
  (let* ((u  x)
         (v  $prim)
         (g1 1)
         (g2 0))
    (loop until (= u 1) do
          (when (zerop u)
            (error "Zero has no inverse"))
          (let ((j (- (integer-length u) (integer-length v))))
            (when (minusp j)
              (rotatef u  v)
              (rotatef g1 g2)
              (setf j (- j)))
            (setf u  (gf+ u  (ash v  j))
                  g1 (gf+ g1 (ash g2 j)))
            ))
    g1))

#|
(defun tst-inv128 (x)
  (labels ((extract (x)
             (ldb (byte 128 0) x))
           (shl (x n)
             (extract (ash x n))))
  (let* ((u  x)
         (v     (extract $prim-128))
         (g1    1)
         (g2    0)
         j)
    (unless (= u 1)
      (setf g2 1
            u  v
            v  x
            j  (- 129 (integer-length x)))
      (setf u (logxor u (shl v j))
            g1 (shl g2 j)))
    (print u)
    (loop until (= u 1) do
          (when (zerop u)
            (error "Zero has no inverse"))
          (setf j (- (integer-length u) (integer-length v)))
          (when (minusp j)
            (rotatef u v)
            (rotatef g1 g2)
            (setf j (- j)))
          (setf u (logxor u (shl v j))
                g1 (logxor g1 (shl g2 j)))
          (print u))
    g1)))
|#          
  
#-:COM.RAL
(defstub c-gf128-inv)
#-:COM.RAL
(defstub c-gf571-inv)

(defun gfinv (x)
  (case x
    (0  (error "Division by zero"))
    (1  1)
    (t  ;; extended Euclidean algorithm
        (case *nbits*
          (571 (with-fast-impl (c-gf571-inv x)
                               (lisp-gfinv x)))
          
          (128 (with-fast-impl (c-gf128-inv x)
                               (lisp-gfinv x)))
          
          (t   (lisp-gfinv x)) ))
    ))

(defun gfmod (x)
  ;; produce x mod f(z)
  (gfinv (gfinv x)))

#|
(defun gfinv (x)
  ;; by Fermat's little theorem
  ;; x^-1 = (gf^ x (1- *gf-order*))
  (when (zerop x)
    (error "Zero has no inverse"))
  (if (= 1 x)
      1
    (let ((ans  1))
      (loop repeat (1- *nbits*) do
            (setf x   (gf* x x)
                  ans (gf* ans x)))
      ans)))
|#

#-:COM.RAL
(defstub c-gf128-div)
#-:COM.RAL
(defstub c-gf571-div)

(defun bin-gf/ (a b)
  (case b
    (0  (error "Division by zero"))
    (1  a)
    (t  (labels ((default-impl ()
                   (gf* a (gfinv b))))
          
          (case *nbits*
            (571 (with-fast-impl (c-gf571-div a b)
                                 (default-impl)))
            
            (128 (with-fast-impl (c-gf128-div a b)
                                 (default-impl)))
            
            (t (default-impl)) )))
    ))

(defun gf/ (a &rest args)
  (if args
      (bin-gf/ a (apply 'gf* args))
    (gfinv a)))

(defun gf^2 (x)
  (gf* x x))

(defun gf-sqrt (x)
  (gf^ x (ash 1 (1- *nbits*))))

;; ----------------------------------------

(defun gf-lisp-trace (x)
  (let ((tr 0))
    (loop for ix from 0 below *nbits*
          do
          (setf tr (gf+ tr x)
                x  (lisp-bin-gf* x x)
                ))
    tr))

(defun gf-trace (x)
  ;; Tr(x) = Sum(x^(2^i),{i,0,m-1}), for GF(2^m)
  ;; exactly 50% of the values in the GF have trace 1
  ;; also Tr(x^2) = Tr(x)
  (case *nbits*
    (571 (gf+ (ldb (byte 1   0) x)
              (ldb (byte 1 561) x)
              (ldb (byte 1 569) x)))
    (t
     (gf-lisp-trace x))
    ))

(defun compute-trace-bits ()
  (let ((v  '(0)))
    (loop for ix from 1 below *nbits* do
          (when (= 1 (gf-lisp-trace (ash 1 ix)))
            (push ix v)))
    v))

;; -----------------------------------------------------------

(defun ensure-trace (x tr)
  ;; make result on x have the same trace as tr
  (gf+ x (gf-trace x) tr))

(defun gen-bits-with-trace (nbits tr)
  ;; generate random bits with specified trace
  (ensure-trace (ctr-drbg-int nbits) tr))

;; -----------------------------------------------------------

(defun gf-htrace (x)
  ;; For *nbits* odd
  ;; Half-trace: HTr(x) = Sum(x^(2^(2*i)),{i,0,(m-1)/2), for m odd
  ;; HTr(x) and HTr(x)+1 are both solutions to y^2 + y = x in GF,
  ;; but only when Tr(x) = 0:  H(x)^2 + H(x) = x + Tr(x)
  (let ((htr 0))
    (loop for ix from 0 to (truncate *nbits* 2)
          do
          (let ((xsq (gf* x x)))
            (setf htr (gf+ htr x)
                  x   (gf* xsq xsq)
                  )))
    htr))

(defun gf-solve-quadratic (gamma)
  ;; sovle for y in: y^2 + y = gamma
  ;; No solution unless Tr(gamma) = 0
  (if (zerop (gf-trace gamma))
      (gf-htrace gamma)
    (error "No solution")))

;; -----------------------------------------------------------------------------
;;

(defstruct crypto-share
  x y)

(defun make-gf-lagrange-interpolator (shares)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (gf* prod (gf+ x2 x)))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum share)
                 (with-accessors ((x crypto-share-x)
                                  (y crypto-share-y)) share
                   (let ((xs (mapcar #'crypto-share-x (remove share shares))))
                     (gf+ sum
                          (gf* y (gf/ (lprod x0 xs)
                                      (lprod x xs)) )) ))) )
        (reduce #'term shares
                :initial-value 0))) ))

(defun solve-gf-lagrange (x0 &rest shares)
  (let ((fn (make-gf-lagrange-interpolator shares)))
    (funcall fn x0)))

;; -----------------------------------------------------------------------------
;;

(defun convert-text-to-int571 (str)
  (ldb (byte *nbits* 0)
       (convert-bytes-to-int
        (convert-text-to-int8-array str))))

(defun convert-int571-to-int8-array (x)
  (ensure-8bitv
   (loop for ix from (truncate *nbits* 8) downto 0 collect
         (ldb (byte 8 (* 8 ix)) x))))

;; ------------------------------------------------------

(defun tst-root (n poly)
  (um:nlet iter ((sum  0)
                 (poly poly)
                 (x    1))
    (if (zerop poly)
        (zerop sum)
      (gp-iter (if (logbitp 0 poly)
                   (gf+ sum x)
                 sum)
               (ash poly -1)
               (gf* x n))
      )))
    
(defvar *pfgrp128*
  ;; prime factors of group order (2^128 - 1)
  (let* ((facts '(3
                  5
                  17
                  257
                  641
                  65537
                  6700417
                  274177
                  67280421310721)))
    (assert (= (1- (ash 1 128))
               (reduce '* facts)))
    facts))

(defun #1=tst-suborders (n orders)
  (labels ((gftst (x fact)
             (when (= 1 (gf^ n (gf* x fact)))
               (return-from #1# nil))))
    (um:nlet outer ((m (1- (length orders))))
      (or (zerop m)
          (progn
            (um:nlet inner ((ords orders)
                            (m    m)
                            (x    1))
              (unless (or (zerop m)
                          (null ords))
                (if (= m 1)
                    (progn
                      ;; (print (mapcar (um:curry #'gf* x) ords))
                      (um:foreach (um:curry #'gftst x) ords))
                  (progn
                    (inner (cdr ords)
                           (1- m)
                           (gf* x (car ords)))
                    (inner (cdr ords)
                           m
                           x)))
                ))
            (go-outer (1- m)))
          ))
    ))

(defun gftst (n factor)
  (= 1 (gf^ n factor)))

(defun is-generator-128? (n)
  (with-gf2^128
    (and ;; (tst-root n $prim-128)
         (tst-suborders n *pfgrp128*))))

(defun find-generator-128 ()
  (um:nlet iter ((ix 2))
    (if (> ix 1000)
        (error "Give up!")
      (let* ((v (ctr-drbg-int 128)))
        (unless (is-generator-128? v)
          (go-iter (1+ ix)))
        v))))

;; ------------------------------------------------------

(defvar *pfgrp256*
  ;; prime factors of group order (2^256 - 1)
  (let* ((facts '(3
                  5
                  17
                  257
                  641
                  65537
                  274177
                  6700417
                  67280421310721
                  59649589127497217
                  5704689200685129054721)))
    (assert (= (1- (ash 1 256))
               (reduce '* facts)))
    facts))
  
(defun is-generator-256? (n)
  (with-gf2^256
    (and ;; (tst-root n $prim-256)
         (tst-suborders n *pfgrp256*))))

#|
(defun is-generator-256? (n)
    (notany (lambda (fact)
              (= 1 (gf^ n fact)))
            *pfgrp256*)
    ))
|#

(defun find-generator-256 ()
  (um:nlet iter ((ix 2))
    (if (> ix 1000)
        (error "Give up!")
      (let* ((v (ctr-drbg-int 256)))
        (unless (is-generator-256? v)
          (go-iter (1+ ix)))
        v))))

