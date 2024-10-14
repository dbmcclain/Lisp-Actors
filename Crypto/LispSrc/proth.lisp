;; proth.lisp -- Proth Numbers, Cullen Numbers, Fermat Numbers
;;
;; DM/RAL  08/16
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

(defpackage :ecc-crypto-b571-proth
  (:use :cl :LW :ecc-crypto-b571)
  (:export
   ))

(in-package :ecc-crypto-b571-proth)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(ecc-crypto-b571::add-mod
            ecc-crypto-b571::sub-mod
            ecc-crypto-b571::mult-mod
            ecc-crypto-b571::div-mod
            ecc-crypto-b571::sqrt-mod
            ecc-crypto-b571::expt-mod
            )))

;; Proth Numbers = k * 2^n + 1
;; Cullen Numbers = n * 2^n + 1
;; Fermat Numbers = 2^(2^n) + 1
;; ---------------------------------------------------------------

(defstruct prime-field
  q gen)

(defvar *prime-field*)

(define-symbol-macro *q*   (prime-field-q   *prime-field*))
(define-symbol-macro *gen* (prime-field-gen *prime-field*))

;; ---------------------------------------------------------------
;; Cullen n = 1, 141, ...
;; n = 1 -> N = 3
;; n = 141 -> N = 393050634124102232869567034555427371542904833

;; The prime field of Cullen 141
;; The field has generator 15
;; factors of c141-1 are 2^141 * 3 * 47
;; so this is good for FFT's to 2^141

(defvar +c141+
  (setf *prime-field*
        (make-prime-field
                :q  (1+ (* 141 (ash 1 141)))
                :gen 15)))
  
;; The prime field of Cullen 4713
;; the field has generator 5
;; factors of c4713-1 are 2^4713 * 3 * 1571
;; so this is good for FFT's to 2^4713

(defvar +c4713+
  (make-prime-field
   :q   (1+ (* 4713 (ash 1 4713)))
   :gen 5))

;; ---------------------------------------------------------
;; Some Proth numbers that we found with PRIMES::FIND-FIELD-FOR-FFT
;; Fields are named for the number of bits they support: e.g., +p5+ = 5 bits, 32 items / FFT
;; For running FFT's over 2^N items, you must choose a field that is at least as large as 2^N

(defvar +p3+ ;; 8 items
  (make-prime-field
   :q   17  ;; = 1 + 2^4
   :gen 2))

(defvar +p4+ ;; 16 items
  (make-prime-field
   :q   17 ;; = 1 + 2^4
   :gen 3))

(defvar +p5+ ;; 32 items
  (make-prime-field
   :q   97 ;; = 1 + 2^5 * 3
   :gen 19))

(defvar +p6+ ;; 64 items
  (make-prime-field
   :q   193 ;; = 1 + 2^6 * 3
   :gen 11))

(defvar +p7+ ;; 128 items
  (make-prime-field
   :q   257 ;; = 1 + 2^8
   :gen 9))

(defvar +p8+ ;; 256 items
  (make-prime-field
   :q   257 ;; = 1 + 2^8
   :gen 3))

(defvar +p9+ ;; 512 items
  (make-prime-field
   :q   7681 ;; = 1 + 2^9 * 3 * 5
   :gen 62))

(defvar +p10+ ;; 1024 items
  (make-prime-field
   :q   12289 ;; = 1 + 2^12 * 3
   :gen 49))

(defvar +p11+ ;; 2048 items
  (make-prime-field
   :q   12289 ;; = 1 + 2^12 * 3
   :gen 7))

(defvar +p12+ ;; 4096 items
  (make-prime-field
   :q   12289 ;; = 1 + 2^12 * 3
   :gen 41))

(defvar +p13+ ;; 8192 items
  (make-prime-field
   :q   40961  ;; = 1 + 2^13 * 5
   :gen 12))

(defvar +p14+ ;; 16384 items
  (make-prime-field
   :q   65537 ;; = 1 + 2^16
   :gen 15))

(defvar +p15+ ;; 32768 items
  (make-prime-field
   :q   65537 ;; = 1 + 2^16
   :gen 9))

(defvar +p16+
  (make-prime-field
   :q   65537 ;; = 1 + 2^16
   :gen 3))

(defvar +p17+ ;; 128k items
  (make-prime-field
   :q   786433 ;; = 1 + 2^18 * 3
   :gen 8))

(defvar +p18+ ;; 256k items
  (make-prime-field
   :q   786433 ;; = 1 + 2^18 * 3
   :gen 5))

(defvar +p19+ ;; 512k items
  (make-prime-field
   :q   5767169 ;; = 1 + 2^19 * 11
   :gen 12))

(defvar +p20+ ;; 1024k = 1M items
  (make-prime-field
   :q   7340033 ;; = 1 + 2^20 * 7
   :gen 5))

(defvar +p24+ ;; 16M items
  (make-prime-field
   :q   167772161 ;; = 1 + 2^25 * 5
   :gen 2))

(defvar +p32+ ;; 4G items
  (make-prime-field
   :q   77309411329 ;; = 1 + 2^33 * 3^2
   :gen 106))

(defvar +p48+
  (make-prime-field
   :q   4222124650659841 ;; = 1 + 2^48 * 3 * 5
   :gen 82))

(defvar +p64+
  (make-prime-field
   :q   221360928884514619393 ;; = 1 + 2^66 * 3
   :gen 3))

(defvar +p128+
  (make-prime-field
   :q   7145929705339707732730866756067132440577 ;; = 1 + 2^128 * 3 * 7
   :gen 41))

(defvar +p256+
  (make-prime-field
   :q   11810793102206251933204240470886166601033538435895337532024673568807139223273473 ;; = 1 + 2^257 * 3 * 17
   :gen 79))

(defvar +p512+
  (make-prime-field
   :q   6435747806372446607795531999138806141190095593884348821307309492986446734435302548864899663120113645291215291929513304409801863749734353574288151522920366081 ;; = 1 + 2^517 * 3 * 5
   :gen 160))

(defvar +p1024+
  (make-prime-field
   :q   559614682110099492812501788671860243476341761636824266865896905440775510475492278193596181565561763332306069703484452666657506198728526369155334337683457370223016341515869261045717757529895041150092669106522539558162409548104793251513032556589173556200023605740689762969026178716649817668300270839867024770187919361 ;; = 1 + 2^1039 * 5 * 19
   :gen 17))

(defvar +p4096+
  (make-prime-field
   :q   1787237660849618174262316025920388849855891142811666695677090510635702972904460013616271570583332863567286925166342178814396433538607182163008397031897487778625934365262437120518620567410378516242520195999307036613878640642251363137289978070801176707470239210676286018074009212072884772614765794812579789761584126398584405535401286029510449147473836027939250925105357330679146562271121985067139307682182557475780999989870056467445421095026749840973006894387438305129615051083904453565974675822252717976380670588080107725657498903724861863772858315830751729280029064246353231502868563074768381347255556391065366419192259246975003485135274618745313534695201125725857532835188013097917357116798042531883500842686669895451171893887352133722509844538057730086695862740030231240594983063015516869770446930136015392578411561073452239103632321507152375853834995964423789948801308260008356708124375498900541524141083057888752184028706127784665438844063846648992080462860309048733106955885181209799750827363319087316353952182370446497589266264109488186249632985069742250897010937005883971570454661001199467723215390424810299796192776097346624045635035748582609935079779205684950155797652506478270130973276074982999921995838012033871682414983122362826753 ;; = 1 + 2^4121 * 3 * 17
   :gen 20))

;; ----------------------------------------------------------

(defun f+ (&rest args)
  (apply #'add-mod *q* args))

(defun f- (&rest args)
  (apply #'sub-mod *q* args))

(defun f* (&rest args)
  (apply #'mult-mod *q* args))

(defun f/ (&rest args)
  (apply #'div-mod *q* args))

(defun finv (arg)
  (f/ 1 arg))

(defun f^ (base expon)
  (funcall #'expt-mod *q* base expon))

(defun fsqrt (arg)
  (funcall #'sqrt-mod *q* arg))

(defun fneg (n)
  (f- 0 n))

(defun fabs (n)
  (min n (fneg n)))

;; -----------------------------------------------------------
;; So now... we have n^(p-1) = 1, and p-1 = 2^n
;; so we can now define the 2^n'th root of unity for n up to 141
;; using arithmetic modulo +c141+.

;; If we want to run FFT's over 2^10 elements, we need the 1024'th root
;; of unity.
;;
;; n^(q-1) = 1 => m'th root of unity is n^((q-1)/m)

(defun split (xs)
  (optima:match xs
    (nil       xs)
    ((list _)  xs)
    ((list x y . tl)
     (multiple-value-bind (xs ys) (split tl)
       (values (cons x xs)
               (cons y ys))))
    ))

(defun bitrev (x)
  ;; NOTE: Not needed when using the recursively defined FFT.
  ;; But is needed for in-place array algorithms which leave elements
  ;; scrambled.
  (if (cdr x)
      (multiple-value-bind (es os) (split x)
        (nconc (bitrev es) (bitrev os)))
    x))

#|
(defun wn (n &optional (dir 1))
  ;; Nth root of unity, or its inverse if dir < 0
  ;; for n = power of 2
  (let ((ans (f^ *gen* (truncate (1- *q*) n))))
    (if (minusp dir)
        (finv ans)
      ans)))
|#

(defun wn (n &optional (dir 1))
  ;; Nth root of unity, or its inverse if dir < 0
  ;; for n = power of 2
  ;; Note: inverse of g^((q-1)/n) = g^((q-1)*(n-1)/n)
  (let ((r  (if (minusp dir)
                (/ (1- n) n)
              (/ n))))
    (f^ *gen* (* (1- *q*) r))
    ))

(defun fftx (xs dir)
  (cond ((null xs) xs)
        ((um:single xs) xs)
        (t
         (multiple-value-bind (es os) (split xs)
           (let* ((ys  (fftx es dir))
                  (zs  (fftx os dir))
                  (ts  (mapcar (let* ((wn  (f* (wn (length xs) dir)))
                                      (wnk 1))
                                 (lambda (z)
                                   (prog1
                                       (f* z wnk)
                                     (setf wnk (f* wnk wn)))))
                               zs))
                  (lf  (mapcar #'f+ ys ts))
                  (rt  (mapcar #'f- ys ts)))
             (nconc lf rt)
             )))
        ))

#|
(defun fftsym (xs)
  (cond ((null xs) xs)
        ((um:single xs) xs)
        (t
         (multiple-value-bind (es os) (split xs)
           (let* ((ys  (fftsym es))
                  (zs  (fftsym os))
                  (ts  (mapcar (let* ((wn  `(^ U ,(/ (length xs))))
                                      (wnk 1))
                                 (lambda (z)
                                   (prog1
                                       (optima:match wnk
                                         (1  z)
                                         (_  `(* ,wnk ,z)))
                                     (setf wnk
                                           (optima:match wnk
                                             (1  wn)
                                             ((list expt u n) `(,expt ,U ,(+ n (/ (length xs))))) )))))
                               zs))
                  (lf  (mapcar (lambda (a b)
                                       `(+ ,a ,b))
                               ys ts))
                  (rt  (mapcar (lambda (a b)
                                 `(- ,a ,b))
                               ys ts)))
             (nconc lf rt)
             )))
        ))
(fftsym '(x0 x1 x2 x3) =>
((+ (+ X0 X2) (+ X1 X3))
 (+ (- X0 X2) (* (^ U 1/4) (- X1 X3)))
 (- (+ X0 X2) (+ X1 X3))
 (- (- X0 X2) (* (^ U 1/4) (- X1 X3))))

(((x0 + x2) + (x1 + x3))
 ((x0 - x2) + w (x1 - x3))
 ((x0 + x2) - (x1 + x3))
 ((x0 - x2) - w (x1 - x3))
 ((1  1  1  1)
  (1  w -1 -w)
  (1 -1  1 -1)
  (1 -w -1  w))
 |#

(defun fft (xs &optional (dir 1))
  ;; use negative dir for inverse FFT
  (let ((ans (fftx xs dir)))
    (if (plusp dir)
        ans
      (mapcar (um:rcurry #'f/ (length xs)) ans))))

#|
(defun bitrev (x)
  (let* ((xv  (coerce x 'vector))
         (N   (length x))
         (N-2 (- N 2))
         num j L
         (scratch (copy-seq xv)))
    ;;; bit reverse the data (see Oppenheim and Schafer, 1989, p. 594)
    (dotimes (i N-2)
      (setf num (1+ i)
            J 0
            L N)
      (do ((k 1 (+ k k)))
          ((>= k n))
        (setf L (ash L -1))
        (when (>= num L)
          (incf j k)
          (decf num L)))
      (setf (aref scratch (1+ i)) (aref xv j)))
    scratch))
|#

#|
(defun evens (a)
  (when a
    (cons (car a) (odds (cdr a)))))

(defun odds (a)
  (when a
    (evens (cdr a))))

(defun bitrev (x)
  ;; bit reversal addressing shuffle - idempotent
  (if (cdr x)
      (let* ((a (evens x))
             (b (odds  x)))
        (append (bitrev a) (bitrev b)))
    ;; else
    x))
|#


#|
;; ----------------------------
;; Check that powers of roots of unity sum to zero

(let* ((n 1024)
       (wn (wn n)))
  (mod
   (loop repeat n
         for w = 1 then (f* w wn)
         sum w)
   *q*))
|#

;; ---------------------------------------------------------------------------------
#|
;; Since FT^2 = time reversal we can rewrite the bidirectional filtering in PLParEQ
;;
;;  sig -> filt -> timeRev -> filt -> timeRev -> out
;;
;; Now becomes, with timeRev = FT -> FT
;;
;;  sig -> filt -> FT -> FT -> filt -> FT -> FT -> out
;;
;; If we do filtering in the Fourier domain with multiplication:
;;
;;  sig -> (FT -> mult -> FTInv) -> FT -> FT -> (FT -> mult -> FTInv) -> FT -> FT -> out
;;     =
;;   sig -> FT -> mult -> FT -> FT -> mult -> FT -> out
;;
;; IOW, no more need of inverse FFT's. ... just a thought...
;;

(defun cv-float (n)
  (let ((absn (fabs n)))
    (cond ((= absn n)
           (float (/ n *q*) 1d0))
          (t
           (- (cv-float absn)))
          )))

(defun fdot (x y)
  (um:foldl #'+
         0 (mapcar #'f* x y)))

(let* ((*prime-field*  +p14+)
       (n 128)
       ;; (xs (make-list n :initial-element 1))
       ;; (xs (cons 1 (make-list (1- n) :initial-element 0)))
       ;; (xs (loop repeat n collect (f^ *gen* (lw:mt-random *q*))))
       #|
       (xs (nconc (loop repeat (truncate n 2) collect (f^ *gen* (lw:mt-random *q*)))
                  (make-list (truncate n 2) :initial-element 0)))
       |#
       #||#
       (xs (let ((xs (make-list n :initial-element 0)))
             (setf (nth 46 xs) (truncate n 2))
             xs))
       #||#
       #|
       (xs (nconc (make-list (truncate n 4) :initial-element 1)
                  (make-list (truncate n 2) :initial-element 0)
                  (make-list (truncate n 4) :initial-element 1)))
       |#
       (ans (fft xs))
       (px  (fdot xs xs))
       (py  (fdot ans ans))
       ;; (ans (fft (fft xs) -1))
       ;; (ans (mapcar (um:rcurry #'f/ n n) (fft (fft (fft (fft xs)))))) ;; identity signal
       ;; (ans (mapcar (um:rcurry #'f/ n) (fft (fft xs))))               ;; time-revversed signal
       )
  (terpri)
  (print (list :xs xs))
  (print (list :ys ans))
  (print (list :px px :py py))
  (plt:plot 'plt (map 'vector #'cv-float ans)
            :clear t
            ;;  :yrange '(-0.5 0.5)
            )
  (plt:plot 'plt (map 'vector #'cv-float xs)
            :color :red)
  ans)

(let* ((*prime-field* +p16+)
       (n  128)
       (xs (let ((vals nil))
             (do ((ix 0 (1+ ix))
                  (g  1 (* g *gen*)))
                 ((>= ix n) vals)
               (push g vals)))))
  xs)

(let* ((*prime-field* +p512+)
       (n    4096)
       (xs (let ((vals nil))
             (do ((ix  0   (1+ ix))
                  (g   1   (* g *gen*)))
                 ((>= ix n) (coerce vals 'vector))
               (push (cv-float g) vals))))
       (ys (fft:fwd xs)))
  ;; (setf (aref ys 0) 0)
  (plt:plot 'rplt (map 'vector #'abs ys)
            :clear t
            ;; :xrange '(0 10)
            )
  xs)
|#

#|
;; show that sqrt (q-1) acts like -1
(let* ((n     (f^ *gen* (lw:mt-random *q*)))
       (sqrt  (wn 2)))
  (assert (zerop (f+ n (f* n sqrt))))
  (assert (=
           (f* n sqrt)
           (fneg n)))
  (assert (= n (f* n sqrt sqrt))))
|#

