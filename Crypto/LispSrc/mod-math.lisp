;; mod-math.lisp -- Prime Field Arithmetic
;; DM/Acudora 11/11
;; -----------------------------------------------------
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

(in-package :crypto-mod-math)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------

(defvar *blinders* (make-hash-table))

(defun create-blinder (m)
  (* m (ecc-crypto-b571:random-between #.(ash 1 31) #.(ash 1 32))))

(defun get-blinder (m)
  (or 0 ;; no blinding...
      (gethash m *blinders*)
      (setf (gethash m *blinders*) (create-blinder m))))

(defun reset-blinders ()
  (clrhash *blinders*))

;; -----------------------------------------------------
;; Prime-Field Arithmetic

#|
 ;; this is slower than just allowing Lisp to work
 ;; directly on bignums
(defun expt-mod-naf (m x e)
  (declare (integer base exponent modulus))
  (cond ((zerop x)  0)
        ((zerop e)  1)
        (t
         (let* ((nns   (naf e))
                (inv-x (inv-mod m x))
                (v     x))
           (declare (integer inv-x v))
           (loop for n in (cdr nns) do
                 (setf v (mult-mod m v v))
                 (case n
                   (-1  (setf v (mult-mod m v inv-x)))
                   ( 1  (setf v (mult-mod m v x)))
                   ))
           v))
        ))
|#

#||#
(defun expt-mod (modulus base exponent)
  ;; base^exponent mod modulus, for any modulus
  (declare (integer base exponent modulus))
  (let* ((exp (+ exponent (get-blinder (1- modulus))))
         (n   (integer-length exp)))
    (declare (fixnum n)
             (integer exp))
    (do ((b  (+ base (get-blinder modulus))
             (mult-mod modulus b b))
         (p  1)
         (ix 0    (1+ ix)))
        ((>= ix n) p)
      (declare (integer b p)
               (fixnum ix))
      (when (logbitp ix exp)
        (setf p (mult-mod modulus p b)))) ))
#||#

(defun quadratic-residue-p (m x)
  ;; aka Legendre Symbol (x|m)
  (= 1 (expt-mod m x (truncate (1- m) 2))))

(defvar *fq2-mod*)
(defvar *fq2-red*)

(defstruct fq2
  x y)

(defun fq2+ (a b)
  (um:bind* ((:struct-accessors fq2 ((ax x) (ay y)) a)
             (:struct-accessors fq2 ((bx x) (by y)) b))
    (make-fq2
     :x (add-mod *fq2-mod* ax bx)
     :y (add-mod *fq2-mod* ay by))))

(defun fq2* (a b)
  (um:bind* ((:struct-accessors fq2 ((ax x) (ay y)) a)
             (:struct-accessors fq2 ((bx x) (by y)) b))
    (make-fq2
     :x (add-mod *fq2-mod*
                 (mult-mod *fq2-mod* ax bx)
                 (mult-mod *fq2-mod* ay by (cadr *fq2-red*)))
     :y (add-mod *fq2-mod*
                 (mult-mod *fq2-mod* ax by)
                 (mult-mod *fq2-mod* ay bx))
     )))

(defun cipolla (m x)
  ;; Cipolla method for finding square root of x over prime field m
  (let* ((*fq2-mod* m)
         (*fq2-red* (um:nlet iter ((a  2))
                      (let ((v  (mod (- (* a a) x) m)))
                        (if (quadratic-residue-p m v)
                            (go-iter (1+ a))
                          (list a v))
                        )))
         ;; exponentiation in Fq^2: (a, sqrt(a^2 - n))^((q-1)/2)
         (xx   (make-fq2
                :x (car *fq2-red*)
                :y 1))
         (exp  (truncate (1+ m) 2))
         (n    (integer-length exp)))
    (declare (fixnum n)
             (integer exp))
    (do ((b  xx
             (fq2* b b))
         (p  (make-fq2
              :x 1
              :y 0))
         (ix 0    (1+ ix)))
        ((>= ix n) (fq2-x p))
      (declare (fixnum ix))
      (when (logbitp ix exp)
        (setf p (fq2* p b))))
    ))

(defun sqrt-mod (m x)
  ;; assumes m is prime
  ;; a^(m-1) = 1 for m prime
  ;; a^m = a
  ;; a^(m+1) = a^2
  ;; a^((m+1)/4) = a^(1/2) -- works nicely when m = 3 mod 4
  ;; 1/2 = 2/4 = 3/6 = 4/8 = 5/10 = 6/12 = 7/14 = 8/16
  ;; in general:  for m = (2k+1) mod 4k, use (m + (2k-1))/4k, k = 1,2,...
  (let* ((xx  (mod x m)))
    (cond ((= xx 1) 1)
          (t (let ((ix (isqrt xx)))
               (cond ((= xx (* ix ix)) ix)
                     ((quadratic-residue-p m xx)
                      (let ((x (cond
                                ((= 3 (mod m 4))
                                 (expt-mod m x (truncate (1+ m) 4)))
                                ((= 5 (mod m 8))
                                 (expt-mod m x (truncate (+ 3 m) 8)))
                                ((= 7 (mod m 12))
                                 (expt-mod m x (truncate (+ 5 m) 12)))
                                ((= 9 (mod m 16))
                                 (expt-mod m x (truncate (+ 7 m) 16)))
                                (t  (cipolla m x))
                                )))
                        (assert (= xx (mod (* x x) m)))
                        x))

                     (t (error "not a square"))
                     )))
          )))

#|
 ;; this is much slower than just allowing Lisp to work
 ;; on raw bignums
(defun mult-mod1 (m a b)
  (cond ((or (zerop a)
             (zerop b))  0)
        (t
         (let ((nns (naf (mod b m)))
               (sum (mod a m)))
           (loop for n in (cdr nns) do
                 (setf sum (mod (+ sum sum) m))
                 (case n
                   (-1  (setf sum (mod (- sum a) m)))
                   ( 1  (setf sum (mod (+ sum a) m)))
                   ))
           sum))
        ))
|#
#||#
(defun mult-mod (m arg &rest args)
  (declare (integer m arg))
  (let* ((blinder (get-blinder m)))
    (declare (integer blinder))
    (dolist (opnd args arg)
      (declare (integer opnd))
      (setf arg (mod (* arg (+ opnd blinder)) m)))
    ))

#|
(defun split* (m a b)
  (declare (integer m a b))
  (let* ((nbot (floor (integer-length m) 2))
         (alo  (ldb (byte nbot 0) a))
         (ahi  (mod (- a alo) m))
         (blo  (ldb (byte nbot 0) b))
         (bhi  (mod (- b blo) m)))
    (declare (integer nbot alo ahi blo bhi))
    (+ (mod (* alo blo) m)
       (mod (* alo bhi) m)
       (mod (* ahi blo) m)
       (mod (* ahi bhi) m))
    ))

(defun mult-mod2 (m &rest args)
  (declare (integer m))
  (let ((blinder (get-blinder m)))
    (reduce (lambda (prod x)
              (declare (integer prod x))
              ;; (mult-mod1 m prod x)
              (mod (split* m prod (+ x blinder)) m)
              )
            args)))
|#
#||#
#|
(defun %mult-mod (m a b)
  (declare (integer m a b))
  (when (< a b)
    (rotatef a b)) ;; smallest item in b
  (if (zerop b)
      0
    (do ((ans  a)
         (l    (- (integer-length b) 2) (1- l)))
        ((minusp l) ans)
      (setf ans (add-mod m ans ans))
      (when (logbitp l b)
        (setf ans (add-mod m ans a)))) ))

(defun mult-mod (m &rest args)
  (declare (integer m))
  (reduce (lambda (prod x)
            (declare (integer prod x))
            (%mult-mod m prod x))
          args))
|#
;; ------------------------------------------------------------
#|
(defun add-mod (m arg &rest args)
  (declare (integer m arg))
  (if args
      (let ((blinder (get-blinder m)))
        (declare (integer blinder))
        (reduce (lambda (sum x)
                  (declare (integer sum x))
                  (mod (+ sum (+ x blinder)) m))
                args
                :initial-value (+ arg blinder)))
    (mod arg m)))

(defun sub-mod (m arg &rest args)
  (declare (integer m arg))
  (let ((blinder (get-blinder m)))
    (declare (integer blinder))
    (if args
        (reduce (lambda (diff x)
                  (declare (integer diff x))
                  (mod (- diff (+ x blinder)) m))
                args
                :initial-value (+ arg blinder))
      (mod (- (+ arg blinder)) m))))
|#
;; ------------------------------------------------------------
#|
(defun add-mod (m arg &rest args)
  (declare (integer m arg))
  (let ((blinder (get-blinder m)))
    (declare (integer blinder))
    (mod
     (if args
         (reduce (lambda (sum x)
                   (declare (integer sum x))
                   (+ sum (+ x blinder)))
                 args
                 :initial-value (+ arg blinder))
       (+ arg blinder))
     m)))

(defun sub-mod (m arg &rest args)
  (declare (integer m arg))
  (let ((blinder (get-blinder m)))
    (declare (integer blinder))
    (mod
     (if args
         (reduce (lambda (diff x)
                   (declare (integer diff x))
                   (- diff (+ x blinder)))
                 args
                 :initial-value (+ arg blinder))
       (- (+ arg blinder)))
     m)))
|#

(defun add-mod (m arg &rest args)
  (declare (integer m arg))
  (let* ((blinder (get-blinder m))
         (ans     (+ arg blinder)))
    (declare (integer blinder ans))
    (dolist (opnd args)
      (declare (integer opnd))
      (incf ans (+ opnd blinder)))
    (mod ans m)))

(defun sub-mod (m arg &rest args)
  (declare (integer m arg))
  (let* ((blinder (get-blinder m))
         (ans     (- arg blinder)))
    (declare (integer blinder ans))
    (if args
        (dolist (opnd args)
          (declare (integer opnd))
          (decf ans (+ opnd blinder)))
      (setf ans (- ans)))
    (mod ans m)))

;; ------------------------------------------------------------
#|
(defun extended-gcd (a b)
  (declare (integer a b))
  ;; solve Bezout's identity: a*x + b*y = gcd(a,b)
  ;; for (x, y) => x is the modular multiplicative inverse of a modulo b,
  ;; when a coprime to b.
  (multiple-value-bind (q m) (truncate a b)
    (declare (integer q m))
    (if (zerop m)
        (values 0 1)
      ;; else
      (multiple-value-bind (x y) (extended-gcd b m)
        (declare (integer x y))
        (values y
                (- x (* y q))) ))))

(defun inv-mod (m a)
  (declare (integer m a))
  (assert (= 1 (gcd a m))) ;; ensure A coprime with M
  (mod (extended-gcd a m) m))
|#

(defun inv-mod (m a)
  (declare (integer m a))
  (let* ((u  (mod a m))
         (v  m)
         (x1 1)
         (x2 0))
    (declare (integer u v x1 x2))
    (do ()
        ((= u 1) x1)
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (sub-mod m x2 (mult-mod m q x1))))
          (declare (integer x))
          (shiftf v u r)
          (shiftf x2 x1 x)) ))))


(defun div-mod (m arg &rest args)
  (declare (integer m arg))
  (dolist (opnd args
                (if args arg (inv-mod m arg)))
    (declare (integer opnd))
    (setf arg (mult-mod m arg (inv-mod m opnd)))))

#|
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
          (um:take 17 *mrs-cs*)))

(defvar *mrs-bigm*
  (reduce '* *mrs-ms*))

(defvar *mrs-ims*
  (let* ((mks (mapcar (um:curry 'truncate *mrs-bigm*) *mrs-ms*)))
    (mapcar (lambda (mk m)
              (* mk (inv-mod m (mod mk m))))
            mks *mrs-ms*)))

(defun to-rns (bignum)
  (mapcar (um:curry 'mod bignum) *mrs-ms*))

(defun from-rns (rns)
  (mod (reduce '+ (mapcar '* rns *mrs-ims*)) *mrs-bigm*))

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
  (mapcar (lambda (a b m)
            (mod (* a b) m))
          a b *mrs-ms*))

(defun rns-add (a b)
  (mapcar (lambda (a b m)
            (mod (+ a b) m))
          a b *mrs-ms*))

(defun rns-sub (a b)
  (mapcar (lambda (a b m)
            (mod (- a b) m))
          a b *mrs-ms*))

(defun xmul (a b)
  (from-rns
   (rns-mul (to-rns a)
            (to-rns b))))
  |#
  
#|
;; test I/M ratio (I = Inversion, M = multiply)
(time
 ;; abt 650 usec/I
 (loop repeat 1000 do
       (let ((x (random-between 1 *ed-q*)))
         (inv-mod *ed-q* x))))
(time
 (loop repeat 1000 do
       ;; abt 1000 usec/I
       (let ((x (random-between 1 *ed-q*)))
         (expt-mod *ed-q* x (- *ed-q* 2)))))

(time
 (loop repeat 10000 do
       ;; abt 100 usec/M
       (let ((x (random-between 1 *ed-q*)))
         (mult-mod *ed-q* x x)
         ;; (xmul x x )
         )))
;; So... I/M ~ 6
 |#