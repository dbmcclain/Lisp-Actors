;; nextprime.lisp
;;
;; DM/RAL  2024/03/07 04:08:21 UTC
;; ----------------------------------

(defpackage #:nextprime
  (:use #:common-lisp)
  (:export
   #:next-prime
   ))

(in-package #:nextprime)

;; ----------------------------------

(defun next-prime (n)
)

;; -----------------------------------

Euclid's Algorithm for GCD
--------------------------

x = a*q^n
y = b*q^m

where a, b, not divisible by q nor each other, and n < m. So gcd(x,y) = q^n.

y/x = (b/a)*q^(m-n)