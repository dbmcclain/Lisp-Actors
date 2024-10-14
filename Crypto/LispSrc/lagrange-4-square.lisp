;; lagrange-4-square.lisp -- Lagrange 4-square decomposition of natural integers >= 0
;;
;; DM/Emotiq  01/18
;; --------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

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

(in-package :com.ral.crypto.lagrange-4-square)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0))
          (inline empty singleton create))

(declaim (inline mod4 mod8 trunc2 trunc4 expt2))

;; ------------------------------------------------------------------

(defun mod4 (n)
  (declare (integer n))
  (ldb (byte 2 0) n))

(defun mod8 (n)
  (declare (integer n))
  (ldb (byte 3 0) n))

(defun trunc2 (n)
  (declare (integer n))
  (ash n -1))

(defun trunc4 (n)
  (declare (integer n))
  (ash n -2))

(defun expt2 (n)
  (declare (fixnum n))
  (ash 1 n))

;; ------------------------------------------------------------------
;; DECOMPOSE-INTEGER -- main entry point

(defun decompose-integer (n)
  ;; slightly smarter version
  (declare (integer n))
  (check-type n (integer 0))
  (if (zerop n)
      '(0 0 0 0)
    ;; else - pull out common factor of (2^v)^2 = 4^v
    (multiple-value-bind (nn v)
        (nlet iter ((v 0))
          (declare (fixnum v)) ;; for all practical purposes...
          ;; check successive pairs of LSB bits = mod 4
          ;; WARNING: Don't feed this routine n = 0
          (if (zerop (ldb (byte 2 v) n))
              (go-iter (+ 2 v))
            ;; else - termination
            (if (zerop v)
                (values n 0)
              (values (ash n (- v))
                      (trunc2 v)))
            ))
      
      (declare (integer nn)
               (fixnum  v))
      ;; at this point we have removed 4^v common factor, N = (2^v)^2 * nn
      ;; so nn can only be 1,2,3 mod 4 = 1,2,3,5,6,7 mod 8
      ;;
      ;; when nn /= (7 mod 8) we have sum of 3 squares, nn = (a^2 + b^2 + c^2)
      ;;  so choose d = 0 for fourth one, N = (2^v)^2 * (a^2 + b^2 + c^2 + d^2).
      ;;
      ;; when nn = (7 mod 8), choose d = 1, and now (nn-1) = (6 mod 8) as sum of 3 squares
      ;;
      (let* ((d      (if (= 7 (mod8 nn)) 1 0))
             (nn-d   (- nn d)) ;; can never be 0 mod 4
             (sqrt   (if (= 1 (mod4 nn-d))
                         ;; squares can only be (0 mod 4) or (1 mod 4)
                         ;; we already removed all 4^v so can only be (1 mod 4)
                         (isqrt nn-d)
                       0))
             ;;
             ;; The following decomp are exceptions where we cannot
             ;; find a prime p = 1 mod 4, nor where there is x,y such
             ;; that nn-d = x^2 + y^2 discovered during search for
             ;; prime p.
             ;;
             ;; where n /= x^2 + p, for n = 1,2 mod 4
             ;; or where n /= x^2 + 2*p, for n = 3 mod 8
             ;; or where n /= x^2 + y^2 for n = 1,2 mod 4
             ;;
             ;; The Rabin & Shallit conjecture is that for
             ;; sufficiently large n, an appropriate prime can
             ;; always be found
             ;;
             (decomp (assoc nn-d '((   3 . ( 1  1  1)) ;; 3 mod 8
                                   ( 214 . ( 3  6 13)) ;; 2 mod 4 = 6 mod 8
                                   ( 526 . ( 6  7 21)) ;; 2 mod 4 = 6 mod 8
                                   (1414 . ( 6 17 33)) ;; 2 mod 4 = 6 mod 8
                                   ) )))
        (declare (fixnum d)
                 (integer sqrt nn-d)
                 (list decomp))
        
        (labels ((answer (a b c)
                   (declare (integer a b c))
                   (let ((ans (mapcar
                               (curry '* (expt2 v))
                               (list a b c d))))
                     (declare (cons ans))
                     (assert (= n (reduce '+
                                          (mapcar '* ans ans))))
                     ans)))
          
          (cond ((= nn-d (* sqrt sqrt)) ;; already a square?
                 (answer sqrt 0 0))
                
                (decomp
                 ;; special case by table lookup
                 (apply #'answer (cdr decomp)))
                
                ((= 3 (mod8 nn-d)) ;; nn-d = (3 mod 8)
                 ;;
                 ;; Find odd x s.t. prime p = (nn-d - x^2)/2.
                 ;; For odd x, (nn-d - x^2) is always even.
                 ;; Prime p = 1 mod 4.
                 ;;
                 (multiple-value-bind (x p) (find-prime-nx/2 nn-d)
                   (declare (integer x p))
                   (multiple-value-bind (y z) (decompose-prime p)
                     (declare (integer y z))
                     (answer x (+ y z) (abs (- y z))))))
                
                (t
                 ;; nn-d is 1,2,5,6 mod 8 = 1,2 mod 4.
                 ;;
                 ;; Can't be 0,4 mod 8 since we removed common factors
                 ;; of 4.
                 ;;
                 ;; Can't be 7 mod 8 since we removed that case in the
                 ;; LET above to form nn-d.
                 ;;
                 ;; Can't be 3 mod 8 since we took care of that in the
                 ;; previous COND clause.
                 ;;
                 ;; For nn-d = 1 mod 4 = 1,5 mod 8, it could be a
                 ;; square already but we took care of that in a
                 ;; previous COND clause.
                 ;;
                 (multiple-value-bind (x p is-prime) (find-prime-nx nn-d)
                   (declare (integer x p))
                   (cond (is-prime
                          (multiple-value-bind (y z) (decompose-prime p)
                            (answer x y z)))
                         (t
                          ;; (nn-d - x^2) was exact square, p is that square root
                          (answer x p 0))
                         )))
                ))))))

;; ------------------------------------------------------------------------
;; suport routines

(defun #1=find-prime-nx (n)
  ;; starting with largest possible x
  ;; find x such that prime p = n - x^2
  ;; return x and p
  ;;
  ;; n = 1,2,5,6 mod 8 = 1,2 mod 4
  ;;
  ;; we could also have p not prime and n = x^2 + p^2
  ;;
  (declare (integer n))
  (check-type n (integer 1))
  (assert (member (mod4 n) '(1 2)))
  (let ((m  (mod4 n)))
    (declare (fixnum m))
    (cond ((= 1 m)
           ;; check every x, since r = n - x^2 will always be 0,1 mod 4
           (loop for x of-type integer from (the integer (isqrt n)) downto 1 do
                 (let* ((r    (- n (* x x)))
                        (sqrt (isqrt r)))
                   (declare (integer r sqrt))
                   (cond ((= r (* sqrt sqrt))
                          (return-from #1# (values x sqrt nil)))
                         ((and (= 1 (mod4 r))
                               (is-prime? r))
                          (return-from #1# (values x r t)))
                         ))))
          (t ;; (= 2 m)
             ;; check every other x, since r = n - x^2 would bounce between 1,2 mod 4
             ;; and we only need 1 mod 4.
             ;; n can't be perfect square in this case. And we need x odd.
             (let ((start (isqrt n)))
               (declare (integer start))
               (when (evenp start)
                 (decf start))
               (loop for x of-type integer from start downto 1 by 2 do
                     (let* ((r    (- n (* x x))) ;; r always 1 mod 4
                            (sqrt (isqrt r)))
                     (declare (integer r sqrt))
                     (cond ((= r (* sqrt sqrt))
                            (return-from #1# (values x sqrt nil)))
                           ((is-prime? r)
                            (return-from #1# (values x r t)))
                           )))))
          )))
            
(defun #1=find-prime-nx/2 (n)
  ;; for case of n = 3 mod 8
  ;; starting with largest possible odd x
  ;; find x such that prime p = (n - x^2)/2
  ;; return x and p
  ;;
  ;; n = 3 mod 8 = 3 + 8*m, always odd
  ;; x odd = 2*k + 1
  ;; x^2 = 4*k^2 + 4*k + 1 = 1 mod 4, always odd
  ;; n - x^2 = 8*m - 4*k^2 - 4*k + 2 = 2 mod 4, always even
  ;; (n - x^2)/2 = 1 mod 4
  ;;
  (declare (integer n))
  (check-type n (integer 3))
  (assert (= 3 (mod8 n)))
  (let ((start (isqrt n)))
    (declare (integer start))
    (when (evenp start)
      (decf start))
    (loop for x of-type integer from start downto 1 by 2 do
          (let* ((r  (- n (* x x))) ;; always 2 mod 4, never a perfect square
                 (p  (trunc2 r)))   ;; r always even, p always 1 mod 4
            (declare (integer r p))
            (when (is-prime? p)
              (return-from #1# (values x p))
              ))) ))

(defun decompose-prime (p)
  ;; p = 1 mod 4 = x^2 + y^2
  ;; variant of Cornacchia's algorithm
  ;;
  ;; x (or y) must necessarily be even = 2*n
  ;; y (or x) must necessarily be odd  = 2*n + 1
  ;;
  (declare (integer p))
  (check-type p (integer 1))
  (assert (= 1 (mod4 p)))
  (let* ((b (if (= 5 (mod8 p)) ;; p = 5 mod 8
                2
              (nlet iter ((b 3))
                (if (= 1 (expt-mod b (trunc2 (1- p)) p)) ;; Fermat's theorem
                    (go-iter (next-prime b)) ;; try next smallest prime > b
                  b))))
         (bb (expt-mod b (trunc4 (1- p)) p))) ;; bb now imag unit, i.e., bb^2 = -1 mod p
    (declare (integer b bb))
    (nlet iter ((a  p)
                (b  bb))
      (declare (integer a b))
      ;; hmmm... this looks like Chinese Remainder Theorem...
      (if (> (* b b) p)
          (go-iter b (mod a b))
        (values b (mod a b))))))


(defun next-prime (b)
  ;; assumes b > 2, all higher primes are odd
  (declare (integer b))
  (check-type b (integer 3))
  (assert (and (oddp b)
               (> b 2)))
  (nlet iter ((p (+ b 2)))
    (declare (integer p))
    (if (is-prime? p)
        p
      (go-iter (+ p 2)))))

;; ----------------------------------------------------------------------------
#|
(defvar *big-p* 0)

(lw:defadvice (is-prime? :look-for-largest-prime :before) (p)
  (setf *big-p* (max *big-p* p)))

(lw:remove-advice 'is-prime? :look-for-largest-prime)
|#

;; -----------------------------------------------------------------------------
#|
 ;; testing...
 
(time (loop repeat 1000000 do (decompose-integer 512)))      ;;  2.5 sec/M

(time (loop for ix from 0 to 1000000 do (decompose-integer ix))) ;; 11.6 sec/M
 |#

;; -----------------------------------------------------------------------------
#|
 -- Confidentiality Protocol --
 
ECC curve C, generator G, widely known
Purchaser Bob, pubkey B a point on curve C
Vendor Alice, pubkey A a point on curve C
All UTXOs are stored as encrypted points u*G

Alice and Bob set up DH key k

Bob requests price from Alice with encrypted query,

Alice sends Bob encrypted price E(p,tid_A,k) with authentication signature

Bob verifies authentication, extracts tid_A, and price p
Bob computes UTXOs u sufficient to cover cost = p + fee
Bob computes Lagrange 4-squares x_i, i = 1..4 for difference (u - p - fee)
    u = p + fee + Sum(x_i^2)
Bob encrypts u, p with G:  u*G, p*G
Bob hashes H = Hash(u*G, p*G, fee, x_i, UTXO ids,tid_A,tid_B)

Bob publishes transaction:
     T = H, u*G, p*G, fee, x_i,A,B,tid_A,tid_B,UTXO_id's
  Bob authorizes destruction of input UTXO's, creation of p*G UTXO for Alice,
    and return change creation of UTXO = Sum(x_i^2)*G for Bob
    
Miner verifies:
     Hash H = Hash(u*G, p*G, fee, x_i, UTXO ids, tid_A, tid_B)
     u*G = p*G + fee*G + Sum(x_i^2)*G

Miner records u UTXO's as spent
Miner keeps fee
Miner records new UTXO p*G forwards to Alice along with tid_A
  Alice verifies UTXO from cost p and generator G
Miner returns new UTXO = Sum(x_i^2)*G along with tid_B to Bob, unless sum is zero
  Bob verifies UTXO from spent u, cost p, fee, and x_i

Only fee and x_i are visible
Only Bob knows amount paid, only Bob and Alice know cost of item.

 |#
;; -----------------------------------------------------------------------------

#|
 ;; routines to search for exceptional cases
 
(defun tst (n)
  ;; for n = 1,2 mod 4 try to express as n = x^2 + p, for p prime 1 mod 4,
  ;; or for which n = x^2 + p^2
  ;;
  ;; only exceptions are '(214 526 1414) tested to n = 1,000,000 => n2 = 4,000,002
  (let (ans)
    (loop for ix from 0 to n
          for n1 from 1 by 4
          for n2 from 2 by 4
          do
          (multiple-value-bind (x p is-prime) (find-prime-nx n1)
            (unless x
              (push n1 ans)))
          (multiple-value-bind (x p is-prime) (find-prime-nx n2)
            (unless x
              (push n2 ans))))
    (nreverse ans)))

(defun tst2 (n)
  ;; for n = 3 mod 8, try to express as n = x^2 + 2*p, for p prime 1 mod 4
  ;; only exception is 3, tested to n = 1,000,000 => n3 = 8,000,003
  (let (ans)
    (loop for ix from 0 to n
          for n3 from 3 by 8
          do
          (multiple-value-bind (x p) (find-prime-nx/2 n3)
            (unless x
              (push n3 ans))))
    (nreverse ans)))
|#
