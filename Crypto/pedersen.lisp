;; pedersen.lisp - a look at Pedersen Commitments
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------
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

(defpackage :pedersen
  (:use :common-lisp)
  (:import-from :ecc-crypto-b571
   :mult-mod
   :expt-mod
   :random-between
   )
  (:export
   ))

(in-package :pedersen)

(defstruct prime-field
  q   ;; order of the prime field (a large prime value)
  g   ;; generator for the prime field
  r   ;; large prime factor of (q-1) for prime subfield
  rg  ;; generator of the prime subfield
  )

#|
(defun new-prime-field (nbits)
  (let* ((prec  (primes::compute-diffie-hellman nbits))
         (q     (getf prec :dh-p)) ;; prime field
         (g     (getf prec :dh-g)) ;; field generator
         (fs    (primes:factors-of (1- q)))
         (r     (caar (last fs))) ;; large prime subgroup
         (rg    (expt-mod q g (truncate (1- q) r)))) ;; subgroup generator
    (make-prime-field
     :q  q
     :g  g
     :r  r
     :rg rg)))
|#
(defun new-prime-field (nbits)
  ;; generate prime q = 2*r + 1
  (let* ((q     (primes::par-gen-safe-prime nbits));; prime field
         (fs    (primes:factors-of (1- q)))
         (g     (primes::lucas-lehmer-test q fs)) ;; field generator
         (r     (caar (last fs))) ;; large prime subgroup
         (rg    (expt-mod q g (truncate (1- q) r)))) ;; subgroup generator
    (make-prime-field
     :q  q
     :g  g
     :r  r
     :rg rg)))

(defvar *prime-field*
  (make-prime-field
   ;; >= 1024 bits, q = 2*r + 1, q,r prime
   :q  #1=101142009669126330018810549957548254673720859796894121541003022336621904897238452620343723708597331458422761688963892345062116800346067933362699040259842057084268335378303869632478454890852931794688441597219114161829560012553085943899693301167747673624892451529198446757361128490933632731315254551080270734319
   :g  #2=43018132486292989725232649596793307734131223251215918365442950566865163314808444581780327500585826208206107858640716529959458688622257981720847367686810959341407293076819041424424291582915850777471325268280158872338931621043948199803772325047711336653918440768571546310158912802691151377311458116397305604978
   :r  #3=50571004834563165009405274978774127336860429898447060770501511168310952448619226310171861854298665729211380844481946172531058400173033966681349520129921028542134167689151934816239227445426465897344220798609557080914780006276542971949846650583873836812446225764599223378680564245466816365657627275540135367159
   :rg (expt-mod #1# #2# (truncate (1- #1#) #3#))
   ))

(define-symbol-macro *pf-q*  (prime-field-q *prime-field*))
(define-symbol-macro *pf-g*  (prime-field-g *prime-field*))
(define-symbol-macro *pf-r*  (prime-field-r *prime-field*))
(define-symbol-macro *pf-rg* (prime-field-rg *prime-field*))

(defun pf-mul (&rest args)
  (apply #'mult-mod *pf-q* args))

(defun pf-expt (x y)
  ;; x^y mod p
  (expt-mod *pf-q* x y))

(defun rand-val ()
  ;; random value in Z_r
  (random-between 1 *pf-r*))

(defstruct commitment
  q   ;; the prime field order
  ;; r   ;; the subgroup order - don't really need to share this, but can be computed from q
  g   ;; generator on the subgroup
  h   ;; random value in subgroup
  com ;; the commitment value
  )

(defun commit-vector (a-vec g-vec h-gen blinding-s)
  ;; h-gen = (pf-expt *pf-rg* secret-a)
  ;; keep secrets: secret-a, blinding-s
  (make-commitment
   :q   *pf-q*
   ;; :r   *pf-r* ;; not really needed
   :g   g-vec
   :h   h-gen
   :com (pf-mul
         ;; shield dot prod with blinding
         (pf-expt h-gen blinding-s)
         ;; compute g^(alpha .dot. a)
         (reduce 'pf-mul
                 (mapcar 'pf-expt g-vec a-vec)))
   ))

;; ---------------------------------------------------------------
;; COMMIT - For a vector of values a-vec, create a Pedersen Commitment
;;
;; This current version effectively forms the dot-product between
;; a-vec and a random blinding vector formed as g-vec. When ready to
;; reveal the commitment, share the a-vec and the random blinding-s
;; value formed during the commitment.
;;
(defun commit (a-vec)
  ;; send com portion, keep secret-a & blinding-s secret for now
  ;; when ready to reveal the commited values, send a-vec and blinding-s
  (let* ((secret-a   (rand-val))
         (blinding-s (rand-val))
         (g-vec      (make-gens (length a-vec)))
         (h-gen      (pf-expt *pf-rg* secret-a))
         (com        (commit-vector a-vec g-vec h-gen blinding-s)))
    (list :com com ;; public share
          :secret-a secret-a  ;; keep secret
          :blinding-s blinding-s))) ;; keep secret

;; -------------------------------------------------------------------

(defun make-gens (nel)
  ;; form a random blinding vector of generators g^alpha_i
  ;; ensure no duplicates (very small chance of that)
  (um:nlet iter ((nel nel)
                      (ans nil))
    (if (plusp nel)
        (let ((g-alpha (pf-expt *pf-rg* (rand-val))))
          (if (member g-alpha ans)
              (go-iter nel ans)
            (go-iter (1- nel) (cons g-alpha ans))))
      ans)))

(defun make-coffs (nel)
  ;; choose some random values for test a-vec
  (loop repeat nel collect
        (rand-val)))

#|
;; verify prime field
(pf-expt (pf-expt *pf-g* (truncate (1- *pf-q*) *pf-r*)) *pf-r*) ;; S.B. 1

;; try it out... 
(let* ((nel  5)
       (av   (make-coffs nel)))
  (commit av))
 |#
