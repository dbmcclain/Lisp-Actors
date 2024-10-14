
#|
b_i = (A_i*x + ψ_i) mod p

for prime modulus p, 1 < x < p, i in {1..NRows},
ψ_i zero mean, max|ψ_i| < 2^(NNoise-1), where NNoise << log2(p)

Choose ψ', x' = (b_1 - ψ')/A_1 mod p.

Then, if:

   d  = (b_2 - A_2*x') mod p
  |d| < 2^(NNoise-1)

try again with i=3,4,...,NRows. At i=NRows if you succeed, then you
have definitely found the solution for x. Otherwise, pick another
trial ψ' and try again.

There must be a solution somewhere over the interval -2^NNoise/2 < ψ' <
2^NNoise/2.

For a system with NRows = 320, NCode = 256, NNoise = 55, and p =
2^320-197 prime, the search space for ψ' is 2^55. This might be
feasible in reasonable time on an ordinary computer.

Abs in mod p arith:

  |x| mod p --> if x < p/2 then x else (p - x)
|#

(in-package :com.ral.crypto.lattice-crypto)

(defun #1=solve (sys pkey)
  (let* ((p          (getf sys :modulus))
         (mat-a      (getf sys :mat-a))
         (nnoise     (getf sys :nnoise))
         (nrows      (getf sys :nrows))
         (ainv       (with-mod p
                       (m/ (aref (aref mat-a 0) 0))))
         (b1         (aref pkey 0))
         (p/2        (/ p 2))
         (half-noise (ash 1 (1- nnoise)))
         (noise      (ash 1 nnoise)))
    (loop for ix from 0 below noise do
            (let* ((psi   (- ix half-noise))
                   (x     (mod (* ainv (- b1 psi)) p)))
              (when (um:nlet iter ((row-ix  1))
                      (if (>= row-ix nrows)
                          t
                        (let* ((d     (mod (- (aref pkey row-ix)
                                              (* (aref (aref mat-a row-ix) 0) x))
                                           p))
                              (absd  (if (> d p/2) (- p d) d)))
                          (when (< absd hnoise)
                            (go-iter (1+ row-ix)))
                          )))
                (return-from #1# (list ix psi x))
                )))
    ))

(let* ((sizes  '( 32  33   34  35))
       (nnoise '( 24  25   26  27))
       (times  '(2.2 6.6 12.5  25))
       ))

(let* ((nbits  35)
       (p      ;; (- (ash 1 10) 3)
               ;; (- (ash 1 11) 9)
               ;; (- (ash 1 12) 3)
               ;; (- (ash 1 16) 15)
               ;; (- (ash 1 32) 5)
               ;; (- (ash 1 33) 9)
               ;; (- (ash 1 34) 41)
               (- (ash 1 35) 41)
               )
       (sys    (fgen-sys :nbits nbits
                         :ncode 1
                         :nrows nbits
                         :ncols 1
                         :modulus p))
       (skey   (fgen-skey sys))
       (pkey   (fgen-pkey skey sys)))
  (format t "~%NNoise = ~A" (getf sys :nnoise))
  (time (solve sys pkey)))

(let* ((ntrials  10000)
       (nbits    320)
       (half     (ash 1 (1- nbits)))
       (one      (ash 1 nbits))
       (coll     (loop repeat ntrials collect
                       (let ((x (- (prng:ctr-drbg-int nbits) half)))
                         (/ (abs x) one)
                         ))))
  (plt:histogram 'xhisto coll
                 :clear t)
  (list :mn (float (vm:mean coll))
        :sd (float (vm:stdev coll))))


(let* ((ncode   256)
       (ncols     2)
       (nsecur  128)
       (nnoise  (/ nsecur ncols))
       ;; (nnoise  nsecur)
       (nsigma    6))
  (um:nlet iter ((nrows  (+ ncode nnoise)))
    (let* ((nunit  (- nrows ncode))
           (xnoise (noise-nbits nunit nrows nsigma)))
      (if (> xnoise nnoise)
          (list :nbits  nrows
                :nrows  nrows
                :ncode  ncode
                :nnoise xnoise
                :nsigma nsigma
                :nunit  (- nrows ncode))
        (go-iter (1+ nrows))
        ))))


#| For uniform distributions, there is equal probablility of being
chosen from any N subgroups of the range. So consider 3 grades of
difficulty, depending on the location from which a uniformly
distributed item is chosen. Call them Easy, Moderate, and Difficult.
Then the probability of a single trial being difficult is 1/3.

What we would like is that among N trials, at least one of them would
be difficult. The probability of all of them being Easy or Moderate is
(2/3)^N. If we want that to be miniscule, below 2^(-s), then:

   N*Log2(2/3) < -s =>  N > -s/Log2(2/3) = 1.71*s.

   So for s = 128, N >= 219, 2^128 ≈ 10^38
   10^12 ≈ 2^40, so s = 40, N >= 69.

Trial solution of multi-component Secret Key, with NCols elements,
requires guessing NCols values of ψ_i, then solving a NCols x NCols
matrix equation for the trial Secret Key components. Then substitute
in remaining rows to verify solution according to:

      |(b_i - A.x)| mod p < 2^NNoise.

The ψ_i are uniformly distributed over the 2^NNoise range. We want the
hard category to be ≥ 128 bits of brute force search, in the upper 1/3rd
of the noise range. So NNoise = 191, and:

   Easy:       0.. 63 bits search
   Moderate:  64..127
   Hard:     128..191

Consider the case for NCols > NRows, and additive noise seems
unnecessary. But we still want to prevent a serendipitous brute force
attack on the Secret Key elements. Because we want to transport
256-bit keys in the KEM, we have log2(p) > 256. Suppose we use 257
bits.

We still want a Hard search to require ≥ O(2^128). In a brute force
search the fraction below 128 bits in our numbers is 128/258 = 0.496.
The Hard part has probability 0.504. How many numbers must be attacked
in order to ensure at least one Hard case?

Looking at N elements, we get:

     eps = (64/129)^N

Since all NCols elements must be attacked, and NCols > NRows, and
NRows = NBits, and NBits > 256, it certainly looks like we have key
security ≈ O(2^NCols). For NRows = NBits = 257, NCols = 258, security
is 258 bits. Encrypted messages are 8,514 Bytes.

  |#