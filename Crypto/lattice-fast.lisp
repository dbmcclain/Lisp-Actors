;; lattice-fast.lisp -- Multi-bit LWE Encryption
;; DM/RAL 01/24
;; ----------------------------------------------

(in-package :com.ral.crypto.lattice-crypto)

;; ----------------------------------------------
;;
;; The goal of this code is to enable sending an encryption key for an
;; accompanying AES-256 ciphertext package, across the network, using
;; as little network bandwidth as possible. We want to send an entire
;; 256-bit key along with the AES cryptotext package.
;;
;; Public knowledge is the System Matrix and Public Keys, as well as
;; the LWE Encryption Algorithm used here. Secret Keys are kept
;; hidden. The noise added to the Public Keys is discarded and
;; forgotten, as is the selection vector used for every new
;; encryption.
;;
;; -----------------------------------------------------------

#|
;; For 8-bit transfers
(defparameter *flat-nbits* 26)
(defparameter *flat-ncode* 8)
(defparameter *flat-nrows* 160)
(defparameter *flat-ncols* 128)
|#

;; -----------------------------------------------------------
;; Using a bigger system allows us to connect across the network,
;; sending a 256-bit key using 3990 bytes = 70*456 bits.
;;
;; Compare with 366kB for 1-bit transfers, and 20kB for 8-bit
;; transfers.
;;
;; The cost is BIGNUM arithmetic instead of FIXNUM. But it was
;; worthwhile.
;;
;; ---------------------------------------------------------------
;; Review of literature suggests that Subset-Sum is most difficult to solve
;; for Density = 1, and when exactly half of the selections are taken.
;;
;;   Density = NRows / (Log2 (Max A_i))
;;
;; Literature search shows best attack complexities, Time & Space,
;; around 2^(0.22*NRows). Densities near unity are the most difficult
;; and tend toward exponential difficulty. Too low or too high density
;; results in sub-exponential or polynomial complexity.
;;
;; -- For Secret Key Security --
;;
;; We should use NCols > 1 in the System Matrix because, with only one
;; column, you can mount a restricted search for coincidence over the
;; noise intervals, which are relatively small. For NBits = 320, NCode
;; = 256, we have NNoise = 55 bits.
;;
;; For NCols = 1, each Public Key element is just:
;;
;;    b_i = (A_i*x + ψ_i) mod p
;;
;; And the Secret Key, x, is just a single 320-bit number. The only
;; uncertainty between Public Key elements is the 55-bit noise, ψ_i.
;;
;; But by having each Public Key element depend on more than one
;; Secret Key element:
;;
;;    b_i = (A_i1*x_1 + A_i2*x_2 + ... + ψ_i) mod p
;;
;; it becomes impossible to mount a search over a single
;; noise-restricted space to solve for a single numnber, x. You have
;; to search NCols noise-restricted spaces, then solve a square matrix
;; for a provisional Secret Key, and verify the key by checking on the
;; noise bounds in the remaining rows of the system.
;;
;; The Secret Key is protected by NCols*NNoise bits of keying.
;;
;; -- For Encryption Security --
;;
;; We rely on the Subset-Sum problem - which random selection of
;; elements from the Public Key and System Matrix were involved in
;; forming the cryptotext?
;;
;; We also invoke random selection with weights in the set
;; {-3,-2,-1,0,1,2,3}, making our overt Subset-Sum complexity
;; O(7^NRows) ≈ O(2^(2.81*NRows)).
;;
;; Hence against the current best attacks, we have effective security
;; around (0.22*2.81*NRows) = 0.62*NRows bits. For NRows = 320, that
;; gives us 2^197 security.
;;
;; (I also get the impression from the research papers on best
;; attacks, that they are considering arithmetic modulo 2^k instead of
;; modulo p prime. Binary overflow/wrap does not change bits in the
;; range of the modulus. It simply discards overflow bits. Modular
;; arithmetic against a prime modulus does change significant bits.
;; Hence our use of prime modulus may also make the problem harder.)
;;
;; The Subset-Sum problem is at its most difficult when:
;;
;;  (A) the random selection used for encryption contains exactly
;; NRows/2 elements, and
;;
;;  (B) the difficulty tends toward exponential when the Density of
;;  the problem is very close to unity:
;;
;;     Density = NRows / Log2(max A_i), for system matrix A.
;;
;;  This tends to be near unity when NRows = NBits.
;;
;; The statistics of a Binomial Distribution shows that NRows/2 is the
;; peak of the selection bits histogram, with a stdev around
;; (1/2)*Sqrt(NRows). The 1-sigma width is only around ±5.6% of peak
;; value (NRows/2) for NRows = 320.  And at ±3σ, a call to generate
;; 320 random bits should produce 160±27 non-zero bits.
;;
;; Surely, a deviation of only a few percent from exactly NRows/2
;; would not be enough to significantly ease the problem.(??)
;;
;; -------------------------------------------------------------------
;;
;; --- NOTE: WHAT NOT TO DO - EVER! ---
;;
;; You should *NEVER* choose NCols > NRows. Period!
;;
;; Doing so sets up a null-space of dimension (NCols-NRows), in which
;; a nearly unlimited number of Secret Key synonyms can easily be
;; found.
;;
;; Finding them simply requires transforming the System Matrix
;; and Public Key so that the left side of the System Matrix contains
;; an identity matrix.
;;
;; Then it becomes trivial to find Secret Keys as a parameterization
;; in each of the extra (NCols-NRows) dimensions.  Every possible
;; coordinate value in those extra dimensions generates a Secret Key
;; synonym.
;;
;; Adding noise to the published PKey does nothing to protect
;; encrypted messages. The purpose of the added noise was to make it
;; difficult to find the SKey, not to cloak the encryption.
;;
;; But since you can easily find pseudo Skeys, which all behave the
;; same during decryption as the actual Skey, it won't matter which
;; pseudo SKey you choose. The pseudo SKeys are just the SKey with an
;; added multiple of some null-space vector. And all vectors in the
;; null-space do nothing during decryption of encrypted messages.
;;
;; --- Sizing Things Up ---
;;
;; Our goal is to minimize the size of the KEM transfer, which has
;; size NBits*(1 + NCols).  Hence we make, NCols < NRows, for which it
;; is always possible to form a square matrix from NCols rows. To
;; protect against direct algebraic inversion to reveal the Secret
;; Key, we add noise to each element of the Public Key.
;;
;; NBits is determined by the need to accommodate NCode bits for
;; message, and NNoise bits of random noise added to Public Key
;; elements, plus NGuard bits to accommodate the growth of noise sums
;; during encryption.
;;
;; --- How Much Noise? ---
;;
;;   -- Given --
;;   NCode     = 256
;;   NSigma    =   6
;;   NSecurity = 256
;;
;;   -- Constraints --
;;   NBits  = NCode + NGuard + NNoise
;;   NRows  = NBits
;;   NGuard = log2(NSigma) + (1/2)*log2(3*NRows)
;;   NNoise = NSecurity / NCols
;;   NNoise > (1/2)*Sqrt(NRows)
;;   NModulus = NBits
;;
;;   -- Iterative Solution --
;;   NBits  = 280 ;; = 35 Bytes
;;   NGuard =   7
;;   NNoise =  17
;;   NCols  =  16
;;   Modulus = 2^280-47
;;
;; Since the size of noise is so much smaller than the overall Secret
;; Key size, an attacker would search the noise for NCols elements,
;; then derive trial key elements via matrix solution. A brute force
;; search guesses NCols values of noise, ψ_i, subtracting that guess
;; from the corresponding Public Key elements, for NCols rows. Then
;; solve the square matrix equation on those rows of the System Matrix
;; for NCols trial values of Secret Key elements.
;;
;; Noise is zero mean, sampled from a Uniform Distribution with total
;; range 2^NNoise. If you have the correct Secret Key elements, then
;; the remaining rows of Public Key should show residuals that are
;; just noise:
;;
;;    Abs(b_i - A . x) mod p < 2^(NNoise-1)
;;
;; If not, then try another guess on NCols noise values and iterate.
;; It is known that there is a solution among the NCols noise values.
;; We want to make finding it Hard.
;;
;; --- What Effective Security Do We Have? ---
;;
;; The Secret Key is protected by the additive noise on the Public
;; Key. It has an effective size of:
;;
;;   Effective Protective KeySize = NCols * NNoise.
;;
;; From the previous section on the security of encryptions, with
;; NRows = NBits, hence Density ≈ 1.00 for every one
;; of the visible NCols Row-Sums in the vector component of the
;; encryption. We have encryption securty ≈ O(2^0.58*NBits).
;;
;; Encryptions require NBits*(NCols+1) bits to convey an NCode-bits
;; message.
;;
;; ----------------------------------------------------------------------

(defparameter *flat-nbits*    280) ;; = NRows for density = 1
(defparameter *flat-ncode*    256)
(defparameter *flat-nrows*    280)
(defparameter *flat-ncols*     16)
(defparameter *flat-nnoise*    17)

(defparameter *flat-modulus*  (- (ash 1 280) 47))

;; ---------------------------------------------

(defun fvdot (v1 v2)
  (reduce #'+ (map 'vector #'* v1 v2)))

;; ---------------------------------------------

(defun sum-nbits (nbits nsum nsigma)
  ;; For nsum of products with n-bit numbers, each from a uniform
  ;; distribution, what is the expected max sum width in bits?
  ;;
  ;; For each number E(x) = 2^(NBits-1), Var(x) = 1/3*2^(2*NBits-2).
  ;; Summing with NSum terms produces a variance NSum times larger.
  ;;
  ;; Var(x*y) = E(x)^2*Var(y) + E(y)^2*Var(x)
  ;;          = 2*E(x)^2*Var(x)
  ;;          = 2*2^(2*Nbits-2)*2^(2*NBits)/12
  ;;          = 2^(2*NBits-2+1+2*NBits-2)/3
  ;;          = 2/3*2^(4*NBits-4)
  ;;
  ;; Stdev(x*y) ≈ 2^(2*NBits-2)*Sqrt(2/3)
  ;;
  ;; Hence E(x*y) = E(x)*E(y) = E(x)^2 = 2^(2*NBits-2).
  ;; E(Sum(x_i*y_i,{i,1,NSum})) = NSum*E(x*y) = NSum*2^(2*NBits-2))
  ;; Var(Sum(x_i*y_i,{i,1,NSum})) = NSum*Var(x*y) = 2/3*NSum*2^(2*(2*NBits-2))
  ;;
  ;; For NSigma guard max, we could expect to need:
  ;;
  ;;  Max ≈ NSum*2^(2*NBits-2) + NSigma*Sqrt(2/3*NSum)*2^(2*NBits-2))
  ;;      ≈ 2^(2*NBits-2)*NSum*(1 + NSigma*Sqrt(2/(3*NSum)))
  ;;
  ;; So, Log2(Max) ≈ 2*NBits-2+Log2(NSum)+1
  ;;
  (+ (* 2 nbits) -2
     (log nsum 2)
     (log (+ 1 (* nsigma (sqrt (/ 2 (* 3 nsum))))) 2)
     ))

(defun density (sys)
  (let* ((nrows    (getf sys :nrows))
         (ncols    (getf sys :ncols))
         (mat-a    (getf sys :mat-a))
         (maxnorms (loop for colix from 0 below ncols collect
                           (loop for row across mat-a maximize
                                   (aref row colix)))))
    (map 'vector (lambda (maxnorm)
                   (/ nrows (log maxnorm 2)))
         maxnorms)))

(defun fcheck-system (sys &optional (nsigma 6))
  (declare (ignore nsigma))
  (let ((ncols   (lat2-ncols sys))
        (nrows   (lat2-nrows sys))
        (nnoise  (getf sys :nnoise)))

    (when (< ncols 2)
      ;; For thwarting algebraic attacks on Public Key and System
      ;; Matrix to find the Secret Key.
      (error "NCols should be > 1: ~A" ncols))
   
    (when (< (* 0.62 NRows) 128)
      ;; For 128-bit encryption security
      (error "NRows should be > 206: ~A" nrows))

    (let ((density (density sys)))
      (when (some (um:rcurry #'< 0.999) density)
        (error "Density too low in at least one column: ~A" density))
      (when (some (um:rcurry #'> 1.001) density)
        (error "Density too high in at least one column: ~A" density)))
    
    (unless (> nnoise (* 0.5 (log nrows 2)))
      (error "Too few noise bits: ~A" nnoise))
    ))

;; --------------------------------------------------------------------
;; If we want NCode = 256, and NNoise > 128, then NRows = NBits = 393
;; If we want NCode = 256, and NCols * NNoise > 128, then NRows = NBits = 329
;; --------------------------------------------------------------------
(defun nrows-for-nnoise (ncode nnoise nsigma)
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

(defun noise-nbits (nbits-for-unit nrows nsigma)
  ;;
  ;; -- Bipolar Noise Values --
  ;;
  ;; With Subset-Sum weights, eps, in {-3,-2,-1,0,1,2,3}^NRows, we
  ;; effectively add Log2(6) bits to the N-bit noise values.
  ;;
  ;; For a uniform distribution of width 2^(N+Log2(6)), the variance
  ;; is (1/12)*2^(2*N+2*Log2(6)).  If we sum NRows of these together,
  ;; the variance becomes NRows/12*2^(2*N+2*Log2(6)).  We need to allow
  ;; for some multiple of the standard deviation and that multiple
  ;; must remain below half our unit scale.
  ;;
  ;; So, if unity is represented as 2^NUnit, then we need:
  ;;
  ;;  Log2(NSigma) + 1/2*Log2(NRows) + (N+Log2(6)) - 1/2*Log2(12) < NUnit-1
  ;;
  ;; Solving for N:
  ;;
  ;;  N < NUnit-1 - Log2(NSigma) + 1/2*Log2(12) - 1/2*Log2(NRows) - Log2(6)
  ;;
  ;; Plugging in NUnit = (320-256) = 64, NRows = 320, NSigma = 6, we get:
  ;;
  ;;    N < 55.46 bits, so use 55
  ;;
  ;; This needs to be greater than 1/2*log2(NRows) = 19.22, for security.
  ;;
  (- nbits-for-unit 1
     (- (log nsigma 2)
        (/ (log 12 2) 2))
     (/ (log nrows 2) 2)
     (log 6 2)))

(defun fgen-sys (&key (nbits   *flat-nbits*)
                      (ncode   *flat-ncode*)
                      (nrows   *flat-nrows*)
                      (ncols   *flat-ncols*)
                      (nnoise  *flat-nnoise*)
                      (modulus *flat-modulus*))
  (let ((mat-a  (make-array nrows)))
    (loop for ix from 0 below nrows do
            (let ((v (make-array ncols)))
              (loop for jx from 0 below ncols do
                      (let ((x (prng:ctr-drbg-int nbits)))
                        (setf (aref v jx) (mod x modulus))
                        ))
              (setf (aref mat-a ix) v)))
    (let* (;; (nsigma  6)
           ;; (noise-bits (floor (noise-nbits (- nbits ncode) nrows nsigma)))
           (sys (list :nbits   nbits
                      :ncode   ncode
                      :nrows   nrows
                      :ncols   ncols
                      :modulus modulus
                      :nnoise  nnoise
                      :mat-a   mat-a)))
      (fcheck-system sys)
      (format t "~%Densities: ~S" (density sys)) 
      sys)))

;; ---------------------------------------------

(defun fgen-skey (sys)
  (let* ((nbits   (getf sys :nbits))
         (ncols   (getf sys :ncols))
         (modulus (getf sys :modulus))
         (ans     (make-array ncols)))
    (loop for ix from 0 below ncols do
            (setf (aref ans ix)
                  (mod (prng:ctr-drbg-int nbits)
                       modulus)))
    ans))

(defun flat-gen-deterministic-skey (sys &rest seeds)
  ;; SKey is a NCols vector of NBits values
  (fcheck-system sys)
  (let* ((ncols           (lat2-ncols sys))
         (modulus         (getf sys :modulus))
         (nbits-per-word  (integer-length modulus))
         (nbits-total     (* nbits-per-word ncols))
         (bytes-per-word  (ceiling nbits-per-word 8))
         (hstretch        nil))
    (dotimes (ix 1000)
      (setf hstretch (apply #'hash/256 hstretch ix :deterministic-skey seeds)))
    (let* ((h    (vec (apply #'get-hash-nbits nbits-total hstretch :deterministic-skey seeds)))
           (ans  (make-array ncols)))
      (loop for ixcol from 0 below ncols
            for pos from 0 by nbits-per-word
            do
              (let ((byte-pos (floor pos 8)))
                (setf (aref ans ixcol)
                      (mod
                       (ldb (byte nbits-per-word 0)
                            (int (subseq h byte-pos (+ byte-pos bytes-per-word))))
                       modulus))
                ))
      ans)
    ))

(defun gen-noise (sys)
  (let* ((nsmall  (getf sys :nnoise))
         (small/2 (ash 1 (1- nsmall))))
    (- (prng:ctr-drbg-int nsmall) small/2)))

(defun fgen-pkey (skey sys)
  ;; PKey is a NRows vector of NBits values with added NNoise-bits of
  ;; uniformly distributed zero-mean noise.
  (let* ((nrows   (getf sys :nrows))
         (mat-a   (getf sys :mat-a))
         (modulus (getf sys :modulus))
         (nsmall  (getf sys :nnoise))
         (small   (ash 1 nsmall))
         (chk     (assert (> small (sqrt nrows)))))
    (declare (ignore chk))
    (map 'vector (lambda (arow)
                   (mod (+ (fvdot arow skey) (gen-noise sys)) modulus))
         mat-a)))

;; ------------------------------------------------------------------

(defun flat-encode1 (x pkey sys)
  (let* ((nrows   (getf sys :nrows))
         (ncols   (getf sys :ncols))
         (mat     (getf sys :mat-a))
         (ncode   (getf sys :ncode))
         (modulus (getf sys :modulus))
         (sf      (floor modulus (ash 1 ncode)))
         (sel     (gen-random-sel nrows)) ;; selection bits
         (sgn     (gen-random-sel nrows)) ;; sign bits
         (scl     (gen-random-sel nrows)) ;; scale bits
         (dbl     (gen-random-sel nrows)) ;; double scale bits
         (bsum    0)
         (vsum    (make-array ncols
                              :initial-element 0)))
    (loop for vrow across mat
          for b across pkey
          for ix from 0
          do
            ;; Subset-Sum weight is in (-3, -2, -1, 0, +1, +2, +3).
            ;; Random sel, sgn, scl, dbl.
            ;;
            ;; This has the effect of causing complexity to become
            ;; O(7^NRows) = O(2^(2.81*NRows))
            (when (logbitp ix sel)
              (cond ((logbitp ix sgn)
                     (decf bsum b)
                     (map-into vsum #'- vsum vrow)
                     (when (logbitp ix scl)
                       (decf bsum b)
                       (map-into vsum #'- vsum vrow)
                       (when (logbitp ix dbl)
                         (decf bsum b)
                         (map-into vsum #'- vsum vrow))))
                    (t
                     (incf bsum b)
                     (map-into vsum #'+ vsum vrow)
                     (when (logbitp ix scl)
                       (incf bsum b)
                       (map-into vsum #'+ vsum vrow)
                       (when (logbitp ix dbl)
                         (incf bsum b)
                         (map-into vsum #'+ vsum vrow))))
                    )))
    (vector (mod (+ bsum
                    (* sf x))
                 modulus)
            (map-into vsum (um:rcurry #'mod modulus) vsum))
    ))

(defun flat-encode (pkey v &optional (sys (get-lattice-system)))
  ;; Encrypt an octet vector
  (let* ((v     (ub8v v))
         (nb    (length v))
         (ncode (getf sys :ncode)))
    (declare (fixnum nb ncode))
    (cond ((= ncode 8)
           (let ((ans (make-array nb)))
             (loop for ix fixnum from 0 below nb
                   do
                     (setf (aref ans ix)
                           (flat-encode1 (aref v ix) pkey sys)))
             ans))
          ((zerop (logand ncode 7)) ;; multiples of 8
           (let* ((ngrp (ash ncode -3))
                  (nel  (ceiling nb ngrp))
                  (ans  (make-array nel)))
             (loop for ix fixnum from 0 below nel
                   for pos from 0 by ngrp
                   do
                     (setf (aref ans ix)
                           (flat-encode1 (int (subseq v pos (min nb (+ pos ngrp)))) pkey sys)))
             ans))
          (t
           (error "non-NByte data not yet supported"))
          )))

(defun flat-enc (pkey &rest objs)
  (flat-encode pkey (loenc:encode (loenc:unshared-list objs
                                                       :max-portability t))))

;; ------------------------------------------------------------------

(defun flat-decode1 (v skey sys)
  (let* ((bsum    (aref v 0))
         (vsum    (aref v 1))
         (ncode   (getf sys :ncode))
         (modulus (getf sys :modulus))
         (one     (floor modulus (ash 1 ncode)))
         (half    (ceiling modulus (ash 1 (1+ ncode)))))
    (floor 
     (mod (+ (- bsum
                (fvdot skey vsum))
             half)
          modulus)
     one)
    ))

(defun flat-decode (skey cs &optional (sys (get-lattice-system)))
  ;; decode a list of cyphertext vectors into an octet vector
  #F
  (let* ((nel   (length cs))
         (ncode (getf sys :ncode))
         (bv    (make-array nel
                            :element-type `(unsigned-byte ,ncode))))
    (loop for ix fixnum from 0 below nel
          do
            (setf (aref bv ix) (flat-decode1 (aref cs ix) skey sys)))
    (cond ((eql ncode 8)
           bv)
          (t
           (let* ((nbytes-per-word (ash ncode -3))
                  (tlen (* nbytes-per-word nel))
                  (ans  (make-array tlen
                                    :element-type '(unsigned-byte 8))))
             (loop for ix from 0 below nel
                   for pos from 0 by nbytes-per-word
                   do
                   (let ((v (vec (aref bv ix))))
                     (replace ans v :start1 pos)))
             ans))
          )))

(defun flat-dec (skey cs)
  (values-list (loenc:decode (flat-decode skey cs))))

;; -----------------------------------------------------------------

#|
(defun sqr (x)
  (* x x))

(let* ((coll (vm:unoise 10000 2))
       (sd   (vm:stdev coll)))
  (list :mn (vm:mean coll)
        :sd sd
        :var (sqr sd)))

(let* ((nel  160)
       (coll (loop repeat 10000 collect
                     (/ (reduce #'+
                                (map 'vector #'round
                                     (vm:unoise nel
                                                (1- (ash 1 13)))))
                        nel))))
  (plt:histogram 'histo coll
                 :clear t
                 :norm nil)
  (list :mn (float (vm:mean coll))
        :sd (float (vm:stdev coll))))

;; !!Don't execute this on large code-spaces!!
(let* ((pkey (fgen-pkey *tst-skey* *flat-sys*)))
  (loop for ix from 0 below (ash 1 (getf *flat-sys* :ncode)) do
          (let* ((v (flat-encode1 ix pkey *flat-sys*))
                 (dec (flat-decode1 v *tst-skey* *flat-sys*)))
            (assert (eql dec ix)))))

(let* ((nbits (getf *flat-sys* :nbits))
       (ncode (getf *flat-sys* :ncode))
       (pos   (- nbits ncode))
       (coll  (loop repeat 10000 collect
                      (let ((v (flat-encode1 0 *tst-pkey* *flat-sys*)))
                        (flat-decode1 v *tst-skey* *flat-sys*)))))
       (plt:histogram 'histo coll
                      :clear t
                      :norm  nil
                      ;; :yrange '(0 100)
                      ))

;; ----------------------------------------------------
;; Histogram of Encryptionxs Noise
;; Should look like a Gaussian distribution above the value of the x data value

(defparameter *flat-sys* (fgen-sys))
;; -------------------------
(ac:send kvdb:kvdb nil :add :flat-system *flat-sys*)
;; --------------------------

(defparameter *tst-skey* (fgen-skey *flat-sys*))
(defparameter *tst-pkey* (fgen-pkey *tst-skey* *flat-sys*))

(let* ((x        0)
       (ncoll    64000)
       (ncode    (getf *flat-sys* :ncode))
       (modulus  (getf *flat-sys* :modulus))
       (one      (floor modulus (ash 1 ncode)))
       (half     (ceiling modulus (ash 1 (1+ ncode))))
       (coll     (loop repeat ncoll collect
                         (let ((v (flat-encode1 x *tst-pkey* *flat-sys*)))
                           (-
                            (float
                             (- (/ (mod (+ (- (aref v 0)
                                              (fvdot *tst-skey* (aref v 1)))
                                           half)
                                        modulus)
                                   one)
                                x))
                            0.5))
                       )))
  ;; (inspect coll)
  (plt:histogram 'histo coll
                 :clear t
                 ;; :cum   t
                 ;; :norm  nil
                 ;; :yrange '(0 100)
                 :title  "Recovered Encryption Noise"
                 :xtitle (format nil "x - ~D" x)
                 :ytitle "Density"
                 )
  (list :mn (vm:mean coll)
        :sd (vm:stdev coll)))

;; -------------------------------------------
;; Histogram of Scalar Encryption Component
;; Should look like a uniform distribution

(let* ((x       0)
       (ncoll   4000)
       (modulus (getf *flat-sys* :modulus))
       (coll    (loop repeat ncoll collect
                        (let ((v (flat-encode1 x *tst-pkey* *flat-sys*)))
                          (float (/ (aref v 0) modulus))
                          ))))
  (plt:histogram 'histo coll
                 :clear t
                 ;; :cum   t
                 ;; :norm  nil
                 ;; :yrange '(0 600)
                 :title  "Raw Encryption Scalar"
                 :xtitle "Fractional Modular Value"
                 :ytitle "Density"
                 )
  (list :mn (vm:mean coll)    ;; should ≈ 0.5
        :sd (vm:stdev coll))) ;; should ≈ 1/Sqrt(12) = 0.289

;; -----------------------------------------------------------
;;

(let* ((nbits 1024)
       (ntrials 1000)
       (coll (loop repeat ntrials collect
                   (logcount (prng:ctr-drbg-int nbits)))))
  (plt:histogram 'histo coll
                 :clear t)
  (list :mn (float (vm:mean coll))
        :sd (vm:stdev coll)))

(let* ((nbits 761)
       (nrows 1024)
       (ntrials 1000)
       (zero   (ash 1 (1- nbits)))
       ;; (zero  0)
       (sums  (loop repeat ntrials collect
                      (log (abs (loop repeat nrows sum
                                        (- (prng:ctr-drbg-int nbits) zero)))
                                2))))
  (plt:histogram 'histo sums
                 :clear t))
       

b = A^x + ψ
B = Sum(b,s) 
c = (B + m, A
|#
    
