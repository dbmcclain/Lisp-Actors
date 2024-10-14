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
;; -----------------------------------------------------------
;;
;; Using a bigger system with LWE Lattice crypto allows us to connect
;; across the network, sending a 256-bit key using only 3990 bytes =
;; 70*456 bits.
;;
;; Compare with 366kB for 1-bit transfers, and 20kB for 8-bit
;; transfers.
;;
;; The cost is BIGNUM arithmetic instead of FIXNUM. But it was
;; worthwhile.
;;
;; But now, consider a case where NCols > NRows. It becomes impossible
;; to form trial Secret Key elements via matrix inversion - even when
;; you don't have any additive noise. We no longer need noise to help
;; protect the Secret Key.
;;
;; The resulting NBits size only needs to be large enough to
;; accommodate the message size, NCode. We still need protection
;; against brute force search for Secret Key elements, but now the
;; search space is NBits. And since NRows = NBits and NCols > NRows,
;; we get an effective protection security of O(2^NCols). Message size
;; is 256 bits, so NRows = 256, and NCols could be 257, for security
;; O(2^257).
;;
;; Furthermore, we gain additional benefits of being able to scale
;; Subset-Sums by arbitrary random weights, not from just the
;; restricted set {-3,-2,-1,0,1,2,3}. The Scaled Subset-Sum problem
;; becomes much more difficult, for protection of encrypted messages.
;;
;; And we get a Homomorphic encryption where adding two encryptions
;; produces the encryption for the sum of two messages. Scaling an
;; encryption forms the encryption of a scaled message. No longer any
;; worry about additive noise overflowing into the message bits,
;; needing NGuard bits dependent on the synthesized range of noise
;; values.
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
;; -- For Encryption Security --
;;
;; We rely on the Scaled Subset-Sum problem. Elements of the Public
;; Key are given arbitrary weights, modulo prime p, and rowsums
;; receive those same random weights.
;;
;; I have no idea how difficult it is to unravel arbitrarily scaled
;; subset sums. Using the estimates from best algorithms in simple
;; Subset-Sum attacks, they give a security level of
;; O(2^(0.22*NRows^2)).
;;
;; A brute force attack would have to consider every element of the
;; subset sum having a fan-out of 2^NBits, making the overt security
;; on the order of O(2^(NRows^2)). This is impossibly strong and
;; probably not worth considering any more. Encryption is locked up
;; Drum-Tight.
;;
;; The Subset-Sum problem is at its most difficult when the random
;; selection used for encryption contains exactly NRows/2 elements.
;;
;; But the statistics of a Binomial Distribution shows that NRows/2 is
;; the peak of the selection bits histogram, with a stdev around
;; (1/2)*Sqrt(NRows). The 1-sigma width is only around ±5.6% of peak
;; value (NRows/2) for NRows = 320.  And at ±3σ, a call to generate
;; 320 random bits should produce 160±27 non-zero bits.
;;
;; Surely, a deviation of only a few percent from exactly NRows/2
;; would not be enough to significantly ease the problem.(??)
;;
;; -------------------------------------------------------------------
;;
;; --- Sizing Things Up ---
;;
;; Our goal is to minimize the size of the KEM transfer, which has
;; size NBits*(1 + NCols).  Hence we make, NCols > NRows, but not by
;; too much. NRows is decided by NBits, which must be large enough to
;; accommodate the messages in the modular field, prime p. So NBits =
;; NCode+1. But we increase it enough to make each element occupy
;; whole bytes.
;;
;; Attacking the Secret Key from knowledge of the Public Key and
;; System Matrix is algebraically impossible, give the projective
;; nature of the System Matrix. A brute-force search now has to survey
;; the NCols*NBits Secret Key size.
;;
;; For NCode = 256, the next larger whole-byte size is 264 for NBits
;; and NRows. We could make NCols = 265. Hard category is ≥128 bits of
;; search. That category is now slightly greater than 50% of search
;; values.
;;
;;  log2(eps) = NCols*log2(64/129) ≈ -1.011*NCols
;;
;; With NCols = 265 we get eps < 2^(-267). So it looks like the Secret
;; Key will be safe.
;;
;; Encryptions require 266*264 bits = 8,778 Bytes to transmit a
;; 256-bit (32-Byte) key. Compare with our best LWE Lattice Crypto
;; using 3,990 bytes. So we are not as space efficient as LWE Lattice,
;; but still acceptable. Our element sizes are smaller but we ship
;; more of them.
;;
;; In effect, the Public Key Scaled Subset-Sum becomes a random 1-time
;; pad added to each message. How good is that randomness? Probably
;; depends entirely on the quality of the PRNG, which is Fortuna.
;; Repeated encryptions of the same message will produce different
;; random cryptotexts with the NBit random scalings applied every time.
;;
;; ---------------------------------------------------------------
;;
;; --- Many Apparent Solutions for Secret Key ---
;;
;; ... But only one is the true solution. Let's take a simple example
;; of a 3-D to 2-D projection:
;;
;;  [b1]   [2 3 5] [x1]
;;  [b2] = [1 4 7] [x2]
;;                 [x3]
;;
;; And consider "THE" Secret Key to be x = [1 2 3]. Then b = [23 30].
;;
;; By converting to canonical form with an Idenity matrix on the left,
;; and a residual matrix on the right.
;;
;;   23 = 2 x1 + 3 x2 + 5 x3
;;   30 =   x1 + 4 x2 + 7 x3
;;
;;   23/2 = x1 + 3/2 x2 + 5/2 x3
;;   37/2 =      5/2 x2 + 9/2 x3
;;
;;   23/2 = x1 + 3/2 x2 + 5/2 x3
;;   37/5 =          x2 + 9/5 x3
;;
;;    2/5 = x1          - 1/5 x3
;;   37/5 =          x2 + 9/5 x3
;;
;; All solutions lie along the 3-D line, parameterized by x3:
;;
;;    x1 =  2/5 + 1/5 x3
;;    x2 = 37/5 - 9/5 x3
;;
;; Let's re-parameterize by parameter w, such that w=0 corresponds to
;; "THE" solution for the secret key:
;;
;;    x1 =  2/5 + 1/5 (3 + w) = 1 + w
;;    x2 = 37/5 - 9/5 (3 + w) = 2 - w
;;    x3 = 3 + w
;;
;; There are p possible solutions, p possible values for w, in the
;; modular field. There is only one correct solution, and the attacker
;; won't know it. He'll pick one of the lattice locations
;; corresponding to some value of w that he won't know.
;;
;; Any of these solutions will match the initial condition shown by
;; the public key, correspondong to when x1 = 1, x2 = 2, and x3 = 3.
;; He'll have no way of knowing if he got the right secret key values.
;;
;; When encrypting a message, we form scaled row-sums:
;;
;;    b = msg + 23 r1 + 30 r2, for random r1, r2
;;   v1 = 2 r1 + r2
;;   v2 = 3 r1 + 4 r2
;;   v3 = 5 r1 + 7 r2
;;
;; Now on decryption, using any but "THE" secret key we perform:
;;
;;    msg =? b - (v1 x1 + v2 x2 + v3 x3)
;;        =? b - (v1 (1+w) + v2 (2-w) + v3 (3+w)
;;        =? b - (v1 1 + v2 2 + v3 3) + w (v1 - v2 + v3)
;;        =? b - (v . x) + w (4 r1 + 4 r2)
;;        =? msg + 4 w (r1 + r2)
;;
;; While this works correctly when w=0, it deviates badly for any
;; other pseudo-solution, by the w term. The pseudo-solutions will
;; fail to decrypt the message.
;;
;; Anyone can study the System Matrix and find their own canonical
;; form, revealing the hyper-region containing all possible solutions
;; for the Secret Key. But only one of those solutions will be the
;; correct one. They have a (1/p)^(NCols-NRows) chance of finding it.
;;
;; ----------------------------------------------------------------------

(defparameter *flat-nbits*    264) ;; = 8*(1 + Floor(NCode,8))
(defparameter *flat-ncode*    256)
(defparameter *flat-nrows*    264) ;; NRows = NBits for density ≈ 1
(defparameter *flat-ncols*    265) ;; = NRows + 1

(defparameter *flat-modulus*  (- (ash 1 264) 275))

;; ---------------------------------------------

(defun fvdot (v1 v2)
  (reduce #'+ (map 'vector #'* v1 v2)))

;; ---------------------------------------------

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

(defun fcheck-system (sys)
  (let ((ncols   (lat2-ncols sys))
        (nrows   (lat2-nrows sys))
        (nbits   (getf sys :nbits))
        (ncode   (getf sys :ncode)))
    
    (unless (zerop (logand nbits 7))
      (error "NBits should specify whole bytes: ~A" nbits))
    (unless (zerop (logand ncode 7))
      (error "NCode should specify whole bytes: ~A" ncode))

    (unless (> nbits ncode)
      (error "NBits must be > NCode: (~A nbits, ~A ncode)" nbits ncode))
    
    (when (<= ncols nrows)
      (error "Drumtight needs NCols > NRows: (~A rows, ~A cols)" nrows ncols))

    #|
    (let ((density (density sys)))
      (when (some (um:rcurry #'< 0.999) density)
        (error "Density too low in at least one column: ~A" density))
      (when (some (um:rcurry #'> 1.001) density)
        (error "Density too high in at least one column: ~A" density)))
    |#
    ))

(defun fgen-sys (&key (ncode   *flat-ncode*)
                      (nrows   *flat-nrows*)
                      (ncols   *flat-ncols*)
                      (modulus *flat-modulus*)
                      (nbits   *flat-nbits*))
  ;; (assert (zerop (logand ncode 7)))
  (let* ((nbits (or nbits (* 8 (1+ (floor ncode 8)))))
         (nrows (or nrows nbits)))
    (assert (= nbits (integer-length modulus)))
    (assert (> ncols nrows))
    (let ((mat-a  (make-array nrows)))
      (loop for ix from 0 below nrows do
              (let ((rowv (make-array ncols)))
                (loop for jx from 0 below ncols do
                        (let ((x (prng:ctr-drbg-int nbits)))
                          (setf (aref rowv jx) (mod x modulus))
                          ))
                (setf (aref mat-a ix) rowv)))
      (let ((sys (list :nbits nbits
                       :ncode ncode
                       :nrows nrows
                       :ncols ncols
                       :modulus modulus
                       :mat-a mat-a)))
        ;; (fcheck-system sys)
        (format t "~%Densities: ~S" (density sys)) 
        sys))))

;; ---------------------------------------------

(defun fgen-skey (sys)
  ;; (fcheck-system sys)
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
  ;; skey is a NCols vector of NBits-bit values
  (fcheck-system sys)
  (let* ((ncols           (lat2-ncols sys))
         (modulus         (getf sys :modulus))
         (nbits-per-word  (getf sys :nbits))
         (nbytes-per-word (ash nbits-per-word -3))
         (nbits-total     (* nbits-per-word ncols))
         (hstretch        nil))
    (dotimes (ix 1000)
      (setf hstretch (apply #'hash/256 hstretch ix :deterministic-skey seeds)))
    (let* ((h    (vec (apply #'get-hash-nbits nbits-total hstretch :deterministic-skey seeds)))
           (hlen (length h))
           (ans  (make-array ncols)))
      (loop for ix from 0 below ncols
            for pos from 0 by nbytes-per-word
            do
              (let ((x (int (subseq h pos (min hlen (+ pos nbytes-per-word))))))
                (setf (aref ans ix) (mod x modulus))
                ))
      ans)
    ))

(defun fgen-pkey (skey sys)
  ;; (fcheck-system sys)
  (let* ((mat-a   (getf sys :mat-a))
         (modulus (getf sys :modulus)))
    (declare (ignore chk))
    (map 'vector (lambda (arow)
                   (mod (fvdot arow skey) modulus))
         mat-a)))

;; ------------------------------------------------------------------
;; c is Homomorphic.
;;  c1 + c2 = E(m1+m2)
;;      γ*c = E(γ*m)

(defun flat-encode1 (x pkey sys)
  ;; c = (b, v)
  ;;   b = x + Sum(r_i*b_i), r_i random ;; scalar sum
  ;;   v_j = Sum(A_ij*r_i)              ;; NCols rowsum
  (let* ((ncols   (getf sys :ncols))
         (mat     (getf sys :mat-a))
         (modulus (getf sys :modulus))
         (nbits   (integer-length modulus))
         (bsum    x)
         (vsum    (make-array ncols
                              :initial-element 0)))
    (loop for vrow across mat
          for b across pkey
          do
            (let ((r (prng:ctr-drbg-int nbits)))
              (incf bsum (* b r))
              (map-into vsum (lambda (v1 v2)
                               (+ v1 (* r v2)))
                        vsum vrow)
              ))
    (vector (mod bsum modulus)
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
         (modulus (getf sys :modulus)))
    (mod (- bsum
            (fvdot skey vsum))
         modulus)
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
;; ----------------------------------------------------

(defparameter *flat-sys* (fgen-sys :nrows 40 :ncols 41))

;; -------------------------
(ac:send kvdb:kvdb nil :add :flat-system *flat-sys*)
;; --------------------------

(defparameter *tst-skey* (fgen-skey *flat-sys*))
(defparameter *tst-pkey* (fgen-pkey *tst-skey* *flat-sys*))

;; Check randomness of the OTP
(let* ((ntrials 16000)
       (x       0)
       (sys     (ask kvdb:kvdb :find :flat-system))
       (sys     (fgen-sys :nrows 40 :ncols 41))
       (skey    (fgen-skey sys))
       (pkey    (fgen-pkey skey sys))
       (one     (getf sys :modulus))
       (coll    (loop repeat ntrials collect
                      (let ((v (flat-encode1 x pkey sys)))
                        (float (/ (aref v 0) one))
                        ))))
  (plt:histogram 'histo coll
                 :clear t
                 :yrange '(0 2))
  (list :mn    (float (vm:mean coll))
        :stdev (float (vm:stdev coll))))

(defparameter *flat-sys* (fgen-sys :nrows 2 :ncols 3 :nbits 10 :modulus (- (ash 1 10) 3)))

;; -----------------------------------------------------
(defun idn (nrows)
  (let ((ans (make-array nrows)))
    (loop for ix from 0 below nrows do
          (let ((row (make-array nrows
                                 :initial-element 0)))
            (setf (aref row ix) 1
                  (aref ans ix) row)))
    ans))

(defun noise-mat (nrows ncols modulus)
  (let ((ans   (make-array nrows))
        (nbits (integer-length modulus)))
    (loop for ix from 0 below nrows do
            (let ((row (make-array ncols)))
              (loop for cix from 0 below ncols do
                      (setf (aref row cix)
                            (mod (prng:ctr-drbg-int nbits) modulus)))
              (setf (aref ans ix) row)))
    ans))

(defun copy-matrix (mat)
  (let ((ans (copy-seq mat)))
    (loop for v across mat
          for ix from 0
          do
          (setf (aref ans ix) (copy-seq v)))
    ans))

(defun mat-inv (mat v modulus)
  (with-mod modulus
    (let* ((nrows (length mat))
           (ncols (length (aref mat 0)))
           (idn   (idn nrows))
           (mat   (copy-matrix mat))
           (v     (copy-seq v)))
      (loop for rix from 0 below (min nrows ncols) do
              (let* ((row  (aref mat rix))
                     (irow (aref idn rix))
                     (rinv (m/ (aref row rix)))
                     (x    (aref v rix)))
                (map-into row (um:rcurry #'m* rinv) row)
                (map-into irow (um:rcurry #'m* rinv) irow)
                (setf x (m* rinv x)
                      (aref v rix) x
                      (aref mat rix) row
                      (aref idn rix) irow)
                (loop for rrix from 0 below nrows do
                        (unless (eql rrix rix)
                          (let* ((rrow (aref mat rrix))
                                 (irrow (aref idn rrix))
                                 (rr   (aref rrow rix))
                                 (xx   (aref v rrix)))
                            (setf (aref v rrix) (m- xx (m* rr x)))
                            (map-into (aref mat rrix) (lambda (rrx rx)
                                                        (m- rrx (m* rr rx)))
                                      rrow row)
                            (map-into (aref idn rrix) (lambda (rrx rx)
                                                        (m- rrx (m* rr rx)))
                                      irrow irow)
                            )))))
      (values mat v idn)
      )))

(defun trn (a)
  (let* ((nrows (length a))
         (ncols (length (aref a 0)))
         (ans   (make-array ncols)))
    (loop for ix from 0 below ncols do
          (let ((v (make-array nrows)))
            (loop for jx from 0 below nrows do
                  (setf (aref v jx) (aref (aref a jx) ix)))
            (setf (aref ans ix) v)))
    ans))

(defun mat-mul (a b modulus)
  (let* ((nrows  (length a))
         (ncols  (length b))
         (bt     (trn b))
         (ans    (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let ((v (make-array ncols))
                  (row (aref a rix)))
              (loop for cix from 0 below ncols do
                      (setf (aref v cix)
                            (mod
                             (fvdot row (aref bt cix))
                             modulus)))
              (setf (aref ans rix) v)))
    ans))
              

(defun mat-left (m ncols)
  (let* ((nrows (length m))
         (ans   (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let* ((v     (aref m rix))
                   (row   (make-array ncols
                                      :initial-element 0)))
              (replace row v)
              (setf (aref ans rix) row)))
    ans))

(defun mat-right (m ncols)
  (let* ((nrows (length m))
         (ans   (make-array nrows)))
    (loop for rix from 0 below nrows do
            (let* ((v      (aref m rix))
                   (vcols  (length v))
                   (start2 (max 0 (- vcols ncols)))
                   (row    (make-array ncols
                                       :initial-element 0)))
              (replace row v :start2 start2)
              (setf (aref ans rix) row)))
    ans))

(defun mat-top (m nrows)
  (let* ((ans   (make-array nrows))
         (mrows (length m))
         (ncols (length (aref m 0))))
    (loop for rix from 0 below (min nrows mrows) do
            (setf (aref ans rix) (copy-seq (aref m rix))))
    (loop for rix from (min nrows mrows) below nrows do
            (setf (aref ans rix) (make-array ncols
                                             :initial-element 0)))
    ans))

(defun mat-bottom (m nrows)
  (let* ((ans   (make-array nrows))
         (mrows (length m))
         (ncols (length (aref m 0)))
         (start (max 0 (- mrows nrows))))
    (loop for rix from 0 below (min nrows mrows) do
            (setf (aref ans rix) (copy-seq (aref m (+ start rix)))))
    (loop for rix from (min nrows mrows) below nrows do
            (setf (aref ans rix) (make-array ncols
                                             :initial-element 0)))
    ans))

(let* ((base  (- (ash 1 10) 3))
       (m     (noise-mat 3 3 base)))
  (list m (mat-left (mat-top m 2) 2)))

(let* ((base  (- (ash 1 10) 3))
       (mat   (noise-mat 4 3 base))
       (v     (vector 1 2 3)))
  (multiple-value-bind (_ vinv inv)
      (mat-inv mat v base)
    (list
     :mat mat
     :v   v
     :inv inv
     :vinv vinv
     :mod base
     :prod (mat-mul mat inv base))))

;; -----------------------------------------------------

(defparameter *tst-skey* nil)
(defparameter *tst-pkey* nil)

(let* ((mat-a   (getf *flat-sys* :mat-a))
       (modulus (getf *flat-sys* :modulus))
       (skey    (setf *tst-skey* (fgen-skey *flat-sys*)))
       (pkey    (setf *tst-pkey* (fgen-pkey skey *flat-sys*))))
  (to-canon mat-a pkey modulus)
  (inspect (list mat-a skey pkey)))

(let* ((modulus (getf *flat-sys* :modulus))
       (mat-a   (getf *flat-sys* :mat-a))
       (skey    *tst-skey*)
       (pkey    *tst-pkey*))
  (inspect (list mat-a pkey))
  (labels ((x1 (x3)
             (with-mod modulus
               (m- (aref pkey 0) (m* x3 (aref (aref mat-a 0) 2)))))
           (x2 (x3)
             (with-mod modulus
               (m- (aref pkey 1) (m* x3 (aref (aref mat-a 1) 2))))))
    (let* ((x3 100)
           (sk (vector (x1 x3)
                       (x2 x3)
                       x3)))
      (inspect (list mat-a pkey
                     (vector (mod (fvdot skey (aref mat-a 0)) modulus)
                             (mod (fvdot skey (aref mat-a 1)) modulus))))
      )))
  
|#
    
