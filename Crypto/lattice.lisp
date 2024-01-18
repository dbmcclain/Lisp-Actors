;; lattice.lisp - experiments in LWE Encryption
;;
;; DM/RAL  2023/03/20 06:17:16
;; ----------------------------------

(defpackage #:com.ral.crypto.lattice-crypto
  (:use #:common-lisp #:edec #:modmath #:vec-repr #:hash #:ac)
  (:export
   #:lattice-system
   #:lat2-encode
   #:lat2-enc
   #:lat2-decode
   #:lat2-dec
   #:lat2-gen-deterministic-skey
   #:lat2-gen-pkey

   #:fgen-pkey
   #:flat-encode
   #:flat-decode
   #:flat-gen-deterministic-skey
   ))

(in-package #:com.ral.crypto.lattice-crypto)

;; ----------------------------------
;; Modular arithmetic using values between (-m/2, m/2) instead of [0, m).

(defun lmod (a)
  #F
  (declare (fixnum a))
  (let* ((m   (mod-base))
         (aa  (mod a m))
         (m/2 (ash m -1)))
    (declare (fixnum aa m m/2))
    (if (> aa m/2)
        (- aa m)
      aa)))

(defun lmneg (x)
  #F
  (declare (fixnum x))
  (lmod (- x)))

(defun lm+ (a b)
  #F
  (declare (fixnum a b))
  (lmod (+ a b)))

(defun lm* (a b)
  #F
  (declare (fixnum a b))
  (lmod (* a b)))

(defun vec+ (v1 v2)
  #F
  (assert (eql (length v1) (length v2)))
  (map 'vector #'lm+ v1 v2))

(defun vdot (v1 v2)
  #F
  (assert (eql (length v1) (length v2)))
  (let* ((m     (mod-base))
         (nbits (integer-length m))
         (wrap  (- (ash 1 nbits) m))
         (nshft (- nbits))
         (bits  (byte nbits 0)))
    (declare (fixnum m nbits wrap nshft))
    (lmod (loop for x1 across v1
                for x2 across v2
                sum
                  (if (zerop x2)
                      0
                    (let ((prod (* x1 x2)))
                      (+ (ldb bits prod)
                         (* wrap (ash prod nshft)))
                      ))))
    ))

(defun mat*v (m v)
  #F
  (let ((ans (make-array (length m)
                         :element-type 'fixnum)))
    (loop for mv across m
          for ix fixnum from 0
          do
            (setf (aref ans ix) (vdot mv v)))
    ans))

(defun enc-mat*v (m sel)
  ;; m should be the system matrix, A, stored row-wise. Selector is a
  ;; random NRows-bit integer in the range [1,2^nrows). We simply add
  ;; the matrix rows corresponding to non-zero bits in the selector
  ;; integer.
  #F
  (let ((vans (make-array (length (aref m 0))
                          :element-type 'fixnum
                          :initial-element 0)))
    (loop for vrow across m
          for ix fixnum from 0
          do
            (when (logbitp ix sel)
              (map-into vans #'+ vans vrow)))
    (map-into vans #'lmod vans)))

;; ------------------------------------------------------

(defun to-bitvec (v)
  #F
  (declare ((simple-array (unsigned-byte 8) 1) v))
  (let ((bv (make-array (* 8 (length v))
                        :element-type 'bit)))
    (declare ((simple-array bit 1) bv))
    (loop for x fixnum across v
          for ix fixnum from 0 by 8
          do
          (loop for jx fixnum from ix
                for kx fixnum from 7 downto 0
                do
                (setf (sbit bv jx) (ldb (byte 1 kx) x))))
    bv))

(defun bitvec-to (bv pos nbits)
  #F
  (declare ((simple-array bit 1) bv)
           (fixnum pos nbits))
  (let* ((nb  (length bv))
         (end (min (+ pos nbits) nb))
         (val 0))
    (declare (fixnum nb end val))
    (loop for ix fixnum from pos below end do
            (setf val (+ val val (sbit bv ix))))
    val))
  
(defun bitvec-to-nibble (bv pos)
  (bitvec-to bv pos 4))

(defun bitvec-to-octet (bv pos)
  (bitvec-to bv pos 8))

(defun bitvec-to-octets (bv)
  #F
  (declare ((simple-array bit 1) bv))
  (let* ((nel  (ash (length bv) -3))
         (ans  (make-array nel
                           :element-type '(unsigned-byte 8))))
    (declare (fixnum nel)
             ((simple-array (unsigned-byte 8) 1) ans))
    (loop for ix fixnum from 0 below nel
          for bpos fixnum from 0 by 8
          do
          (setf (aref ans ix) (bitvec-to-octet bv bpos)))
    ans))

(defun bref (v ix)
  ;; access an octet vector at bit position ix
  ;; assumes big-endian encoding
  #F
  (declare ((simple-array (unsigned-byte 8) 1) v)
           (fixnum ix))
  (let ((x  (aref v (ash ix -3))))
    (declare (fixnum x))
    (ldb (byte 1 (- 7 (logand ix 7))) x)
    ))

(defun set-bref (v ix b)
  ;; set actet vector v at bit position ix with bit value b
  ;; assumes big-endian encoding
  #F
  (declare ((simple-array (unsigned-byte 8) 1) v)
           (fixnum ix)
           (bit b))
  (let* ((ixv  (ash ix -3))
         (x    (aref v ixv)))
    (declare (fixnum ixv x))
    (setf (aref v ixv)
          (dpb b (byte 1 (- 7 (logand ix 7))) x))
    ))

(defsetf bref set-bref)

;; ---------------------------------------------------
;; NRows sets the difficulty of the subset sum problem O(2^NRows).
;;
;; If you can solve the subset sum problem on each entry of the
;; cryptotext vector then, with knowledge of the public key you could
;; determine the bit value being encrypted.
;;
;; -----------------------------------------------------------
;;
;; For the problem of attacking the encryption, when given the public
;; key, the system random A matrix, and a cryptotext vector:
;;
;; The public key is a NRows vector, and the system matrix contains
;; NRows by NCols of random values.  The first element of the
;; cryptotext vector is a subset sum of public key vector elements
;; plus the bit being encoded. The second element of the cryptotext
;; vector is the subset sum vector of the same corresponding
;; NCols-element row vectors of the system matrix.
;;
;; For encryption, each bit of the message uses a different randomly
;; generated binary selection vector. We guarantee that no fewer than
;; NRows/4 elements of the selection vector will contain a 1.
;;
;; But there are NRows selection weights to solve for, given only
;; NCols representative sums from the system matrix rows. So if NCols
;; < NRows, then the system is under-determined, and can't be solved
;; directly. The only way to solve is by brute force search over a
;; problem of O(2^NRows).
;;
;; And because this selected set is different for each encrypted bit
;; of the message, you have a large number of subset sum problems to
;; solve. We assume that generated random numbers are robust and
;; unpredictable.
;;
;; This is an NP-hard problem, growing exponentially difficult with
;; order O(2^NRows). For large enough NRows, a brute force search is
;; unfeasible.
;;
;; Every time you encrypt a 1-bit message, you get a different random
;; cyphertext vector. Different random subset selection keying is used
;; for each encryption. So a chosen plaintext attack is useless.
;;
;; ----------------------------------------------------------
;;
;; == Cracking the Encryption ==
;;
;; By just looking at the public key, b, we have:
;;
;;     b = A • x + ψ
;;
;; for system matrix, A, secret key, x, and noise vector, ψ.
;;
;; Cracking the code from this angle means recovering the NCols 30-bit
;; numbers in secret key, x, plus NRows of 30-bit numbers in ψ. Total
;; effective key length is (NRows + NCols)*30. This needs to be large
;; enough to thwart a brute-force search, including the "Birthday"
;; effect. For NRows = 160 and NCols = 128, we have an effective key
;; length of 8,640 bits.
;;
;; A cryptotext vector is:
;;
;;     (u = bsum + bit*(m-1)/2, v = rowsum) mod m
;;
;; where bsum is a random subset sum of the public key, b, elements,
;; and rowsum is the same subset-sum of rows of the system matrix, A.
;;
;; Decryption, to reveal bit, follows by perfoming:
;;
;;    u - v • x
;;
;; for secret key, x.
;;
;; So, cracking the encryption could mean recovering the secret key,
;; x, and the message bit.  Secret key, x, is a vector of NCols x
;; 30-bit numbers, for a total key length of 30*NCols bits. Clearly,
;; we need NCols sufficiently large to thwart a brute-force search for
;; the secret key.
;;
;; But there is still the unknown message bit, and every trial crack
;; of the secret key would reveal either a 0 or 1 value for bit. This
;; is a 50/50 proposition. We really need more than the secret key, x.
;;
;; Examining only the subset-sums, looking for that random selection
;; used to form u and v in the encryption, means that if we could find
;; that random subset then we could recover the bit from u, all
;; without needing the secret key, x. The subset would be found from
;; examination of the system A matrix and the vector elements of v.
;;
;; Cracking a subset sum of a rowsum vector is not much harder than
;; cracking a subset sum for a single scalar. In general, since matrix
;; A contains random values, and sums are modulo m, it is possible
;; that there may be several subsets that produce the same sum, for
;; any one vector element in rowsum. But the other elements can serve
;; as a check on the subset, until all rowsum elements agree on the
;; subset.
;;
;; So we need the subset selection vector to be large enough to thwart
;; a brute-force search of the subset-sum solution. And that size is
;; NRows.
;;
;; Subset-Sum is NP-Hard, with a O(2^NRows) complexity. On my computer
;; I get a worst-case solution duration of:
;;
;;     Log2(Duration) ≈ NRows - 19.4
;;
;;   -- or --
;;
;;     NRows > Log2(Duration) + 19.4
;;
;; for Duration in seconds. I was running a full parallel solution
;; search over an effective collection of 4.8 cores.
;;
;; If we assume an attacker might be up to 1 billion times faster than
;; me, then we should increase safety by adding 30 extra rows. So,
;;
;;   NRows >= Log2(Duration) + 50
;;
;; For 30-year security, we only need NRows >= 80.
;;
;; At our initial sizing with NRows = 320, we should have security for
;; ≈ 10^81 years against the GHz attacker. A slight overkill?
;;
;; But then, we have to consider the "Birthday" effect, and square the
;; complexity of our systems. So, why not choose KCols = 128, and
;; NRows = 160? With 30-bit modulo arithmetic, this produces a key
;; strength of 8,640 bits against cracking the secret key. And it
;; gives us 30 year security against decryption attacks. This would
;; cut down on network traffic by 2 times. Instead of the initial
;; handshake being 366KB, we could get by with only 183 KB.
;;
;; So security requirements are:
;;
;;  1. (NRows + NCols)*30-bits > minimum key size in bits
;;  2. NRows > minimum subset-sum size
;;
;; We use 30-bit prime modulus to keep all numbers and intermediate
;; products to FIXNUM size.
;;
;; ----------------------------------------------------------
;;
;; For the problem of attacking the secret key, and obtaining the
;; weight vector and noise values: this has nothing to do with
;; cryptovectors.  It relies solely on cracking the public key with
;; the given system matrix.
;;
;; Each element of the pubkey vector represets a weighted sum of NCols
;; elements from the system matrix, plus random additive noise. There
;; are NCols weights, and NRows noise values. All of these are
;; unconstrained values, unlike the 1-bit selection weights from the
;; previous attack.
;;
;; You have NRows equations with (NRows + NCols) unknowns.
;; And since always, NRows < (NCols + NRows), the system is forever
;; under-determined - meaning, you can't solve for the weights and
;; noise, except by brute force.
;;
;; Here, the constraints are that these values are somewhere in the
;; range [0,m), or about 2^30 possible choices for each. This is
;; effectively unconstrained. You need to find just the right 30-bit
;; value, for (NRows + NCols) of them. At 320x256 this becomes a
;; search for a 17,280-bit key. Even a Birthday attack needs on
;; average 2^(8,640) ≈ 10^(2,592) trials.
;;
;; And, again, since all of these weights and noise are random values,
;; there are no underlying field periodicities to discover, rendering
;; no advantage to a quantum computer. The only weakness to this
;; system may be the underlying random noise generator.
;;
;; Interestingly, since additive noise is used in forming the public
;; key corresponding to a given secret key, there are any number of
;; different public keys for the same secret key. For a 320x256 system
;; matrix, and for 30-bit modular arithmetic over a prime field, the
;; secret key is a 7,680-bit key, and the public keys are 9,600-bit
;; keys.
;; ---------------------------------------------------------------
;;
;; Secret key SKey = |x> = #(x_1 x_2 ...  x_NCols), for
;; x_i random in [-Ceil(m/2),Floor(m/2)], prime modulus m.
;;
;; Using Bra-Ket notation, |x> is a column-vector, and <x| is a
;; row-vector.
;;
;; The A matrix is an Nrow x NCols random matrix, serving to expand
;; the dimensionality of the secret key.
;;        A_i,j in [-Ceil(m/2), Floor(m/2)],
;; using bipolar modular representation.
;;
;; We compute noisy expansion |b> = A|x> + |ψ>, for noise vector |ψ>.
;; Each element of ψ comes from a sampled Gaussian distribution with
;; mean 0 and sigma 1, scaled by (m/4)/gmax, where gmax is determined
;; as the max absolute sum of all positive samples vs all negative
;; samples, considering that the worst case random selection vector
;; will choose one of these pathalogical cases. This ensures that any
;; summed noise contribution will never be outside of the bounds
;; (-m/4, m/4).
;;
;; Public key is presented as PKey = |b>. Because |ψ> is random noise,
;; there are many possible PKey for each SKey.
;;
;; Encryption occurs one bit at a time, scaled by m/2. So bit value
;; with added noise will either be in the range (-m/4,m/4) for bit 0,
;; or (-3m/4,-m/4) or (m/4,3m/4) for bit 1. Rounding these by m/2
;; should return -1, 0, or +1. Take that modulo 2 to get back 0 or 1.
;;
;; For each bit, encryption is by way of choosing a random NRow
;; non-zero selection vector |r> = (r_1, r_2, ... r_Nrows), for r_i in
;; (0,1).  Then 2-element cryptotext vector for a single bit is:
;;
;;    c = #( <b|r> + m/2*bit, Trn(A)|r> ),
;;
;; where first element is a 30-bit scalar value, and second element,
;; Trn(A)|r>, is an NCol vector of 30-bit values.
;;
;; For |b> = A|x> + |ψ>, we have <b|r> = <x|Trn(A)|r> + <ψ|r>.
;;
;; With binary elements for selection vector |r>, then <b|r> is a
;; scalar subset sum of selected elements of public key, |b>, and
;; Trn(A)|r> is a vector subset sum of the NCol-element row vectors of
;; matrix A.  Computationally, we never need to form the actual
;; transpose of matrix A.
;;
;; Decryption is by way of taking the difference of the first c
;; element, and the dot product of second element with SKey, <x|:
;;
;;  c[0] - <x|c[1]
;;           = (<b|r> + m/2*bit) - <x|Trn(A)|r>
;;           = ((<x|Trn(A) + <ψ|)|r> + m/2*bit) - <x|Trn(A)|r>
;;           = <x|Trn(A)|r> + <ψ|r> + m/2*bit - <x|Trn(A)|r>
;;           = <ψ|r> + m/2*bit
;;
;; All arithmetic is modulo m.
;;
;; Quantity <ψ|r> is unknown, but it is guaranteed that
;;     Abs(<ψ|r>) < m/4,
;; for all possible selection vectors, |r>.
;;
;; Then,
;;     Round(<ψ|r> + (m/2)*bit, m/2) mod 2 => bit
;; where rounding occurs in the field of Integers.
;;
;; Here, prime modulus is chosen so that intermediate products remain
;; FIXNUM.

(defvar *lattice-m*      (- (ash 1 30) 35)) ;; prime modulus = 1 mod 4
(defvar *lattice-nrows*  160)  ;; public keys have this length
(defvar *lattice-ncols*  128)  ;; private key vector and cryptotext vectors have this length

;; ------------------------------------------------------

(defun gen-random-list (nel)
  #F
  (declare (fixnum nel))
  (let ((m  (mod-base)))
    (declare (fixnum m))
    (loop for ix fixnum from 1 to nel collect
          (lmod (prng:random-between 0 m)))
    ))

(defun gen-random-vec (nel)
  (coerce (gen-random-list nel) 'vector))

(defun gen-random-matrix (nrows ncols)
  ;; Matrix is a vector of row-vectors
  #F
  (declare (fixnum nrows ncols))
  (coerce
   (loop for ix fixnum from 1 to nrows collect
           (gen-random-vec ncols))
   'vector))

(defun gen-noise-vec (nel)
  ;; m happens to be 1 mod 4. But wouldn't matter otherwise.
  ;;
  ;; Generate a Gaussian random vector, worst-case sum bounded by
  ;;    [-(m >> 2)/4, (m >> 2)/4)
  (declare (fixnum nel))
  (let ((v  (vm:gnoise nel))
        (v+ 0.0f0)
        (v- 0.0f0))
    (declare (single-float v+ v-))
    (map 'nil (lambda (x)
                (declare (single-float x))
                (if (plusp x)
                    (incf v+ x)
                  (decf v- x)))
         v)
    (let* ((vmax (max v+ v-))
           (sf   (/ (ash (mod-base) -2) vmax)))
      (declare (single-float vmax sf))
      (format t "~%VMax = ~f" vmax)
      (map 'vector #'truncate ;; (abs (max sum)) < (m-1)/4
           (vops:vscale sf v)))
    ))

(defun gen-random-gaussian-matrix (nrows ncols)
  (let ((sf  (/ (ash (mod-base) -2) nrows ncols))
        (mat (make-array nrows)))
      (loop for rix from 0 below nrows do
            (setf (aref mat rix)
                  (map 'vector #'round
                       (vops:vscale sf (vm:gnoise ncols)))))
      mat))

(defun gen-random-sel (nbits)
  ;; Produce an nbits random value, with at lesat a quarter of them
  ;; nonzero.  At 320 nbits, the likelihood of fewer than 80 bits
  ;; being 1 is a 9-sigma event, about 5.1e-20. Ain't gonna happen...
  (um:nlet iter ()
    (let ((r  (prng:ctr-drbg-int nbits)))
      (if (< (logcount r) (/ nbits 4))
          (go-iter)
        r)
      )))

;; ----------------------------------------------------------------------

(defun stopwatch-beh (&optional (start 0))
  (alambda
   ((:start)
    (become (stopwatch-beh (get-universal-time))))
   ((:stop)
    (send fmt-println "Duration: ~A sec" (- (get-universal-time) start)))
   ))

(deflex stopwatch (create (stopwatch-beh)))

(defun subset-sum-beh (m found)
  (alambda
   ((cust n set pos sol)
    (unless found
      (when set
        (send self cust n (cdr set) (cdr pos) sol)
        (let ((new-n (with-mod m
                       (lmod (- n (car set)))))
              (new-sol (acons (car pos) (car set) sol)))
          (when (zerop new-n)
            (send cust (reverse new-sol))
            (become (subset-sum-beh m t))
            (send stopwatch :stop))
          (send self cust new-n
                (cdr set)
                (cdr pos)
                new-sol)))
      ))
   ((cust :init)
    (become (subset-sum-beh m nil))
    (send cust :ok))
   ))

(deflex subset-sum (create (subset-sum-beh *lattice-m* nil)))

(defun solve-subset-sum (cust n set)
  (send stopwatch :start)
  (β _
      (send subset-sum β :init)
    (send subset-sum cust n set
          (loop for x in set
                for ix from 0
                collect ix)
          nil)))
#|
(solve-subset-sum println 5 '(1 2 1 3))
(setf (actor-beh subset-sum) #'lw:do-nothing)
(with-mod *lattice-m*
  (let* ((nel 27)
         (b   (gen-random-list nel))
         (sel (gen-random-sel nel))
         (sel (ash 1 (1- nel)))
         (sum (lmod
               (loop for x in b
                     for ix from 0
                     when (logbitp ix sel)
                       sum x))))
    (send println (list sum (format nil "~b" sel)))
    (solve-subset-sum println sum b)))

(let ((siz  #(20 21 22 23 24 25  26  27))
      (tim  #( 1  3  6 13 22 45 101 265)))
  (plt:plot 'plt siz tim
            :clear t
            :thick 2
            :ylog  t
            :title  "Scalar Subset-Sum Solution"
            :xtitle "Set Size"
            :ytitle "Worst Case Duration [s]"
            )
  #+nil
  (multiple-value-bind (y0 sigma)
      (linfit:regress-fixed-slope siz (map 'vector (um:rcurry #'log 2) tim) 1 1)
    (list :y0 y0
          :sigma sigma) )
  
  (multiple-value-bind (xmn ymn slope sigma)
      (linfit:regression siz (map 'vector (um:rcurry #'log 2) tim) 1)
    (let ((off (/ (loop for s across siz
                        for tt across tim
                        sum (- (log tt 2) s))
                  (length siz))))
      (plt:fplot 'plt '(0 100)
                 (lambda (x)
                   (expt 2 (+ off x)))
               :color :red)
      (list :xmn xmn
            :ymn ymn
            :slope slope
            :sigma sigma)
      (list :off off))))

;; log2(dur) ≈ N - 19.4, i.e., at N = 19.4 we have 1s duration
;; log10(dur) ≈ 0.3*N - 5.8
;; Subtract 4.9 for Log10 duration in days
;; Subtract 7.5 for Log10 duration in years
;; So for 1 hour cloaking, use 31 NRows, 61 for 1G Safety
;;        1 day                36        66
;;        1 week               39        69
;;        1 month              41        71
;;        1 year               44        74
;;       30 year               49        79
;; Assume safety factor for attackers being 1 billion times faster
|#

#|
(let* ((m     (lat2-modulus))
       (nrows (lat2-nrows))
       (ncols (lat2-ncols))
       (mat   (with-mod m
                (gen-random-gaussian-matrix nrows ncols)))
       (mat   (with-mod m
                (gen-random-matrix nrows ncols)))
       ;; (mat   (lat2-matrix))
       (vals  (make-array (list (* nrows ncols))
                          :element-type 'single-float)))
  (loop for row across mat
        for rix from 0
        do
          (loop for x across row
                for cix from 0
                do
                  (setf (aref vals (+ cix (* rix ncols))) (float (/ x m)))
                  ))
  (plt:histogram 'histo vals
                 :clear t))

(let* ((v (vm:gnoise 10000))
       (v (map 'vector 'abs v)))
  (plt:histogram 'histo v
                 :clear t))

(let ((v (vm:gnoise 10000)))
  (/ (reduce (lambda (acc val)
               (+ acc (abs val)))
             v)
     10000))

(let ((v (vm:gnoise 10000)))
  (reduce (lambda (acc val)
            (max acc (abs val)))
          v))

(let* ((m     (lat2-modulus))
       (nrows 3)
       (ncols 2)
       (mat   (with-mod m
                (fgen-random-gaussian-matrix nrows ncols))))
  (values (fgen-noise-vec nrows)
          mat))

(defun fgen-noise-vec (nel)
  ;; generate a Gaussian random vector, worst-case bounded by [-m/4, m/4)
  (declare (fixnum nel))
  (let ((v  (vm:gnoise nel))
        (v+ 0.0f0)
        (v- 0.0f0))
    (declare (single-float v+ v-))
    (map 'nil (lambda (x)
                (declare (single-float x))
                (if (plusp x)
                    (incf v+ x)
                  (decf v- x)))
         v)
    (let* ((vmax (max v+ v-))
           (sf   (/ 1 4 vmax)))
      (declare (single-float vmax sf))
      (format t "~%VMax = ~f" vmax)
      (vops:vscale sf v))
    ))

(defun fgen-random-gaussian-matrix (nrows ncols)
  (let ((sf  (/ 1 4 nrows ncols))
        (mat (make-array nrows)))
      (loop for rix from 0 below nrows do
            (setf (aref mat rix)
                  (vops:vscale sf (vm:gnoise ncols))))
      mat))

(with-mod *lattice-m*
  (let* ((mat   (gen-random-gaussian-matrix 2 2))
         (map   (gen-random-matrix 2 2))
         (noise (gen-noise-vec 2))
         (skey  (gen-random-vec 2)))
    (labels ((vdot (a b)
               (lmod
                (reduce #'+
                        (map 'vector #'* a b))))
             (v+ (a b)
               (map 'vector #'lm+ a b))
             (pk (skey)
               (vector
                (vdot skey (aref mat 0))
                (vdot skey (aref mat 1))))
             (pkn (skey)
               (v+ noise (pk skey)))
             (show (skey &rest args)
               (let ((vec (pk skey)))
                 (apply #'plt:plot 'plt
                        (list (/ (aref vec 0) (mod-base)))
                        (list (/ (aref vec 1) (mod-base)))
                        ;; :symbol :circle
                        args)))
             (shown (skey &rest args)
               (let ((vec (pkn skey)))
                 (apply #'plt:plot 'plt
                        (list (/ (aref vec 0) (mod-base)))
                        (list (/ (aref vec 1) (mod-base)))
                        ;; :symbol :circle
                        args))))
      (show skey :clear t :color :red
            :xrange '(-0.5 0.5)
            :yrange '(-0.5 0.5))
      (loop for ix from -30 to 30 by 1 do
              (loop for iy from -30 to 30 by 1 do
                      (shown (v+ skey (vector ix iy)) :symbol :dot)
                    ))
      (let ((vec (pkn skey)))
        (plt:draw-circle 'plt (/ (aref vec 0) (mod-base))
                         (/ (aref vec 1) (mod-base))
                         0.25 :color :gray70 :alpha 0.5))
      (show skey :color :red :symbol :circle)
      (shown skey :color :cyan :symbol :circle)
      )))
                           
|#
