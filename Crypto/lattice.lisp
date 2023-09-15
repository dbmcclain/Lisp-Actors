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
  ;; m should be the Pkey matrix [b | -A] with column vector b
  ;; prepended to -A matrix, stored row-wise. Selector is a random
  ;; integer in the range [1,2^nrows). We simply add the matrix rows
  ;; corresponding to non-zero bits in the selector integer.
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
;; NRows sets the difficulty of the subset sum problem O(2^NRows)
;;
;; If you can solve the subset sum problem on each entry of the
;; cryptotext vector then you could determine the bit value being
;; encrypted.
;;
;; -----------------------------------------------------------
;; For the problem of attacking the encryption, when given the public
;; key matrix and a cryptotext vector:
;;
;; Public key matrix contains (NCols+1) rows by NRows columns.  Each
;; element of the (NCols+1) element cryptotext vector represents the
;; same subset sum of selected columns, of up to NRows elements, from
;; the corresponding pubkey matrix row. I.e., element 1 is the sum of
;; selected columns from row 1 of the public key matrix, element 2
;; from row 2, and so on.
;;
;; There will always be at least one column selected. But you don't
;; know which columns. Element 1 of the cryptovector also adds the
;; scaled bit value of the 1-bit message.
;;
;; We do know that the selection weights are 0 or 1. And the bit value
;; is either 0 or 1, scaled by m/2. There are (NCols+1) rows in the
;; matrix, corresponding to the (NCols+1) elements of the cryptotext
;; vector.
;;
;; But there are NRows selection weights to solve for, plus the bit
;; value, with NCols+1 equations.  So, if NCols < NRows, then the
;; system is under-determined, and can't be solved directly. You could
;; solve in the least-squares sense, but that isn't useful here. What
;; we need to find is the (NRows+1)-bit "key".
;;
;; And because this "key" is purely random, and different for each
;; conveyed message bit, having no algorithmic periodicities, a
;; quantum computer offers no advantage here. The only weakness here
;; may be the quality of the underlying random noise generator.
;;
;; This is an NP-hard problem, growing exponentially difficult with
;; order O(2^(NRows+1)). A solution could be found by brute force, but
;; that becomes infeasible when NRows is large.
;;
;; Every time you encrypt a 1-bit message, you get a different random
;; cyphertext vector. Different keying is used for each transmitted
;; bit. So a chosen plaintext attack is useless.
;;
;; ----------------------------------------------------------
;; For the problem of attacking the secret key, and obtaining the
;; weight vector and noise values: This has nothing to do with
;; cryptovectors. It relies solely on cracking the pulic key matrix.
;;
;; Each element of row 1 in the pubkey matrix represets a weighted sum
;; of NCols elements from the column below, plus additive noise. There
;; are NCols weights, and NRows noise values. All of these are
;; unconstrained values, unlike the 1-bit selection weights from the
;; previous attack.
;;
;; You have NRows equations across the first row of the pubkey matrix.
;; And since always NRows < (NCols + NRows), the system is forever
;; under-determined - meaning, you can't solve for the weights and
;; noise, except in a least-squares sense. And since these weights and
;; noise are essentially unconstrained values, a brute force search is
;; infeasible for any dimensions.
;;
;; Here, the constraints are that these values are somewhere in the
;; range [0,m), or about 2^30 possible choices for each. This is
;; effectively unconstrained. You need to find just the right 30-bit
;; weight, for NRows x NCols of them. At 320x256 this becomes a 2.5
;; Mbit "key" to search for.
;;
;; And, again, since all of these weights and noise are random values,
;; there are no underlying field periodicities to discover, rendering
;; no advantage to a quantum computer. The only weakness to this
;; system may be the underlying random noise generator.
;;
;; ---------------------------------------------------------------
;;
;; Secret key skey = #(1 | x), for x = #(x_1 x_2 ...  x_NCols), for
;; x_i random in [-m/2,m/2), prime modulus m.
;;
;; The A matrix is an Nrow x NCols random matrix, serving to expand
;; the dimensionality of the secret key. A_i,j in [-m/2, m/2)
;;
;; We compute noisy expansion b = A•x + psi, for noise vector
;; psi.  Each element of psi comes from a sampled Gaussian
;; distribution with mean 0 and sigma 1, scaled by (m/4)/gmax, where
;; gmax is determined as the max absolute sum of all positive samples
;; vs all negative samples, considering that the worst case random
;; selection vector will choose one of these pathalogical cases. This
;; ensures that any summed noise contribution will never be outside of
;; the bounds (-m/4, m/4).
;;
;; Public key is presented as Ptrn = Trn(b | -A), i.e., first row is
;; b, successive rows are from -Trn(A).
;;
;; Encryption occurs one bit at a time, scaled by m/2. So bit value
;; with added noise will either be in the range (-m/4,m/4) for bit 0,
;; or (-3m/4,-m/4) or (m/4,3m/4) for bit 1. Rounding these to m/2
;; should return -1, 0, or +1. Take that modulo 2 to get back 0 or 1.
;;
;; For each bit, encryption is by way of choosing non-zero random
;; selection vector r = (r_1, r_2, ... r_Nrows) for r_i in (0,1). Then
;; cryptotext vector for a single bit is: c = Ptrn•r + (m/2)*bit
;;
;; Decryption is by way of taking dot product of c with skey:
;;
;;  skey•c = skey•#((Trn(b)•r + (m/2)*bit) | -Trn(A)•r)
;;           = #(1 | Trn(x))•(Trn(x)•Trn(A)•r + Trn(psi)•r + (m/2)*bit | -Trn(A)•r)
;;           = Trn(x)•Trn(A)•r + Trn(psi)•r + (m/2)*bit - Trn(x) . Trn(A)•r
;;           = Trn(psi)•r + (m/2)*bit
;;
;; Then Round(skey•c, m/2) mod 2 => bit
;;
;; Here, prime modulus is chosen so that intermediate products remain
;; FIXNUM.

(defvar *lattice-m*      (- (ash 1 30) 35)) ;; prime modulus
(defvar *lattice-nrows*  320)  ;; cyphertext vectors have this length
(defvar *lattice-ncols*  256)  ;; private key vector has this length

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
    (let ((vmax (max v+ v-)))
      (declare (single-float vmax))
      (map 'vector #'round
           (vops:vscale (/ (mod-base) 4 vmax) v)))
    ))

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
