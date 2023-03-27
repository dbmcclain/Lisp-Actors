;; lattice.lisp - experiments in LWE Encryption
;;
;; DM/RAL  2023/03/20 06:17:16
;; ----------------------------------

(defpackage #:com.ral.crypto.lattice-crypto
  (:use #:common-lisp #:edec #:modmath #:vec-repr #:hash)
  (:export
   #:with-skey
   #:with-pkey
   #:lat-enc    ;; for encoding general objects
   #:lat-dec    ;; for decoding back into general objects
   #:lat-encode ;; for encoding octet vector
   #:lat-decode ;; for decoding to octet vector
   #:lat-gen-skey
   #:lat-gen-pkey
   #:lat-gen-keys ;; create dummy pair (skey, pkey) for testing
   #:trn
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
  (declare (vector fixnum v1 v2))
  ;; (assert (eql (length v1) (length v2)))
  (map 'vector #'lm+ v1 v2))

(defun vdot (v1 v2)
  #F
  (declare (vector fixnum v1 v2))
  ;; (assert (eql (length v1) (length v2)))
  (let* ((m     (mod-base))
         (nbits (integer-length m))
         (wrap  (- (ash 1 nbits) m))
         (nshft (- nbits))
         (bits  (byte nbits 0)))
    (declare (fixnum nbits wrap nshft))
    (lmod (loop for x1 fixnum across v1
                for x2 fixnum across v2
                sum
                  (if (zerop x2)
                      0
                    (the fixnum
                         (let ((prod (* x1 x2)))
                           (declare (fixnum prod))
                           (+ (the fixnum (ldb bits prod))
                              (the fixnum (* wrap (ash prod nshft))))
                           ))
                    ))
          )))

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
  (declare (integer sel))
  (let ((nrow (length m)))
    (declare (fixnum nrow))
    (um:nlet outer ((ix 0))
      (declare (fixnum ix))
      (cond ((logbitp ix sel)
             (let ((ans  (copy-seq (aref m ix))))
             (um:nlet inner ((ix  (1+ ix)))
               (declare (fixnum ix))
               (cond ((>= ix nrow)
                      (map-into ans #'lmod ans))
                     ((logbitp ix sel)
                      (map-into ans #'+ ans (aref m ix))
                      (go-inner (1+ ix)))
                     (t
                      (go-inner (1+ ix)))
                     ))))
            (t
             (go-outer (1+ ix)))
            ))))
               
#||#
(defun trn (m)
  #F
  (let* ((nrows (length m))
         (ncols (length (aref m 0)))
         (ans   (make-array ncols)))
    (declare (fixnum nrows ncols))
    (dotimes (col ncols)
      (let ((colv (make-array nrows
                              :element-type 'fixnum)))
        (dotimes (row nrows)
          (setf (aref colv row)
                (aref (aref m row) col)))
        (setf (aref ans col) colv)))
    ans))
#||#
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

(hcl:defglobal-variable *decode-errs*  0)
(setf *decode-errs* 0)

(defvar *hamming47-enc*
  #(  0 105  42  67
     76  37 102  15
    112  25  90  51
     60  85  22 127))

(defvar *hamming47-dec*
  #(0  0  0  3   0  5 14  7    0  9  2  7   4  7  7  7
    0  9 14 11  14 13 14 14    9  9 10  9  12  9 14  7
    0  5  2 11   5  5  6  5    2  1  2  2  12  5  2  7
    8 11 11 11  12  5 14 11   12  9  2 11  12 12 12 15
    
    0  3  3  3   4 13  6  3    4  1 10  3   4  4  4  7
    8 13 10  3  13 13 14 13   10  9 10 10   4 13 10 15
    8  1  6  3   6  5  6  6    1  1  2  1   4  1  6 15
    8  8  8 11   8 13  6 15    8  1 10 15  12 15 15 15))

;; ------------------------------------------------------
(progn
  ;; check correctness of Hamming tables
  (loop for ix from 0 below 16 do
          (assert (eql ix (aref *hamming47-dec*
                                (aref *hamming47-enc* ix)))))
  
  (let ((v  (make-array 16 :initial-element 0)))
    (loop for ix from 0
          for x across *hamming47-dec* do
            (setf (aref v x) (logxor (aref v x)
                                     ix)))
    ;; (inspect v)
    (assert (every (lambda (x)
                     (eql x #x7f))
                   v))))
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

#|
(defun gen-select-vec (nel)
  ;; geneerate a random non-zero binary row-selection vector
  #F
  (declare (fixnum nel))
  (let ((r (prng:random-between 1 (ash 1 nel))))
    (declare (integer r))
    (coerce
     (loop for ix fixnum from 0 below nel collect
             (ldb (byte 1 ix) r))
     'vector)))
|#

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

;; --------------------------------------------------------
;; LWE Lattice Key-Pair Generation

(defun lat-gen-skey (&key (ncols *lattice-ncols*) (modulus *lattice-m*))
  (when (> modulus (ash 1 30))
    (error "Modulus is too large: ~A" modulus))
  (when (< ncols 256)
    (error "NCols should be > 256: ~A" ncols))
  (with-mod modulus
    (list modulus
          (coerce (cons 1 (gen-random-list ncols)) 'vector))))

(defun lat-gen-pkey (skey &key (nrows *lattice-nrows*))
  (destructuring-bind (m s) skey
    (unless (> nrows (length s))
      (error "NRows should be > ~A: ~A" (length s) nrows))
    (with-mod m
      (let* ((id     (hash/256 skey))
             (xv     (subseq s 1))
             (ncols  (length xv))
             (amat   (gen-random-matrix nrows ncols))
             (noise  (gen-noise-vec nrows))
             (b      (vec+ (mat*v amat xv) noise))
             (pkey   (make-array nrows)))
        (dotimes (row nrows)
          (let ((rowv (make-array (1+ ncols)
                                  :element-type 'fixnum)))
            (setf (aref rowv 0) (aref b row))
            (replace rowv (map 'vector #'- (aref amat row))
                     :start1 1)
            (setf (aref pkey row) rowv)))
        (list (str (hex id)) m pkey))
      )))

(defun lat-gen-keys ()
  (let* ((skey (lat-gen-skey :ncols   *lattice-ncols*
                             :modulus *lattice-m*))
         (pkey (lat-gen-pkey skey
                             :nrows   *lattice-nrows*)))
    (values skey pkey)))

#|
(with-standard-io-syntax
  (print (lat-gen-skey))
  (values))
|#

;; ----------------------------------------------------
;; LWE Lattice Encoding

(defun lat-encode1 (pkey b)
  ;; pkey is ptrn matrix
  ;; b is bit 0, 1
  #F
  (declare (fixnum b))
  (with-mod *lattice-m*
    (let* ((ncols (length (aref pkey 0)))
           ;; (r     (gen-select-vec ncols))
           ;; (v     (mat*v pkey r))
           (r     (prng:random-between 1 (ash 1 ncols)))
           (v     (enc-mat*v pkey r)))
      (setf (aref v 0) (lm+ (aref v 0)
                            (* b (ash (mod-base) -1))))
      v)))

(defun lat-encode-nib (pkey n)
  #F
  (declare (fixnum n))
  #+nil
  (let* ((encn (aref *hamming47-enc* n)))
    (declare (fixnum encn))
    (loop for pos fixnum from 6 downto 0 collect
          (lat-encode1 pkey (ldb (byte 1 pos) encn))))
  #-nil
  (loop for pos fixnum from 3 downto 0 collect
          (lat-encode1 pkey (ldb (byte 1 pos) n)))
  )

(defun lat-encode-byte (pkey x)
  ;; Encode octet as big-endian bitwise encoding
  ;; via Hamming(4,7) ECC encoding.
  #F
  (declare (fixnum x))
  (nconc
   (lat-encode-nib pkey (ldb (byte 4 4) x))
   (lat-encode-nib pkey (ldb (byte 4 0) x))))

(defun lat-encode (pkey v)
  ;; v should be a vector of octets
  ;; Encodes octet vector into a list of cyphertext vectors
  (coerce
   (cons *lattice-m*
         (loop for x across v nconc
                 (lat-encode-byte pkey x)))
   'vector))
  
(defun lat-enc (pkey &rest objs)
  ;; general object encryption
  (lat-encode pkey (loenc:encode (coerce objs 'vector))))

(defun do-with-pkey (pkey fn)
  (destructuring-bind (id m ptrn) pkey
    (declare (ignore id))
    (let ((*lattice-m* m))
      (funcall fn ptrn))
    ))

(defmacro with-pkey ((key pkey) &rest body)
  `(do-with-pkey ,pkey (lambda (,key)
                         ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-pkey" 1)

;; ---------------------------------------------------------------
;; LWE Lattice Decoding

(defun lat-decode1 (skey c)
  ;; c is a cryptotext vector
  (with-mod *lattice-m*
    (let ((cdots (vdot c skey)))
      (mod (round cdots (ash (mod-base) -1)) 2))
    ))
  
(defun lat-decode-nib (skey cs)
  ;; cs is a list of cryptotext vectors,
  ;; one vector for each bit of the message.
  ;; Encoding was a Hamming(4.7) code in big-endian bit order.
  #F
  #+nil
  (um:nlet iter ((cs  cs)
                 (n   0)
                 (ct  7))
    (declare (fixnum n ct))
    (cond ((plusp ct)
           (go-iter (cdr cs)
                    (+ (ash n 1)
                       (lat-decode1 skey (car cs)))
                    (1- ct)))
          (t
           (unless (find n *hamming47-enc*)
             (sys:atomic-fixnum-incf *decode-errs*))
           (values (aref *hamming47-dec* n) cs))
          ))
  #-nil
  (um:nlet iter ((cs  cs)
                 (n   0)
                 (ct  4))
    (declare (fixnum n ct))
    (cond ((plusp ct)
           (go-iter (cdr cs)
                    (+ (ash n 1)
                       (lat-decode1 skey (car cs)))
                    (1- ct)))
          (t
           (values n cs))
          ))
  )

(defun lat-decode-byte (skey cs)
  ;; cs is a list of cryptotext vectors,
  ;; one vector for each bit of the message.
  ;; Encoding is big-endian nibble-wise.
  (multiple-value-bind (nhi new-cs)
      (lat-decode-nib skey cs)
    (multiple-value-bind (nlo new-cs)
        (lat-decode-nib skey new-cs)
      (values (+ (ash nhi 4) nlo) new-cs))
    ))

(defun lat-decode (skey cs)
  ;; decode a list of cyphertext vectors into an octet vector
  (let ((cs (coerce cs 'list)))
    (let ((m (car cs)))
      (assert (eql m *lattice-m*)))
    (um:nlet iter ((cs    (cdr cs))
                   (bytes nil))
      (if (endp cs)
          (coerce (nreverse bytes) '(vector (unsigned-byte 8)))
        (multiple-value-bind (b new-cs)
            (lat-decode-byte skey cs)
          (go-iter new-cs (cons b bytes)))
        ))))

(defun lat-dec (skey cs)
  ;; general object decryption
  (values-list (coerce (loenc:decode (lat-decode skey cs)) 'list)))

(defun do-with-skey (skey fn)
  (destructuring-bind (m key) skey
    (let ((*lattice-m* m))
      (funcall fn key))))

(defmacro with-skey ((key skey) &body body)
  `(do-with-skey ,skey (lambda (,key)
                         ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-skey" 1)

;; -------------------------------------------------

#|
(defvar *tst-skey*)
(defvar *tst-pkey*) ;

(defun re-key ()
  (multiple-value-bind (skey pkey)
      (lat-gen-keys)
    (setf *tst-skey* skey
          *tst-pkey* pkey)))
(re-key)

;; -------------------------

(with-skey (skey *tst-skey*)
  (lat-dec skey
           (with-pkey (pkey *tst-pkey*)
                      (lat-enc pkey :hello 'there pi 15 (hash/256 :hash)))))

(with-pkey (pkey *tst-pkey*)
  (let ((enc (lat-enc pkey (hash/256 :hello 'there pi 15))))
    (inspect enc)
    (with-skey (skey *tst-skey*)
      (lat-dec skey enc))))

(with-pkey (pkey *tst-pkey*)
  (let ((enc (lat-encode pkey (vec (hash/256 :hello 'there pi 15)))))
    (inspect enc)))

(let* ((v (vec (hash/256 :hello 'there pi 15)))
       (e (with-pkey (pkey *tst-pkey*)
            (lat-encode pkey v)))
       (enc (loenc:encode (coerce e 'vector))))
  (length enc))
  
(defun chk-timing (&optional (ntimes 1000))
  (let ((v (vec (hash/256 :hello 'there pi 15))))
    (time
     (dotimes (ix ntimes)
       (with-skey (skey *tst-skey*)
         (lat-decode skey
                     (with-pkey (pkey *tst-pkey*)
                       (lat-encode pkey v)))))
     )))

;; approx 1280 bps (yes, bits) at 320x256 size
(chk-timing 100)

(inspect
 (let ((v (vec (hash/256 :hello 'there pi 15))))
   (with-pkey (pkey *tst-pkey*)
     (lat-encode pkey v))))

(with-pkey (pkey *tst-pkey*)
  (let* ((v (lat-encode pkey (vec (hash/256 :hello 'there pi 15))))
         (lst (mapcan (lambda (x)
                        (coerce x 'list))
                      (coerce (subseq v 1) 'list))))
    (plt:histogram 'histo lst
                   :clear t
                   :title "Lattice Encryption Image Histogram"
                   :xtitle "Cryptotext Value"
                   :ytitle "Probability Density"
                   )))
  
(let* ((h     (hash/256 :hello 'there pi 15))
       (v     (with-pkey (pkey *tst-pkey*)
                (coerce (subseq (lat-encode pkey (vec h)) 1) 'list)
                ))
       (nrows (length v))
       (ncols (length (car v)))
       (magn  2)
       (img   (make-array (list nrows ncols)
                          :element-type 'fixnum)))
  (loop for row from 0 below nrows
        for rowv in v
        do
          (loop for col from 0 below ncols
                for x across rowv
                do
                  (setf (aref img row col) x)))
  (plt:window 'img
              :height (* magn nrows)
              :width  (* magn ncols))
  (plt:tvscl 'img (vm:shifth img)
             :clear t
             :magn  magn)
  (hex h))

(defun chk-errs (&optional (ntimes 1000))
  ;; Trace:  R = rekey, . = normal, x = soft error, X = hard error
  (let ((v (vec (hash/256 :hello 'there pi 15))))
    (let ((errs *decode-errs*))
      (dotimes (ix ntimes)
        (when (zerop (mod ix 100))
          (terpri)
          (princ #\R)
          (re-key))
        (let ((ans (with-skey (skey *tst-skey*)
                     (lat-decode skey
                                 (with-pkey (pkey *tst-pkey*)
                                   (lat-encode pkey v))))))
          (princ
           (if (equalp v ans)
               (if (eql *decode-errs* errs)
                   #\.
                 (progn
                   (setf errs *decode-errs*)
                   #\x))
             (progn
               (setf errs *decode-errs*)
               #\X)))
          )))))

(chk-errs 1000)
(chk-errs 100)

(let* ((nbits 30)
       (wrap  35)
       (base  (- (ash 1 nbits) wrap))
       (rsq   (mod (* wrap wrap) base))
       (ninv  -6)
       (shft  (- nbits))
       (bits  (byte 30 0)))
  (declare (fixnum nbits wrap base rsq ninv shft))
  
  (defun redc (x)
    #F
    (declare (fixnum x))
    (let* ((q  (ldb bits (* (ldb bits x) ninv)))
           (a  (ash (- x (* q base)) shft)))
      (declare (fixnum q a))
      (if (minusp a)
          (+ a base)
        a)))
  
  (defun to-monty (x)
    #F
    (declare (fixnum x))
    (redc (* x rsq)))

  (defun from-monty (x)
    #F
    (declare (fixnum x))
    (redc x))

  (defun f*monty (a b)
    #F
    (declare (fixnum a b))
    (redc (* a b))))


(defun tst ()
  (with-mod *lattice-m*
    (let ((niter 1000)
          (x  (gen-random-vec 256))
          (a  (gen-random-matrix 320 256))
          (m  (mod-base)))
      (time
       (dotimes (ix niter)
         (map 'vector (lambda (av)
                        (mod (reduce #'+
                                     (map 'vector (lambda (a b)
                                                    (mod (* a b) m))
                                          av x))
                             m))
              a)))
      (let ((y  (map 'vector #'to-monty x))
            (b  (map 'vector (lambda (av)
                               (map 'vector #'to-monty av))
                     a)))
        (time
         (dotimes (ix niter)
           (map 'vector (lambda (av)
                          (mod (reduce #'+
                                       (map 'vector #'f*monty av y))
                               m))
                b))))
      )))
(tst)
|#
;; ----------------------------------------------------------------------
