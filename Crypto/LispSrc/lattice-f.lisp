;; lattice-f.lisp - experiments in LWE Encryption, using Floating Point Arithmetic
;;
;; DM/RAL  2023/03/20 06:17:16
;; ----------------------------------

(defpackage #:flattice
  (:use #:common-lisp #:edec #:modmath #:vec-repr #:hash))

(in-package #:flattice)

;; ----------------------------------
;; Modular arithmetic using values between [-m/2, m/2) instead of [0, m).

(defun vec+ (v1 v2)
  #F
  (declare (vector single-float v1 v2))
  (assert (eql (length v1) (length v2)))
  (map 'vector #'+ v1 v2))

(defun vdot (v1 v2)
  #F
  (declare (vector single-float v1 v2))
  (assert (eql (length v1) (length v2)))
  (reduce #'+ (map 'vector #'* v1 v2)))

(defun mat*v (m v)
  #F
  (coerce
   (loop for mv across m collect
           (vdot mv v))
   'vector))

(defun enc-vdot (v r)
  #F
  (loop for ix fixnum from 0
        for x across v
        sum
          (if (logbitp ix r)
              x
            0.0f0)))

(defun enc-mat*v (m v)
  #F
  (coerce
   (loop for mv across m collect
           (enc-vdot mv v))
   'vector))
#|
(defun trn (m)
  #F
  (let* ((nrows (length m))
         (ncols (length (aref m 0))))
    (declare (fixum nrows ncols))
    (coerce
     (loop for ix fixnum from 0 below ncols collect
             (coerce 
              (loop for jx from 0 below nrows collect
                      (aref (aref m jx) ix))
              'vector))
     'vector)))
|#
;; ---------------------------------------------------
;; NRows sets the difficulty of the subset sum problem O(2^NRows)
;;
;; If you could solve the subset sum problem on each entry of the
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
;; know which columns. Element 1 of the cryptovector also adds the bit
;; value of the 1-bit message.
;;
;; There are NRows selection weights to solve for, plus the bit value,
;; with NCols+1 equations.  If NCols < NRows, then the system is
;; under-determined, and can't be solved directly.
;;
;; But we know that the selection weights are 0 or 1. And the bit
;; value is either 0 or 1. There are (NCols+1) rows in the matrix,
;; corresponding to the (NCols+1) elements of the cryptotext vector.
;;
;; This is an NP-hard problem, growing exponentially difficult with
;; order O(2^(NRows+1)). A solution could be found by brute force, but
;; infeasible when NRows is large.
;;
;; ----------------------------------------------------------
;; For the problem of attacking the secret key, and obtaining the
;; weight vector and noise values: This has nothing to do with
;; cryptovectors.
;;
;; Each element of row 1 in the pubkey matrix represets a weighted sum
;; of NCols elements from the column below, plus additive noise.
;;
;; You don't know the weights nor the noise. Hence there are NCols
;; weights, and NRows noise values. These values are unconstrained,
;; unlike selection weights in the section above.
;;
;; You have NRows equations across the first row of the pubkey matrix.
;; So since NRows < (NCols + NRows), the system is under-determined -
;; meaning, you can't solve for the weights and noise. And since these
;; weights and noise are unconstrained values, a brute force search is
;; infeasible at any dimension.
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

(defvar *lattice-nrows*  320)  ;; cyphertext vectors have this length
(defvar *lattice-ncols*  256)  ;; private key vector has this length

(hcl:defglobal-variable *decode-errs*  0)

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
  (coerce (gen-random-vec nel) 'list))

(defun gen-random-vec (nel)
  ;; (vops:voffset -1.0f0 (vm:unoise nel 2.0f0))
  (vm:gnoise nel))

(defun gen-select-vec (nel)
  ;; geneerate a random non-zero binary row-selection vector
  #F
  (declare (fixnum nel))
  (let ((r (prng:random-between 1 (ash 1 nel))))
    (declare (integer r))
    (coerce
     (loop for ix fixnum from 0 below nel collect
             (float (ldb (byte 1 ix) r) 1.0f0))
     'vector)))

(defun gen-random-matrix (nrows ncols)
  ;; Matrix is a vector of row-vectors
  #F
  (declare (fixnum nrows ncols))
  (coerce
   (loop for ix fixnum from 1 to nrows collect
           (gen-random-vec ncols))
   'vector))

#+nil
(defun gen-noise-vec (nel)
  ;; generate a Gaussian random vector, worst-case bounded by [-m/4, m/4)
  (declare (fixnum nel))
  (let ((v  (vm:gnoise nel))
        (v+ 0.0f0)
        (v- 0.0f0))
    (declare (single-float v+ v-))
    (map 'nil (lambda (x)
                (if (plusp x)
                    (incf v+ x)
                  (decf v- x)))
         v)
    (let ((vmax (max v+ v-)))
      (declare (single-float vmax))
      (vops:vscale (/ 0.5f0 vmax) v)
      )))

#-+nil
(defun gen-noise-vec (nel)
  ;; generate a Gaussian random vector, worst-case bounded by [-m/4, m/4)
  (declare (fixnum nel))
  (vm:gnoise nel :sd (/ 0.5f0 3f0 (sqrt nel))))

;; --------------------------------------------------------
;; LWE Lattice Key-Pair Generation

(defun lat-gen-keys ()
  (let* ((tt     (gen-random-list *lattice-ncols*))
         (ttv    (coerce tt 'vector))
         (skey   (coerce (cons 1.0f0 tt) 'vector))
         (amat   (gen-random-matrix *lattice-nrows* *lattice-ncols*))
         (noise  (gen-noise-vec *lattice-nrows*))
         (b      (vec+ (mat*v amat ttv) noise))
         (ptrn   (coerce
                  (cons b
                        ;; form -Trn(A)
                        (loop for col fixnum from 0 below *lattice-ncols* collect
                                (coerce
                                 (loop for row fixnum from 0 below *lattice-nrows* collect
                                         (- (aref (aref amat row) col)))
                                 'vector)))
                  'vector)))
      ;; (print (list g+ g- gmax))
      (values skey ptrn)))

;; ----------------------------------------------------
;; LWE Lattice Encoding

(defun lat-encode1 (pkey b)
  ;; pkey is ptrn matrix
  ;; b is bit 0, 1
  #F
  (declare (fixnum b))
  (let* ((ncols (length (aref pkey 0)))
         ;; (r     (gen-select-vec ncols))
         (r     (prng:random-between 1 (ash 1 ncols)))
         (v     (enc-mat*v pkey r)))
    (setf (aref v 0) (+ (aref v 0)
                        (float b 1.0f0)))
    v))

(defun lat-encode-nib (pkey n)
  #F
  (declare (fixnum n))
  #-nil
  (let* ((encn (aref *hamming47-enc* n)))
    (declare (fixnum encn))
    (loop for pos fixnum from 6 downto 0 collect
          (lat-encode1 pkey (ldb (byte 1 pos) encn))))
  #+nil
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
  (loop for x across v nconc
        (lat-encode-byte pkey x)))

(defun lat-enc (pkey &rest objs)
  ;; general object encryption
  (lat-encode pkey (loenc:encode (coerce objs 'vector))))

;; ---------------------------------------------------------------
;; LWE Lattice Decoding

(defun lat-decode1 (skey c)
  ;; c is a cryptotext vector
  (let ((cdots (vdot c skey)))
    (declare (single-float cdots))
    (mod (round cdots) 2)))
  
(defun lat-decode-nib (skey cs)
  ;; cs is a list of cryptotext vectors,
  ;; one vector for each bit of the message.
  ;; Encoding was a Hamming(4.7) code in big-endian bit order.
  #F
  #-nil
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
  #+nil
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
  (um:nlet iter ((cs    cs)
                 (bytes nil))
    (if (endp cs)
        (coerce (nreverse bytes) 'vector)
      (multiple-value-bind (b new-cs)
          (lat-decode-byte skey cs)
        (go-iter new-cs (cons b bytes)))
      )))

(defun lat-dec (skey cs)
  ;; general object decryption
  (values-list (coerce (loenc:decode (lat-decode skey cs)) 'list)))

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

(lat-dec *tst-skey* (lat-enc *tst-pkey* :hello 'there pi 15 (hash:hash/256 :hash)))

(let ((enc (lat-enc *tst-pkey* (hash:hash/256 :hello 'there pi 15))))
  ;; (inspect enc)
  (lat-dec *tst-skey* enc))

(let ((enc (lat-encode *tst-pkey* (vec-repr:vec (hash:hash/256 :hello 'there pi 15)))))
  (inspect enc))

(let* ((v (vec (hash/256 :hello 'there pi 15)))
       (e (lat-encode *tst-pkey* v))
       (enc (loenc:encode (coerce e 'vector))))
  (length enc))
  
(defun chk-timing (&optional (ntimes 1000))
  (let ((v (vec-repr:vec (hash:hash/256 :hello 'there pi 15))))
    (time
     (dotimes (ix ntimes)
       (lat-decode *tst-skey*
                   (lat-encode *tst-pkey* v))))
    ))

;; approx 400 bps (yes, bits) at 320x256 size
(chk-timing 100)

(inspect
 (let ((v (vec-repr:vec (hash:hash/256 :hello 'there pi 15))))
   (lat-encode *tst-pkey* v)))

(let* ((v (lat-encode *tst-pkey* (vec-repr:vec (hash:hash/256 :hello 'there pi 15))))
       (lst (mapcan (lambda (x)
                      (coerce x 'list))
                    v)))
  (plt:histogram 'histo lst
                 :clear t
                 :title "Histogram of FLattice Image"
                 :xtitle "Cryptotext Value"
                 :ytitle "Probability Density"
                 ))

(let* ((v (lat-encode *tst-pkey* (vec-repr:vec (hash:hash/256 :hello 'there pi 15))))
       (lst (mapcan (lambda (x)
                      (cdr (coerce x 'list)))
                    v)))
  (print (list (vm:mean lst) (vm:stdev lst)))
  (plt:histogram 'histo lst
                 :clear t
                 :title "Histogram of FLattice Image sans Stripe"
                 :xtitle "Cryptotext Value"
                 :ytitle "Probability Density"
                 ))

(let* ((v   (lat-encode *tst-pkey* (vec-repr:vec (hash:hash/256 :hello 'there pi 15))))
       (lst (mapcar (lambda (v)
                      (aref v 0))
                    v)))
  ;; (inspect lst)
  (print (list (vm:mean lst) (vm:stdev lst)))
  (plt:histogram 'histo lst
                 :clear t
                 :title "Histogram of FLattice Stripe"
                 :xtitle "Cryptotext Value"
                 :ytitle "Probability Density"
                 ))

(let* ((h     (hash:hash/256 :hello 'there pi 15))
       (v     (lat-encode *tst-pkey* (vec-repr:vec h)))
       (nrows (length v))
       (ncols (length (car v)))
       (magn  2)
       (img   (make-array (list nrows ncols)
                          :element-type 'single-float)))
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
  (let ((v (vec-repr:vec (hash:hash/256 :hello 'there pi 15))))
    (let ((errs *decode-errs*))
      (dotimes (ix ntimes)
        (when (zerop (mod ix 100))
          (terpri)
          (princ #\R)
          (re-key))
        (let ((ans (lat-decode *tst-skey*
                                       (lat-encode *tst-pkey* v))))
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

(defun shake7 (sd nrow)
  (let ((err 0))
    (dotimes (ix 7)
      (let ((sum (reduce #'+ (vm:gnoise (prng:random-between 1 nrow)
                                        :sd (/ sd (sqrt nrow))
                                        ))))
        (when (>= (abs sum) 1.0f0)
          (incf err))))
    err))

(defun chk-sd (&key (sd 1.0f0) (n 1000) (nrow 320))
  ;; Trace:  R = rekey, . = normal, x = soft error, X = hard error
  (dotimes (ix n)
    (when (zerop (mod ix 100))
      (terpri)
      (princ #\R))
    (let ((ans (shake7 sd nrow)))
      (princ
       (cond ((< ans 1) #\.)
             ((< ans 2) #\s)
             (t #\X)))
      )))

(chk-sd :n 10000 :sd (/ 3f0))
|#
;; ------------------------------------------------------------------
