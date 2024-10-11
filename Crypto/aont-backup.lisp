;; reed-solomon.lisp - Encoding (8,6) in GF(2^128)
;;
;; Correctable AES/256 AONT Encryption.
;; Each AES/256 block is 128 bytes - consider each block as GF(2^128) integer.
;;
;; We record 6 blocks, followed by 2 blocks of CRC, such that,
;; if one of the 8 blocks is damaged, it can be corrected.
;;
;; Possible use case is guarding source data (eg. ECC Curves) against
;; accidental source code changes. Keep an AONT encrypted copy of a
;; data structure. Compare decryption against source plaintext
;; version. Notify user if there is a mismatch, informing them of the
;; correct replacement source code.
;;
;; DM/RAL  2024/10/09 20:04:19 UTC
;; ----------------------------------

(in-package :ecc)

;; -------------------------------------------------------------
;; polynomials are represented by coefficent vectors (lowest to highest order)
;;
;; poly-xxx fns are expected to be called within (WITH-MOD base ...)
;; gf-poly-xx fns are expected to be called within (WITH-GFxxx ...)

;; ----------------------------------------------
;; structural functions

(defun zero-extend-order (v nel)
  ;; add more positions of high order
  (let ((vnew (make-array (+ (length v) nel)
                          :initial-element 0)))
    (replace vnew v)
    vnew))

(defun make-same-length (a b)
  (let ((lena (length a))
        (lenb (length b)))
    (cond
     ((< lena lenb)
      (setf a (zero-extend-order a (- lenb lena))))
     ((< lenb lena)
      (setf b (zero-extend-order b (- lena lenb)))))
    (values a b)))

(defun trim-order (v)
  (if (zerop (aref v (1- (length v))))
      (let ((pos (position-if (complement #'zerop) v
                              :from-end t)))
        (if pos
            (subseq v 0 (1+ pos))
          (vector 0)))
    v))

(defun poly-prepend-element (v x)
  (concatenate 'vector (vector x) v))

(defun poly-prepend-zero (v)
  (poly-prepend-element v 0))

;; ------------------------------------------------
;; arithmetic functions

(defun gen-poly-add (a b add-fn)
  (multiple-value-bind (a b) (make-same-length a b)
    (trim-order (map 'vector add-fn a b))))

(defun poly-add (a b)
  (gen-poly-add a b #'m+))

(defun gf-poly-add (a b)
  (gen-poly-add a b #'gf+))

(defun z-poly-add (a b)
  (gen-poly-add a b #'+))

(defun poly-sub (a b)
  (gen-poly-add a b #'m-))

(defun gf-poly-sub (a b)
  (gen-poly-add a b #'gf-))

(defun z-poly-sub (a b)
  (gen-poly-add a b #'-))


(defun gen-poly-scale (v sf mul-fn)
  (map 'vector (um:curry mul-fn sf) v))

(defun poly-scale (v sf)
  (gen-poly-scale v sf #'m*))

(defun gf-poly-scale (v sf)
  (gen-poly-scale v sf #'gf*))

(defun z-poly-scale (v sf)
  (gen-poly-scale v sf #'*))


(defun gen-poly-mul (a b poly-add-fn poly-scale-fn)
  (let ((prod (vector)))
    (loop for mpx across (reverse b) do
          (setf prod (funcall poly-add-fn
                              (poly-prepend-zero prod)
                              (funcall poly-scale-fn a mpx))))
    (trim-order prod)))

(defun poly-mul (a b)
  (gen-poly-mul a b #'poly-add #'poly-scale))

(defun gf-poly-mul (a b)
  (gen-poly-mul a b #'gf-poly-add #'gf-poly-scale))

(defun z-poly-mul (a b)
  (gen-poly-mul a b #'z-poly-add #'z-poly-scale))


(defun gen-poly-divmod (p q inv-fn mul-fn sub-fn)
  (let* ((p    (trim-order p))
         (plen (length p))
         (q    (trim-order q))
         (qlen (length q)))
    (labels ((scale-fn (v sf)
               (map 'vector (um:curry mul-fn sf) v)))
      (let ((sf  (aref q (1- qlen))))
        (unless (eql sf 1)
          (let ((inv-sf (funcall inv-fn sf)))
            (setf q (scale-fn q inv-sf)
                  p (scale-fn p inv-sf))
            )))
      (um:nlet iter ((p    p)
                     (pend plen)
                     (quot (vector)))
        (if (< pend qlen)
            (values (trim-order quot)
                    (trim-order p)) ;; remainder mod q
          (let* ((start (- pend qlen))
                 (pref  (subseq p 0 start))
                 (suf   (subseq p start))
                 (ld    (aref suf (1- qlen))))
            (setf quot (poly-prepend-element quot ld)
                  suf  (subseq (if (zerop ld)
                                   suf
                                 (map 'vector sub-fn suf
                                      (scale-fn q ld)))
                               0 (1- qlen))
                  p    (concatenate 'vector pref suf))
            (go-iter p (1- pend) quot))
          )))))

(defun poly-divmod (p q)
  (gen-poly-divmod p q #'m/ #'m* #'m-))

(defun gf-poly-divmod (p q)
  (gen-poly-divmod p q #'gf/ #'gf* #'gf-))

(defun z-poly-divmod (p q)
  (gen-poly-divmod p q #'/ #'* #'-))


(defun gen-poly-eval (p x add-fn mul-fn)
  (reduce (lambda (c sum)
            (funcall add-fn c
                     (funcall mul-fn x sum)))
          p
          :initial-value 0
          :from-end t))

(defun poly-eval (p x)
  (gen-poly-eval p x #'m+ #'m*))

(defun gf-poly-eval (p x)
  (gen-poly-eval p x #'gf+ #'gf*))

(defun z-poly-eval (p x)
  (gen-poly-eval p x #'+ #'*))


(defun gen-poly-dot (v1 v2 op-add op-mul)
  (assert (eql (length v1)
               (length v2)))
  (reduce op-add (map 'vector op-mul v1 v2)))

(defun poly-dot (v1 v2)
  (gen-poly-dot v1 v2 #'m+ #'m*))

(defun gf-poly-dot (v1 v2)
  (gen-poly-dot v1 v2 #'gf+ #'gf*))

(defun z-poly-dot (v1 v2)
  (gen-poly-dot v1 v2 #'+ #'*))

;; -----------------------------------------------------------------
;; Reed-Solomon Erasure Codes
;;
;; To summarize:
;;
;;   We can think of valid data patterns as polynomial coeffients.
;;   Suppose we work with 8-bit values where all patterns 00..FF are
;;   valid bit patterns. This would encourage us to think about
;;   working in Galois Field GF(2^8).
;;
;;   Let's take groups of 6 of these bytes as our data patterns. Every
;;   possible combination of 6 of these 8-bit patterns is acceptable.
;;   And our "data polynomial" would be:
;;
;;    P(x) = A*x^5 + B*x^4 + C*x^3 + D*x^2 + E*x + F
;;
;;   where our data is '(A B C D E F) and x is arbitrary.
;;
;;   So let's think about transmitting these 6-byte groups to someone
;;   else. How would they know if any bytes got changed along the way?
;;
;;   Well, we could attach a parity byte to make communication
;;   patterns of 7 bytes.
;;
;;   And we could consider these to be coefficients of a 6th order
;;   polynomial, with the check byte computed so that a valid received
;;   pattern wil always produce a polynomial value (the Syndrome) of
;;   zero for x=2.
;;
;;    Q(x) = A*x^6 + B*x^5 + C*x^4 + D*x^3 + E*2 + F*x + Chk
;;         = x*P(x) + Chk
;;
;;   where Q(2) = 0 for valid '(A B C D E F Chk)
;;
;;   IOW, Chk = -2*P(2)
;;
;;   In GF(2^8) all arithmetic produces results that are 00..FF. And
;;   in GF there is no distinction between operations of + and -.
;;   Adding is by XOR.
;;
;;   Now suppose the received pattern is '(A' B C D E F Chk), A' != A.
;;   Then the receiver will think the polynomial should be:
;;
;;      Q'(x) = x*P'(x) + Chk.
;;
;;    And so they compute the Syndrome:
;;
;;    Syndrome = Q'(2)
;;             = 2*P'(2) + Chk
;;             = 2*P'(2) + 2*P(2)
;;             = 2*(A'-A)*2^6      ;; You and I know this, but receiver doesn't...
;;            != 0
;;
;;   And so the non-zero Syndrome alerts the receiver that an error
;;   has occurred in transmission. We don't know which byte got
;;   changed, but one of the 7 bytes is definitely not part of a valid
;;   pattern.
;;
;;   This gives the general idea behind Reed-Solomon Erasure Codes. By
;;   using more than one check byte we can develop systems that can
;;   self-correct errors, or detect multiple errors.
;;
;;   Note too, that these aren't just bit-pattern checks. They check
;;   entire data elements. There could be any number of bit errors in
;;   the elements. Unless there is just the right pattern of bit
;;   errors to cancel each other out, we will know that something has
;;   changed.
;; --------------------------------------------
#|

     For (6,8) coding in GF(2^8)
     
     Q(x) = (x-x0)*(x-x1)*P(x)
          = x^2 P(x) + (x0+x1) x P(x) + x0 x1 P(x)
          = x^2 P(x) + Chk1 x + Chk2

     Q(x0) = x0^2 P(x0) + Chk1 x0 + Chk2 = 0
     Q(x1) = x1^2 P(x1) + Chk1 x1 + Chk2 = 0

     [ x0 1 ] [Chk1] = [x0^2 P(x0)]
     [ x1 1 ] [Chk2] = [x1^2 P(x1)]

     Det = (x0-x1)

     ∀ x ∊ GF: x+x = 0
     ∀ a,b ∊ GF: a+b = a-b
     ∀ a,b ∊ φ:  (a + b)^2 = a^2 + b^2

     [Chk1] = 1/Det [1   1][x0^2 P(x0)]
     [Chk2]         [x1 x0][x1^2 P(x1)]

     Syndrome = (Q(x0), Q(x1))

     If x0 = 2, x1 = 2^2 = 4:

      [Chk1] = 122 [1  1][ 4 P(2)]
      [Chk2]       [4  2][16 P(4)]

    4 P(2) = 128 A +  64 B +  32 C + 16 D +  8 E +  4 F
   16 P(4) =  19 A + 205 B + 116 C + 29 D + 64 E + 16 F

   Chk1 = 182 A + 179 B + 237 C + 120 D + 28 E + 6 F
   Chk2 =
|#
;;
;; --------------------------------------------

#|
(with-gf2^8
  (let* ((gpoly (gf-poly-mul #(1 4) #(1 2)))
         (msg    #(127 63))
         (mpoly (gf-poly-mul msg #(1 0 0))))
    (multiple-value-bind (_ rem)
        (gf-poly-divmod mpoly gpoly)
      (let ((cpoly (gf-poly-sub mpoly rem)))
        (setf ;; (aref cpoly 1) (gf+ (aref cpoly 1) 3)
              ;; (aref cpoly 0) (gf+ (aref cpoly 0) 5)
              ;; (aref cpoly 0) 0
              (aref cpoly 1) 0
              ;; (aref cpoly 2) (gf+ (aref cpoly 2) 1)
              )
        (let ((s1    (gf-poly-eval cpoly 2))
              (s2    (gf-poly-eval cpoly 4))
              (s3    (gf-poly-eval cpoly 8))
              (s4    (gf-poly-eval cpoly 16)))
          (let ((e1  (gf* 36 (gf+ (gf* 4 s1) s2)))
                (e2  (gf* 72 (gf+ (gf* 8 s1) s2))))
          (list cpoly `(:syn ,s1 ,s2 ,s3 ,s4) `(:err ,e1 ,e2))
          ))))))


(with-gf2^8
  (let* ((ndrives   8)
         (prim-root 2)
         (p1        prim-root)
         (p2        (gf^ p1 2))
         (gpoly     (gf-poly-mul (vector p1 1) (vector p2 1)))
         (msg       #(127 63 32 47 65 66))
         ;; (msg       #(1   1  0  0  0  0))
         (mpoly     (gf-poly-mul msg #(1 0 0)))
         (bad-drive #(1 2))
         ;; bad drives A & B
         (ixA   (aref bad-drive 0))
         (ixB   (aref bad-drive 1)))
    (multiple-value-bind (_ rem)
        (gf-poly-divmod mpoly gpoly)
      (let ((cpoly (gf-poly-sub mpoly rem)))
        (setf ;; (aref cpoly 1) (gf+ (aref cpoly 1) 3)
              ;; (aref cpoly 0) (gf+ (aref cpoly 0) 5)
              ;; (aref cpoly 0) 0
              ;; (aref cpoly 3) 0
              ;; (aref cpoly 2) (gf+ (aref cpoly 2) 1)
              ;; (aref cpoly (1- (aref bad-drive 0))) 0
              (aref cpoly IXA) 0
              )
        (let* ((s1    (gf-poly-eval cpoly p1))
               (s2    (gf-poly-eval cpoly p2))
               (mat   (vector (gf^ p1 ixA) (gf^ p1 ixB)
                              (gf^ p2 ixA) (gf^ p2 ixB)))
               (det-inv (gf/ (gf+ (gf* (aref mat 0) (aref mat 3))
                                  (gf* (aref mat 1) (aref mat 2)))))
               (matinv (gf-poly-scale (vector (aref mat 3) (aref mat 1)
                                              (aref mat 2) (aref mat 0))
                                      det-inv)))
          (let ((e1  (gf+ (gf* (aref matinv 0) s1)
                          (gf* (aref matinv 1) s2)))
                (e2  (gf+ (gf* (aref matinv 2) s1)
                          (gf* (aref matinv 3) s2))))
          `(:bad-drive ,bad-drive
            :recv      ,cpoly
            :syn       (,s1 ,s2)
            :err       (,e1 ,e2)
            :corr ,(let ((v (copy-seq cpoly)))
                     (setf (aref v ixA) (gf+ e1 (aref v ixA))
                           (aref v ixB) (gf+ e2 (aref v ixB)))
                     v))        
          ))))))

(with-gf2^8
  (let* ((prim  2)
         (rt1   prim)
         (rt2   (gf^ rt1 2))
         (gpoly (gf-poly-mul (vector rt1 1) (vector rt2 1))))
    (loop for ix from 0 below 6
          for msg = #(1) then (poly-prepend-zero msg)
          collect
          (multiple-value-bind (_ rem)
              (gf-poly-divmod (gf-poly-mul #(0 0 1) msg) gpoly)
            (list ix rem)))))

(defmacro w8 (&body body) `(with-gf2^8 ,@body))

;; -----------------------------------------------------------------------------------
Encoding Matrix for 2-code checksum for up to 6 drives.
For fewer drives use a lower left submatrix.
For 8-bit encoding using GF[2^8]
Using 2 checks allows for fixing 2 errors in erasure mode, with known drive errors,
or finding and fixing a single drive error, or detecting more than 2 errors.

Using rt1 = 2, rt2 = 4 = rt1^2

  [  8  48 224 231  59 241]                  [chk0] using root 2^2    (x^0)
  [  6  28 120 237 179 182]                  [chk1] using root 2      (x^1)
  [  1   0   0   0   0   0] msg              [A]                      (x^2)
  [  0   1   0   0   0   0] [A] drive 1      [B]                      (x^3)
  [  0   0   1   0   0   0] [B]              [C]                      (x^4)
  [  0   0   0   1   0   0] [C]           => [D]                      (x^5)
  [  0   0   0   0   1   0] [D]              [E]                      (x^6)
  [  0   0   0   0   0   1] [E]              [F]                      (x^7)

  Syndromes: (for rt1, rt2)
    syn = A*rt^7 + B*rt^6 + C*rt^5 + D*rt^4 + E*rt^3 + F*rt^2 + chk1*rt + chk2

                                      [chk0]
                                      [chk1]
                                      [A]
                                      [B]
    [  1   2   4   8  16  32  64 128] [C]       => [syn(2)  ]
    [  1   4  16  64  29 116 205  19] [D]          [syn(2^2)]
                                      [E]
                                      [F]

  If single error, then syn(2^2)/syn(2) = 2^N for error ord N (7 = F, 0 = chk1)
  Then error E = syn(2)/2^N = syn(2)^2/syn(2^2)

  If known double errors (erasure mode) then if E1 at ord N1, E2 and ord N2:

     1/Det * [4^N2  2^N2] * [syn(2)  ]  => [E1]
             [4^N1  2^N1]   [syn(2^2)]     [E2]

      with

         Det = 2^N1 * 4^N2 + 2^N2 * 4^N1  in GF(256)

  Make corrections with (Cn + En) - code + error and position ord N
  
|#

(defun enc-6/8 (vec)
  (assert (eql 6 (length vec)))
  (with-gf2^8
    (concatenate 'vector
                 (vector
                  (gf-poly-dot vec #(8 48 224 231  59 241))  ;; chk0
                  (gf-poly-dot vec #(6 28 120 237 179 182))) ;; chk1
                 vec)))

(defun dec-6/8-syn (vec)
  (values
   (gf-poly-eval vec 2)   ;; syn2
   (gf-poly-eval vec 4))) ;; syn4

(defun dec-6/8 (vec &key erasure)
  (assert (eq 8 (length vec)))
  (let ((v (subseq vec 2))
        errpos)
    (with-gf2^8
      (multiple-value-bind (syn2 syn4) (dec-6/8-syn vec)
        (cond ((and (zerop syn2)
                    (zerop syn4))
               v)
              
              ((setf errpos (position (gf/ syn4 syn2) #(1 2 4 8 16 32 64 128)))
               (when erasure
                 (unless (find errpos erasure)
                   (error "New error in position ~A" errpos)))
               (case errpos
                 ((0) (values v :chk0-error))
                 ((1) (values v :chk1-error))
                 (otherwise
                  (let* ((vpos (- errpos 2))
                         (c    (aref v vpos))
                         (err  (gf/ (gf* syn2 syn2) syn4)))
                    (setf (aref v vpos) (gf+ c err))
                    (values v
                            :correctable-error
                            `(:pos ,vpos))))
                 ))

              (erasure
               ;; should be a list or vector of 2 indices
               (let* ((ixA   (elt erasure 0))
                      (ixB   (elt erasure 1))
                      (A2    (gf^ 2 ixA))
                      (B2    (gf^ 2 ixB))
                      (A4    (gf^ 4 ixA))
                      (B4    (gf^ 4 ixB))
                      (1/det (gf/ (gf+ (gf* A2 B4) (gf* B2 A4))))
                      (eA    (gf* 1/det (gf+ (gf* B4 syn2) (gf* B2 syn4))))
                      (eB    (gf* 1/det (gf+ (gf* A4 syn2) (gf* A2 syn4))))
                      (cA    (aref vec ixA))
                      (cB    (aref vec ixB))
                      (v     (copy-seq vec)))
                 (setf (aref v ixA) (gf+ cA eA)
                       (aref v ixB) (gf+ cB eB))
                 (values (subseq v 2)
                         :erasure-mode-dual-correction
                         `(:errs ,eA ,eB))))
              
              (t
               (values vec
                       :uncorrectable-error
                       `(:syn ,(vector syn2 syn4))))
              )))
    ))
  
#|
;; Encoding...
(enc-6/8 #(1 2 3 4 5 6))
=> #(37 243 1 2 3 4 5 6)

;; Decoding - no errors
(dec-6/8 #(37 243 1 2 3 4 5 6))
=> #(1 2 3 4 5 6)

;; Decoding - single error in drive 2
(dec-6/8 #(37 243 11 2 3 4 5 6))
=> #(1 2 3 4 5 6) 
:CORRECTABLE-ERROR 
(:POS 0) 

;; Decoding - another single error but in drive 3
(dec-6/8 #(37 243 1 21 3 4 5 6))
=> #(1 2 3 4 5 6) 
:CORRECTABLE-ERROR 
(:POS 1) 

;; Decoding - double errors, drives 2&3 - detected, but can't correct
(dec-6/8 #(37 243 11 21 3 4 5 6))
=> #(37 243 11 21 3 4 5 6) 
:UNCORRECTABLE-ERROR 
(:SYN #(144 9)) 

;; Decoding - error in chk1 position
(dec-6/8 #(36 243 1 2 3 4 5 6))
=> #(1 2 3 4 5 6) 
:CHK1-ERROR 

;; Decoding - error in chk2 position
(dec-6/8 #(37 241 1 2 3 4 5 6))
=> #(1 2 3 4 5 6) 
:CHK2-ERROR 

;; Erasure mode - no errors
(dec-6/8 #(37 243 1 2 3 4 5 6) :erasure '(2 3))
=> #(1 2 3 4 5 6) 

;; Erasure mode - single error in drive 2
(dec-6/8 #(37 243 11 2 3 4 5 6) :erasure '(2 3))
=> #(1 2 3 4 5 6) 
:CORRECTABLE-ERROR 
(:POS 0) 

;; Erasure mode - single error in drive 4, not in spec'd 2 or 3
(dec-6/8 #(37 243 1 2 33 4 5 6) :erasure '(2 3))
=> Error: New error in position 4 ;; thought you'd want to know...

;; Erasure mode - double error case in spec'd drives 2&3
(dec-6/8 #(37 243 11 22 3 4 5 6) :erasure '(2 3))
=> #(1 2 3 4 5 6) 
:ERASURE-MODE-DUAL-CORRECTION 
(:ERRS 10 20) 

;; Erasure mode - double error case, but in drives 2&4 not 2&3. We are blind to it.
(dec-6/8 #(37 243 11 2 33 4 5 6) :erasure '(2 3))
=> #(12 206 33 4 5 6) 
:ERASURE-MODE-DUAL-CORRECTION 
(:ERRS 7 204) 

export PROMPT='%2~ %# '
export zle_bracketed_paste=( )
|#

;; --------------------------------------------------------------------------
;; Look at using 6/8 encoding in GF(2^128) for use with AES encryption blocks

(defvar *chk0-enc* (make-array 6
                               :initial-element 0))
(defvar *chk1-enc* (make-array 6
                               :initial-element 0))

(with-gf2^128
  (let* ((prim  2)
         (rt1   prim)
         (rt2   (gf^ rt1 2))
         (gpoly (gf-poly-mul (vector rt1 1) (vector rt2 1))))
    (loop for ix from 0 below 6
          for msg = #(0 0 1) then (poly-prepend-zero msg)
          collect
          (multiple-value-bind (_ rem)
              (gf-poly-divmod msg gpoly)
            (declare (ignore _))
            (setf (aref *chk0-enc* ix) (aref rem 0)
                  (aref *chk1-enc* ix) (aref rem 1))
            ))))


(defun is-ubyte (x)
  (<= 0 x 255))

(defun vec-to-gf2^128 (vec)
  ;; treat vec as big-endian encoding of 128-bit integer
  (assert (and (vectorp vec)
               (eql 16 (length vec))
               (every #'is-ubyte vec)))
  (let ((nbr 0))
    (loop for ix from 0 below 16 do
          (setf (ldb (byte 8 (- 120 (ash ix 3))) nbr) (aref vec ix)))
    nbr))

(defun make-ub-vec (nel &rest args)
  (apply #'make-array nel
         :element-type '(unsigned-byte 8)
         args))

(defun gf2^128-to-vec (nbr)
  ;; encode nbr as 16-byte vector in big-endian order
  (assert (<= 0 nbr #.(1- (ash 1 128))))
  (let ((ans (make-ub-vec 16)))
    (loop for ix from 0 below 16 do
          (setf (aref ans ix) (ldb (byte 8 (- 120 (ash ix 3))) nbr)))
    ans))

(defun enc-6/8-128 (vec)
  ;; RS encode a block of 96 bytes
  (assert (and (vectorp vec)
               (eql 96 (length vec))
               (every #'is-ubyte vec)))
  (let ((vals (map 'vector #'vec-to-gf2^128 (um:group vec 16))))
    (map 'vector #'gf2^128-to-vec 
         (concatenate 'vector
                      (with-gf2^128
                        (vector
                         (gf-poly-dot vals *chk0-enc*)
                         (gf-poly-dot vals *chk1-enc*)))
                      vals))
    ))

(defun dec-6/8-128 (vec &key erasure)
  ;; RS decode a block of 8 vectors into 96 bytes
  (assert (and (vectorp vec)
               (eq 8 (length vec))
               (every (lambda (v)
                        (and (vectorp v)
                             (eq 16 (length v))
                             (every #'is-ubyte v)))
                      vec)))
  (labels ((cvt (v)
             (apply #'concatenate 'vector 
                    (map 'list #'gf2^128-to-vec v))))
    (let* ((vec (map 'vector #'vec-to-gf2^128 vec))
           (v   (subseq vec 2))
           errpos)
      (with-gf2^128
        (multiple-value-bind (syn2 syn4) (dec-6/8-syn vec)
          (cond ((and (zerop syn2)
                      (zerop syn4))
                 (cvt v))
              
                ((setf errpos (position (gf/ syn4 syn2) #(1 2 4 8 16 32 64 128)))
                 (when erasure
                   (with-simple-restart (continue "Noted... continue")
                     (unless (find errpos erasure)
                       (cerror "New error in position ~A" errpos))))
                 (case errpos
                   ((0) (values (cvt v) :chk0-error))
                   ((1) (values (cvt v) :chk1-error))
                   (otherwise
                    (let* ((vpos (- errpos 2))
                           (c    (aref v vpos))
                           (err  (gf/ (gf* syn2 syn2) syn4)))
                      (setf (aref v vpos) (gf+ c err))
                      (values (cvt v)
                              :correctable-error
                              `(:pos ,vpos))))
                   ))

                (erasure
                 ;; should be a list or vector of 2 indices
                 (let* ((ixA   (elt erasure 0))
                        (ixB   (elt erasure 1))
                        (A2    (gf^ 2 ixA))
                        (B2    (gf^ 2 ixB))
                        (A4    (gf^ 4 ixA))
                        (B4    (gf^ 4 ixB))
                        (1/det (gf/ (gf+ (gf* A2 B4) (gf* B2 A4))))
                        (eA    (gf* 1/det (gf+ (gf* B4 syn2) (gf* B2 syn4))))
                        (eB    (gf* 1/det (gf+ (gf* A4 syn2) (gf* A2 syn4))))
                        (cA    (aref vec ixA))
                        (cB    (aref vec ixB))
                        (v     (copy-seq vec)))
                   (setf (aref v ixA) (gf+ cA eA)
                         (aref v ixB) (gf+ cB eB))
                   (values (cvt (subseq v 2))
                           :erasure-mode-dual-correction
                           `(:errs ,eA ,eB))))
              
                (t
                 (error "Uncorrectable Error"))
                )))
      )))

(defvar *canary* (uuid:uuid-to-byte-array #/uuid/{314ab850-4c7e-11eb-8209-787b8acbe32e}))

(defun make-random-v (nel)
  (let ((ans (make-ub-vec nel)))
    (loop for ix from 0 below nel do
          (setf (aref ans ix) (random-between 0 256)))
    ans))

(defun ub-catl-vecs (vecs)
  (let* ((nels (mapcar #'length vecs))
         (nel  (reduce #'+ nels))
         (ans  (make-ub-vec nel)))
    (um:nlet iter ((vecs vecs)
                   (nels nels)
                   (pos  0))
      (if (endp vecs)
          ans
        (progn
          (replace ans (car vecs) :start1 pos)
          (go-iter (cdr vecs) (cdr nels) (+ pos (car nels))))
        ))))
         
(defun ub-cat-vecs (&rest vecs)
  (ub-catl-vecs vecs))

(defun ovly (arr nel &optional (off 0))
  (make-array nel
              :element-type (array-element-type arr)
              :displaced-to arr
              :displaced-index-offset off))

;; --------------------------------------------
;; Encoding

(defun rs-encode-grps (bytes)
  (assert (and (vectorp bytes)
               (zerop (rem (length bytes) 96))
               (every #'is-ubyte bytes)))
  (loop for start from 0 below (length bytes) by 96
        for end from 96 by 96
        collect
        (enc-6/8-128 (subseq bytes start end))
        ))

(defun rs-encode-bytes (bytes)
  (let* ((ans     (make-array 8))
         (enc     (rs-encode-grps bytes)))
    (loop for ix from 0 below 8 do
          (setf (aref ans ix)
                (ub-catl-vecs (map 'list (um:rcurry #'aref ix) enc))))
    ans))
  
;; --------------------------------------------

(defun aont-encode (obj)
  (let* ((bytes  (loenc:encode obj))
         (nel    (length bytes))
         (key    (make-random-v 16)) ;; AES/128
         (nonce  (make-random-v 16))
         (cipher (ironclad:make-cipher :aes
                                       :key  key
                                       :mode :ctr
                                       :initialization-vector nonce))
         (digest (ironclad:make-digest :sha3/256)))
    (multiple-value-bind (ngrps nrem)
        ;; 16 for nonce
        ;; 16 for canary-buffer
        ;; 16 for embedded key
        (ceiling (+ nel 16 16 16) 96)
      (declare (ignore ngrps))
      (let* ((bytes     (ub-cat-vecs nonce
                                     bytes
                                     (make-ub-vec (- 16 nrem))
                                     key))
             (nel       (length bytes))
             (keypos    (- nel 16))
             (canarypos (- keypos 16)))
        (ironclad:update-digest digest bytes :start 16 :end canarypos)
        (let ((hash (ironclad:produce-digest digest)))
          (replace bytes hash :start1 canarypos :end1 keypos)
          (reinitialize-instance digest))
        (ironclad:encrypt-in-place cipher bytes :start 16 :end keypos)
        (ironclad:update-digest digest bytes :start 16 :end keypos)
        (let ((hash (ironclad:produce-digest digest))
              (hkey (ovly bytes 16 keypos)))
          (map-into hkey #'logxor hash key))
        bytes
        ))))

(defun aont-rs-encode (obj)
  (rs-encode-bytes (aont-encode obj)))

;; --------------------------------------------

(defun simple-rs-prep (obj)
  (let* ((bytes  (loenc:encode obj))
         (nel    (length bytes)))
    (multiple-value-bind (ngrps nrem)
        ;; 16 for nonce
        ;; 16 for canary-buffer
        ;; 16 for embedded key
        (ceiling nel 96)
      (declare (ignore ngrps))
      (ub-cat-vecs bytes
                   (make-ub-vec (- nrem))
                   ))))

(defun simple-rs-encode (obj)
  (rs-encode-bytes (simple-rs-prep obj)))

;; --------------------------------------------
;; Decoding

(defun rs-decode-grps (vecs)
  (assert (and (consp vecs)
               (every (lambda (v)
                        (and (vectorp v)
                             (eql 8 (length v))
                             (every (lambda (subv)
                                      (and (vectorp subv)
                                           (eql 16 (length subv))
                                           (every #'is-ubyte subv)))
                                    v)))
                      vecs)))
  (let* ((ngrps  (length vecs))
         (nel    (* ngrps 96))
         (bytes  (make-ub-vec nel)))
    (loop for grp in vecs
          for pos from 0 by 96
          do
          (replace bytes (dec-6/8-128 grp) :start1 pos))
    bytes))

(defun rs-decode-bytes (vecs)
  (assert (and (vectorp vecs)
               (eql 8 (length vecs))
               (every (lambda (v)
                        (and (vectorp v)
                             (zerop (rem (length v) 16))
                             (every #'is-ubyte v)))
                      vecs)))
  (let* ((nel  (length (aref vecs 0)))
         (grps (loop for pos from 0 by 16 below nel collect
                     (apply #'vector
                            (map 'list (lambda (v)
                                         (subseq v pos (+ pos 16)))
                                 vecs)))))
    (rs-decode-grps grps)
    ))

;; --------------------------------------------

(defun aont-decode (bytes)
  (assert (and (vectorp bytes)
               (every #'is-ubyte bytes)
               (zerop (rem (length bytes) 16))
               (>= (length bytes) 48)))
  (let* ((nel       (length bytes))
         (keypos    (- nel 16))
         (canarypos (- keypos 16))
         (nonce     (ovly bytes 16))
         (key       (subseq bytes keypos))
         (digest    (ironclad:make-digest :sha3/256)))
    (ironclad:update-digest digest bytes :start 16 :end keypos)
    (let ((hash (ironclad:produce-digest digest)))
      (map-into key #'logxor hash key)
      (reinitialize-instance digest))
    (let ((cipher (ironclad:make-cipher :aes
                                        :key  key
                                        :mode :ctr
                                        :initialization-vector nonce)))
      (ironclad:decrypt-in-place cipher bytes :start 16 :end keypos)
      (ironclad:update-digest digest bytes :start 16 :end canarypos)
      (let* ((hash   (ironclad:produce-digest digest))
             (hashv  (ovly hash 16))
             (canary (ovly bytes 16 canarypos)))
        (unless (equalp hashv canary)
          (error "Can't decode AONT")
          ))
      (loenc:decode bytes :start 16))
    ))

(defun aont-rs-decode (vecs)
  (aont-decode (rs-decode-bytes vecs)))

;; --------------------------------------------

(defun simple-rs-decode (vecs)
  (loenc:decode (rs-decode-bytes vecs)))

;; --------------------------------------------

#|
(let* ((vec (coerce
             (loop repeat 96 collect (random-between 0 256))
             'vector))
       (enc (enc-6/8-128 vec))
       (dec (dec-6/8-128 enc)))
  (assert (equalp dec vec))
  `(:original ,vec
    :enc      ,enc))

(let* ((vec (coerce
             (loop repeat 96 collect (random-between 0 256))
             'vector))
       (enc (aont-encode vec))
       (dec (aont-decode enc)))
  (assert (equalp dec vec))
  `(:original ,vec
    :enc      ,enc))

(let* ((vec (coerce
             (loop repeat 96 collect (random-between 0 256))
             'vector))
       (enc (simple-rs-encode vec))
       (dec (simple-rs-decode enc)))
  (assert (equalp dec vec))
  `(:original ,vec
    :enc      ,enc))
    
|#


