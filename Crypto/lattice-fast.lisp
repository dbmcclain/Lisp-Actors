;; lattice-fast.lisp -- Multi-bit LWE Encryption
;; DM/RAL 01/24
;; ----------------------------------------------

(in-package :com.ral.crypto.lattice-crypto)

(defparameter *flat-nbits*    26)
(defparameter *flat-ncode*     8)
(defparameter *flat-nrows*   160)
(defparameter *flat-ncols*   128)

;; ---------------------------------------------

(defun flat-mod (x sys)
  (let ((nbits (getf sys :nbits)))
    (ldb (byte nbits 0) x)))

(defun fvdot (v1 v2)
  (reduce #'+ (map 'vector #'* v1 v2)))

;; ---------------------------------------------

(defun fcheck-system (sys)
  (let ((ncols   (lat2-ncols sys))
        (nrows   (lat2-nrows sys))
        (nbits   (getf sys :nbits)))
    (when (> (+ nbits nbits (integer-length ncols)) 60)
      (error "NBits is too large: ~A" nbits))
    (when (< ncols 128)
      (error "NCols should be >= 128: ~A" ncols))
    (when (< nrows 160)
      (error "NRows should be >= 160: ~A" nrows))
    #|
    (unless (> nrows ncols)
      (error "NRows should be > NCols: ~A x ~A" nrows ncols))
    |#
    ))

#|
(let* ((base (1- (ash 1 *flat-nbits*)))
       (coll (loop repeat 10000 collect
                     (let ((v1 (map 'vector #'round (vm:unoise *flat-ncols* base)))
                           (v2 (map 'vector #'round (vm:unoise *flat-ncols* base))))
                       (integer-length (fvdot v1 v2))))))
  (plt:histogram 'histogram coll
                 :clear t))
                     
 |#

(defun fgen-sys (&key (nbits *flat-nbits*)
                      (ncode *flat-ncode*)
                      (nrows *flat-nrows*)
                      (ncols *flat-ncols*))
  (let ((a    (make-array nrows))
        (base (1- (ash 1 nbits))))
    (loop for ix from 0 below nrows do
            (setf (aref a ix) (map 'vector #'round (vm:unoise ncols base))))
    (let ((sys (list :nbits nbits
                     :ncode ncode
                     :nrows nrows
                     :ncols ncols
                     :mat-a a)))
      (fcheck-system sys)
      sys)))

;; ---------------------------------------------

(defun fgen-skey (sys)
  (let* ((nbits (getf sys :nbits))
         (base  (1- (ash 1 nbits)))
         (ncols (getf sys :ncols)))
    (map 'vector #'round (vm:unoise ncols base))))

(defun flat-gen-deterministic-skey (sys &rest seeds)
  ;; skey is a ncol vector of 26-bit values
  (fcheck-system sys)
  (let* ((ncols           (lat2-ncols sys))        
         (nbits-per-word  (getf sys :nbits))
         (nbits-total     (* nbits-per-word ncols))
         (hstretch        nil))
    (dotimes (ix 1000)
      (setf hstretch (apply #'hash/256 hstretch ix :deterministic-skey seeds)))
    (let* ((h   (apply #'get-hash-nbits nbits-total hstretch :deterministic-skey seeds))
           (hbv (to-bitvec (vec h))))
      (coerce
       (loop for pos from 0 below nbits-total by nbits-per-word collect
               (bitvec-to hbv pos nbits-per-word))
       'vector))
    ))

(defun noise-nbits (nbits-for-unit nrows nsigma)
  (- nbits-for-unit 1
     (- (log nsigma 2)
        (/ (log 12 2) 2))
     (/ (log nrows 2) 2)))

(defun fgen-pkey (skey sys)
  (let* ((nrows   (getf sys :nrows))
         (nbits   (getf sys :nbits))
         (ncode   (getf sys :ncode))
         (mat-a   (getf sys :mat-a))
         ;;
         ;; -- Bipolar Noise Values --
         ;;
         ;; For a uniform distribution of width N, the variance is
         ;; N^2/12.  If we sum NRows of these together, the variance
         ;; becomes NRows*N^2/12.  We need to allow for some multiple of
         ;; the standard deviation and that multiple must remain below
         ;; half our unit scale.
         ;;
         ;; So, if unity is represented as 2^NUnit, then we need:
         ;;
         ;;  Log2(NSigma) + 1/2*Log2(NRows) + Log2(N) - 1/2*Log2(12) < NUnit-1
         ;;
         ;; Solving for N:
         ;;
         ;;  Log2(N) < NUnit - Log2(6) + 1/2*Log2(12) - 1/2*Log2(NRows)
         ;;
         ;; Plugging in NUnit = 18, NRows = 160, NSigma = 6, we get:
         ;;
         ;;    Log2(N) < 12.54, so use 12
         ;;
         ;; This needs to be greater than 1/2*log2(NRows) = 3.66, for security.
         ;;
         (nsmall  (floor (noise-nbits (- nbits ncode) nrows 6)))
         (small   (ash 1 nsmall))
         (chk     (assert (> small (sqrt nrows))))
         (small/2 (ash small -1))
         (noise (map 'vector (lambda (x)
                               (round (- x small/2)))
                     (vm:unoise nrows small))))
    (declare (ignore chk))
    (map 'vector (lambda (arow err)
                   (flat-mod (+ (fvdot arow skey)
                                err)
                             sys))
         mat-a noise)))

;; ------------------------------------------------------------------

(defun flat-encode1 (x pkey sys)
  (let* ((nrows (getf sys :nrows))
         (ncols (getf sys :ncols))
         (mat   (getf sys :mat-a))
         (nbits (getf sys :nbits))
         (ncode (getf sys :ncode))
         (nsh   (- nbits ncode))
         (sel   (gen-random-sel nrows))
         (bsum  0)
         (vsum  (make-array ncols
                            :element-type 'fixnum
                            :initial-element 0)))
    (loop for vrow across mat
          for b across pkey
          for ix from 0
          do
            (when (logbitp ix sel)
              (incf bsum b)
              (map-into vsum #'+ vsum vrow)))
    (vector (flat-mod (+ bsum
                         (ash x nsh))
                      sys)
            (map-into vsum (um:rcurry #'flat-mod sys) vsum))
    ))

(defun flat-encode (pkey v &optional (sys (get-lattice-system)))
  (let* ((v   (ub8v v))
         (nb  (length v))
         (ans (make-array nb)))
    (declare (fixnum nb))
    (loop for ix fixnum from 0 below nb
          do
            (setf (aref ans ix)
                  (flat-encode1 (aref v ix) pkey sys)))
    ans))

(defun flat-enc (pkey &rest objs)
  (flat-encode pkey (loenc:encode (loenc:unshared-list objs
                                                       :max-portability t))))

;; ------------------------------------------------------------------

(defun flat-decode1 (v skey sys)
  (let* ((bsum  (aref v 0))
         (vsum  (aref v 1))
         (nbits (getf sys :nbits))
         (ncode (getf sys :ncode))
         (pos   (- nbits ncode))
         (half  (ash 1 (1- pos))))
    (ldb (byte ncode pos)
         (+ (- bsum
               (fvdot skey vsum))
            half))
    ))

(defun flat-decode (skey cs &optional (sys (get-lattice-system)))
  ;; decode a list of cyphertext vectors into an octet vector
  #F
  (let* ((nel  (length cs))
         (bv   (make-array nel
                           :element-type '(unsigned-byte 8))))
    (loop for ix fixnum from 0 below nel
          do
            (setf (aref bv ix) (flat-decode1 (aref cs ix) skey sys)))
    bv))

(defun flat-dec (skey cs)
  (values-list (loenc:decode (flat-decode skey cs))))

;; -----------------------------------------------------------------

#|
(defparameter *flat-sys* (fgen-sys))
(defparameter *tst-skey* (fgen-skey *flat-sys*))
(defparameter *tst-pkey* (fgen-pkey *tst-skey* *flat-sys*))

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
;; Histogram of Raw Decryptions
;; Should look like a Gaussian distribution above the value of the x data value
(let* ((x     255)
       (nbits (getf *flat-sys* :nbits))
       (ncode (getf *flat-sys* :ncode))
       (pos   (- nbits ncode))
       (half  (ash 1 (1- pos)))
       (one   (ash 1 pos))
       (coll  (loop repeat 10000 collect
                      (let ((v (flat-encode1 x *tst-pkey* *flat-sys*)))
                        (/ (flat-mod (+ (- (aref v 0)
                                           (fvdot *tst-skey* (aref v 1)))
                                        half)
                                     *flat-sys*)
                           one))
                      )))
  (plt:histogram 'histo coll
                 :clear t
                 :norm  nil
                 ;; :yrange '(0 100)
                 ))

;; -------------------------------------------
;; Histogram of Scaler component of Encryption
;; Should look like a uniform distribution

(let* ((x     2)
       (nbits (getf *flat-sys* :nbits))
       (ncode (getf *flat-sys* :ncode))
       (pos   (- nbits ncode))
       (one   (ash 1 pos))
       (coll  (loop repeat 10000 collect
                      (let ((v (flat-encode1 x *tst-pkey* *flat-sys*)))
                        (/ (aref v 0) one)
                        ))))
       (plt:histogram 'histo coll
                      :clear t
                      :norm  nil
                      ;; :yrange '(0 100)
                      ))

|#
    
