;; lattice-v2.lisp -- constant shared matrix, shortened PKeys
;; DM/RAL  2023/04/13 08:29:45

(in-package :lattice)

;; ----------------------------------------------------
;; Rev-2 Uses shared constant A matrix.

(defun check-system (sys)
  (let ((ncols   (lat2-ncols sys))
        (nrows   (lat2-nrows sys))
        (modulus (lat2-modulus sys)))
    (when (> modulus (ash 1 30))
      (error "Modulus is too large: ~A" modulus))
    (when (< ncols 128)
      (error "NCols should be >= 128: ~A" ncols))
    (when (< nrows 160)
      (error "NRows should be >= 160: ~A" nrows))
    (unless (> nrows ncols)
      (error "NRows should be > NCols: ~A x ~A" nrows ncols))
    ))

(defun lat2-gen-system (&key (nrows *lattice-nrows*)
                             (ncols *lattice-ncols*)
                             (modulus *lattice-m*))
  (let ((sys (with-mod modulus
               (let ((mat  (gen-random-gaussian-matrix nrows ncols)))
                 (list
                  :modulus modulus
                  :nrows   nrows
                  :ncols   ncols
                  :mat-a   mat)))))
    (check-system sys)
    sys))

#|
(send kvdb:kvdb println :add :lat2-system (lat2-gen-system))

(send kvdb:kvdb println :add :lat2g-system (lat2-gen-system))
 |#

;; ------------------------------------------------

(deflex lattice-system
  (create
   (alambda
    ((cust)
     (let+ ((me  self)
            (:β  (sys) (racurry kvdb:kvdb :find :lat2g-system)))
       (check-system sys)
       (send me cust :update sys)
       ))
    ((cust :update sys)
     (become (const-beh sys))
     (send cust sys))
    )))

(defun get-lattice-system ()
  (ask lattice-system))

(defun lat2-modulus (&optional (sys (get-lattice-system)))
  (getf sys :modulus))

(defun lat2-nrows (&optional (sys (get-lattice-system)))
  (getf sys :nrows))

(defun lat2-ncols (&optional (sys (get-lattice-system)))
  (getf sys :ncols))

(defun lat2-matrix (&optional (sys (get-lattice-system)))
  (getf sys :mat-a))

(defun lat2-gen-skey (&optional (sys (get-lattice-system)))
  ;; skey is a ncol vector of 30-bit bipolar field values
  (check-system sys)
  (let ((ncols   (lat2-ncols sys))        
        (modulus (lat2-modulus sys)))
    (with-mod modulus
      (gen-random-vec ncols))
    ))

(defun lat2-gen-deterministic-skey (sys &rest seeds)
  ;; skey is a ncol vector of 30-bit bipolar field values
  (check-system sys)
  (let* ((sys     (or sys (get-lattice-system)))
         (ncols   (lat2-ncols sys))        
         (modulus (lat2-modulus sys))
         (nbits-per-word  (integer-length modulus))
         (nbits-total     (* nbits-per-word ncols))
         (hstretch        nil))
    (dotimes (ix 1000)
      (setf hstretch (apply #'hash/256 hstretch ix :deterministic-skey seeds)))
    (let* ((h   (apply #'get-hash-nbits nbits-total hstretch :deterministic-skey seeds))
           (hbv (to-bitvec (vec h))))
      (with-mod modulus
        (coerce
         (loop for pos from 0 below nbits-total by nbits-per-word collect
                 (lmod (bitvec-to hbv pos nbits-per-word)))
         'vector))
      )))

(defun lat2-gen-pkey (skey &optional (sys (get-lattice-system)))
  ;; pkey is a nrow vector of 30-bit bipolar field values
  (check-system sys)
  (with-mod (lat2-modulus sys)
    (let* ((amat   (lat2-matrix sys))
           (nrows  (lat2-nrows sys))
           (noise  (gen-noise-vec nrows)))
      (vec+ (mat*v amat skey) noise))))

(defun lat2-gen-keys (&optional (sys (get-lattice-system)))
  ;; skey is a ncol vector of 30-bit bipolar field values
  ;; pkey is a nrow vector of 30-bit bipolar field values
  (let* ((skey (lat2-gen-skey sys))
         (pkey (lat2-gen-pkey skey sys)))
    (values skey pkey)))

;; --------------------------------------------------------------

(defun lat2-enc-mat*v (pkey m sel sys)
  ;; pkey is the nrow b column vector.
  ;; m is the system matrix,
  ;;    stored as an nrow vector of rows, each with ncol elements.
  ;; sel is a random nrow-bit integer in the range [1,2^nrows).
  ;;
  ;; We simply add the matrix rows corresponding to non-zero bits in
  ;; the selector integer.
  #F
  (let ((vsum (make-array (lat2-ncols sys)
                          :element-type 'fixnum
                          :initial-element 0))
        (bsum 0))
    (declare ((simple-array fixnum 1) vsum)
             (fixnum bsum))
    (loop for vrow across m
          for b fixnum across pkey
          for ix fixnum from 0
          do
            (when (logbitp ix sel)
              (incf bsum b)
              (map-into vsum #'+ vsum vrow)))
    (values (lmod bsum)
            (map-into vsum #'lmod vsum))))

;; ----------------------------------------------------
;; LWE Lattice Encoding

(defun lat2-encode1 (pkey bit sys)
  ;; bit 0, 1
  ;; Produces a 2 element vector, with a scalar followed by an ncol element vector.
  ;; All elements modulo a 30-bit prime, expressed as a bipolar field value.
  #F
  (with-mod (lat2-modulus sys)
    (let* ((nrows (lat2-nrows sys))
           (mat-a (lat2-matrix sys))
           (r     (gen-random-sel nrows)))
      (multiple-value-bind (bsum vsum)
          (lat2-enc-mat*v pkey mat-a r sys)
        (vector (lm+ bsum
                     (* bit (ash (mod-base) -1)))
                vsum)
        ))))

(defun lat2-encode (pkey v &optional (sys (get-lattice-system)))
  ;; v should be a vector of octets
  ;; Encodes octet vector into a list of cyphertext vectors
  #F
  (let* ((v     (ub8v v)) ;; ensure simple vector of octets
         (nb    (length v))
         (nbits (* 8 nb))
         (ans   (make-array nbits)))
    (declare (fixnum nb nbits))
    (loop for bix fixnum from 0 below nbits
          do
            (setf (aref ans bix)
                  (lat2-encode1 pkey (bref v bix) sys)))
    ans))

(defun lat2-enc (pkey &rest objs)
  ;; general object encryption
  (lat2-encode pkey (loenc:encode (loenc:unshared-list objs)
                                  :max-portability t)))

;; ---------------------------------------------------------------
;; LWE Lattice Decoding

(defun lat2-decode1 (skey c)
  ;; c is a cryptotext vector
  #F
  (let ((cdots (- (aref c 0)
                  (vdot (aref c 1) skey))))
    (declare (fixnum cdots))
    (mod (round cdots (ash (mod-base) -1)) 2)
    ))

(defun lat2-decode (skey cs &optional (sys (get-lattice-system)))
  ;; decode a list of cyphertext vectors into an octet vector
  #F
  (let* ((nel  (length cs))
         (bv   (make-array nel
                           :element-type 'bit)))
    (with-mod (lat2-modulus sys)
      (loop for ix fixnum from 0 below nel
            do
              (setf (sbit bv ix) (lat2-decode1 skey (aref cs ix))))
      (bitvec-to-octets bv)
      )))

(defun lat2-dec (skey cs)
  ;; general object decryption
  (values-list (loenc:decode (lat2-decode skey cs))))

;; ----------------------------------------------------------------

#|
(let* ((mat  (lat2-matrix))
      (m     (lat2-modulus))
      (nrows (lat2-nrows))
      (ncols (lat2-ncols))
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
  
|#
