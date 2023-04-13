;; lattice-v2.lisp -- constant shared matrix, shortened PKeys
;; DM/RAL  2023/04/13 08:29:45

(in-package :lattice)

;; ----------------------------------------------------
;; Rev-2 Uses shared constant A matrix.

(defun lat2-gen-system (&key (nrows *lattice-nrows*)
                             (ncols *lattice-ncols*)
                             (modulus *lattice-m*))
  (when (> modulus (ash 1 30))
    (error "Modulus is too large: ~A" modulus))
  (when (< ncols 256)
    (error "NCols should be > 256: ~A" ncols))
  (unless (> nrows ncols)
    (error "NRows should be > ~A: ~A" ncols nrows))
  (with-mod modulus
    (let ((mat  (gen-random-matrix nrows ncols)))
      (list
       :modulus modulus
       :nrows   nrows
       :ncols   ncols
       :mat-a   mat))))

#|
(send kvdb:kvdb println :add :lattice-system (lat2-gen-system))
 |#

(deflex lattice-system
  (create
   (lambda (cust)
     (send kvdb:kvdb cust :find :lat2-system))))

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
  (subseq
   (second (lat-gen-skey :ncols   (lat2-ncols sys)
                         :modulus (lat2-modulus sys)))
   1))

(defun lat2-gen-pkey (skey &optional (sys (get-lattice-system)))
  (with-mod (lat2-modulus sys)
    (let* ((amat   (lat2-matrix sys))
           (nrows  (lat2-ncols sys))
           (noise  (gen-noise-vec nrows)))
      (vec+ (mat*v amat skey) noise))))

(defun lat2-gen-keys (&optional (sys (get-lattice-system)))
  (let* ((skey (lat2-gen-skey sys))
         (pkey (lat2-gen-pkey skey sys)))
    (values skey pkey)))

;; --------------------------------------------------------------

(defun lat2-enc-mat*v (pkey m sel sys)
  ;; m should be the Pkey matrix [b | -A] with column vector b
  ;; prepended to -A matrix, stored row-wise. Selector is a random
  ;; integer in the range [1,2^nrows). We simply add the matrix rows
  ;; corresponding to non-zero bits in the selector integer.
  #F
  (declare (vector fixnum pkey)
           (integer sel))
  (let ((vans (make-array (lat2-ncols sys)
                          :initial-element 0))
        (bsum 0))
    (declare (vector fixnum vans)
             (fixnum bsum))
    (loop for vrow across m
          for b fixnum across pkey
          for ix fixnum from 0
          do
            (when (logbitp ix sel)
              (incf bsum b)
              (map-into vans #'+ vans vrow)))
    (values (lmod bsum)
            (map-into vans #'lmod vans))))

;; ----------------------------------------------------
;; LWE Lattice Encoding

(defun lat2-encode1 (pkey bit sys)
  ;; bit 0, 1
  #F
  (declare (vector fixnum pkey)
           (fixnum bit))
  (with-mod (lat2-modulus sys)
    (let* ((nrows (length pkey))
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
  (let* ((nb    (length v))
         (nbits (* 8 nb))
         (ans   (make-array nbits)))
    (declare (vector (unsigned-byte 8) ans)
             (fixnum nb nbits))
    (loop for bix fixnum from 0 below nbits
          do
            (setf (aref ans bix)
                  (lat2-encode1 pkey (bref v bix) sys)))
    ans))

(defun lat2-enc (pkey &rest objs)
  ;; general object encryption
  (lat2-encode pkey (loenc:encode (coerce objs 'vector))))

;; ---------------------------------------------------------------
;; LWE Lattice Decoding

(defun lat2-decode1 (skey c)
  ;; c is a cryptotext vector
  (let ((cdots (- (aref c 0)
                  (vdot (aref c 1) skey))))
    (declare (fixnum cdots))
    (mod (round cdots (ash (mod-base) -1)) 2)
    ))

(defun lat2-decode (skey cs &optional (sys (get-lattice-system)))
  ;; decode a list of cyphertext vectors into an octet vector
  (declare (vector fixnum skey))
  (let* ((nel  (length cs))
         (bv   (make-array nel
                           :element-type 'bit)))
    (declare (fixnum nel)
             (vector bit bv))
    (with-mod (lat2-modulus sys)
      (loop for ix fixnum from 0 below nel
            do
              (setf (sbit bv ix) (lat2-decode1 skey (aref cs ix))))
      (bitvec-to-octets bv)
      )))

(defun lat2-dec (skey cs)
  ;; general object decryption
  (values-list (coerce (loenc:decode (lat2-decode skey cs)) 'list)))

#|
(let* ((msg :hello)
       (enc (lat2-enc *my-lat2-pkey* msg))
       (dec (lat2-dec *my-lat2-skey* enc)))
  (assert (eql dec msg))
  dec)

(inspect (lat2-enc *my-lat2-pkey* :hello))
(inspect (lat2-decode *my-lat2-skey* (lat2-enc *my-lat2-pkey* :hello)))
|#
