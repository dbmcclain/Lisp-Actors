;; lattice-1d.lisp
;;
;; DM/RAL  2024/01/18 12:29:31 UTC
;; ----------------------------------

(in-package :com.ral.crypto.lattice-crypto)

(defparameter *tst-sys*  (fgen-sys :nbits 320
                                   :ncode 256
                                   :nrows 160
                                   :ncols 1))

(defparameter *tst-sys* (fgen-sys))

(defparameter *tst-skey* (flat-gen-deterministic-skey *tst-sys* :test-key))
(defparameter *tst-pkey* (fgen-pkey *tst-skey* *tst-sys*))

(defparameter *tst-msg*  (vec (hash/256 (uuid:make-v1-uuid))))

(let ((enc (flat-encode *tst-pkey* *tst-msg* *tst-sys*)))
  (flat-decode *tst-skey* enc *tst-sys*))
*tst-msg*
(noise-nbits (- 320 256) 160 6)