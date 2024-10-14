
(defpackage #:com.ral.base58-chk
  (:use #:common-lisp
   #:vec-repr)
  (:export
   #:base58-chk
   ))

(in-package #:com.ral.base58-chk)

;; --------------------------------------------------------------
;; Base58-chk -- value as Base58-chk string
;;

(defclass base58-chk (base58)
  ())

(defun sha256d (vec)
  (hash:hash/sha2/256
   (hash:hash/sha2/256 vec)))

(defun base58-check-bytes (vec)
  (subseq (hash:hash-bytes (sha256d vec)) 0 4))

;; ---

(defmethod base58-chk ((x base58-chk))
  x)

(defmethod base58-chk ((x string))
  (check-repr (make-instance 'base58-chk
                             :str (sbs x))))

(defmethod base58-chk ((vec sequence))
  ;; version prefix (if any) should have been applied prior to this encoding
  (let* ((ubvec    (ub8v vec))
         (chkvec   (concatenate 'vector ubvec
                                (base58-check-bytes ubvec)))
         (base-enc (str (base58 (int chkvec))))
         (nzrs     (position-if (complement #'zerop) ubvec))
         (pref     (make-string nzrs :initial-element #\1))
         (enc      (concatenate 'simple-base-string pref base-enc)))
    (make-instance 'base58-chk
                   :str enc)))

(defmethod base58-chk (x)
  (base58-chk (bev-vec x)))

(defmethod bev ((x base58-chk))
  (let* ((vec (bev-vec (call-next-method)))
         (end (- (length vec) 4))
         (pre (subseq vec 0 end))
         (chk (subseq vec end))
         (cal (base58-check-bytes pre)))
    (assert (equalp chk cal))
    (make-instance 'bev
                   :vec pre)))

