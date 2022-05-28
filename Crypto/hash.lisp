;; hash.lisp -- Standardized hashing for crypto needs
;;
;; DM/Emotiq 03/18
;; ---------------------------------------------------------
#|
Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :hash)

;; -------------------------------------------------

(defclass hash (bev)
  ((val :reader hash-val
        :reader hash-bytes
        :initarg :vec)))

(defmethod make-load-form ((ahash hash) &optional env)
  (make-load-form-saving-slots ahash :environment env))


(defclass hash/ripemd/160 (hash)
  ())

(defclass hash/sha2/256 (hash)
  ())

(defclass hash/256 (hash)
  ())

(defclass hash/384 (hash)
  ())

(defclass hash/512 (hash)
  ())

(defclass hash/var (hash)
  ((hashfn  :reader  hash-fn
            :initarg :hashfn)))

;; -------------------------------------------------

(defmethod hash-length ((x hash))
  (length (hash-bytes x)))

;; -------------------------------------------------
;; what to hash of various types

#|
(defmethod hashable (x)
  (loenc:encode x))
|#
#||#
(defgeneric hashable (x)
  (:method ((x ub8v-obj))
   (bev-vec x))
  (:method ((x integer))
   (hashable (bev x)))
  (:method ((x cons))
   (loenc:encode (mapcar #'hashable x)))
  (:method ((x sequence))
   (or (ignore-errors
         (coerce x 'ub8-vector))
       (call-next-method)))
  (:method ((x string))
   (hashable (map 'vector 'char-code x)))
  (:method ((x symbol))
   (hashable (prin1-to-string x)))
  (:method ((x pathname))
   (hashable (namestring x)))
  (:method (x)
   (loenc:encode x)))
#||#

;; -------------------------------------------------

(defun local-digest (dig-kind dig-class &rest args)
  (let ((dig (ironclad:make-digest dig-kind)))
    (dolist (arg args)
      (ironclad:update-digest dig (hashable arg)))
    (let ((hv  (ironclad:produce-digest dig)))
      (values (make-instance dig-class
                             :vec hv)
              (length hv)))))

(defun hash/ripemd/160 (&rest args)
  (apply 'local-digest :ripemd-160 'hash/ripemd/160 args))

(defun hash/sha2/256 (&rest args)
  (apply 'local-digest :sha256 'hash/sha2/256 args))

(defun hash/256 (&rest args)
  (apply 'local-digest :sha3/256 'hash/256 args))

(defun hash/384 (&rest args)
  (apply 'local-digest :sha3/384 'hash/384 args))

(defun hash/512 (&rest args)
  (apply 'local-digest :sha3 'hash/512 args))

;; -----------------------------------------------------

#|
  ;; Ironclad SHAKE is broken (DM/RAL 11/21) and refuses to reliably
  ;; return more than 200 bytes. So for now we will use iterated
  ;; SHA3/256.
  ;;
  ;; DM/RAL 05/22 -- now appears to be fixed and working...
  |#
(defun get-raw-hash-nbytes (nb &rest seeds)
  ;; returns a vector of nb raw-bytes
  (let ((dig (ironclad:make-digest :shake256 :output-length nb)))
    (dolist (seed seeds)
      (ironclad:update-digest dig (hashable seed)))
    (ironclad:produce-digest dig)))

#|
(defun get-raw-hash-nbytes (nb &rest seeds)
  ;; returns a vector of nb raw-bytes
  (let ((ans (make-array nb
                         :element-type '(unsigned-byte 8))))
    (do ((offs  0  (+ offs 32)))
        ((>= offs nb) ans)
      (let ((dig (ironclad:make-digest :sha3/256)))
        (dolist (seed (cons offs seeds))
          (ironclad:update-digest dig (hashable seed)))
        (let ((bytes  (ironclad:produce-digest dig)))
          (replace ans bytes :start1 offs)
          )))
    ))
|#

(defun make-bare-hash (vec fn)
  (values
   (make-instance 'hash/var
                  :vec    vec
                  :hashfn fn)
   (length vec)))

(defun get-hash-nbytes (nb &rest seeds)
  (declare (fixnum nb))
  (check-type nb (integer 0))
  (apply (get-cached-symbol-data
          'hash/var 'get-hash-nbytes nb
          (lambda ()
            (labels ((hash-fn (&rest args)
                       (make-bare-hash
                        (apply 'get-raw-hash-nbytes nb args)
                        #'hash-fn)))
              #'hash-fn)))
         seeds))

(defun hash-to-range (range &rest seeds)
  ;; This function is useful for hashing onto an Elliptic curve
  ;; by considering the hash value to be an X coord.
  ;; No need here to restrict range to (Sqrt[base], base - Sart[base])
  ;; as we do for safe-random values, because of difficulty of ECDLP.
  ;; &rest seeds -> hash, int(hash)
  (declare (integer range))
  (check-type range (integer 2)) ;; produce N s.t. N in [0:N)
  (apply (get-cached-symbol-data
          'hash/var 'hash-to-range range
          (lambda ()
            (let* ((maxbits (integer-length (1- range)))
                   (nbytes  (ceiling maxbits 8))
                   (nhibits (- maxbits (* 8 (1- nbytes)))))
              (labels ((hash-fn (&rest args)
                         (let ((vec (apply 'get-raw-hash-nbytes nbytes args)))
                           ;; vec is viewwed as big-endian encoding of an integer
                           (when (< nhibits 8)
                             (setf (aref vec 0) (ldb (byte nhibits 0) (aref vec 0))))
                           (let ((hval (int vec)))
                             (if (< hval range)
                                 (values (make-bare-hash vec #'hash-fn)
                                         hval) ;; for efficiency when int will be called anyway
                               (hash-fn vec)))
                           )))
                #'hash-fn))))
         seeds))

(defun get-hash-nbits (nbits &rest seeds)
  (declare (fixnum nbits))
  (check-type nbits (integer 1))
  (apply 'hash-to-range (ash 1 nbits) seeds))

;; -----------------------------------------------------

(defmethod hash-check (item (expected string))
  (string-equal expected (hex-str (hash/256 item))))

(defmethod hash= ((hash1 hash) (hash2 hash))
  (vec= hash1 hash2))

(defmethod ord:compare ((hash1 hash) (hash2 hash))
  (ord:compare (vec hash1) (vec hash2)))

;; -----------------------------------------------------

(defmethod hash-function-of-hash ((h hash))
  ;; many of the hash functions are named the same as their
  ;; correspdonding class wrappers. So given a hash object, we can
  ;; find the hash to produce another like it.
  (let ((sym (class-name (class-of h))))
    (when (fboundp sym)
      (symbol-function sym))))

(defmethod hash-function-of-hash ((h hash/var))
  (hash-fn h))

;; -------------------------------------------------------

(defun in-place-otp (bytevec &rest keys)
  #|
  ;; Ironclad has now fixed SHAKE256 to produce a correct arbitrary length digest... DM/RAL 05/22
  (let* ((nel  (length bytevec))
         (ovly (make-array nel
                           :element-type (array-element-type bytevec)
                           :adjustable   t
                           :displaced-to bytevec))
         (ekey (apply #'hash/256 keys)))
    (do ((offs  0  (+ offs 32)))
        ((>= offs nel) bytevec)
      (let ((mask  (vec (hash/256 ekey offs))))
        (adjust-array ovly (min 32 (- nel offs))
                      :displaced-to bytevec
                      :displaced-index-offset offs)
        (map-into ovly #'logxor ovly mask))
      ))
  |#
  (let ((dig  (apply 'get-raw-hash-nbytes (length bytevec) keys)))
    (map-into bytevec 'logxor bytevec dig)))


