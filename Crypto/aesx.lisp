;;;; aesx.lisp -- implementation of the extended Rijndael block cipher
;;;
;;; Currently limited to 128-bit block sizes, although the full range of
;;; key sizes is supported.
;;; AES modified according to recommendations by Bruce Schneier
;;; For 128-bit keys use 16 rounds
;;; For 192-bit keys use 20 rounds
;;; For 256-bit keys use 28 rounds
;;; DM/Acudora 02/12
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

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

(in-package :crypto)

(declaim (type (simple-array (unsigned-byte 32) (16)) roundx-constants))
(defconst roundx-constants
#32@(#x01000000 #x02000000 #x04000000 #x08000000
     #x10000000 #x20000000 #x40000000 #x80000000
     #x1B000000 #x36000000 #x6C000000 #xD8000000
     #xAB000000 #x4D000000 #x9A000000 #x2F000000 ))


;;; the actual AESX implementation

;;; waste a little space for "common" 128-bit keys, but is anybody really
;;; going to notice?
(deftype aesx-round-keys () '(simple-array (unsigned-byte 32) (128)))

(defclass aesx (cipher 16-byte-block-mixin)
  ((encryption-round-keys :accessor encryption-round-keys
                          :type aesx-round-keys)
   (decryption-round-keys :accessor decryption-round-keys
                          :type aesx-round-keys)
   (n-rounds :accessor n-rounds)))

(defun allocate-xround-keys (key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (ecase (length key)
    ((16 24 32)
     (make-array 128 :element-type '(unsigned-byte 32) :initial-element 0))))

(defun generate-128-bit-xround-keys (round-keys key)
  (declare (type aesx-round-keys round-keys)
           (type (simple-array (unsigned-byte 8) (16)) key)
           (optimize (speed 3) (space 0) (debug 0)))
  (let ((round-key-offset 0))
    (declare (type (integer 0 43) round-key-offset))
    (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-key-offset))))
      (dotimes (i 4)
        (setf (rk-ref i) (nibbles:ub32ref/be key (* 4 i))))
      (dotimes (i 16 (values round-keys 16))
        (declare (type (integer 0 10) i))
        (let ((tmp (rk-ref 3)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 4)
                (logxor (rk-ref 0)
                        (logand (aref Te4 (third-byte tmp)) #xff000000)
                        (logand (aref Te4 (second-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (first-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (fourth-byte tmp)) #x000000ff)
                        (aref roundx-constants i))
                (rk-ref 5) (logxor (rk-ref 1) (rk-ref 4))
                (rk-ref 6) (logxor (rk-ref 2) (rk-ref 5))
                (rk-ref 7) (logxor (rk-ref 3) (rk-ref 6)))
          (incf round-key-offset 4))))))

(defun generate-192-bit-xround-keys (round-keys key)
  (declare (type aesx-round-keys round-keys)
           (type (simple-array (unsigned-byte 8) (24)) key)
           (optimize (speed 3) (space 0) (debug 0)))
  (let ((round-key-offset 0))
    (declare (type (integer 0 51) round-key-offset))
    (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-key-offset))))
      (dotimes (i 6)
        (setf (rk-ref i) (nibbles:ub32ref/be key (* 4 i))))
      (dotimes (i 10)
        (let ((tmp (rk-ref 5)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 6)
                (logxor (rk-ref 0)
                        (logand (aref Te4 (third-byte tmp)) #xff000000)
                        (logand (aref Te4 (second-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (first-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (fourth-byte tmp)) #x000000ff)
                        (aref roundx-constants i))
                (rk-ref 7) (logxor (rk-ref 1) (rk-ref 6))
                (rk-ref 8) (logxor (rk-ref 2) (rk-ref 7))
                (rk-ref 9) (logxor (rk-ref 3) (rk-ref 8)))
          (when (= 10 (1+ i))
            (return-from generate-192-bit-xround-keys (values round-keys 20)))
          (setf (rk-ref 10) (logxor (rk-ref 4) (rk-ref 9))
                (rk-ref 11) (logxor (rk-ref 5) (rk-ref 10)))
          (incf round-key-offset 6))))))

(defun generate-256-bit-xround-keys (round-keys key)
  (declare (type aesx-round-keys round-keys)
           (type (simple-array (unsigned-byte 8) (32)) key)
           (optimize (speed 3) (space 0) (debug 0)))
  (let ((round-key-offset 0))
    (declare (type (integer 0 127) round-key-offset))
    (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-key-offset))))
      (dotimes (i 8)
        (setf (rk-ref i) (nibbles:ub32ref/be key (* 4 i))))
      (dotimes (i 14)
        (let ((tmp (rk-ref 7)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 8)
                (logxor (rk-ref 0)
                        (logand (aref Te4 (third-byte tmp)) #xff000000)
                        (logand (aref Te4 (second-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (first-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (fourth-byte tmp)) #x000000ff)
                        (aref roundx-constants i))
                (rk-ref 9) (logxor (rk-ref 1) (rk-ref 8))
                (rk-ref 10) (logxor (rk-ref 2) (rk-ref 9))
                (rk-ref 11) (logxor (rk-ref 3) (rk-ref 10)))
          (when (= 14 (1+ i))
            (return-from generate-256-bit-xround-keys (values round-keys 28))))
        (let ((tmp (rk-ref 11)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 12)
                (logxor (rk-ref 4)
                        (logand (aref Te4 (fourth-byte tmp)) #xff000000)
                        (logand (aref Te4 (third-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (second-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (first-byte tmp)) #x000000ff))
                (rk-ref 13) (logxor (rk-ref 5) (rk-ref 12))
                (rk-ref 14) (logxor (rk-ref 6) (rk-ref 13))
                (rk-ref 15) (logxor (rk-ref 7) (rk-ref 14)))
          (incf round-key-offset 8))))))

(defun generate-xround-keys-for-encryption (key round-keys)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (ecase (length key)
    (16 (generate-128-bit-xround-keys round-keys key))
    (24 (generate-192-bit-xround-keys round-keys key))
    (32 (generate-256-bit-xround-keys round-keys key))))

(defun generate-xround-keys-for-decryption (round-keys n-rounds)
  (declare (type aesx-round-keys round-keys)
           (type (unsigned-byte 16) n-rounds))
  ;; invert the order of the round keys
  (do ((i 0 (+ 4 i))
       (j (* 4 n-rounds) (- j 4)))
      ((>= i j))
    (declare (type (unsigned-byte 16) i j))
    (rotatef (aref round-keys i) (aref round-keys j))
    (rotatef (aref round-keys (+ 1 i)) (aref round-keys (+ 1 j)))
    (rotatef (aref round-keys (+ 2 i)) (aref round-keys (+ 2 j)))
    (rotatef (aref round-keys (+ 3 i)) (aref round-keys (+ 3 j))))
  ;; apply inverse MixColumn transform to all round keys but the first
  (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-keys-offset))))
    (do ((i 1 (+ 1 i))
         (round-keys-offset 4 (+ 4 round-keys-offset)))
        ((>= i n-rounds) (values round-keys n-rounds))
      (declare (type (unsigned-byte 16) round-keys-offset))
      (macrolet ((mix-column (x)
                   `(let ((column (rk-ref ,x)))
                      (declare (type (unsigned-byte 32) column))
                      (setf (rk-ref ,x)
                            (logxor
                             (aref Td0 (first-byte (aref Te4 (fourth-byte column))))
                             (aref Td1 (first-byte (aref Te4 (third-byte column))))
                             (aref Td2 (first-byte (aref Te4 (second-byte column))))
                             (aref Td3 (first-byte (aref Te4 (first-byte column)))))))))
        (mix-column 0) (mix-column 1) (mix-column 2) (mix-column 3)))))

(macrolet ((mix (rk a0 a1 a2 a3 sym0 sym1 sym2 sym3)
                   `(logxor (aref ,a0 (fourth-byte ,sym0))
                            (aref ,a1 (third-byte ,sym1))
                            (aref ,a2 (second-byte ,sym2))
                            (aref ,a3 (first-byte ,sym3))
                            (rk-ref ,rk)))
           (mix-s-into-t-encrypting (offset)
             `(setf t0 (mix ,offset Te0 Te1 Te2 Te3 s0 s1 s2 s3)
               t1 (mix (1+ ,offset) Te0 Te1 Te2 Te3 s1 s2 s3 s0)
               t2 (mix (+ ,offset 2) Te0 Te1 Te2 Te3 s2 s3 s0 s1)
               t3 (mix (+ ,offset 3) Te0 Te1 Te2 Te3 s3 s0 s1 s2)))
           (mix-t-into-s-encrypting (offset)
               `(setf s0 (mix ,offset Te0 Te1 Te2 Te3 t0 t1 t2 t3)
                 s1 (mix (1+ ,offset) Te0 Te1 Te2 Te3 t1 t2 t3 t0)
                 s2 (mix (+ ,offset 2) Te0 Te1 Te2 Te3 t2 t3 t0 t1)
                 s3 (mix (+ ,offset 3) Te0 Te1 Te2 Te3 t3 t0 t1 t2)))
           (mix-s-into-t-decrypting (offset)
             `(setf t0 (mix ,offset Td0 Td1 Td2 Td3 s0 s3 s2 s1)
               t1 (mix (1+ ,offset) Td0 Td1 Td2 Td3 s1 s0 s3 s2)
               t2 (mix (+ ,offset 2) Td0 Td1 Td2 Td3 s2 s1 s0 s3)
               t3 (mix (+ ,offset 3) Td0 Td1 Td2 Td3 s3 s2 s1 s0)))
           (mix-t-into-s-decrypting (offset)
               `(setf s0 (mix ,offset Td0 Td1 Td2 Td3 t0 t3 t2 t1)
                 s1 (mix (1+ ,offset) Td0 Td1 Td2 Td3 t1 t0 t3 t2)
                 s2 (mix (+ ,offset 2) Td0 Td1 Td2 Td3 t2 t1 t0 t3)
                 s3 (mix (+ ,offset 3) Td0 Td1 Td2 Td3 t3 t2 t1 t0)))
           (rk-ref (x) `(aref round-keys (+ ,x round-key-offset)))
           #+nil (rk-ref (x) `(aref round-keys (+ ,x 0))))

(define-block-encryptor aesx 16
  (let ((round-keys (encryption-round-keys context))
        (n-rounds (n-rounds context)))
    (declare (type aesx-round-keys round-keys))
    (declare (type (integer 0 28) n-rounds))
    ;; the "optimized implementation" also had a fully unrolled version of
    ;; this loop hanging around.  it might be worthwhile to translate it and
    ;; see if it actually gains us anything.  a wizard would do this with a
    ;; macro which allows one to easily switch between unrolled and
    ;; non-unrolled versions.  I am not a wizard.
    (with-words ((s0 s1 s2 s3) plaintext plaintext-start)
      (let ((t0 0) (t1 0) (t2 0) (t3 0)
            (round-key-offset 0))
        (declare (type (unsigned-byte 32) t0 t1 t2 t3))
        (declare (type (unsigned-byte 16) round-key-offset))
        ;; initial whitening
        (setf s0 (logxor s0 (aref round-keys 0))
              s1 (logxor s1 (aref round-keys 1))
              s2 (logxor s2 (aref round-keys 2))
              s3 (logxor s3 (aref round-keys 3)))
        (do ((round (truncate n-rounds 2) (1- round)))
            ((zerop round))
          (declare (type (unsigned-byte 16) round))
          (mix-s-into-t-encrypting 4)
          (incf round-key-offset 8)
          (when (= round 1)
            (return-from nil (values)))
          (mix-t-into-s-encrypting 0))
        ;; apply last round and dump cipher state into the ciphertext
        (flet ((apply-round (round-key u0 u1 u2 u3)
                 (declare (type (unsigned-byte 32) round-key u0 u1 u2 u3))
                 (logxor (logand (aref Te4 (fourth-byte u0)) #xff000000)
                         (logand (aref Te4 (third-byte u1)) #x00ff0000)
                         (logand (aref Te4 (second-byte u2)) #x0000ff00)
                         (logand (aref Te4 (first-byte u3)) #x000000ff)
                         round-key)))
          (declare (inline apply-round))
          (store-words ciphertext ciphertext-start
                       (apply-round (rk-ref 0) t0 t1 t2 t3)
                       (apply-round (rk-ref 1) t1 t2 t3 t0)
                       (apply-round (rk-ref 2) t2 t3 t0 t1)
                       (apply-round (rk-ref 3) t3 t0 t1 t2)))))))

(define-block-decryptor aesx 16
  (let ((round-keys (decryption-round-keys context))
        (n-rounds (n-rounds context)))
    (declare (type aesx-round-keys round-keys))
    (declare (type (unsigned-byte 16) n-rounds))
    (with-words ((s0 s1 s2 s3) ciphertext ciphertext-start)
      (let ((t0 0) (t1 0) (t2 0) (t3 0)
            (round-key-offset 0))
        (declare (type (unsigned-byte 32) t0 t1 t2 t3))
        (declare (type (unsigned-byte 16) round-key-offset))
        ;; initial whitening
        (setf s0 (logxor s0 (aref round-keys 0))
              s1 (logxor s1 (aref round-keys 1))
              s2 (logxor s2 (aref round-keys 2))
              s3 (logxor s3 (aref round-keys 3)))
        (do ((round (truncate n-rounds 2) (1- round)))
            ((zerop round))
          (declare (type (unsigned-byte 16) round))
          (mix-s-into-t-decrypting 4)
          (incf round-key-offset 8)
          (when (= round 1)
            (return-from nil (values)))
          (mix-t-into-s-decrypting 0))
        ;; apply last round and dump cipher state into plaintext
        (flet ((apply-round (round-key u0 u1 u2 u3)
                 (declare (type (unsigned-byte 32) round-key u0 u1 u2 u3))
                 (logxor (logand (aref Td4 (fourth-byte u0)) #xff000000)
                         (logand (aref Td4 (third-byte u1)) #x00ff0000)
                         (logand (aref Td4 (second-byte u2)) #x0000ff00)
                         (logand (aref Td4 (first-byte u3)) #x000000ff)
                         round-key)))
          (declare (inline apply-round))
          (store-words plaintext plaintext-start
                       (apply-round (rk-ref 0) t0 t3 t2 t1)
                       (apply-round (rk-ref 1) t1 t0 t3 t2)
                       (apply-round (rk-ref 2) t2 t1 t0 t3)
                       (apply-round (rk-ref 3) t3 t2 t1 t0)))))))

) ; MACROLET

(defmethod schedule-key ((cipher aesx) key)
  (multiple-value-bind (encryption-keys n-rounds)
      (generate-xround-keys-for-encryption key (allocate-xround-keys key))
    (declare (type aesx-round-keys encryption-keys))
    (let ((decryption-keys (copy-seq encryption-keys)))
      (generate-xround-keys-for-decryption decryption-keys n-rounds)
      (setf (encryption-round-keys cipher) encryption-keys
            (decryption-round-keys cipher) decryption-keys
            (n-rounds cipher) n-rounds)
      cipher)))

(defcipher aesx
  (:encrypt-function aesx-encrypt-block)
  (:decrypt-function aesx-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 24 32)))

(export '(aesx) :ironclad)

