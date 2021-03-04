;; vec-repr.lisp -- Uniform and varied representation of UB8 Vectors
;;
;; DM/Emotiq 02/18
;; ----------------------------------------------------------------
#|
The MIT License

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

(in-package :vec-repr)
;; ---------------------------------------------------------

;; This package describes interchangeable representations of vectors
;; of (unsigned-byte 8) values.
;;
;; Type UB8 represents '(UNSIGNED-BYTE 8). Type UB8-VECTOR is defined
;; to represent any vector of UB8 elements. Endian interpretation of
;; the vector is in the eye of the beholder.
;;
;;
;;                Class Hierarchy
;;                ---------------
;;
;;                    UB8V-OBJ
;;                       |
;;                       +-----------+---------+
;;                                   |         |
;;      UB8V-AS-STR                 BEV       LEV
;;           |                       |
;;    +------+-------+               |
;;    |      |       |               |
;;   HEX   BASE58  BASE64            |
;;           |                       |   
;;       BASE58-CHK                 HASH
;;                                   |
;;                        +----------+-----------+
;;                        |          |      ...  |
;;                      HASH/256   HASH/512 ...  |
;;                                              ...
;;
;; Class UB8V-OBJ serves as an abstract superclass.  Any object of one
;; of the subclasses can be instantly converted into another
;; representation, without affecting the value and order of bytes
;; within the vector. Think of these as distinct wrapper labels around
;; a raw UB8-VECTOR, and each has the same contents and byte ordering.
;;
;; Endianness really only applies to vectors when viewed as
;; representations of INTEGERS. A HASH byte vector, for example, has
;; no particular endianness. But a HASH byte vector could be converted
;; to an INTEGER using some endianness convention for Hashes, which
;; could make it more convenient to use in computations than a byte
;; vector might be.
;;
;; BEV wraps a UB8-VECTOR viewed as Big-Endian.
;; LEV wraps a UB8-VECTOR viewed as Little-Endian.
;; HEX holds a UB8-VECTOR denoted by a simple-base-string of hex
;; digits.  It claims no overt endianness, but it defaults to being
;; Big Endian when converted to an INTEGER.
;;
;; Additionlly we make provision for conversion from bignum integers
;; to/from these vector representations.
;;
;; UB8V-OBJ denotes classes of 8-bit byte vectors. Subclass
;; UB8V-AS-STR serves as abstract superclass of wrapped data that are
;; held as strings, e.g., BASE58.  Subclass BEV serves as the
;; superclass for all wrapped data that are held as UB8-VECTORS.
;;
;; Each of the subclasses also has methods defined with the same name
;; as their class name, to perform conversions to their specific form
;; of representation. For example:
;;
;;    (describe (hex #(1 2 3 4)))
;;  ==>
;;    #<HEX 01020304> is a HEX
;;    VAL      "01020304"
;;
;;    (describe (base58 (hex #(1 2 3 4))))
;;  ==>
;;    #<BASE58 2VfUX> is a BASE58
;;    VAL      "2VfUX"
;;
;;    (describe (bev (base58 #(1 2 3 4))))
;;  ==>
;;    #<BEV 01020304> is a BEV
;;    VAL      #(1 2 3 4)
;;
;;    (describe (lev (base58 #(1 2 3 4))))
;;  ==>
;;    #<LEV 01020304> is a LEV
;;    VAL      #(1 2 3 4)
;;
;;    (int (bev #(1 2 3 4)))
;;  ==>
;;    #x1020304
;;
;;    (int (lev #(1 2 3 4)))
;;  ==>
;;    #x4030201
;;
;;    (int (base58 #(1 2 3 4)))
;;  ==>
;;    #x1020304
;;
;;    (with-standard-io-syntax (write (bev #(1 2 3 4))))
;;  ==>
;;    #.(make-instance 'VEC-REPR:BEV :val (bev-vec (hex "01020304"))
;;
;;    (with-standard-io-syntax (write (hash/256 :this)))
;;  ==>
;;    #.(make-instance 'HASH:HASH/256 :val (bev-vec (hex "93DEEB98776CE8E1D64C1C7BB226A0A6ABF97A4C28C010D073499BA049AC58A0"))
;;
;; These conversion operators may be applied to objects of any of the
;; parallel subclasses, as well as to INTEGER, LIST, and VECTOR. The
;; latter sequences must contain only elements of type UB8.
;;
;; The operators can also be applied to any class that inherits from
;; the mixin class UB8V-REPR and which implements a method by that
;; same name to return an instance of one of these parallel
;; subclasses. (c.f., PBC.LISP)
;;
;; NOTE: Items of these types should be considered inviolable atomic
;; values, just like bignums. Modification of their internal contents
;; may have unpredictable consequences.
;;
;; Of the classes of vectors, only BEV and LEV overtly have any meaning
;; when interpreted as integer values. The others merely represent an
;; ordered sequence of byte values.
;;
;; However, lexical ordering of byte vectors can be also performed
;; from front to back, by convention, just like for strings, and this
;; has similar lexical ordering as if twp equal length vectors were
;; considered big-endian encodings of integers.
;;
;; So, by default, vectors can be converted to integers as though
;; they were also Big Endian vectors, unless explicitly declared
;; as Little Endian.
;;
;; So while a vector can be viewed as BEV or LEV, as needed, a vector
;; produced by converting an integer to BEV will have oppossite byte
;; order from the same integer converted to LEV.
;; ----------------------------------------------------------
;; Declare Types UB8 and UB8-VECTOR

(deftype ub8 ()
  '(unsigned-byte 8))

(deftype ub8-vector (&optional nel)
  `(array ub8 (,nel)))

;; --------------------------------------------------------------
;; Utility functions

(defun sbs (str)
  (coerce str 'simple-base-string))

(defun ub8v (vec)
  (coerce vec 'ub8-vector))

(defun make-ub8-vector (nel &rest args)
  (apply 'make-array nel
         :element-type 'ub8
         (append args '(:initial-element 0))))

;; ---------------------------------------------------------
;; UB8V-OBJ - the top abstract class of objects which represent
;; UB8-VECTORs. Declare a single slot to hold the data. Objects of
;; these classes are intended to be immuatable.  All subclasses share
;; this same slot.

(defclass ub8v-obj ()
  ((val :reader  ub8v-val
        :initarg :val)))

(defclass ub8v-as-str ()
  ((val :reader ub8v-str
        :reader str
        :initarg :str)))

;; ----------------------------------------------------------
;; Base58 encodes integers into character strings of the restricted
;; alphabet.

(defclass base58 (ub8v-as-str)
  ((val :reader base58-str
        :initarg :str)))

(defgeneric base58 (x))

(defmethod base58-str (x)
  (base58-str (base58 x)))

(defclass base58-chk (base58)
  ())

;; ----------------------------------------------------------
;; Base64 encodes UB8 vectors and integers into character strings of
;; the restricted alphabet. Encoding has a 6-character prefix that
;; represents the total number of bytes in the vector, up to 2^32
;; elements.

(defclass base64 (ub8v-as-str)
  ((val :reader base64-str
        :initarg :str)))

(defgeneric base64 (x))

(defmethod base64-str (x)
  (base64-str (base64 x)))

;; -----------------------------------------------------------
;; Hex-string representation, 1 char per 4-bit nibble

(defclass hex (ub8v-as-str)
  ((val :reader hex-str
        :initarg :str)))

(defgeneric hex (x))

(defmethod hex-str (x)
  (hex-str (hex x)))

;; -----------------------------------------------------------
;; BEV-UB8 are big-endian vectors of UB8 elements

(defclass bev (ub8v-obj)
  ((val :reader bev-vec
        :reader vec
        :initarg :vec)))

(defgeneric bev (x))

(defmethod bev-vec (x)
  (bev-vec (bev x)))

;; -----------------------------------------------------------
;; LEV-UB8 are little-endian vectors of UB8 elements

(defclass lev (ub8v-obj)
  ((val :reader lev-vec
        :reader vec
        :initarg :vec)))

(defgeneric lev (x))

(defmethod lev-vec (x)
  (lev-vec (lev x)))

#|
(let* ((x (bev #x010203))
 |#

;; ---------------------------------------------------------
;; Integer to/from vector form - conversion starts at LSB and works
;; toward MSB.

(defun convert-vec-to-int (vec &key
                               (start  0)
                               (end    (length vec))
                               lev) ;; t for little-endian vector
  (let ((val  0))
    (labels ((acc (ix pos)
               (setf val (dpb (aref vec ix) (byte 8 pos) val))))
      (if lev
          (do ((ix  start  (1+ ix))
               (pos 0      (+ pos 8)))
              ((>= ix end)  val)
            (acc ix pos))
      ;; else
      (do ((ix  (1- end)  (1- ix))
           (pos 0         (+ pos 8)))
          ((< ix start)  val)
        (acc ix pos))
      ))))

(defun convert-int-to-vec (val &key
                               lev
                               (nb  (max 1 (ceiling (integer-length val) 8))))
  (let ((v  (make-ub8-vector nb)))
    (labels ((acc (ix pos)
               (setf (aref v ix) (ldb (byte 8 pos) val))))
      (if lev
          (do ((ix  0   (1+ ix))
               (pos 0   (+ 8 pos)))
              ((>= ix nb)  v)
            (acc ix pos))
          ;; else
          (do ((ix  (1- nb)  (1- ix))
               (pos 0        (+ 8 pos)))
              ((minusp ix)  v)
            (acc ix pos))
          ))))

;; --------------------------------------------------------------
;; Integer conversions - these produce raw integers without any type
;; context

(defmethod int ((x integer))
  x)

(defmethod int ((x sequence))
  ;; vectors default to big-endian
  (convert-vec-to-int (ub8v x)))

(defmethod int ((x lev))
  (convert-vec-to-int (lev-vec x) :lev t))

(defmethod int ((u uuid:uuid))
  (uuid:uuid-to-integer u))

(defmethod int (x)
  (int (bev-vec x)))

;; --------------------------------------------------------------
;; Base58 -- value as Base58 string
;;

(defun check-repr (repr)
  (and (bev repr) ;; check validity
       repr))

(defmethod base58 ((x base58))
  x)

(defmethod base58 ((x string))
  (check-repr (make-instance 'base58
                             :str (sbs x))))

(defmethod base58 ((x sequence))
  ;; this code assumes x is big-endian...
  (make-instance 'base58
                 :str (sbs
                       (base58:encode (map 'base-string 'code-char x)))
                 ))

(defmethod base58 (x)
  ;; preserve leading zeros
  (base58 (bev-vec x)))

;; --------------------------------------------------------------
;; Base58-chk -- value as Base58-chk string
;;

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

;; -------------------------------------------------------------
;; Base64 Conversions

(defun convert-vector-to-base64-string (ivec)
  (sbs
   (with-output-to-string (s)
     (s-base64:encode-base64-bytes ivec s))))

(defun convert-base64-string-to-vector (str)
  (let ((vec (ub8v
              (with-input-from-string (s str)
                (s-base64:decode-base64-bytes s)))))
    ;; convert to simple-array (suitable for Ironclad input)
    (subseq vec 0 (length vec))))

;; ---

(defmethod base64 ((x base64))
  x)

(defmethod base64 ((x sequence))
  ;; assumes x is big endian vector
  (make-instance 'base64
                 :str (convert-vector-to-base64-string (ub8v x))))

(defmethod base64 ((x string))
  (check-repr (make-instance 'base64
                             :str (sbs x))))

(defmethod base64 (x)
  (base64 (bev-vec x)))

;; -------------------------------------------------------------
;; HEX Conversions
;;
;; A HEX is a convenient printable representation of the bytes
;; in a vector, from from to back, reading from left to right.

(defun convert-hex-string-to-vector (str)
  (let* ((ovec  (make-ub8-vector (ash (length str) -1)))
         (occ   0)
         (cy    0)
         (ix    0))
    (map nil (lambda (ch)
               (setf cy (+ (ash cy 4) (digit-char-p ch 16)))
               (incf occ 4)
               (when (= occ 8)
                 (setf (aref ovec ix) cy
                       ix             (1+ ix)
                       occ            0
                       cy             0)))
         str)
    ovec))

(defun convert-vector-to-hex-string (vec)
  (with-output-to-string (s)
    (map nil (lambda (byt)
               (format s "~2,'0x" byt))
         vec)))

(defmethod hex ((x hex))
  x)

(defmethod hex ((x string))
  (when (oddp (length x))
    (setf x (concatenate 'simple-base-string "0" x)))
  (check-repr (make-instance 'hex
                             :str (sbs x))))

(defmethod hex ((x sequence))
  (make-instance 'hex
                 :str (convert-vector-to-hex-string (ub8v x))))

(defmethod hex (x)
  (hex (bev-vec x)))

;; ------------------------------------------------------
;; LEV -- encode vector as little-endian (LSB first)

(defmethod lev ((x lev))
  x)

(defmethod lev ((x bev))
  (make-instance 'lev
                 :vec (reverse (bev-vec x))))

(defmethod lev ((val integer))
  (make-instance 'lev
                 :vec (convert-int-to-vec val :lev t)))

(defmethod lev ((x sequence))
  (make-instance 'lev
                 :vec (ub8v x)))

(defmethod lev (x)
  (lev (bev x)))

(defmethod levn (x nb)
  (lev (bevn x nb)))

;; -----------------------------------------------------------
;; BEV -- encode vector as big-endian (MSB first)

(defmethod bev ((x bev))
  x)

(defmethod bev ((x lev))
  (make-instance 'bev
                 :vec (reverse (lev-vec x))))

(defmethod bev ((x sequence))
  ;; LIST and VECTOR when can be coerced to UB8-VECTOR
  (make-instance 'bev
                 :vec (ub8v x)))

(defmethod bev ((x string))
  (bev (hex x)))

(defmethod bev ((x integer))
  ;; convert integer to BEV vector
  (make-instance 'bev
                 :vec (convert-int-to-vec x)))

(defmethod bev ((x base58))
  (map 'ub8-vector 'char-code
       (base58:decode (str x))))

(defmethod bev ((x base58-chk))
  (let* ((vec (bev-vec (call-next-method)))
         (end (- (length vec) 4))
         (pre (subseq vec 0 end))
         (chk (subseq vec end))
         (cal (base58-check-bytes pre)))
    (assert (equalp chk cal))
    (make-instance 'bev
                   :vec pre)))

(defmethod bev ((x base64))
  (make-instance 'bev
                 :vec (convert-base64-string-to-vector (str x))))

(defmethod bev ((x hex))
  (make-instance 'bev
                 :vec (convert-hex-string-to-vector (str x))))

(defmethod bev ((u uuid:uuid))
  (bev (uuid:uuid-to-byte-array u)))

(defun bevn (x nb)
  ;; construct a BEV with a specified number of UB8 bytes
  (let* ((bev  (bev x))
         (nel  (length (bev-vec bev))))
    (cond ((< nel nb)
           ;; prepend with zero filled prefix
           (let* ((diff (- nb nel))
                  (pref (make-ub8-vector diff)))
             (make-instance 'bev
                            :vec (concatenate 'ub8-vector pref (bev-vec bev)))))
          ((> nel nb)
           ;; take a portion from the LSB side
           (make-instance 'bev
                          :vec (subseq (bev-vec bev) (- nel nb))))
          (t
           bev)
          )))

;; --------------------------------------------------------------
;; Testing

(defun int= (a b)
  "Poor way to do equality testing on UB8V items. This method fails to
account for vectors with leading null bytes. Okay for comparing
compressed ECC points - public keys, signatures. Not okay for hash
comparisons."
  (= (int a) (int b)))

(defun int< (a b)
  (< (int a) (int b)))

(defun int> (a b)
  (> (int a) (int b)))

(defun int<= (a b)
  (not (int> a b)))

(defun int>= (a b)
  (not (int< a b)))

(defun int/= (a b)
  (not (int= a b)))

;; ---------------------------------------------

(defmethod vec (x)
  (bev-vec x))

(defun vec-cmp (a b)
  (ord:compare (vec a) (vec b)))

(defun vec= (a b)
  "Easy way to do equality testing on UB8V items"
  (zerop (vec-cmp a b)))

(defun vec< (a b)
  (minusp (vec-cmp a b)))

(defun vec> (a b)
  (plusp (vec-cmp a b)))

(defun vec<= (a b)
  (not (vec> a b)))

(defun vec>= (a b)
  (not (vec< a b)))

(defun vec/= (a b)
  (not (vec= a b)))
  
;; -----------------------------------------------
;; Printing

(defmethod print-object ((obj bev) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (format out-stream
                "#.(make-instance '~W :val (bev-vec (hex ~S)))"
                (class-name (class-of obj))
                (hex-str obj)))
    ;; else
    (print-unreadable-object (obj out-stream :type t)
      (princ (short-str (hex-str obj)) out-stream))
    ))

(defmethod print-object ((obj ub8v-as-str) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (format out-stream
                "#.(make-instance '~W :val ~S)"
                (class-name (class-of obj))
                (ub8v-str obj)))
    ;; else
    (print-unreadable-object (obj out-stream :type t)
      (princ (short-str (ub8v-str obj)) out-stream))
    ))

(defun short-str (str)
  (let ((len (length str)))
    (if (< len 17)
        str
      (concatenate 'simple-base-string
              (subseq str 0 7)
              ".."
              (subseq str (- len 7)))
      )))

