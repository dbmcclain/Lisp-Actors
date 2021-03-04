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
;;            Class Hierarchy
;;            ---------------
;;
;;                 UB8V
;;                   |
;;      +-----+------+--------+-------+
;;      |     |      |        |       |
;;     LEV   BEV   BASE58   BASE64   HEX
;;
;; Class UB8V serves as an abstract superclass for all of these
;; parallel sub-classes. Any object of one of the subclasses can be
;; instantly converted into another parallel representation.
;;
;; Additionlly we make provision for conversion from bignum integers
;; to/from these vector representations.
;;
;; Finally, the Class UB8V-REPR is a mixin for future classes to use,
;; to indicate that they have the ability to produce a UB8V
;; representation, e.g., public keys, secret keys, compressed points
;; of Elliptic Curves, etc. This representation may be requested from
;; them with the UB8V-REPR method call.
;;
;; Type UB8 represents '(UNSIGNED-BYTE 8). Type UB8-VECTOR is defined
;; to represent any vector of actual UB8 values. Endian interpretation
;; is in the eye of the beholder.
;;
;; Each of the subclasses also has methods defined with the same name
;; as their class name, to perform conversions to their specific form
;; of representation. For example:
;;
;;    (base58 (lev #(1 2 3 4)))
;;  ==>
;;    #<BASE58 1111156wxj2 >
;;
;; These conversion operators may be applied to objects of any of the
;; parallel subclasses, as well as to INTEGER, LIST, and VECTOR. The
;; latter two must contain only elements of type UB8.
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
;; ----------------------------------------------------------
;; Declare Types UB8 and UB8-VECTOR

(deftype ub8 ()
  '(unsigned-byte 8))

(deftype ub8-vector (&optional nel)
  `(array ub8 (,nel)))

(defun make-ub8-vector (nel &rest args)
  (apply 'make-array nel
         :element-type 'ub8
         args))

;; ----------------------------------------------------------
;; from https://bitcointalk.org/index.php?topic=1026.0

(um:defconstant+ +alphabet-58+
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(um:defconstant+ +len-58+
  (length +alphabet-58+)) ;; should be 58

(defun make-inverse-alphabet (alphabet)
  (let ((arr (make-ub8-vector 256
                         :initial-element 0)))
    (loop for c across alphabet
          for ix from 1
          do
          (setf (aref arr (char-code c)) ix))
    arr))
  
(defvar +inv-alphabet-58+
  (make-inverse-alphabet +alphabet-58+))

;; ----------------------------------------------------------

(um:defconstant+ +alphabet-64+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defvar +inv-alphabet-64+
  (make-inverse-alphabet +alphabet-64+))

;; ---------------------------------------------------------
;; UB8V - the top abstract class of objects which represent
;; UB8-VECTORs. Declare a single slot to hold the data. Object of
;; these classes are intended to be immuatable.  All subclasses share
;; this same slot.

(defclass ub8v ()
  ((val :reader  ub8v-vec
        :initarg :vec)))

;; ---------------------------------------------------------
;; Declare a useful mixin class to allow future classes to show a UB8V
;; representation

(defclass ub8v-repr ()
  ())

(defgeneric ub8v-repr (x)
  ;; subclasses of the mixin should return whatever representation
  ;; they need
  (:method ((x ub8v-repr))
   (error "Subclass responsibility")))

;; -----------------------------------------------

(defmethod print-object ((obj ub8v) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (princ "#." out-stream)
        (prin1 `(,(class-name (class-of obj)) (hex ,(hex-str (hex obj))))
               out-stream))
    ;; else
    (print-unreadable-object (obj out-stream :type t)
      (princ (short-str (ub8v-vec obj)) out-stream))
    ))

(defmethod print-object ((obj ub8v-repr) out-stream)
  ;; subclasses of UB8V-REPR mixin should feel free to override this
  (if *print-readably*
      (format out-stream "#.(make-instance '~W :value ~A)"
              (class-name (class-of obj))
              (ub8v-repr obj))
    ;; else
    (print-unreadable-object (obj out-stream :type t)
      (princ (short-str (ub8v-repr obj)) out-stream))
    ))

(defun short-str (str)
  (let ((len (length str)))
    (if (< len 17)
        str
      (concatenate 'string
              (subseq str 0 7)
              ".."
              (subseq str (- len 7)))
      )))

;; ----------------------------------------------------------
;; Base58 encodes integers into character strings of the restricted
;; alphabet.

(defclass base58 (ub8v)
  ((val  :reader   base58-str
         :initarg  :str)
   ))

(defgeneric base58 (x))

(defmethod base58-str (x)
  (base58-str (base58 x)))

;; ----------------------------------------------------------
;; Base64 encodes UB8 vectors and integers into character strings of
;; the restricted alphabet. Encoding has a 6-character prefix that
;; represents the total number of bytes in the vector, up to 2^32
;; elements.

(defclass base64 (ub8v)
  ((val  :reader   base64-str
         :initarg  :str)
   ))

(defgeneric base64 (x))

(defmethod base64-str (x)
  (base64-str (base64 x)))

;; -----------------------------------------------------------
;; Hex-string representation, 1 char per 4-bit nibble

(defclass hex (ub8v)
  ((val  :reader hex-str
         :initarg :str)))

(defgeneric hex (x))

(defmethod hex-str (x)
  (hex-str (hex x)))

;; -----------------------------------------------------------
;; LEV-UB8 are little-endian vectors of UB8 elements

(defclass lev (ub8v)
  ((val  :reader   lev-vec
         :initarg  :vec)
   ))

(defgeneric lev (x))

(defmethod lev-vec (x)
  (lev-vec (lev x)))

;; -----------------------------------------------------------
;; BEV-UB8 are big-endian vectors of UB8 elements

(defclass bev (ub8v)
  ((val  :reader   bev-vec
         :initarg  :vec)
   ))

(defgeneric bev (x))

(defmethod bev-vec (x)
  (bev-vec (bev x)))

;; ---------------------------------------------------------
;; Integer to/from vector form - conversion starts at LSB and works
;; toward MSB. So FROR-END should be true for big-endian vectors, and
;; false for little-endian vectors.

(defun convert-vec-to-int (vec &key
                           (start    0)
                           (end      (length vec))
                           (from-end t))
  ;; from-end t is suitable for big-endian format
  (let ((val  0))
    (if from-end
        (do ((ix  (1- end)  (1- ix))
             (pos 0         (+ pos 8)))
            ((< ix start)  val)
          (setf val (dpb (aref vec ix) (byte 8 pos) val)))
      ;; else
      (do ((ix  start  (1+ ix))
           (pos 0      (+ 8 pos)))
          ((>= ix end)  val)
        (setf val (dpb (aref vec ix) (byte 8 pos) val)))
      )))

(defun convert-int-to-vec (val &key
                               (from-end t)
                               (nb  (max 1 (ceiling (integer-length val) 8))))
  ;; convert val to little-endian vector of UB8
  (let ((v  (make-ub8-vector nb
                             :initial-element 0)))
    (if from-end
        (do ((ix  (1- nb)  (1- ix))
             (pos 0        (+ 8 pos)))
            ((minusp ix)  v)
          (setf (aref v ix) (ldb (byte 8 pos) val)))
      ;; else
      (do ((ix  0   (1+ ix))
           (pos 0   (+ 8 pos)))
          ((>= ix nb)  v)
        (setf (aref v ix) (ldb (byte 8 pos) val)))
      )))

;; --------------------------------------------------------------
;; Integer conversions - these produce raw integers without any type
;; context

(defmethod int ((x integer))
  x)

(defmethod int ((x lev))
  (convert-vec-to-int (lev-vec x)
                      :from-end nil))

(defmethod int ((x bev))
  (convert-vec-to-int (bev-vec x)))

(defmethod int ((x base58))
  (let* ((str   (base58-str x))
         (limit (length str)))
    (um:nlet iter ((pos  0)
                   (val  0))
      (if (>= pos limit)
          val
        (let* ((c (char str pos))
               (v (aref +inv-alphabet-58+ (char-code c))))
          (when (zerop v)
            (error "Invalid Base58 string: ~A" str))
          (go-iter (1+ pos) (+ (1- v) (* 58 val)))
          ))
      )))

(defmethod int ((x base64))
  (let* ((str   (base64-str x))
         (limit (length str)))
    (um:nlet iter ((pos 0)
                   (val 0))
      (if (>= pos limit)
          val
        (let* ((c (char str pos)))
          (if (char= c #\=)
              val
            (let ((v  (aref +inv-alphabet-64+ (char-code c))))
              (when (zerop v)
                (error "Invalid Base64 string: ~A" str))
              (go-iter (1+ pos) (+ (1- v) (ash val 6)))
              )))
        ))))

(defmethod int ((x hex))
  (let ((str (hex-str x)))
    (if (string= "" str)
        0
      (let ((*read-eval* nil)
            (*read-base* 16))
        (read-from-string str)))
    ))

(defmethod int (x)
  (int (bev x)))

;; --------------------------------------------------------------
;; Base58 -- Integer value as Base58 string
;;
;; NOTE: The rules followed in this package try to preserve the byte
;; vector as the fundamental unit of importance. So if a byte vector
;; (BEV or LEV) is converted into a Base58, Base64, or HEX
;; representation, that representation will have at least the same
;; overall bit-length as the underlying byte vector.
;;
;; This means, e.g., suppose we have a byte vector with 32 bytes. That
;; is 256 bits.
;;
;; In Base58, each character accounts for 5.86 bits, so
;; the Base58 representation will have 44 (= (ceiling 256 5.86))
;; chars, for a bit length of 257.84 bits.
;;
;; A Base64 representation of the same 256 bit value will have 43
;; chars, for a bit length of 258 bits.
;;
;; With this understanding, on conversion back to byte vector form,
;; the underlying byte vector will end up with (FLOOR nbits 8) bytes,
;; where nbits is the number of bits represented by the string. In the
;; case of Base58 this will be (floor (* 5.86 (length str)) 8) bits.
;; In the case of Base64 it will be (floor (* 6 (length str)) 8).
;;
;; For Hex encodings, every byte vector will produce an even number of
;; hex digits. But if a Hex value with an odd number of digits is
;; converted back to a byte vector, we round upward. So HEX "012" will
;; produce a BEV vector of #(0 #x12).
;;
;; For now, if this departs from Bitcoin standards, too bad.
;; ------------------------------------------------------------------

(defun validate-base58-string (str)
  (every (lambda (c)
           (plusp (aref +alphabet-58+ (char-code c))))
         str))

(defun convert-int-to-base58-string (val vec)
  ;; vec is byte vector from which val was computed
  (declare (integer val))
  (let* ((nbytes (length vec))
         (n58    (ceiling (* nbytes #.(log 256 58))))
         (cs     nil))
    (declare (fixnum nbytes n58))
    (um:nlet iter ((val  val)
                   (ct   n58))
      (declare (integer val)
               (fixnum ct))
      (when (plusp ct)
        (multiple-value-bind (vw vf) (floor val 58)
          (declare (integer vw)
                   (fixnum vf))
          (let ((c  (aref +alphabet-58+ vf)))
            (push c cs)
            (go-iter vw (1- ct)))))
      (coerce cs 'string))
    ))

;; ---

(defmethod base58 ((x base58))
  x)

(defmethod base58 ((x string))
  (assert (every (um:rcurry 'find +alphabet-58+) x))
  (make-instance 'base58
                 :str x))

(defmethod base58 ((x vector))
  ;; this code assumes x is big-endian...
  (make-instance 'base58
                 :str (convert-int-to-base58-string (convert-vec-to-int x) x)))

(defmethod base58 ((x bev))
  (base58 (bev-vec x)))

(defmethod base58 ((x lev))
  (make-instance 'base58
                 :str (convert-int-to-base58-string (int x) (lev-vec x))))

(defmethod base58 (x)
  ;; preserve leading zeros
  (base58 (bev x)))

;; -------------------------------------------------------------
;; Base64 Conversions

(defun convert-int-to-base64-string (val vec)
  ;; vec is byte vector from which val was computed
  (declare (integer val))
  (let* ((nbytes (length vec))
         (n64    (ceiling (* nbytes #.(log 256 64))))
         (ntail  (logand n64 3))
         (cs     nil))
    (declare (fixnum nbytes n64 ntail))
    (unless (zerop ntail)
      (loop repeat (- 4 ntail) do (push #\= cs)))
    (um:nlet iter ((val  val)
                   (ct   n64))
      (declare (integer val)
               (fixnum ct))
      (when (plusp ct)
        (let* ((bits  (ldb (byte 6 0) val))
               (c     (aref +alphabet-64+ bits)))
          (declare (fixnum bits))
          (push c cs)
          (go-iter (ash val -6) (1- ct)))))
    (coerce cs 'string)))
  
(defmethod base64 ((x bev))
  (make-instance 'base64
                 :str (convert-int-to-base64-string (int x) (bev-vec x))))

(defmethod base64 ((x lev))
  (make-instance 'base64
                 :str (convert-int-to-base64-string (int x) (lev-vec x))))

(defmethod base64 ((x string))
  (assert (every (um:rcurry 'find +alphabet-64+) (string-right-trim '(#\=) x)))
  (make-instance 'base64
                 :str x))

(defmethod base64 ((x base64))
  x)

(defmethod base64 (x)
  (base64 (bev x)))

;; -------------------------------------------------------------
;; HEX Conversions

(defun convert-int-to-hex-string (val vec)
  ;; convert int val to hex string of length sufficient to cover vec
  ;; from which val was derived.
  (declare (integer val))
  (let ((nc  (* 2 (length vec))))
    (declare (fixnum nc))
    (with-standard-io-syntax ;; cancel bignum abbrev
      (format nil "~@?" (format nil "~~~D,'0x" nc) val)
      )))

(defmethod hex ((x bev))
  (make-instance 'hex
                 :str (convert-int-to-hex-string (int x) (bev-vec x))))

(defmethod hex ((x lev))
  (make-instance 'hex
                 :str (convert-int-to-hex-string (int x) (lev-vec x))))

(defmethod hex ((x hex))
  x)

(defmethod hex ((x string))
  (assert (every (um:rcurry 'digit-char-p 16) x))
  (make-instance 'hex
                 :str x))

(defmethod hex (x)
  (hex (bev x)))

;; ------------------------------------------------------
;; LEV -- encode vector as little-endian (LSB first)

(defmethod lev ((x lev))
  x)

(defmethod lev ((x bev))
  (make-instance 'lev
   :vec (reverse (bev-vec x))))

(defmethod lev ((val integer))
  (make-instance 'lev
   :vec (convert-int-to-vec val :from-end nil)))

(defmethod lev ((x sequence))
  ;; LIST and VECTOR when can be coerced to UB8-VECTOR
  (make-instance 'lev
                 :vec (coerce x 'ub8-vector)))

(defmethod lev ((x ub8v-repr))
  (lev (ub8v-repr x)))

(defmethod lev ((x base58))
  ;; convert Base58 to LEV, respecting leading zeros
  (let* ((str (base58-str x))
         (val (int x))
         (nel (floor (* (length str) #.(log 58 256)))))
    (levn val nel)))

(defmethod lev ((x base64))
  ;; convert Base64 to LEV, respecting leading zeros
  (let* ((str (string-right-trim "=" (base64-str x)))
         (val (int x))
         (nel (floor (* (length str) #.(log 64 256)))))
    (levn val nel)))

(defmethod lev ((x hex))
  ;; convert HEX to LEV, respecting leading zeros
  (let* ((str  (hex-str x))
         (val  (int x))
         (nel  (floor (1+ (length str)) 2)))
    (levn val nel)))

(defmethod lev (x)
  (make-instance 'lev
                 :vec (nreverse
                       (bev-vec (bev x)))))

(defun levn (x nb)
  ;; create a LEV with a specified number of UB8 bytes
  (let* ((lev (lev x))
         (nel (length (lev-vec lev))))
    (cond ((< nel nb)
           ;; extend with zero filled tail
           (let* ((diff (- nb nel))
                  (tail (make-ub8-vector diff
                                    :initial-element 0)))
             (make-instance 'lev
                            :vec (concatenate 'ub8-vector (lev-vec lev) tail))))
          ((> nel nb)
           ;; take from the LSB side
           (make-instance 'lev
                          :vec (subseq (lev-vec lev) 0 nb)))
          (t
           lev)
          )))

;; -----------------------------------------------------------
;; BEV -- encode vector as big-endian (MSB first)

(defmethod bev ((x bev))
  x)

(defmethod bev ((x ub8v-repr))
  (bev (ub8v-repr x)))

(defmethod bev ((x lev))
  ;; conversion of LEV to BEV
  (make-instance 'bev
                 :vec (reverse (lev-vec x))))

(defmethod bev ((x sequence))
  ;; LIST and VECTOR when can be coerced to UB8-VECTOR
  (make-instance 'bev
                 :vec (coerce x 'ub8-vector)))

(defmethod bev ((x string))
  (bev (hex x)))

(defmethod bev ((x integer))
  ;; convert integer to BEV vector
  (make-instance 'bev
                 :vec (convert-int-to-vec x :from-end t)))

(defmethod bev ((x base58))
  ;; convert Base58 to BEV, respecting leading zeros
  (let* ((str  (base58-str x))
         (val  (int x))
         (nel  (floor (* (length str) #.(log 58 256)))))
    (bevn val nel)))

(defmethod bev ((x base64))
  ;; convert Base64 to BEV respecting leading zeros
  (let* ((str  (string-right-trim "=" (base64-str x)))
         (val  (int x))
         (nel  (floor (* (length str) #.(log 64 256)))))
    (bevn val nel)))

(defmethod bev ((x hex))
  ;; convert HEX to BEV, respecting leading zeros
  (let* ((str  (hex-str x))
         (val  (int x))
         (nel  (ash (1+ (length str)) -1)))
    (bevn val nel)))

(defun bevn (x nb)
  ;; construct a BEV with a specified number of UB8 bytes
  (let* ((bev  (bev x))
         (nel  (length (bev-vec bev))))
    (cond ((< nel nb)
           ;; prepend with zero filled prefix
           (let* ((diff (- nb nel))
                  (pref (make-ub8-vector diff
                                    :initial-element 0)))
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

(defun vec= (a b)
  "Easy way to do equality testing on UB8V items"
  (equalp (bev-vec a) (bev-vec b)))

;; --------------------------------------------------------------
;; Utility functions

(defun sbs (str)
  (coerce str 'simple-base-string))

