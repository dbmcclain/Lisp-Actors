;; utilities.lisp
;; DM/Acudora  11/11
;; -------------------------------------------------
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

(in-package :ecc-crypto-b571)



;;;; Stubs for Unimplemented Functions

;;; DEFSTUB: define a stub function named FUNCTION-NAME and set property
;;; STUB-FUNCTION-P true on FUNCTION-NAME. The property setting is done at both
;;; macro-evaluation and macro-expansion times, allowing its value to be used
;;; both at compile and load times.

;;; There are a set of functions that, for purposes of code clarity, need to
;;; exist in regularly compiled code, but which shall, for the forseeable
;;; future, never actually be called and therefore need not be defined.  We are
;;; free, however, to define them as "stub" functions. A `stub function' should
;;; never be called. If it is called at runtime, its behavior is undefined in
;;; production. (It's OK for it to behave the same as in development, but that
;;; is not required and should not be relied upon.)  In development, it's highly
;;; desireable that calling a stub function should result in a runtime error
;;; being signaled.

;;; The main purpose and benefit of using defstub is to prevent the compiler
;;; from complaining about unimplemented functions every single compile, when
;;; you have no intention of ever fixing the situation in the present
;;; development period.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro stub-function-p (function-name)
  "Accessor on FUNCTION-NAME (getable, setf'able). Value either true, if
FUNCTION-NAME is a symbol that is the name of a stub function, or false (nil)
for any other symbol."
  `(get ,function-name 'stub-function-p))
)
  

(defmacro defstub (function-name)
  (unless (fboundp function-name)
    (setf (stub-function-p function-name) t) ; set both at compile and load time
    `(progn
       (setf (stub-function-p ',function-name) t)
       (defun ,function-name (&rest args)
         (declare (ignore args))
         (error "~s, a stub function, called at run time, but it should not be."
                ',function-name))
       ',function-name)))





;; -----------------------------------------------------------------------------
;; with-fast-impl (macro)

(defmacro error-running-fast-impl-function? (fast-name)
  "Accessor on a fast-impl-function name (getable, setf'able). Value can either
be nil (initially) the Lisp error condition object from a first error condition
from calling the function."
  `(get ',fast-name 'error-running-fast-impl-function))

(defun do-with-fast-impl (fast-name fast-fn slow-fn)
  (or (and (null (error-running-fast-impl-function? fast-name))
           (handler-case
               (funcall fast-fn)
             (error (error-condition)
               (progn
                 ;; Throw a bone to a developer tracking down the error: log to
                 ;; error output, and store error condition in a property on the
                 ;; function name symbol.
                 (format *error-output*
                         "!!! *** Taking function ~S out. *** !!!~%" 
                         fast-name)
                 (format *error-output*
                         "!!! ***   Error condition = ~A *** !!!~%" 
                         error-condition)
                 (format *error-output*
                         "!!! ***   Error condition type = ~S *** !!!~%"
                         (type-of error-condition)))

               ;; Consider enabling this, maybe just in development mode:
               ;; (cerror "Continue" "Error on fast-impl call of ~S" fast-name)

               (setf (error-running-fast-impl-function? fast-name)
                     error-condition)
               nil)))
      (funcall slow-fn)))

(defmacro with-fast-impl (fast-form slow-form)
  (let ((fast-name (car fast-form)))
    (if (stub-function-p fast-name)
        ;; If at expansion time we already know FAST-NAME names a stub function,
        ;; do not expand a call to it: simply emit SLOW-FORM straight inline.
        slow-form
        `(do-with-fast-impl ',fast-name
           (lambda ()
             ,fast-form)
           (lambda ()
             ,slow-form)) )))

;; -----------------------------------------------------------------------------
;; junk PRNG - don't use this for cryptographic strength

(def-cached-var my-random-state
  #+:LISPWORKS
  (lw:make-mt-random-state t)
  #-lispworks
  (make-random-state t))

(um:defmonitor
    ((my-random (n)
       #+:LISPWORKS
       (lw:mt-random n (my-random-state))
       #+(or :ALLEGRO :CLOZURE)
       (random n (my-random-state)))
     ))

;; -----------------------------------------------------------------------------
;;

(deftype ubyte ()
  `(unsigned-byte 8))

(deftype ub-vector (nb)
  `(vector ubyte ,nb))

(defun make-ub-array (nb &rest keys)
  (apply #'make-array nb :element-type 'ubyte keys))

;; -----------------------------------------------------------------------------
;;

(defun safe-char-code (c)
  (let ((v (char-code c)))
    (if (<= 0 v 255)
        v
      (char-code #\?)) ))

(defmethod convert-text-to-int8-array ((str string))
  (map-into (make-ub-array (length str)) #'safe-char-code str))

(defmethod convert-text-to-int8-array (x)
  x)

#|
(defun convert-bytes-to-int (bytes)
  (let* ((bytes (coerce bytes 'vector))
         (len (length bytes))
         (ans 0))
    (loop for ix from 0 below len
          do
          (setf ans (logior (ash ans 8) (aref bytes ix))))
    ans))
|#

(defmethod ensure-8bitv ((x integer))
  (ensure-8bitv (convert-int-to-bytes x)))

(defmethod ensure-8bitv ((s string))
  (convert-text-to-int8-array s))

(defmethod ensure-8bitv ((v sequence))
  (or (ignore-errors
        (coerce v '(ub-vector *)))
      (call-next-method)))

(defmethod ensure-8bitv ((s symbol))
  (ensure-8bitv (string s)))

(defmethod ensure-8bitv ((p pathname))
  (ensure-8bitv (namestring p)))

(defmethod ensure-8bitv (x)
  (ensure-8bitv (loenc:encode x)))

;; handy output defn
#||#
(defun hexit (x)
  (let ((*print-length* nil))
    (write x :base 16)))
#||#
(defun hex ()
  (setf *print-base* 16))

(defun octal ()
  (setf *print-base* 8))

(defun binary ()
  (setf *print-base* 2))

(defun decimal ()
  (setf *print-base* 10))

(defun big32 (&rest args)
  ;; assume 8-xdigit groups (32-bits)
  (let ((ans 0))
    (loop for arg in args do
          (setf ans (logior (ash ans 32)
                            arg)))
    ans))


(defun convert-string-to-bytes (str)
  (map 'vector #'char-code str))

(defun convert-bytes-to-string (bytes)
  (map 'string #'code-char bytes))

(defun convert-int-to-nbytes (x n)
  (do ((x   x   (ash x -8))
       (ix  0   (1+ ix))
       (ans nil))
      ((>= ix n) ans)
    (push (ldb (byte 8 0) x) ans)))

(defun convert-int-to-nbytesv (x n)
  (ensure-8bitv (convert-int-to-nbytes x n)))

(defun convert-int-to-bytes (x)
  (do ((ans nil)
       (x  x   (ash x -8))
       (nb (ceiling (integer-length (max 1 (abs x))) 8) (1- nb)))
      ((zerop nb) ans)
    (push (ldb (byte 8 0) x) ans)))

(defmethod convert-bytes-to-int ((lst list))
  (let ((ans 0))
    (loop for byte in lst do
          (setf ans (logior (ash ans 8) byte)))
    ans))

(defmethod convert-bytes-to-int ((v vector))
  (convert-bytes-to-int (coerce v 'list)))

(defun fragmentize (x &key (size 4))
  (let* ((lst (convert-int-to-bytes x))
         (rem (rem (length lst) size)))
    (when (plusp rem)
      (setf lst (append (make-list (- size rem) :initial-element 0) lst)))
    (mapcar #'convert-bytes-to-int
            (um:group lst size))) )

(defun format-fragments (stream x &key (size 4))
  (format stream "(big32 ~{#x~8,'0x~^ ~} )" (fragmentize x :size size)))

(defun strengthen-key (arr n)
  ;; key strengthening
  (let ((ans (make-ub-array 32
                         :initial-element 0)))
    (loop repeat n do
          (replace ans (sha2-buffers ans arr)))
    #|
    (loop repeat n do
          (let ((dig (ironclad:make-digest :sha256)))
            (ironclad:update-digest dig ans)
            (ironclad:update-digest dig arr)
            (replace ans (ironclad:produce-digest dig))))
    |#
    ans))

(defun make-key-from-plaintext (text)
  (convert-bytes-to-int (strengthen-key (convert-text-to-int8-array text) 8192)))

(defun print-c-array (arr)
  (format t "{ ~{0x~2,'0x~^, ~} };" (coerce arr 'list)))

;; -----------------------------------------------------------------------------
;;

(defun encode-bytes-to-base64 (bytes)
  (with-output-to-string (s)
    (s-base64:encode-base64-bytes bytes s)))
 
(defun encode-object-to-base64 (obj)
  (encode-bytes-to-base64 (loenc:encode obj)))

(defun decode-bytes-from-base64 (str)
  (subseq (with-input-from-string (s str)
            (s-base64:decode-base64-bytes s))
          0))
  
(defun decode-object-from-base64 (str)
  (loenc:decode (decode-bytes-from-base64 str)))

;; -----------------------------------------------------------------------------
;;

(defun convert-lev-to-int (bytes)
  (let ((ans 0))
    (loop for b across bytes
          for pos from 0 by 8
          do
          (setf ans (dpb b (byte 8 pos) ans)))
    ans))

(defun convert-int-to-lev (val &optional nb)
  (let* ((nb (or nb
                 (ceiling (integer-length val) 8)))
         (ans (make-array nb
                          :element-type '(unsigned-byte 8))))
    (loop for ix from 0 below nb
          for pos from 0 by 8
          do
          (setf (aref ans ix) (ldb (byte 8 pos) val)))
    ans))
                         
(defun encode-bytes-to-base58 (bytes)
  (vec-repr:base58 (vec-repr:lev bytes)))
 
(defun encode-object-to-base58 (obj)
  (encode-bytes-to-base58 (loenc:encode obj)))

(defun decode-bytes-from-base58 (str &optional nb)
  (declare (ignore nb))
  (vec-repr:lev (make-instance 'vec-repr:base58
                             :str str)))
  
(defun decode-object-from-base58 (str)
  (loenc:decode (decode-bytes-from-base58 str)))

;; --------------------------------------------------

(defun format-bytes (bytes &optional stream)
  (let ((grps (um:group (coerce bytes 'list) 32)))
    (format stream "~{~{~2,'0X~^~}~^~%~}" grps)))

(defun read-blob (blob)
  (let* ((blob  (remove-if (complement (um:rcurry #'digit-char-p 16)) blob))
         (len   (length blob)))
    (when (oddp len)
      (error "Invalid blob"))
    (let* ((alen (truncate len 2))
           (buf  (make-string 4))
           (enc  (make-ub-array alen)))
      (setf (char buf 0) #\#
            (char buf 1) #\x)
      (loop for ix from 0 below alen
            for jx from 0 by 2
            do
            (setf (aref buf 2)  (char blob jx)
                  (aref buf 3)  (char blob (1+ jx))
                  (aref enc ix) (read-from-string buf)))
      enc)))

;; -----------------------------------------------------------------------------
;;

(defun write-int (v nb stream)
  (write-sequence (convert-int-to-nbytes v nb) stream))

(defun write-point (pt stream)
  (write-int (ecc-pt-x pt) 72 stream)
  (write-int (ecc-pt-y pt) 72 stream))

(defun write-vector (v stream)
  (write-int (length v) 4 stream)
  (write-sequence v stream))

(defun write-cp-string (s stream)
  (write-byte (length s) stream)
  (write-sequence (convert-text-to-int8-array s) stream))

(defun write-sequences (stream &rest seqs)
  (dolist (seq seqs)
    (write-sequence seq stream)))

;; -----------------------------------------------------------

(defun make-cp-string-vector (s)
  (let* ((nb  (length s))
         (v   (make-ub-array (1+ nb))))
    (setf (aref v 0) (length s))
    (loop for ix from 0 below nb
          for jx from 1
          do
          (setf (aref v jx) (safe-char-code (char s ix))))
    v))

(defun read-nvector (nb stream)
  (let ((ans (make-ub-array nb)))
    (read-sequence ans stream)
    ans))

(defun read-int (nb stream)
  (convert-bytes-to-int (read-nvector nb stream)))

(defun read-point (stream)
  (let* ((x (read-int 72 stream))
         (y (read-int 72 stream)))
    (make-ecc-pt
     :x x :y y)))

(defun read-vector (stream)
  (read-nvector (read-int 4 stream) stream))

(defun read-cp-string (stream)
  (let* ((nb    (read-byte stream))
         (bytes (read-nvector nb stream))
         (str   (make-string nb)))
    (map-into str #'code-char bytes)
    str))

(defun read-sequences (stream &rest seqs)
  (dolist (seq seqs)
    (read-sequence seq stream)))

;; -----------------------------------------------------------------------------
;;

(defun wipe (&rest objs)
  (dolist (obj objs)
    (wipe-obj obj)))

(defmethod wipe-obj ((s sequence))
  (fill s 0))

(defmethod wipe-obj ((s string))
  (fill s #\null))

(defmethod wipe-obj (x)
  x)

(defmethod wipe-obj ((d ironclad:sha256))
  (reinitialize-instance d))

(defmethod wipe-obj ((c ironclad:aes))
  (reinitialize-instance c))

#+:COM.RAL
(defmethod wipe-obj ((c ironclad:aesx))
  (reinitialize-instance c))

(defmethod wipe-obj ((c ironclad:twofish))
  (reinitialize-instance c))

;; -----------------------------------------------------------------------------
;;

(defun need-ubyte-vector (v)
  (assert (typep v '(ub-vector *))))

(defun safe-update-digest (dig &rest vs)
  (dolist (v vs)
    (need-ubyte-vector v)
    (ironclad:update-digest dig v)))

(defun safe-update-hmac (hmac &rest vs)
  (dolist (v vs)
    (need-ubyte-vector v)
    (ironclad:update-hmac hmac v)))

(defmethod safe-encrypt-in-place ((cipher ironclad::cipher) &rest vs)
  (dolist (v vs)
    (need-ubyte-vector v)
    (ironclad:encrypt-in-place cipher v)))

(defmethod safe-decrypt-in-place ((cipher ironclad::cipher) &rest vs)
  (dolist (v vs)
    (need-ubyte-vector v)
    (ironclad:decrypt-in-place cipher v)))

;; -------------------------------------------------
;; AES-256/CBC Encrypted Payloads

(defun convert-hashint-to-32bytes (x)
  (let ((ans (make-ub-array 32
                            :initial-element 0)))
    (loop for ix from 31 downto 0 do
          (setf (aref ans ix) (ldb (byte 8 0) x)
                x             (ash x -8)))
    ans))


#-:COM.RAL
(defstub sha2_file)

#+:LISPWORKS
(defun fast-sha2-file (fname)
  (fli:with-dynamic-foreign-objects ()
    (let ((carr (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 32))
          (ans  (make-ub-array 32)))
      (unless (zerop (sha2_file (namestring fname) carr))
        (error "File error"))
      (loop for ix from 0 below 32 do
            (setf (aref ans ix)
                  (fli:dereference carr :index ix)))
      ans)))

#-:LISPWORKS
(defstub fast-sha2-file)
  

(defun sha2-file (fname)
  (with-fast-impl
   (fast-sha2-file fname)
   (let ((dig (ironclad:make-digest :sha256)))
     (ironclad:digest-file dig fname))))


#-:COM.RAL
(defstub shad2_file)

#+:LISPWORKS
(defun fast-shad2-file (fname)
  (fli:with-dynamic-foreign-objects ()
    (let ((carr (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 32))
          (ans  (make-ub-array 32)))
      (unless (zerop (shad2_file (namestring fname) carr))
        (error "File error"))
      (loop for ix from 0 below 32 do
            (setf (aref ans ix)
                  (fli:dereference carr :index ix)))
      ans)))

#-:LISPWORKS
(defstub fast-shad2-file)
  
(defun shad2-file (fname)
  (with-fast-impl
   (fast-shad2-file fname)
   (let ((dig (ironclad:make-digest :sha256))
         (pre (make-ub-array 64
                             :initial-element 0)))
     (safe-update-digest dig pre)
     (ironclad:digest-file dig fname)
     (let ((h  (ironclad:produce-digest dig)))
       (reinitialize-instance dig)
       (safe-update-digest dig h)
       (ironclad:produce-digest dig)))))

;; -----------------------------------------------------
(defun sha2d-file (fname)
  (let* ((dig  (ironclad:make-digest :sha256))
         (hash (sha2-file fname)))
    (safe-update-digest dig hash)
    (ironclad:digest-file dig fname)))

(defun sha2-key (fname)
  (convert-bytes-to-int (sha2-file fname)))

(defun sha2-buffers (&rest bufs)
  (let ((dig  (ironclad:make-digest :sha256)))
    (apply #'safe-update-digest dig bufs)
    (ironclad:produce-digest dig)))

(defun sha_d-256 (msg)
  (let ((dig (ironclad:make-digest :sha256))
        (m   (ensure-8bitv msg))
        (pre (make-ub-array 64
                              :initial-element 0)))
    (safe-update-digest dig pre m)
    (let ((h  (ironclad:produce-digest dig)))
      (reinitialize-instance dig)
      (safe-update-digest dig h)
      (ironclad:produce-digest dig))))


;; -------------------------------------------

(defun sha3/256-file (fname)
  (let ((dig (ironclad:make-digest :sha3/256)))
    (ironclad:digest-file dig fname)))

(defun sha3-file (fname)
  (let ((dig (ironclad:make-digest :sha3)))
    (ironclad:digest-file dig fname)))

(defun sha3-buffers (&rest bufs)
  (let ((dig (ironclad:make-digest :sha3)))
    (dolist (buf bufs)
      (ironclad:update-digest dig (ensure-8bitv buf)))
    (ironclad:produce-digest dig)))

(defun sha3/256-buffers (&rest bufs)
  (let ((dig (ironclad:make-digest :sha3/256)))
    (dolist (buf bufs)
      (ironclad:update-digest dig (ensure-8bitv buf)))
    (ironclad:produce-digest dig)))

#|
(defun sha3-buffers (&rest bufs)
  (let ((dig (sha3:sha3-init :output-bit-length 512)))
    (dolist (buf bufs)
      (sha3:sha3-update dig (ensure-8bitv buf)))
    (sha3:sha3-final dig)))
|#                         
#|
  ;; Needs to load the reference implementation with test code from
  ;; keccak-reference.lisp
(keccak:test-keccak-msgkat
 #+:WINDOWS "/Users/Dave/Downloads/KeccakKAT-3/KeccakKAT/"
 #+:MACOSX  "~/Downloads/KeccakKAT/"
 (lambda (total-bits bit-rate output-bits message)
   (declare (ignore total-bits bit-rate))
   (sha3:sha3-digest-vector message :output-bit-length output-bits)))
 |#

;; -------------------------------------------

(defun basic-hash-with-protocol (hash-type &rest msgs)
  (let ((msgs (mapcar #'ensure-8bitv msgs))
        (dig  (ironclad:make-digest hash-type)))
    (apply #'safe-update-digest dig msgs)
    (ironclad:produce-digest dig)))

(defun hash-with-protocol (hash-type &rest msgs)
  ;; Produce an iterated hash: (H (H m) m)
  ;; for any protocol supported by Ironclad.
  (let ((hash (apply #'basic-hash-with-protocol hash-type msgs)))
    (apply #'basic-hash-with-protocol hash-type hash msgs)))

(defun std-hash-with-protocol (hash-type &rest msgs)
  ;; std-hash ensures standard encoding of msgs
  ;; before applying iterated hash
  (apply #'hash-with-protocol hash-type
                      (mapcar #'loenc:encode msgs)))

;; -------------------------------------------
#|
(defun hash-monetize (hash-type nbits keystr)
  ;; repeatedly hash an incremented random value and the key
  ;; until nbits leading bits of the hash are zero.
  (let* ((mxlen (* 8 (ironclad:digest-length (ironclad:make-digest hash-type))))
         (r     (convert-bytes-to-int (ctr-drbg mxlen)))
         (nb    (min nbits mxlen))
         (len   (- mxlen nb)))
    (loop for ix from 1
          do
          (let* ((msg (format nil "~A:~A:~A:~A:~A"
                              keystr
                              (string hash-type)
                              nbits
                              (get-universal-time)
                              (encode-object-to-base64 r)))
                 (hash    (basic-hash-with-protocol hash-type msg))
                 (hashint (convert-bytes-to-int hash)))
            (if (<= (integer-length hashint) len)
                (return-from hash-monetize (list msg ix hashint))
              (incf r))))
    ))

(defun check-hash-monetize (enc)
  (multiple-value-bind (start end gstart gend)
      (#~m%^.*:(.*):([0-9]+):([0-9]+):.*$% enc)
    (declare (ignore start end))
    (let* ((type  (intern (subseq enc
                                  (aref gstart 0)
                                  (aref gend 0))
                          :keyword))
           (nbits (read-from-string (subseq enc
                                            (aref gstart 1)
                                            (aref gend 1)) ))
           (hash  (basic-hash-with-protocol type enc))
           (mxlen (* 8 (length hash)))
           (hashint (convert-bytes-to-int hash))
           (len   (integer-length hashint)))
      (assert (<= len (- mxlen nbits)))
      hashint)))
|#

;; -------------------------------------------

(defun convert-int571-to-80bytes (x)
  (ensure-8bitv (convert-int-to-nbytes x 80)))

(defmacro with-sensitive-objects ((&rest names) &body body)
  `(unwind-protect
       (progn
         ,@body)
     (wipe ,@names)))

;; -------------------------------------------

(defun check-paths-not-equal (fname-in fname-out)
  (unless (probe-file fname-in)
    (error "No file named ~A" fname-in))
  (when (equalp (probe-file fname-in)
                (probe-file fname-out))
    (error "Input and output paths must differ")))

;; --------------------------------------------
;; ECC point representations -- affine and projective

(defclass ecc-infinity ()
  ())

(defvar +ecc-inf+
  (make-instance 'ecc-infinity))

;; -----------------------------

(defstruct ecc-pt
  x y)

(defclass ecc-projective-pt ()
  ((x  :accessor ecc-projective-pt-x  :initarg :x)
   (y  :accessor ecc-projective-pt-y  :initarg :y)
   (z  :accessor ecc-projective-pt-z  :initarg :z)))

(defmethod ecc-projective-pt-p ((pt ecc-projective-pt))
  t)

(defmethod ecc-projective-pt-p (pt)
  nil)
  

#-:COM.RAL
(defstub gf-random-k*)

(defun make-ecc-projective-pt (&key x y (z 1) alpha)
  (let* ((alpha   (or alpha
                      (gf-random-k*)))
         (alpha^2 (gf^2 alpha)))
    (make-instance 'ecc-projective-pt
                   :x (gf* alpha^2 x)
                   :y (gf* alpha alpha^2 y)
                   :z (gf* alpha z))))

;; ---------------------------------------------------------------------------------
;; ECC Curve Definition

(defvar *curve*  nil)

(define-symbol-macro *ecc-a*   (ecc-curve-a     *curve*))
(define-symbol-macro *ecc-b*   (ecc-curve-b     *curve*))
(define-symbol-macro *ecc-gen* (ecc-curve-gen   *curve*))
(define-symbol-macro *ecc-h*   (ecc-curve-h     *curve*))
(define-symbol-macro *ecc-r*   (ecc-curve-r     *curve*))
(define-symbol-macro *nbits*   (ecc-curve-nbits *curve*))
(define-symbol-macro $prim     (ecc-curve-gf    *curve*))
(define-symbol-macro *ecc-d*   (ecc-curve-d     *curve*))
(define-symbol-macro *ecc-s*   (ecc-curve-s     *curve*))
(define-symbol-macro *ecc-e*   (ecc-curve-e     *curve*))

;; for curves of the form:  y^2 + x*y = x^3 + a*x^2 + b
(defstruct ecc-curve
  (name    :anon)
  (parent  *curve*)
  (a       *ecc-a*)    ;; coeff of x^2
  (b       *ecc-b*)    ;; coeff of unity
  (gen     *ecc-gen*)  ;; generator point on curve
  (h       *ecc-h*)    ;; cofactor
  (r       *ecc-r*)    ;; prime order
  (nbits   *nbits*)    ;; GF nbits
  (gf      $prim)      ;; GF polynomial
  (d       *ecc-d*)    ;; random curve gen multiple
  (s       *ecc-s*)    ;; random curve translation
  (e       *ecc-e*))   ;; random curve Frobenius rotation expon

(defmacro with-ecc-curve (curve &body body)
  ;; perform body over iosmorphic curve
  `(let ((*curve* ,(if (and (consp curve)
                            (keywordp (car curve)))
                       `(make-ecc-curve ,@curve)
                     curve)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-ecc-curve" 1)

;; -----------------------------------------------------------

(defvar *nist-b571*
  (setf *curve*
        (make-ecc-curve
         :name "NIST-B571"
         :parent nil
         :nbits 571
         :gf    (logior (ash 1 571) #x425)
         :a     1
         :b     (big32 #x02F40E7E #x2221F295 #xDE297117 #xB7F3D62F
                       #x5C6A97FF #xCB8CEFF1 #xCD6BA8CE #x4A9A18AD
                       #x84FFABBD #x8EFA5933 #x2BE7AD67 #x56A66E29
                       #x4AFD185A #x78FF12AA #x520E4DE7 #x39BACA0C
                       #x7FFEFF7F #x2955727A )
         :gen   (make-ecc-pt
                 :x 
                 (big32 #x0303001D #x34B85629 #x6C16C0D4 #x0D3CD775
                        #x0A93D1D2 #x955FA80A #xA5F40FC8 #xDB7B2ABD
                        #xBDE53950 #xF4C0D293 #xCDD711A3 #x5B67FB14
                        #x99AE6003 #x8614F139 #x4ABFA3B4 #xC850D927
                        #xE1E7769C #x8EEC2D19 )
                 :y
                 (big32 #x037BF273 #x42DA639B #x6DCCFFFE #xB73D69D7
                        #x8C6C27A6 #x009CBBCA #x1980F853 #x3921E8A6
                        #x84423E43 #xBAB08A57 #x6291AF8F #x461BB2A8
                        #xB3531D2F #x0485C19B #x16E2F151 #x6E23DD3C
                        #x1A4827AF #x1B8AC15B ))
         :h     2
         ;; prime order of the base point
         ;; (r * gen = infinity )
         :r     (big32 #x03FFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
                       #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
                       #xFFFFFFFF #xE661CE18 #xFF559873 #x08059B18
                       #x6823851E #xC7DD9CA1 #x161DE93D #x5174D66E
                       #x8382E9BB #x2FE84E47 )
         :d     1
         :s     0
         :e     0)))
  
(defmacro with-b571 (&body body)
  `(with-ecc-curve *nist-b571*
     ,@body))

;; -----------------------------------------------------------

(defvar *nist-b163*
  (make-ecc-curve
   :name "NIST-B163"
   :parent nil
   :nbits 163
   :gf    #x0800000000000000000000000000000000000000C9
   :h     2
   :a     1
   :b     #x020A601907B8C953CA1481EB10512F78744A3205FD
   :r     #x040000000000000000000292FE77E70C12A4234C33
   :gen   (make-ecc-pt
           :x #x03F0EBA16286A2D57EA0991168D4994637E8343E36
           :y #x00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1)))

(defmacro with-b163 (&body body)
  `(with-ecc-curve *nist-b163*
     ,@body))

;; factors of (2^163-1)
;; 150287 * 704161 * 110211473 * 27669118297 * 36230454570129675721  (5 distinct prime factors)

;; -----------------------------------------------------------

#-:diddly
(let ((prev nil))
  (defun make-nonce ()
    (let ((next (do ((next (usec:get-universal-time-usec)
                           (usec:get-universal-time-usec)))
                    ((not (eql next (shiftf prev next))) next)
                  (sleep 0))))
      (convert-int-to-nbytesv (ash next 64) 16))))

#+:diddly
(let ((prev nil))
  (defun make-nonce ()
    (let ((next (do ((next (uuid:make-v1-uuid)
                           (uuid:make-v1-uuid)))
                    ((not (eql next (shiftf prev next))) next)
                  (sleep 0))))
      (convert-int-to-nbytesv (uuid:uuid-to-integer next) 16))))

;; --------------------------------------------------------
;; Obfuscated private key multiplication

(defun ecc-stitch (r lst)
  (let ((a (ecc-mul r (car lst))))
    (if (endp (cdr lst))
        a
      (ecc-add a (ecc-stitch a (cdr lst))))))

(defun my-eval (e)
  (funcall (compile nil `(lambda () ,e))))

(defun list-stitch-factors (key levels)
  (um:nlet iter ((e   (primes:decompose key))
                 (lvl levels))
    ;; e is an expression '(* factor1 (1+ (* factor2 ....)))
    ;; where each factor is a number
    (if (zerop lvl)
        (list (eval e))
      (destructuring-bind (star factor (oneplus subexpr)) e
        (assert (eq '* star))
        (assert (eq '1+ oneplus))
        (cons (+ *ecc-r* factor)
              (iter subexpr (1- lvl)))) )))

(defmacro ecc-mul-kpriv (arg key &key (levels 7))
  ;; key must be a number, not an expression
  `(ecc-stitch ,arg ',(list-stitch-factors key levels)))

;; -------------------------------------------------------

(defun ctr-drbg-int (nbits)
  (convert-bytes-to-int (ctr-drbg nbits)))

(defun random-between (lo hi)
  ;; random number in interval [lo,hi)
  (let ((rng  (abs (- hi lo)))
        (lmin (min hi lo)))
    (+ lmin (mod (ctr-drbg-int (integer-length rng))
                 rng))))


