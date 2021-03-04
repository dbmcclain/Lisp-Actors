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

;; -----------------------------------------------------------------------------
;; junk PRNG - don't use this for cryptographic strength

(def-cached-var my-random-state
  #+:LISPWORKS
  (lw:make-mt-random-state t)
  #-lispworks
  (make-random-state t))

(um:defmonitor rand-crit
    ()
  (defun my-random (n)
    (critsec:critical-section
      #+:LISPWORKS
      (lw:mt-random n (my-random-state))
      #+(or :ALLEGRO :CLOZURE)
      (random n (my-random-state)))))

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


(defun sha2-file (fname)
  (let ((dig (ironclad:make-digest :sha256)))
    (ironclad:digest-file dig fname)))


(defun shad2-file (fname)
  (let ((dig (ironclad:make-digest :sha256))
        (pre (make-ub-array 64
                            :initial-element 0)))
    (safe-update-digest dig pre)
    (ironclad:digest-file dig fname)
    (let ((h  (ironclad:produce-digest dig)))
      (reinitialize-instance dig)
      (safe-update-digest dig h)
      (ironclad:produce-digest dig))))

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

;; -------------------------------------------------------

(defun ctr-drbg-int (nbits)
  (convert-bytes-to-int (ctr-drbg nbits)))

(defun random-between (lo hi)
  ;; random number in interval [lo,hi)
  (let ((rng  (abs (- hi lo)))
        (lmin (min hi lo)))
    (+ lmin (mod (ctr-drbg-int (integer-length rng))
                 rng))))

(defun field-random (base)
  (random-between 1 base))

(defun safe-field-random (base)
  ;; nondeterministic random in the range (Sqrt[base], base-Sqrt[base])
  
  ;; Safe, because if field is large enough all the values will be far
  ;; enough away from field limits (0, base) to discourage brute force
  ;; search against small positive and negative field values.
  (funcall (get-cached-symbol-data
            'safe-field-random 'safe-field-random-fn base
            (lambda ()
              (let* ((lo  (isqrt base))
                     (hi  (- base lo)))
                (lambda ()
                  (random-between lo hi))))
            )))

;; ---------------------------------------------------------

(defun mask-off (arr rembits)
  (when (plusp rembits)
    (setf (aref arr 0) (ldb (byte rembits 0) (aref arr 0))))
  arr)


