;; bfly-security.lisp
;; --------------------------------------------------------------------------------------
;; Butterfly -- a system for easy distributed computing, going beyond what is available
;; in Erlang with the full power of Common Lisp.
;;
;; Copyright (C) 2008,2009 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08, 06-12/09
;; --------------------------------------------------------------------------------------

(in-package #:actors.security)

;; (pushnew :diddly *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:when-let
            um:group
            um:nlet
            um:defmonitor
            um:critical-section
            
            uuid:uuid   
            uuid:uuid-to-integer
            uuid:uuid-to-byte-array

            actors.network:intf-srp-ph2-begin
            actors.network:intf-srp-ph2-reply
            actors.network:intf-srp-ph3-begin
            actors.network:client-request-negotiation
            )))

;; --------------------------------------------
;; Encryption / Decryption Stuff...
;; --------------------------------------------

(deftype vector-ub8 ()
  `(simple-array (unsigned-byte 8) (*)))

(defun make-u8-vector (nel)
  (make-array nel :element-type '(unsigned-byte 8)))

(defun string-to-vector (str)
  (map 'vector-ub8 'char-code str))

(defun convert-vector-to-integer (v)
  (let* ((nb (length v))
         (n  0))
    (loop for iy from (* 8 (1- nb)) downto 0 by 8
          for ix from 0
          do
          (setf (ldb (byte 8 iy) n) (aref v ix)))
    n))

(defun convert-integer-to-vector (x)
  (declare (integer x))
  (let* ((nb (ceiling (integer-length x) 8))
         (v  (make-u8-vector nb)))
    (declare (fixnum nb))
    (loop for ix of-type fixnum from (* 8 (1- nb)) downto 0 by 8
          for iy of-type fixnum from 0
          do
          (setf (aref v iy) (ldb (byte 8 ix) x)))
    v))

(defun vec-n (x n)
  ;; convert integer to n-byte vector
  (declare (integer x)
           (fixnum  n))
  (let ((v (make-u8-vector n)))
    (loop for ix of-type fixnum from (* 8 (1- n)) downto 0 by 8
          for iy of-type fixnum from 0
          do
          (setf (aref v iy) (ldb (byte 8 ix) x)))
    v))
  
;; -----------------------------------------------

(defmethod integer-of ((x integer))
  x)

(defmethod integer-of ((v vector))
  (convert-vector-to-integer v))

(defmethod integer-of ((u uuid))
  (uuid-to-integer u))

(defmethod integer-of ((s string))
  (integer-of (vector-of s)))

;; ------------

(defmethod vector-of ((v vector))
  (assert (typep v '(simple-array (unsigned-byte 8) (*))))
  v)

(defmethod vector-of ((s string))
  (string-to-vector s))

(defmethod vector-of ((u uuid))
  (uuid-to-byte-array u))

(defmethod vector-of ((x integer))
  (convert-integer-to-vector x))

;; -----------------------------------------------

(defvar *base*)

(defmacro with-mod (base &body body)
  `(let ((*base* ,base))
     ,@body))

(defun m+ (x y)
  #F
  (declare (integer x y))
  (mod (+ x y) *base*))

(defun m- (x y)
  #F
  (declare (integer x y))
  (mod (- x y) *base*))

(defun m* (x y)
  #F
  (declare (integer x y))
  (mod (* x y) *base*))

(defun m/ (x y)
  #F
  (declare (integer x y))
  (m* x (minv y)))

(defun minv (x)
  #F
  (declare (integer x))
  (m^ x (- *base* 2)))

(defun m^ (x n)
  #F
  (declare (integer x n))
  (let ((nbits (integer-length n)))
    (declare (integer nbits))
    (do ((b  x    (m* b b))
         (p  1)
         (ix 0    (1+ ix)))
        ((>= ix nbits) p)
      (declare (integer b p ix))
      (when (logbitp ix n)
        (setf p (m* p b))))
    ))

;; -----------------------------------------------------------------------------
(defvar *mod128* #.(+ (ash 1 128) 385))

(defun prep-interpolation-shares (shares)
  (with-mod *mod128*
    (flet ((prep (share_i)
             (destructuring-bind (xi . yi) share_i
               (flet ((denom (prod share_j)
                        (if (eq share_i share_j)
                            prod
                          (m* prod (m- xi (car share_j))))
                        ))
                 (cons xi (m/ yi (reduce #'denom shares
                                         :initial-value 1)))
                 ))))
      (mapcar #'prep shares))))

(defun make-lagrange-interpolator (shares)
  (let ((preps (prep-interpolation-shares shares)))
    (lambda (x)
      (with-mod *mod128*
        (flet ((term (sum prep_i)
                 (flet ((factor (prod prep_j)
                          (if (eq prep_i prep_j)
                              prod
                            (m* prod (m- x (car prep_j))))
                          ))
                   (m+ sum (reduce #'factor preps
                                   :initial-value (cdr prep_i)))
                   )))
          (reduce #'term preps
                  :initial-value 0)
          )))
    ))

(defun fill-to-132 (n)
  (dpb n (byte 129 0) (random (ash 1 132))))

(defun low-129 (n)
  (ldb (byte 129 0) n))

(defun share-uuid (uuid-str)
  (let* ((k  (uuid:uuid-to-integer (uuid:make-uuid-from-string uuid-str)))
         (share1 (random #.(ash 1 128)))
         (share2 (random #.(ash 1 128)))
         (fn     (make-lagrange-interpolator `((0 . ,k)
                                               (1 . ,share1)
                                               (2 . ,share2))))
         (share3 (funcall fn 3)))
    (mapcar #'um:convert-int-to-wordlist
            (mapcar #'fill-to-132
                    (list share1 share2 share3)))))

(defun uuid-str-from-shares (share1 share2 share3)
  (let ((fn (make-lagrange-interpolator `((1 . ,share1)
                                          (2 . ,share2)
                                          (3 . ,share3)))))
    (uuid:uuid-string (uuid:integer-to-uuid (funcall fn 0)))))

(defun assemble-sks (shares)
  (apply #'uuid-str-from-shares (mapcar #'low-129
                                        (mapcar #'um:convert-wordlist-to-int shares))))

#|
(share-uuid "{cfe31464-f7b1-11ea-82f8-787b8acbe32e}")
(assemble-sks '(116556184643808452113560705861977715537
                55066560204924271324109576419163150535
                179630390834232184955478921582272058641))
|#
;; -----------------------------------------------------------------------------
;;

(defmacro def-cached-var (name creator &optional cache-name)
  (let ((cname (or cache-name (intern (format nil "*~A*" (string name))))))
    `(progn
       (defvar ,cname nil)
       (defun ,name ()
         (or ,cname
             (setf ,cname ,creator)))) ))

(editor:setup-indent "def-cached-var" 1)

;; -----------------------------------------------------------------------------

(def-cached-var ctr-hash-prng
  #-:OS-WINDOWS (ironclad:make-prng :fortuna :seed :urandom)
  #+:OS-WINDOWS (lw:make-mt-random-state t))

;; protected by a global lock
;; ctr-hash-prng is a shared mutable state
(defmonitor rand-monitor ()
  (defun rand (limit)
    (critical-section
      #-:OS-WINDOWS (ironclad:strong-random limit (ctr-hash-prng))
      #+:OS-WINDOWS (lw:mt-random limit (ctr-hash-prng)))))

(defun rand-between (lower upper)
  (declare (integer lower upper))
  ;; generate random (lower <= n < upper)
  (+ lower (rand (- upper lower))))

;; --------------------------------------------------

(defun ensure-simple-array (v)
  ;; the Ironclad crypto functions require simple arrays
  (if (or (adjustable-array-p v)
          (array-has-fill-pointer-p v)
          (array-displacement v))
      (copy-seq v)
    v))

;; -------------------------------------------------

(defclass crypto ()
  ((cypher-in    :accessor crypto-cypher-in    :initform nil)
   (cypher-out   :accessor crypto-cypher-out   :initform nil)
   (sequence-in  :accessor crypto-sequence-in  :initform 0)
   (sequence-out :accessor crypto-sequence-out :initform 0)
   (hmac-key     :accessor crypto-hmac-key     :initform (hash32 (vector-of "default HMAC key")))
   (reneg-key    :accessor crypto-reneg-key    :initform nil)
   (reneg-period :accessor crypto-reneg-period :initform #.(ash 1 63))
   (reneg-time   :accessor crypto-reneg-time   :initform (get-universal-time))
   ))

(defclass server-crypto (crypto)
  ())

(defclass client-crypto (crypto)
  ())

(defmethod init-crypto-for-input ((crypto crypto) key initv)
  (setf (crypto-cypher-in crypto)
        (ironclad:make-cipher :aes
                              :key                   key
                              :mode                  :ctr
                              :initialization-vector initv)))

(defmethod init-crypto-for-output ((crypto crypto) key initv)
  (setf (crypto-cypher-out crypto)
        (ironclad:make-cipher :aes
                              :key                   key
                              :mode                  :ctr
                              :initialization-vector initv)))

(defmethod init-crypto-for-hmac ((crypto crypto) key1 key2)
  (setf (crypto-hmac-key crypto) (hash32 key1 key2)))

(defmethod init-crypto-for-renegotiation ((crypto crypto) s)
  (setf (crypto-reneg-key    crypto) (hash32 s)
        (crypto-sequence-in  crypto) 0
        (crypto-sequence-out crypto) 0
        (crypto-reneg-period crypto) (ash (1+ (rand 32)) 27)
        (crypto-reneg-time   crypto) (get-universal-time)))

;; -----------------------------------------------------------------------

(define-condition signature-mismatch-exn (error)
  ()
  (:default-initargs
   :arg "Signature mismatch on remote connection"))

(defun signature-mismatch-error ()
  (error (make-condition 'signature-mismatch-exn)))

;; -----------------------------------------------------------------------

(defun do-with-error-logging (fn)
  (handler-bind ((error (lambda (exn)
                          (log-info :SYSTEM-LOG exn))))
    (funcall fn)))

(defmacro with-error-logging (&body body)
  `(do-with-error-logging (lambda () ,@body)))

;; --------------------------------------------

(defun hash-objects-to-digest (dig-type objs)
  (let ((dig (ironclad:make-digest dig-type)))
    (dolist (obj objs)
      (ironclad:update-digest dig (vector-of obj)))
    (ironclad:produce-digest dig)))
     
(defun hash32 (&rest objs)
  (hash-objects-to-digest :sha3/256 objs))

(defun generate-hmac (key &rest data)
  (let ((hmac (ironclad:make-hmac key :sha3/256)))
    (dolist (d data)
      (ironclad:update-hmac hmac (vector-of d)))
    (ironclad:hmac-digest hmac)))

#|
(let ((key (hash-objects-to-digest :sha3/256 '("this is a test"))))
  (loop repeat 1000 do
        (generate-hmac key "this is another test")))
 |#
;; -------------------------------------------------------------
;; Secure encrypted read/write

#|
  A secure data packet.
  Sequence number is internal count of encrypted packet data octets sent.

  Prefix (unencrypted):
  +------------------------------------+ 0
  | prefix count (4 bytes BigEndian)   |    ;; count of packet data bytes, excluding itself and HMAC
  +------------------------------------+ 4

  Data (beneath AES-256/CTR encryption):
  +------------------------------------+ 0
  | Encoded Data (n-Bytes)             |   ;; self-clocking serialization
  +------------------------------------+ n
  | zero padding to fill out block     |
  +------------------------------------+ n + npad = 16*N

  Authentication (unencrypted):
  +------------------------------------+ 0
  | SHA3/256 HMAC (32 bytes)           |    ;; HMAC includes prefix count, sequence number, encrypted data
  +------------------------------------+ 32
|#

;; --------------------------------------------------

;; ------------------------------------------------------------
;; for LZW Compression of plaintext
#|

#|
(declaim (ftype (function (vector vector &optional fixnum fixnum) vector)
                vector-append))
|#
(defun vector-append (old new &optional (start2 0) end2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (prog1 old                     
    (let* ((old-fill (fill-pointer old))
           (new-fill (+ old-fill (length new))))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (replace old new :start1 old-fill :start2 start2 :end2 end2))))

#|
(declaim (ftype (function (vector t) vector) vector-append1))
|#
(defun vector-append1 (old new)
  (prog1 old                     
    (let* ((old-fill (fill-pointer old))
           (new-fill (1+ old-fill)))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (setf (aref old old-fill) new))))

#|
(declaim (ftype (function (&optional t) vector) make-empty-vector))
|#
(defun make-empty-vector (&optional (element-type t))
  (make-array 0 :element-type element-type :fill-pointer 0 :adjustable t))
 

#|
(declaim (ftype (function (t &optional t) vector) make-vector-with-elt))
|#
(defun make-vector-with-elt (elt &optional (element-type t))
  (make-array 1 :element-type element-type
                :fill-pointer 1
                :adjustable t
                :initial-element elt))

#|
(declaim (ftype (function (vector t) vector) vector-append1-new))
|#
(defun vector-append1-new (old new)
  (vector-append1 (vector-append (make-empty-vector 'octet) old)
                  new))

#|
(declaim (ftype (function (vector vector) vector) vector-append-new))
|#
(defun vector-append-new (old new)
  (vector-append (vector-append (make-empty-vector 'octet) old)
                 new))
 
(deftype octet () '(unsigned-byte 8))

#|
(declaim (ftype (function () hash-table) build-dictionary))
|#
(defun build-dictionary ()
  (let ((dictionary (make-hash-table :test #'equalp)))
    (loop for i below 256
          do (let ((vec (make-vector-with-elt i 'octet)))
               (setf (gethash vec dictionary) vec)))
    dictionary))

#|
(declaim (ftype (function ((vector octet)) (vector octet))
                lzw-compress-octets))
|#
(defun lzw-compress-octets (octets)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop with dictionary-size of-type fixnum = 256
        with w = (make-empty-vector 'octet)
        with result = (make-empty-vector 't)
        with dictionary = (build-dictionary)
        for c across octets
        for wc = (vector-append1-new w c)
        if (gethash wc dictionary) do (setq w wc)
        else do
          (vector-append result (gethash w dictionary))
          (setf (gethash wc dictionary)
                (make-vector-with-elt dictionary-size)) 
          (incf dictionary-size)
          (setq w (make-vector-with-elt c 'octet))
        finally (unless (zerop (length (the (vector octet) w)))
                  (vector-append result (gethash w dictionary)))
                (return result)))

#|
(declaim (ftype (function (vector) (vector octet)) lzw-decompress))
|#
(defun #1=lzw-decompress (octets)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (zerop (length octets))
    (return-from #1# (make-empty-vector 'octet)))
  (loop with dictionary-size = 256
        with dictionary = (build-dictionary)
        with result = (make-vector-with-elt (aref octets 0) 'octet)
        with w = (copy-seq result)
        for i from 1 below (length octets)
        for k = (make-vector-with-elt (aref octets i) 't)
        for entry = (or (gethash k dictionary)
                        (if (= (aref k 0) dictionary-size)
                            (vector-append1-new w (aref w 0))
                          (error "bad compresed entry at pos ~S" i)))
        do (vector-append result entry)
           (setf (gethash (make-vector-with-elt dictionary-size) dictionary)
                 (vector-append1-new w (aref entry 0)))
           (incf dictionary-size)
           (setq w entry)
        finally (return result)))

(defgeneric lzw-compress (datum)
  #|
  (:method ((string string))
    (lzw-compress
     (babel:string-to-octets string)))
  |#
  (:method ((octets vector))
    (lzw-compress-octets octets)))

#|
(defun lzw-decompress-to-string (octets)
  (babel:octets-to-string (lzw-decompress octets)))
|#

#|
(defun test (string)
  (assert (equal #2=(lzw-decompress-to-string (lzw-compress string)) string) ()
          "Can't compress ~S properly, got ~S instead" string #2#)
  t)
|#

;; -------------------------------------------------------------------------

(defun cvt-intvec-to-octets (v)
  ;; convert vector of integers to vector of octets using 7-bit
  ;; encodings so that numbers >= 128 become a sequence of 7-bit
  ;; sections with hi-bit set until the final section.  If bit pattern
  ;; of number is: #b1XXX XXXX YYY YYYY ZZZ ZZZZ, then output becomes
  ;; the sequence: #b1XXX XXXX #b1YYY YYYY #b0ZZZ ZZZZ
  (ubstream:with-output-to-ubyte-stream (s)
    (loop for x across v do
          (cond ((> x 127)
                 (write-sequence
                  (nlet iter ((x     x)
                              (hibit 0)
                              (ans   nil))
                    (let ((acc  (cons (logior hibit (logand x 127))
                                      ans))
                          (xshf (ash x -7)))
                      (if (plusp xshf)
                          (go-iter xshf #x80 acc)
                        acc)) )
                  s))
                
                (t (write-byte x s))))
    ))

(defun cvt-octets-to-intvec (v)
  ;; convert vector of octets from 7-bit encodings to vector of integers.
  ;; 7-bit values remain as they are. A sequence of octets with hi-bit set
  ;; indicate an integer encoding in 7-bit sections.
  ;; the sequence: #b1XXX XXXX #b1YYY YYYY #b0ZZZ ZZZZ becomes the integer
  ;; with bit pattern: #b1XXX XXXX YYY YYYY ZZZ ZZZZ
  (let ((acc 0)
        (ans (make-empty-vector 't)))
    (loop for x across v do
          (setf acc (logior (ash acc 7) (logand x 127)))
          (unless (> x 127)
            (vector-append1 ans acc)
            (setf acc 0)))
    ans))

;; --------------------------------------------------

(defun encode (obj &rest args)
  (cvt-intvec-to-octets (lzw-compress (apply #'loenc:encode obj args))))

(defun decode (data)
  (loenc:decode (lzw-decompress (cvt-octets-to-intvec data))))
|#  
;; --------------------------------------------------

(defconstant +MAX-FRAGMENT-SIZE+ 65536)

(defun byte-encode-obj (obj)
  (loenc:encode
   (lzw:zl-compress obj)))

(defun byte-decode-obj (vec)
  (multiple-value-bind (v e)
      (ignore-errors 
        (lzw:decompress
         (loenc:decode vec)))
    (if e
        (progn
          ;; (setf *bad-data* (copy-seq data))
          `(actor-internal-message:discard))
      ;; else
      v)))
  
(defun insecure-prep (obj)
  ;; Encode an object for network transmission and split into
  ;; a list of chunks, each with length below max transfer size.
  ;; Each chunk is prefixed with :FRAG or :LAST-FRAG.
  ;;
  ;; Leave some room (64 bytes) on the max fragment size, for the
  ;; prefixed re-encodings.
  (let ((enc    (byte-encode-obj obj))
        (maxlen (load-time-value
                 (- +max-fragment-size+ 64)
                 t)))
    (um:accum acc
      (nlet iter ((len   (length enc))
                  (start 0))
        (when (plusp len)
          (let* ((nel    (min len maxlen))
                 (end    (+ start nel))
                 (rem    (- len nel))
                 (frag   (subseq enc start end))
                 (msg    (if (plusp rem)
                             'actor-internal-message:frag
                           'actor-internal-message:last-frag))
                 (packet (loenc:encode (list msg frag)
                                       :align 16)))
            (acc packet)
            (go-iter rem end)
            )))
      )))

(defun secure-encoding (crypto obj)
  ;; Encode an object for network transmission. Object encoding will
  ;; be split into packets, each with size smaller than max transfer
  ;; size, and each packet will be prefixed with a 4-byte length, and
  ;; suffixed with an HMAC on the encrypted packet.
  ;;
  ;; An internal seaquence counter keeps a cumulative total on the
  ;; number of packet bytes transmitted. This sequence is used in
  ;; deriving the HMAC, but not transmitted. The receiver should be
  ;; keeping its own cumulative byte count to use in checking the HMAC
  ;; on each packet.
  (with-accessors ((cipher       crypto-cypher-out)
                   (sequence-out crypto-sequence-out)
                   (hmkey        crypto-hmac-key)) crypto
    (um:accum acc
      (um:foreach (lambda (frag)
                    (let* ((len (length frag))
                           (cnt (vec-n len 4)))
                      (when cipher
                        (ironclad:encrypt-in-place cipher frag))
                      (incf sequence-out len)
                      (let ((hmac (generate-hmac hmkey
                                                 cnt
                                                 (vec-n sequence-out 8)
                                                 frag)))
                        (acc cnt)
                        (acc frag)
                        (acc hmac)
                        )))
                  (insecure-prep obj)))
    ))

;; --------------------------------------------------

(defun insecure-decoding (enc-buf)
  (multiple-value-bind (v e)
      (ignore-errors 
        (loenc:decode enc-buf))
    (if e
        (progn
          ;; (setf *bad-data* (copy-seq data))
          `(actor-internal-message:discard))
      ;; else
      v)))

(defun secure-decoding (crypto len len-buf enc-buf hmac-buf)
  (with-accessors ((cipher      crypto-cypher-in)
                   (sequence-in crypto-sequence-in)
                   (hmkey       crypto-hmac-key)) crypto
    (let ((hmac (generate-hmac hmkey
                               len-buf
                               (vec-n (+ len sequence-in) 8)
                               enc-buf)))
      (cond
       ((equalp hmac hmac-buf)
        (incf sequence-in len)
        (when cipher
          (ironclad:decrypt-in-place cipher enc-buf))
        (insecure-decoding enc-buf))

       (t
        `(actor-internal-message:discard))
       ))))

;; -----------------------------------------------------------------
;; Primes checking...

(defun factor-out (number divisor)
  "Return two values R and E such that NUMBER = DIVISOR^E * R,
  and R is not divisible by DIVISOR."
  #F
  (declare (integer number divisor))
  (do ((e 0 (1+ e))
       (r number (/ r divisor)))
      ((/= (mod r divisor) 0) (values r e))
    (declare (integer e r)) ))

(defun perfect-square? (c)
  (let ((r (isqrt c)))
    (= c (* r r))))

#|
(defun perfect-square? (c)
  ;; if c actually is a perfect square, this can often fail anyway
  (let* ((n  (integer-length c))
         (m  (ceiling n 2)))
    (do ((x  (rand-between (ash 1 (1- m)) (ash 1 m))
             (truncate (+ c (* x x)) (* 2 x))))
        ((< (* x x) (+ c (ash 1 m)))
         (values (= c (* x x)) x))
      )))
|#

(defun jacobi-symbol (a n)
  (let ((a (mod a n)))
    (cond
     ((or (= 1 a)
          (= 1 n))
      1)
     
     ((zerop a) 0)
     
     (t (multiple-value-bind (a1 e) (factor-out a 2)
          (let* ((rem (mod n 8))
                 (s   (cond
                       ((evenp e) 1)
                       ((or (= 1 rem)
                            (= 7 rem)) 1)
                       ((or (= 3 rem)
                            (= 5 rem)) -1))))
            (when (and (= 3 (mod n 4))
                       (= 3 (mod a1 4)))
              (setf s (- s)))
            (* s (jacobi-symbol (mod n a1) a1)) )))
     )))
                 
(defun probabilistic-lucas-test (c)
  (if (or (evenp c)
          (perfect-square? c))
      nil
    (let* ((d (loop for sign = 1 then (- sign)
                    for d = 5 then (* sign (+ 2 (abs d)))
                    for jac = (jacobi-symbol d c)
                    do
                    (cond ((zerop jac) (return-from probabilistic-lucas-test nil))
                          ((= -1 jac)  (return d)))))
           (k  (1+ c)))

      (labels ((half-mod (x)
                 (mod (ash (if (oddp x)
                               (+ x c)
                             x)
                           -1)
                      c)))

        (nlet iter ((u  1)
                    (v  1)
                    (ix (- (integer-length k) 2)))
          (if (minusp ix)
              (zerop u)
            (let ((utmp (with-mod c
                          (m* u v)))
                  (vtmp (half-mod (+ (* v v) (* d u u)))))
              (if (logbitp ix k)
                  (go-iter (half-mod (+ utmp vtmp))
                           (half-mod (+ vtmp (* d utmp)))
                           (1- ix))
                (go-iter utmp vtmp (1- ix))) ))) ))))
                
(defun make-strong-miller-rabin-liar-test (p)
  #F
  (declare (integer p))
  (let ((p-1 (1- p)))
    (declare (integer p-1))
    (multiple-value-bind (d s) (factor-out p-1 2)
      ;; p-1 = d * 2^s, d odd
      (declare (integer d s))
      
      (lambda (a)
        ;; return true if a not not witness a composite value for p
        (declare (integer a))
        (with-mod p
          (let ((x (m^ a d)))
            (declare (integer x))
            (or (= x 1)
                (loop repeat s
                      for y of-type integer = x then (the integer (m* y y))
                    thereis (= y p-1)))
            ))))))

(defun is-prime? (p &optional (k 40))
  ;; Probabilistic Miller-Rabin test.
  ;; false positive < 1/4^k
  ;; return NIL if p found to be composite
  #F
  (declare (integer p k))
  (and
   (oddp p)
   (> p 2)
   (let ((strong-liar? (make-strong-miller-rabin-liar-test p)))
     (loop repeat k
           for witness of-type integer = (rand-between 2 p)
           always (funcall strong-liar? witness)) )
   (probabilistic-lucas-test p)) )

;; ---------------------------------------------------------------------------
;; Validation tests for public keys, prime bases, prime generators

(defun check-prime-modulus (p)
  (unless (>= (integer-length p) 1024)
    (error "Big prime N not large enough"))
  (unless (is-prime? p)
    (error "Big prime N not prime"))
  (let ((q (truncate (1- p) 2)))
    ;; check that P = 2*Q+1, Q prime
    (unless (is-prime? q)
      (error "Large cofactor Q not prime")) ))

(defun check-generator (g p)
  ;; assumes P = 2*Q+1, P prime, Q prime
  (with-mod p
    (unless (< 1 g (1- p))
      (error "Prime group generator out of range (1 < g < (P-1)"))
    (unless (and (/= 1 (m^ g (truncate (1- p) 2)))
                 (/= 1 (m^ g 2)))
      (error "Prime group generator not of order P"))
    (unless (> (* 2 (integer-length g)) (integer-length p))
      (error "Generator too small"))))

(defun check-public-key (k p)
  (unless (< 1 k p)
    (error "Public key out of range (1 < Key < N)")))

;; -------------------------------------------------------------
;; Anonymous SRP-6
#|
  Client                        Server
  ------                        ------
  If renegotiating,
    send Renegotiation-Key
    instead of Node-ID
    
             --- Node-ID ---->
                                 Randomly select P,G from list of available keys
                                 Check valid prime P
                                 Check valid generator G
                                 If renegotiating, check Node-ID = Renegotiation-Key
                                 Salt = random mod P
                                 x    = H(Salt,Node-ID,Pass-Phrase)
                                         repeat salt, x if x = 0
                                 v    = G^x mod P
                                 b    = 1 < random < P-1
                                 B    = 3*v + G^b mod P
                                      (it is possible that B = 0, so validity checking
                                       should check G^b)
           <--- (P G Salt B) ---
                
  Check valid prime P
  Check valid generator G
  x  = H(Salt,Node-ID,Pass-Phrase)
  Check x /= 0
  B' = B - 3*G^x mod P
  Check valid D-H key B'
  a  = 1 < random < P-1
  A  = G^a
  u  = H(A,B)
         repeat a, A, u if u = 0
  S  = B'^(a + u*x) mod P
  M1 = H(A,B,S)

             --- (A M1) --->
             
                                 u = H(A,B)
                                 Check valid D-H key A
                                 Check u /= 0
                                 S = (A*v^u)^b mod P
                                 Check M1 = H(A,B,S)
                                 M2 = H(A,M1,S)
             <--- M2 ----
             
  Check M2 = H(A,M1,S)
  Key-In   = H(A,S,B)            Key-In   = H(B,S,A)
  Key-Out  = H(B,S,A)            Key-Out  = H(A,S,B)
  HisInitv = H(M1,S) 0:16        HisInitv = H(M2,S) 0:16    
  MyInitv  = H(M2,S) 0:16        MyInitv  = H(M1,S) 0:16
  HMAC-Key = H(H(M1,S)|H(M2,S))  HMAC-Key = H(H(M1,S)|H(M2,S))
  Renegotiation-Key = H(S)       Renegotiation-Key = H(S)

      Hash H = SHA3/256 (32 bytes)
      
      Handshake completed, continue with encrypted I/O:
        AES256/CTR(Key-Out, MyInitv,  msg)  -->  (out)
        AES256/CTR(Key-In,  HisInitv, msg)  <--  (in)
        Each packet uses HMAC-SHA3/256(H(H(M1,S)|H(M2,S)), msgLen | seqNo | Encryped(msg))
        HMAC based on running 64-bit sequence number that counts cumulative bytes transfered in messages.
        No decryption on input attempted unless HMACs agree.

        After every message: seqNo = seqNo + msgLen.
        Whenever either reader sees its seqNo (= bytes transfered) > renegotiation-period, then crypto is
          renegotiated with Renegotiation-Key used in place of Node-ID above,
          and value is verified on server side.
        Renegotiation periods are computed as random multiples of 128 MB up to 4 GB,
          and are derived independently by client and server. Either side may initiate a renegotiation.
        Initial handshake negotiation is in cleartext. Renegotiations are performed under
          pre-existing encryption.

Checks:
  Valid prime P:
    log2(P) >= 1024
    check prime P
    check cofactors of (P-1): should be 2 and Q prime

  Valid generator G: (assumes P = 2*Q+1, Q prime)
    check 1 < G < P-1
    check 1 /= G^2 mod P and 1 /= G^((P-1)/2) mod P  (G is generator of (P-1)-order subgroup)
    check 2*log2(G) > log2(P) to give good bit mixing
    
  Valid D-H Key:
    check 1 < Key < P-1

|#  
;; -------------------------------------------------------------

(defvar $VERSION
  "Actors 2021/02/27 13:13:54Z. Copyright (C) 2008-2021 by Refined Audiometrics Laboratory. All rights reserved.")

(defun srp6-x (salt node-id)
  (integer-of
   (hash32 salt
           node-id
           $VERSION)))

(defun srp6-u (aa bb)
  (integer-of
   (hash32 aa bb)))

(defun unexpected (msg)
  (error "Unexpected Message: ~A" msg))

;; ---------------------------------------------------------------------------

(defmethod client-negotiate-security ((crypto client-crypto) intf)
  ;; Phase-I: send local node ID
  (let* ((node-id (or (crypto-reneg-key crypto)
                      (machine-instance))))
    (=wait ((p-key g-key salt bb) :timeout 5)
        (client-request-negotiation intf =wait-cont node-id)
      ;; Phase-II: receive N,g,s,B
      ;; Compute: x = H32(s,ID,PassPhrase)
      ;;          a = 1 < random < N
      ;;          A = g^a mod N
      ;;          u = H32(A,B)
      ;;          S = (B - 3*g^x)^(a+u*x) mod N
      ;;          M1 = H32(A,B,S)
      ;; Send A,m1
      ;; Hold as secret: x, a, u, S
      
      (check-prime-modulus p-key)
      (check-generator     g-key p-key)

      (with-mod p-key
        ;; Public key B might not be a *valid* public key.
        ;; But we don't use it directly.
        ;; Do the subtraction first, then check for validity.
        (let* ((x   (srp6-x salt node-id))
               (bbb (m- bb
                        (m* 3
                            (m^ g-key x)))
                    ))
          
          (check-public-key bbb p-key)
          (when (zerop x)
            (error "Invalid salt"))
          
          (multiple-value-bind (a aa u)
              ;; can't permit u = 0
              (loop for a  = (rand-between 2 p-key)
                    for aa = (m^ g-key a)
                    for u  = (srp6-u aa bb)
                    do
                    (unless (zerop u)
                      (return (values a aa u))))
            
            (let* ((s  (m^ bbb
                           (m+ a
                               (m* u x))))
                   (m1 (hash32 aa bb s)))
              (=wait ((m2) :timeout 5)
                  (funcall (intf-srp-ph2-reply intf) =wait-cont aa m1)
                ;; Phase 3: receive M2
                ;; Compute: Chk2  = H32(A,M1,S), check Chk2 = M2
                ;;          Key   = H32(S) 
                ;;          InitV = H32(M2,S)
                ;; Init crypto with Key, InitV
                (let* ((chk2      (hash32 aa m1 s))
                       (key-in    (hash32 aa s bb))
                       (key-out   (hash32 bb s aa))
                       (his-initv (hash32 m1 s))
                       (my-initv  (hash32 m2 s)))
                  
                  (unless (equalp chk2 m2)
                    (signature-mismatch-error))
                  
                  (init-crypto-for-hmac   crypto his-initv my-initv)
                  (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                  (init-crypto-for-output crypto key-out (subseq my-initv 0 16))
                  (init-crypto-for-renegotiation crypto s)
                  ))
              )))))
    ))

;; ---------------------------------------------------------

(defun select-public-keys ()
  ;; 1024-bit DH keys for initial SRP-6 handshake [P = 2*Q+1, Q prime]
  ;; generator of order = (P-1) [G^2 mod P /= 1, and G^((P-1)/2) mod P /= 1]
  (let* ((p-keys #.#(110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131953372219
  110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131953542247
  110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131954440903
  110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131954912347
  110733130861390325273223714915708815940841239025086956492482512302431841768612420553926491015864569438667372511804512595013714067655948701888623803535130698355087737510353066609832316632241419637710746726030623432724836837829706946251221511561676095077906242421212235304835973028416739591897787588131955850507
  121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318901362059
  121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318901390307
  121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318901683443
  121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318903543947
  121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318903574907
  121636686180494859351257062932794606637412604912349654946895786579232564207004269642515857596415775103556609593312343761619399270973617792375942737442895269169033370326978057626223123112060577736588615703266513961275805218656107856015364524626128664217938276131801578811416035728088503132610052344318904076759
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701859684739
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701859958483
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860004983
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860519219
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860543159
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701860928467
  125874072370696794236809845433085663986098930803397897971969785329075699829719788543438626417022264395405304793095682096307356853890121801628891244488742033671712430247054683916845968149901694358504515381754379369711976936208218925608516005824179148210832986948494616110927217460975273585946238026701862024703
  126109499319730975072643891561817777090078234159970968064380371675130087731049251176894058682887645606400141915814997634886187326048858326430138948012633141618386065743242120967800116033859745494421234716111317867182827099082997052278782304518545867425519912626164985878601696040793159811247884048409248818959
  128491880968628397295439786826090448071799216337734462740114772282882719015462829336458309749315736156755660134536485137110659061247296862257126145128366477583072943793395168181329513531307158492226048781300219481587675392558457213959505574314676368201921410594701570918097602501976047163949823423891177344307
  141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755291790299
  141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755292134987
  141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755293540883
  141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755293652723
  141748196247699632500986808906971058611550869395507694922590511654883610826606305709233518138170423599408401107955728398351083656546276901782102104779437208477121080540723797281574158246955100480326947890864922029402193565967828467664068413921533244861976824769684937384113548662768837624008184082755294155379
  148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789856351283
  148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789856714019
  148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789859728239
  148757362201947503107350056672229226015846526670066439700592774879557900230840629657269570465529206476597033680891372798894319849683448592503697981688782652297297277079736508164805831459847318748821103794744570332163018109903223211428324274319113375615993139362231079938382046507249340896843828581789859801019
  171050948217267985673436464661659295399250022438505679154384395855169504304692006719499500515962744962166121514204840502682644185949107664427757974606706183917926195232484819082843189438596224733882477303185478371183408054236233026031072171840648622216400655791611595902476601403117316676079943273588450078823
  171050948217267985673436464661659295399250022438505679154384395855169504304692006719499500515962744962166121514204840502682644185949107664427757974606706183917926195232484819082843189438596224733882477303185478371183408054236233026031072171840648622216400655791611595902476601403117316676079943273588450196183
  171050948217267985673436464661659295399250022438505679154384395855169504304692006719499500515962744962166121514204840502682644185949107664427757974606706183917926195232484819082843189438596224733882477303185478371183408054236233026031072171840648622216400655791611595902476601403117316676079943273588451879867
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645108962583
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645111589323
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645111835839
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112131423
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112476843
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112921323
  172995140565004716223251173179587368013743810348742732185154436072499697094058486203981224807130120428602455987779848226007562591515631828603480639245218673811592117300197954957487164919194991946812362886425360148736764108748693333982655118403291835063746524836780814704300732233146047676283200064645112941687
  94151763009122605199016046651212124826776428399875868337139174973411137492045166520124166776862436148366239370758238172290409368422703069135426261530234619164046611105339201384911749696986955053718391402729253242954270262493136526039898838280505289291728617563351299061826021401294993431392010748869152068839))
         
         (g-keys #.#(9614089102400895000034600887444346619400614238850730916032223306559032931761247555658295230283116418129129096805998270227820687998634835246438630416420731374048586673093113922802756946205078569213171246090994103779348129161824638081115638016181002617883485703513143744381289654719193548817308212580789912579
  43378795791211741060774903289757924922382979601248341166523140458527688049756831708370812362373718243361744277486783756884472592952395413276448568617080783527736468468565396869923750048691257010188892065966051814648378832305936285442625842240629653232489175290580796389718257571138528809742098221717752620095
  97295315353569335530883338151839025863649022600749029158536986546050638980460696086725885356705977081255199409151153126554074354132538673505533717751523742543573834525984370189294710459731711048838086276335809431431885827462558018412168260014977365710490621435957472371204195960041803845955768405438284284716
  55549577167749851194845912566107100436679282259741995537521192621243455346612167045656433360145890183264336055619063881461071199840746290188409706362488254757770124171822647501657906369909481267807044530994588386757413955395719895883597008704663303763980555361403820831937457954878951901658815442915488014924
  99579360564600413542683212299237386406337052080301409077365939090485335908767394702274784198952809231451543622342630104414506018897946282201990951244004126272868939852589684345075208384351716442168474390495039216020199019613591745585873615095310351109455364258728615170393883913009956069973484242273720864474
  58594697990167896791326176980958369118034112362035898286759397259630162711412264543932978282635511678719016993136406990503897898351175976386403834382042967354072369096104121441752197239665498599953709754737085459768551958385991571966036518336106787243126726457692250029158793822723246391480135739288722565709
  38659343213743759326971745560748104697019041628526303365768480273449494048437633291365099508852943245854536526985485014719761867564270770907681545585896241024363831655051561062648303836382200886130412545121468741154886705730021879321595093598998740489062593399535202148530269953088850772075650818688368044307
  27873751732470142163475853643920823794964891134663605716922031461237466183236186260086768527796728776793676847101673753268513014814912230669187779790222665323676541890174509980698121156021609194493112690521661424914033056047881339192639534450041285072912276494648086377399651824040109334653392526419042624880
  65684736939806934110109504364805139022016258313581038769966598305070597790048694486218729623212113834236858160189817823553235298310006293777306956204581176422475613518338781623602861728368611098939155330657758802288226829011964069840589091801505302525467707427259430233987761941479058834401822585966521995456
  63438467626479484488867732951106351041211070497076255495911279419269622042469298694474404842496945223243524140033842200288271996123129492511406652614305381389899594180986491429602874330241580254726474018183250867346025828348895365082340928936530073114004827328777844377778845317184749928435168047899477123525
  12959368996743737279508242011048015461479060422097749557357108254238981540607138251841643464977925939239842289990394148475135095412840372554143278582257401596979864835399074304677644890765208163186494088597287516914401636112294251581031496045067045245553985337431097704532614013085130277354697543568400781440
  54973562853867124393636436795100744238089889354116556016554745317352161630955478766510519094740024861337273164986258895970259848103612981671029757375649590862002500236805380550566157947430528522517307013314212993550641920297674318980685849278423776214666283191749408307834758586815568804012368735457483436448
  84230075318128312526794843837117948901701695358163629718941135695435067895806227543315228666363485203225473795868406007361897456952961280893332669109710546893336816408942987634537290868032169193409525193014973096267630045444324297396531940823441508098007999058375063916154264153011317949829615618955539803771
  111553422978448916635150599489660599691628840831944010685432846526322865991928275356968897667063944112866315366234216466627990109632181903827857018630705221133168737913209408350163153646165527626759620180999238303785513607450745196805660119550753912409054802562366337113234776038162756205005568835028103248118
  32049065867215556382443558305436618979387161304951501937739862219985255955864764987252388239774688136974682311004012002023409254473019255312256760257269706243521568787133132116662967519967588340268093451495979620743787010918027557938682937119599191246284403605958107446502482729369841761848729348538800261743
  3228146638517954549709477300495681693993486120960268883174387067052591201857866562470887743191241416692864706452874077317630626705457879070043981486477820461701121870443496125520709460207100709253465506294070887852529076009950562847262743243778137490642389502154882352434209668874677883435188445295386076834
  42682496658596357219516825409871573890434150821639503602137398362001199435182655807835754345409641222085422994960694492758892101893584633971886768206372484954457266069806358603539703852526316735660800041362480319869301375951973536829280164080040634482264816157648300611496403361520426691507535665920886557051
  100717686726177044620037568919591745747395635902042836105226859350905595730819311764584126021754659065258793193991971555124390888265424867209627730544308521225453985462089798597257729164546534683083957323258950964074807137718757144694880059948079615917753404359103733526294896942228913086352868970388524179959
  95080430188490573330372032815102637934954858030987186715258335861873521556300907778724640238274612145364960626345384676190418545717646818250415521530914878410343944433191317295742395387475795979440098751015888103011451753463276043219872393503700702363514087993319272681400194512525711992855421773475758471574
  58650097130808895019596922507967507993770354461480386682680732305146077893380712663065418415368819757914600122420097681968814511399942133284075902748779410222282167314845093929778612822311945147508650348952615441830082773428573961944608555947858242668485392647608032887309941002636083141959495741747143219148
  34143254614877645693050919106745007049225677421820764178449213276052612572929312726902018347912659759279329457455916409933707702538409043919436279671856201594148957985623926098083137194648769992688381311185010992079581297070311504419417955771046297843408247156840900924486183270912621805994274276713146615077
  57353395155629345207635018739640911474770034698461651291537225018065794587697867780564468794659751859564506099105996063791570383854299189544126337073727071910926369821672095062933554138577744453578422018384753204122084462594524745796167530463815604415355120371537852828104788596422560171470991130796478344895
  54019851852228449435058276656519337339737647903976456078333014621557357720421241730964974788087087970630343352603532190352998292992267751517260199135358407079651337120087288707031385607909601469565080552999107603217152327265939882115310318836933238696771618920026096315415702254391696411590199672188253768282
  70286976896833573448921067456156962201004603304552298896858464248415855591532382114884030973973356509563193746083225637918718641342486725244892158473847695523233651066630222154728894401182137898510418161769007141470136012546205619434123548525937104648985535462545690547071854921924990509883075378109620097000
  35213383721964827116560947096251388415237037024559013070718754583337324751809635345978008132049792092990360399026609098420426054183824456773373758806979721485674565188665589660466800210927706592022054869762013234019379023555192646517483438013178744143967915278137515116620467033682674016009177083881663110437
  63917546581503130363252264308060438153644825425196499784673586016695609447989979787031970295186528277405676420786318389659265065549180666978007070329823345002511504141909637049782055507123265968732326802744481615552326494710328756472622645748213709768283861121415277259309915503968775555148283418412066532774
  140158729554794187275656027935233455068910674825442465475435801782743808372818548078628367276659588620376796509114295917676882868699369702087111334876356390287609076104686870548428854314959190160627199633681598462976752276887535187001309988194131129776547095917133652617420945629218011062856764774468924440354
  89492245507556892088415361598404787885977436762624240380725003027012212565041629299555871228140187649206532863537428366631891662020619871313160790889280261727306385152458807580846604422290488606251870827868034428210525395949924049866974544419403887791242112664392082962085210352600830354979231285117840353304
  67037034124589717053377907625317429773261803721413652257478620313310913225425231713583418632493713040993961079198027262244201214195439821152108339214712153676734279102635752116774720948742856214796329286106822624849847057877492599348475873359026736389982994780649704760196859361880785133005710108743829910229
  59922185703157273592355795245980509394013918084476019236135476335269366841516670307817061450259197799907075057090046249946297909624706258010704268385717926142225929983621762212105923839838163847628713153063216601145757243813431100742750256647853612893350236236444162015821014627044743590633968644680167797847
  84104492987800087377191037825412526930880349965210814103752123937422340422712695221700480706481465165556826568740422781674023533770220693974148648264066220360923899189657816107185626306389847704790819876319797104929465356849808756567431635681295626650696750786535521788329549839419628644523710802346156496081
  89493000829029591464450369601530489986614792982440634724347922150844019324622238772570018873849035976512365426821233045464415029738076879783107176970485061941254171102972186633100293110674577616113005484483876129249801508554629664738595558525094377684833403305439787913991209898021394449754724211239959560492
  93803466500924957157591000574635931256162365322552714255533161722055407380080003051384713574034343760431473748824809812461779623472607633158095072782746271567902014054726777941549527788681259675937695816568570956423845600724044932875668199923813701445590351880659238052407762427198963032359125230330559331445
  135059319154656047271378901819932307192225281429371830182867087435656477360310276589741886101209147689906582041801612907410628408880459635935316041921541620061870974844286491036796229677833917112680093899431537603959002616090754704508082239505664443528901554758249440692999050640352436525910211510792691093685
  100613497279551947242259835748623190338928141771532906408781937478198295568149948295082782112546797484612409019709719882045274946834473130120590827464844728308505506211549539610090013975632764258900843419241101245393730394977513896235955022529682004209240259293721846142070747061860529627225986457646042421050
  169316541479925639637198040672373822087140591256266250654428174310384276459381877226888871868617545290637882919020282572679021722159726640270690614449238460872154371760246481634845625665227410657014075822080879913558738193385116681565625514502200279722759966022625050379901104524337284698660553856423305525687
  121056447525993883741269392385619245582554752956124994902951380118201646598163802889039085255554856364717066484473180820923879114023273321154833595316680011731277800969552824668682527058051569023338983542655518992336580293463258866024547190540355999710766560832496301904120178655304693017375226471678027410433
  44182373769687567804303908367575920655696664220978011985297805498694305362289121046992565714864836905885250057717230624521072045976536325205557069644294884481654946107315565321995903491300860135907182521577366417044143337023356982756446805535700110056651303310021112952450172477963502339590499022409851216979
  168516597358932725059741419028317878902514975014526844506427091602719064740793636589431778957022815755768791289655698540940775231539091525727098828971959642176390391056054449327840348969131519258743786776611839311508949504060111715207591201471356585608638513521514941729583683637170827060120861872138307432749
  17242605896284731830921566377864934308010238956787579001243785437328303246660731062322210781929577675919762263857329727700120503680786959040961468337485685197121476112475864740078211481759596272809490436827511191796516358588098139854294373681504048781304478032559695906696983129506742320767071193880932880307))
         
         (ix     (rand (length p-keys)))
         (p-key  (aref p-keys ix))
         (g-key  (aref g-keys ix)))
    (check-prime-modulus p-key)
    (check-generator     g-key p-key)
    (values p-key g-key)))

;; ---------------------------------------------------------------------

(defmethod server-negotiate-security ((crypto server-crypto) intf node-id)
  ;; ensure that our good numbers have not been altered...
  #-:diddly
  (multiple-value-bind (p-key g-key) (select-public-keys)

    (with-mod p-key
      ;; Phase I: Await ID
      (when-let (key (crypto-reneg-key crypto))
        (unless (equalp node-id key)
          (signature-mismatch-error)))
      
      ;; Phase II: Compute: s = random 256-bit
      ;;                    x = H32(s,ID,PassPhrase)
      ;;                    v = g^x mod N
      ;;                    b = 1 < random < N
      ;;                    B = 3*v + g^b mod N
      ;; Write: N,g,s,B
      ;; Hold as secret: x, v, b
      (multiple-value-bind (salt x)
          ;; can't permit x = 0
          (loop for salt = (rand p-key)
                for x    = (srp6-x salt node-id)
                do
                (unless (zerop x)
                  (return (values salt x))))
        
        (let* ((v  (m^ g-key x))
               (b  (rand-between 2 p-key))
               (bb (m+ (m* 3 v) ;; we know that 3 cannot be a primitive root
                       (m^ g-key b))
                   ))
          (=wait ((aa m1) :timeout 5)
              (funcall (intf-srp-ph2-begin intf) =wait-cont p-key g-key salt bb)
            ;; Phase III: Receive A,M1
            ;; Compute: u     = H32(A,B)
            ;;          S     = (A*v^u)^b mod N
            ;;          Chk1  = H32(A,B,S) check Chk1 = M1
            ;;          M2    = H32(A,M1,S)
            ;;          Key   = H32(S)    ;; H16(S)
            ;;          InitV = H32(M1,S) ;; H16(M1,S)
            ;; Send: M2
            ;; Hold as secret: u,S
            ;; Init crypto with Key, initv
            (check-public-key aa p-key)
            
            (let* ((u         (srp6-u aa bb))
                   (s         (m^ (m* aa
                                      (m^ v u))
                                  b))
                   (chk1      (hash32 aa bb s))
                   (m2        (hash32 aa m1 s))
                   (key-in    (hash32 bb s aa))
                   (key-out   (hash32 aa s bb))
                   (his-initv (hash32 m2 s))
                   (my-initv  (hash32 m1 s)))
              
              (when (zerop u)
                (error "Invalid public key A"))
              (unless (equalp chk1 m1)
                (signature-mismatch-error))

              (funcall (intf-srp-ph3-begin intf) m2
                      (lambda ()
                        (init-crypto-for-hmac   crypto my-initv his-initv)
                        (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                        (init-crypto-for-output crypto key-out (subseq my-initv 0 16))
                        (init-crypto-for-renegotiation crypto s)
                        ))
              ))
          )))))

;; ---------------------------------------------------------
