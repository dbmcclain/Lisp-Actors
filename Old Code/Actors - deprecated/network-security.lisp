;; bfly-security.lisp
;; --------------------------------------------------------------------------------------
;; Butterfly -- a system for easy distributed computing, going beyond what is available
;; in Erlang with the full power of Common Lisp.
;;
;; Copyright (C) 2008,2009 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08, 06-12/09
;; --------------------------------------------------------------------------------------

(in-package #:actors/security)

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
            )))

;; -----------------------------------------------------------------------------
(defvar $VERSION
  "Actors 2021/02/27 13:13:54Z. Copyright (C) 2008-2021 by Refined Audiometrics Laboratory. All rights reserved.")
;; -----------------------------------------------------------------------------

(defmonitor rand-monitor
    ((ctr-hash-prng  #-:OS-WINDOWS (ironclad:make-prng :fortuna :seed :urandom)
                     #+:OS-WINDOWS (lw:make-mt-random-state t)))
  ;; protected by a global lock
  ;; ctr-hash-prng is a shared mutable state
  (defun random (limit)
    (critical-section
      #-:OS-WINDOWS (ironclad:strong-random limit ctr-hash-prng)
      #+:OS-WINDOWS (lw:mt-random limit ctr-hash-prng))))

(defun random-between (lower upper)
  (declare (integer lower upper))
  ;; generate random (lower <= n < upper)
  (+ lower (random (- upper lower))))

;; --------------------------------------------------

(defun ensure-simple-array (v)
  ;; the Ironclad crypto functions require simple arrays
  (if (or (adjustable-array-p v)
          (array-has-fill-pointer-p v)
          (array-displacement v))
      (copy-seq v)
    v))

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
#|
(defun share-uuid (uuid-str)
  (with-mod #.(- (ash 1 264) 275)
    (let* ((k  (uuid:uuid-to-integer (uuid:make-uuid-from-string uuid-str)))
           (share1 (random #.(ash 1 264)))
           (share2 (random #.(ash 1 264)))
           (fn     (make-lagrange-interpolator `((0 . ,k)
                                                 (1 . ,share1)
                                                 (2 . ,share2))))
           (share3 (funcall fn 3)))
      (mapcar #'um:convert-int-to-wordlist
              (list share1 share2 share3)))))
|#

(defun reduce-factors (x pair_i pairs init)
  ;; -> init * (x - x1) * (x - x2) * ... * (x - x_i-1) * (x - x_i+1) * ... * (x - x_n)
  ;; i.e., Prod((x - x_j) for j = 1..N, j /= i)
  ;; pairs are (x . y) coords
  (flet ((factor (prod pair_j)
           (if (eq pair_i pair_j)
               prod
            (m* prod (m- x (car pair_j))))
           ))
    (reduce #'factor pairs
            :initial-value init)))

(defun prep-interpolation-shares (shares)
  ;; Map (x_i . y_i) -> (x_i . y_i / Prod((x_i - x_j), j = 1..N, j /= i)) 
  (flet ((prep (share_i)
           (destructuring-bind (xi . yi) share_i
             (cons xi (m/ yi (reduce-factors xi share_i shares 1)))
             )))
    (mapcar #'prep shares)))

(defun make-lagrange-interpolator (shares)
  ;; F(x) = Sum((x_i, y_i) -> y_i * Prod((x - x_j), j = 1 .. N, j /= i)
  ;;                              / Prod((x_i - x_j), j = 1..N, j /= i),
  ;;             i = 1..N)
  (let ((preps (prep-interpolation-shares shares)))
    (lambda (x)
      (flet ((term (sum prep_i)
               (m+ sum (reduce-factors x prep_i preps (cdr prep_i)))
               ))
        (reduce #'term preps
                :initial-value 0)
        ))))

(defun solve-shares (x shares)
  (let ((fn (make-lagrange-interpolator shares)))
    (funcall fn x)))
  
(defun uuid-str-from-shares (share1 share2 share3)
  (uuid:uuid-string
   (uuid:integer-to-uuid
    (with-mod #.(- (ash 1 264) 275)
      (solve-shares 0 `((1 . ,share1)
                        (2 . ,share2)
                        (3 . ,share3)))))
   ))

(defun assemble-sks (shares)
  (apply #'uuid-str-from-shares
         (mapcar #'um:convert-wordlist-to-int shares)))

;; -----------------------------------------------------------------------------
#|
(share-uuid "{cfe31464-f7b1-11ea-82f8-787b8acbe32e}")
(assemble-sks '(116556184643808452113560705861977715537
                55066560204924271324109576419163150535
                179630390834232184955478921582272058641))
|#
;; -------------------------------------------------

(defconstant +reneg-interval+ 600) ;; 10 minutes

(defclass crypto ()
  ((cypher-in    :accessor crypto-cypher-in    :initform nil)
   (cypher-out   :accessor crypto-cypher-out   :initform nil)
   (sequence-in  :accessor crypto-sequence-in  :initform 0)
   (sequence-out :accessor crypto-sequence-out :initform 0)
   (hmac-key     :accessor crypto-hmac-key     :initform (hash32 (vector-of "default HMAC key")))
   (reneg-key    :accessor crypto-reneg-key    :initform nil)
   (reneg-period :accessor crypto-reneg-period :initform (random #.(ash 1 63)))
   (reneg-time   :accessor crypto-reneg-time   :initform (+ (get-universal-time)
                                                            (random (* 2 +reneg-interval+))))
   ))

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
        (crypto-reneg-period crypto) (random #.(ash 1 63))
        (crypto-reneg-time   crypto) (+ (get-universal-time)
                                        (random (* 2 +reneg-interval+)))
        ))

(defmethod time-to-renegotiate? ((crypto crypto))
  (let ((tbytes (+ (crypto-sequence-in crypto)
                   (crypto-sequence-out crypto))))
    (or (> (get-universal-time) (crypto-reneg-time crypto))
        (> tbytes (crypto-reneg-period crypto)))
    ))
                
;; -----------------------------------------------------------------------

(define-condition signature-mismatch-exn (error)
  ()
  (:report "Signature mismatch on remote connection"))

(defun signature-mismatch-error ()
  (error 'signature-mismatch-exn))

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

;; ------------------------------------------------------------

(defconstant +MAX-FRAGMENT-SIZE+ 65536)

(defun byte-encode-obj (obj)
  (lzw:zl-compressed-data
   (lzw:zl-compress obj)))

(defun byte-decode-obj (vec)
  (handler-case
      (lzw:decompress
       (lzw:make-zl-compressed
        :data vec))
    (error ()
      ;; (setf *bad-data* (copy-seq data))
      `(actors/internal-message/network:discard))
    ))
  
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
                             'actors/internal-message/network:frag
                           'actors/internal-message/network:last-frag))
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
  (handler-case
      (loenc:decode enc-buf)
    (error ()
      ;; (setf *bad-data* (copy-seq data))
      `(actors/internal-message/network:discard))
    ))

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
        `(actors/internal-message/network:discard))
       ))))

