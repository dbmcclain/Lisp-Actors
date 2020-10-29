;; lisp-object-encoder.lisp
;; --------------------------------------------------------------------------------------
;; Network and file portable encoding / decoding for Lisp objects
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(defpackage :lisp-object-encoder
  (:use #:common-lisp)
  (:nicknames #:loenc)
  (:import-from #:sdle-store
   #:store-object
   #:store-count
   #:read-count
   #:restore-object
   #:register-code
   #:next-available-code
   #:$unbound-marker
   #:after-retrieve
   #:rawbytes
   #:rawbytes-bytes
   #:make-rawbytes)
  (:export
   ;; #:$LAST-OBJECT-TYPE
   ;; #:add-decoder
   ;; #:add-subtree-decoder
   ;; #:decode-object
   ;; #:subtree-decode-object
   ;; #:encode-object
   ;; #:encode-ubyte
   ;; #:encode-ubytes-of-vector
   ;; #:decode-ubytes-into-vector
   #:register-code
   #:next-available-code
   #:defstore
   #:defrestore
   #:store-count
   #:store-object
   #:read-count
   #:restore-object
   #:skip-data
   #:decode-prefix-length
   #:read-prefix-length
   #:read-bytes
   #:encode
   #:decode
   #:serialize
   #:deserialize
   #:after-retrieve
   #:encode-32-bit-prefix-length
   #:encode-16-bit-prefix-length
   #:encode-8-bit-prefix-length
   ;; #:deserialize-to-tree
   ;; #:encoding-tree
   #:$EOF
   #:early-eof
   #:$unbound-marker
   #:rawbytes
   #:rawbytes-bytes
   #:make-rawbytes
   #:security-failure
   ))

;; ---------------------------------------------------------------
(in-package #:lisp-object-encoder)
;; ---------------------------------------------------------------

(defvar $EOF #(:EOF)
  "Globally unique value representing end-of-file.")

;; (unless (sdle-store:find-backend 'loe-back-end)
(sdle-store:defbackend loe-back-end
    :magic-number #x454C4473  ;; SDLe
    :extends (sdle-store:sdle-store))

;; ---------------------------------------------------------

(defmacro defstore ((obj type stream) &body body)
  `(sdle-store:defstore-sdle-store (,obj ,type ,stream) ,@body))

(defmacro defrestore ((type stream) &body body)
  `(sdle-store:defrestore-sdle-store (,type ,stream) ,@body))

#+:LISPWORKS
(dspec:define-dspec-alias defstore (arglist)
  `(method sdle-store:internal-store-object ,arglist))
#+:LISPWORKS
(dspec:define-dspec-alias defrestore (arglist)
  `(method sdle-store::internal-restore-object ,arglist))

;; ---------------------------------------------------------------

(defparameter +UUID-code+ (register-code 127 'uuid))

(defstore (obj UUID:UUID stream)
  (store-count +UUID-code+ stream)
  (write-sequence (uuid:uuid-to-byte-array obj) stream))

(defrestore (uuid stream)
  (let ((arr (make-array 16 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent arr))
    (read-sequence arr stream)
    (uuid:byte-array-to-uuid arr)))

;; ---------------------------------------------------------
(defun encode-prefix-length (buf prefix-length encrypted-p)
  "The total output encoding for a message has a prefix-length byte prefix count in
network byte-order (big endian). Once the message has been encoded, we can fill in this count,
where the encoding string had reserved the front prefix-length bytes for this count. The prefix
count does not include itself."
  (do ((ix (1- prefix-length) (1- ix))
       (v  (- (if encrypted-p
                  (1+ (length buf))
                (length buf))
              prefix-length)
           (ash v -8)))
      ((minusp ix))
    (setf (aref buf ix) (logand v #x0ff))) )

;; -------------------------------------------

(defun normalize-prefix-length (prefix-length)
  (ecase prefix-length
    (:8-bit  1)
    (:16-bit 2)
    (:32-bit 4)
    ((1 2 4 nil) prefix-length)))

;; -------------------------------------------

(defun encode (msg &key use-magic prefix-length encrypt-fn)
  "Serialize a message to a buffer of unsigned bytes."
  (setf (sdle-store:magic-number (sdle-store:find-backend 'loe-back-end)) use-magic
        prefix-length (normalize-prefix-length prefix-length))
  ;; prefix-length will be one of 1,2,4 or NIL
  (let ((buf (ubyte-streams:with-output-to-ubyte-stream (s)
               (labels ((pad-null (nel)
                          (loop repeat nel do
                                (write-byte 0 s))))

                 ;; reserve room for prefix count and encryption signature
                 (when prefix-length
                   (setf (stream:stream-file-position s)
                         (if encrypt-fn
                             (+ prefix-length 16)
                           prefix-length)))
                 
                 (sdle-store:store msg s 'loe-back-end)

                 ;; pad data to an even number of bytes
                 (let* ((preflen  (or prefix-length 0))
                        (data-len (- (file-position s) preflen))
                        (npad     (- (um:align-pwr2 data-len
                                                    (if encrypt-fn 16 2))
                                     data-len)))
                   (pad-null npad)) ))))
    
    (when prefix-length
      (encode-prefix-length buf prefix-length encrypt-fn) ;; odd prefix-length means encrypted
      (when encrypt-fn
        (funcall encrypt-fn buf prefix-length)))
    buf))

;; -------------------------------------------

(defun serialize (msg stream &key use-magic prefix-length encrypt-fn)
  "Serialize a message to the output stream (usually a socket output port). The
message is encoded as a long string of unsigned bytes, or base-chars, with a
length prefix in network byte-order (big-endian). The prefix count does not include
the length of itself.

Once the message and its prefix count is encoded in the buffer we peform a single
I/O call to write the entire stream to the output port."
  (write-sequence (encode msg
                          :use-magic     use-magic
                          :prefix-length prefix-length
                          :encrypt-fn    encrypt-fn)
                  stream))

;; ---------------------------------------------------------------
;; ---------------------------------------------------------------

(defun early-eof ()
  (error "Unexpected EOF on input stream"))

;; ----------------------------------------------------------

(defun decode-prefix-length (arr nb)
  "Get the message prefix count from the first bytes in the message, which was
transmitted in network byte-order (big-endian)."
  (do ((ix  0 (1+ ix))
       (val 0))
      ((>= ix nb) val)
    (setf val (+ (ash val 8) (aref arr ix)))
    ))

;; ----------------------------------------------------------

(defun common-reader (f nb buf)
  (let* ((bytes (or buf (make-array nb :element-type '(unsigned-byte 8))))
         (rdlen (read-sequence bytes f)))
    (values bytes rdlen)))
  
(defun read-raw-bytes (f nb &optional buf)
  (multiple-value-bind (bytes rdlen) (common-reader f nb buf)
    (cond ((zerop rdlen) $EOF)
          ((< rdlen nb)  (early-eof))
          (t bytes)) ))

(defun must-read-raw-bytes (f nb &optional buf)
  (multiple-value-bind (bytes rdlen) (common-reader f nb buf)
    (cond ((< rdlen nb) (early-eof))
          (t bytes)) ))

;; ----------------------------------------------------------

(defmacro or-eofp (tst-val &body body)
  `(if (eq $EOF ,tst-val)
       $EOF
     (progn
       ,@body)))

(defun actual-length (len)
  (logandc2 len 1))

(defun encrypted-p (len)
  (oddp len))

;; ----------------------------------------------------------

(defun read-prefix-length (f nb)
  (let ((arr (make-array nb :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent arr))
    (or-eofp (read-raw-bytes f nb arr)
      (decode-prefix-length arr nb)) ))

;; ----------------------------------------------------------

(defun skip-data (f prefix-length)
  ;; given that we have a byte prefix length encoding,
  ;; read that length and then skip that many bytes ahead
  (let ((data-len (read-prefix-length f prefix-length)))
    (or-eofp data-len
      (file-position f (+ (file-position f)
                          (actual-length data-len))) )))

;; ----------------------------------------------------------

(defun read-data (f prefix-length)
  (let ((data-len (read-prefix-length f prefix-length)))
    (or-eofp data-len
      (must-read-raw-bytes f (actual-length data-len))) ))

;; ----------------------------------------------------------

;; (defvar *dribble-deserialize* nil)

(define-condition security-failure-exception (error)
  ())

(defun security-failure ()
  (error (make-instance 'security-failure-exception)))

(defun deserialize-prefixed-stream (stream prefix-length decrypt-fn)
  (labels ((security-check (fn)
             (handler-case
                 (funcall fn)
               (error (exn)
                 (if decrypt-fn
                     (security-failure)
                   (error exn))) )))
    (macrolet ((with-security-check (&body body)
                 `(security-check (lambda () ,@body))))
      
      (let ((prefix (with-security-check
                     (read-prefix-length stream prefix-length))))
        (or-eofp prefix
          (let ((nb (actual-length prefix)))
            
            (mgdbuf:with-temporary-buffer (buf)
              (cond ((encrypted-p prefix)
                     (unless decrypt-fn
                       (error "Can't decrypt data"))
                     (unless (>= nb 16)
                       (security-failure))
                     (let ((signature (make-array 16 :element-type '(unsigned-byte 8))))
                       (declare (dynamic-extent signature))
                       (with-security-check
                        (must-read-raw-bytes stream 16 signature))
                       (funcall decrypt-fn signature :check-signature t))
                     (let ((nb-remaining (- nb 16)))
                       (setf buf (mgdbuf:get-buffer nb-remaining))
                       (must-read-raw-bytes stream nb-remaining buf)
                       (funcall decrypt-fn buf)))
                    
                    (decrypt-fn
                     ;; BFLY waiting on input
                     ;; data arrived unencrypted
                     (security-failure))
                    
                    (t
                     (setf buf (mgdbuf:get-buffer nb))
                     (must-read-raw-bytes stream nb buf)) )
              
              #|
              (when *dribble-deserialize*
                (with-open-file (f
                                 (merge-pathnames
                                  (user-homedir-pathname)
                                  "loenc-dribble-deserialize.txt")
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
                  (with-standard-io-syntax
                    (prin1 lenseq f)
                    (terpri f)
                    (prin1 buf f)
                    (terpri f))))
              |#
                         
              (with-security-check
               (ubyte-streams:with-input-from-ubyte-stream (s buf)
                 (sdle-store:restore s 'loe-back-end))) )) ))) ))
               

;; -----------------------------------------------------------------------------

(defun deserialize (stream &key use-magic prefix-length length decrypt-fn)
  "Deserialize an encoded message from the input stream. The message will have a 4-byte
prefix length in network byte-order (big-endian). We strip that count out into a stack
allocated 4-byte array, then decode that length and request a buffer of at least that size
from the buffer manager.

A single I/O read operation then reads the entire encoded message into that buffer, and then
we recursively decode the contents of the buffer. The final decoded message object is returned after
recycling that buffer for another use later. This is an attempt to avoid generating too much garbage."
  (setf (sdle-store:magic-number (sdle-store:find-backend 'loe-back-end)) use-magic
        prefix-length (normalize-prefix-length prefix-length))
  
  (cond (prefix-length
         (deserialize-prefixed-stream stream prefix-length decrypt-fn))
        
        (length
         (mgdbuf:with-temporary-buffer (buf length)
           (must-read-raw-bytes stream length buf)
           (ubyte-streams:with-input-from-ubyte-stream (s buf)
             (sdle-store:restore s 'loe-back-end)) ))
        
        (t  (sdle-store:restore stream 'loe-back-end))
        ))

;; -----------------------------------------------------------------------------

(defun decode (arr &key (start 0) use-magic prefix-length length
                   (reader #'aref)
                   decrypt-fn)
  (ubstream:with-input-from-ubyte-stream (s arr :start start :reader reader)
    (deserialize s
                 :use-magic     use-magic
                 :prefix-length prefix-length
                 :length        length
                 :decrypt-fn    decrypt-fn)))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

#|
#|
(user::asdf :data-objects)

(defvar *dbg* (debug-stream:make-debug-stream
                     :display t
                     :title "Event Montior"
                     :always-on-top t))
(defvar *debugging* t)

(defun dbg (msg &rest args)
  (if (and *debugging*
           *dbg*)
      (debug-stream:debug-print *dbg*
                                (apply #'format nil msg args))))
|#
(defmacro dbg (&rest args)
  (declare (ignore args)))

;; ---------------------------------------------------------

(defvar $EOF #(:EOF)
  "Globally unique value representing end-of-file.")

(defconstant $NIL                   0)
(defconstant $T                     1)
(defconstant $STRING                2)
(defconstant $LISP-READABLE         3)
(defconstant $CHARACTER             4)
(defconstant $POSITIVE-INTEGER      5)
(defconstant $NEGATIVE-INTEGER      6)
(defconstant $SHORT-FLOAT           7)
(defconstant $SINGLE-FLOAT          8)
(defconstant $DOUBLE-FLOAT          9)
(defconstant $LIST                 10)
(defconstant $VECTOR               11)
(defconstant $UBYTE-VECTOR         12)
(defconstant $BYTE-VECTOR          13)
(defconstant $SINGLE-FLOAT-VECTOR  14)
(defconstant $DOUBLE-FLOAT-VECTOR  15)
(defconstant $ARRAY                16)
(defconstant $COMPLEX              17)
(defconstant $RATIONAL             18)
(defconstant $KW-SYMBOL            19)
(defconstant $UNINTERNED-SYMBOL    20)
(defconstant $SYMBOL               21)
(defconstant $PATHNAME             22)
(defconstant $STANDARD-OBJECT      23)
(defconstant $HASH-TABLE           24)
(defconstant $0                    25)
(defconstant $1                    26)
(defconstant $2                    27)
(defconstant $3                    28)
(defconstant $4                    39)
(defconstant $5                    30)
(defconstant $6                    31)
(defconstant $7                    32)
(defconstant $8                    33)
(defconstant $9                    34)
(defconstant $10                   35)
(defconstant $-1                   36)
(defconstant $-2                   37)
(defconstant $FUNCTION             38)
(defconstant $GENERIC-FUNCTION     39)
(defconstant $CONDITION            40)
(defconstant $STANDARD-CLASS       41)
(defconstant $BUILT-IN-CLASS       42)
(defconstant $STRUCTURE            43)
(defconstant $UUID                 44)
(defconstant $LAST-OBJECT-TYPE     $UUID)

;; ------------------------------------------------------------

(defvar *decoders*
  (make-array 256
              :initial-element nil)
  "A vector of decoder routines corresponding to each type code.")

(defun add-decoder (name fn)
  "Name is an integer (0..255) and fn is a decoding function.
Add this decoder for that named type to the decoders table."
  (setf (aref *decoders* name) fn))


(loop for (name fn) in `((,$NIL                 decode-nil)
                         (,$T                   decode-t)
                         (,$STRING              decode-string)
                         (,$LISP-READABLE       decode-lisp-readable)
                         (,$CHARACTER           decode-character)
                         (,$POSITIVE-INTEGER    decode-integer)
                         (,$NEGATIVE-INTEGER    ,(lambda (stream)
                                                  (- (decode-integer stream))))
                         (,$SHORT-FLOAT         decode-short-float)
                         (,$SINGLE-FLOAT        decode-single-float)
                         (,$DOUBLE-FLOAT        decode-double-float)
                         (,$LIST                decode-list)
                         (,$VECTOR              decode-vector)
                         (,$UBYTE-VECTOR        decode-ubyte-vector)
                         (,$BYTE-VECTOR         decode-byte-vector)
                         (,$SINGLE-FLOAT-VECTOR decode-single-float-vector)
                         (,$DOUBLE-FLOAT-VECTOR decode-double-float-vector)
                         (,$ARRAY               decode-array)
                         (,$COMPLEX             decode-complex)
                         (,$RATIONAL            decode-rational)
                         (,$KW-SYMBOL           decode-kw-symbol)
                         (,$UNINTERNED-SYMBOL   decode-uninterned-symbol)
                         (,$SYMBOL              decode-symbol)
                         (,$PATHNAME            decode-pathname)
                         (,$STANDARD-OBJECT     decode-standard-object)
                         (,$HASH-TABLE          decode-hash-table)
                         (,$0                   decode-0)
                         (,$1                   decode-1)
                         (,$2                   decode-2)
                         (,$3                   decode-3)
                         (,$4                   decode-4)
                         (,$5                   decode-5)
                         (,$6                   decode-6)
                         (,$7                   decode-7)
                         (,$8                   decode-8)
                         (,$9                   decode-9)
                         (,$10                  decode-10)
                         (,$-1                  decode-minus-1)
                         (,$-2                  decode-minus-2)
                         (,$FUNCTION            decode-function)
                         (,$GENERIC-FUNCTION    decode-generic-function)
                         (,$CONDITION           decode-condition)
                         (,$STANDARD-CLASS      decode-standard-class)
                         (,$BUILT-IN-CLASS      decode-built-in-class)
                         (,$STRUCTURE           decode-structure)
                         (,$UUID                decode-uuid))
      do
      (add-decoder name fn))

(defun decode-object (stream)
  "Decode an object from the stream by dispatching on the prefix type byte-code. Anything
not covered by this dispatch table will generate an error condition."
  (let ((typ (decode-ubyte stream)))
    (dispatch-decode-object typ stream)))

(defun dispatch-decode-object (typ stream)
  (let ((fn  (aref *decoders* typ)))
    (if fn
        (funcall fn stream)
      (error "Unknown encoding: typ = ~A" typ))
    ))
    
;; ---------------------------------------------------------

(defvar *subtree-decoders*
  (make-array 256
              :initial-element nil)
  "A vector of decoder routines corresponding to each type code.")

(defun add-subtree-decoder (name fn)
  "Name is an integer (0..255) and fn is a subtree-decoding function.
Add this decoder for that named type to the subtree-decoders table."
  (setf (aref *subtree-decoders* name) fn))
  

(loop for (name fn) in `((,$NIL                 subtree-decode-nil)
                         (,$T                   subtree-decode-t)
                         (,$STRING              subtree-decode-string)
                         (,$LISP-READABLE       subtree-decode-lisp-readable)
                         (,$CHARACTER           subtree-decode-character)
                         (,$POSITIVE-INTEGER    subtree-decode-integer)
                         (,$NEGATIVE-INTEGER    ,(lambda (stream)
                                                  (list :negate (subtree-decode-integer stream))))
                         (,$SHORT-FLOAT         subtree-decode-short-float)
                         (,$SINGLE-FLOAT        subtree-decode-single-float)
                         (,$DOUBLE-FLOAT        subtree-decode-double-float)
                         (,$LIST                subtree-decode-list)
                         (,$VECTOR              subtree-decode-vector)
                         (,$UBYTE-VECTOR        subtree-decode-ubyte-vector)
                         (,$BYTE-VECTOR         subtree-decode-byte-vector)
                         (,$SINGLE-FLOAT-VECTOR subtree-decode-single-float-vector)
                         (,$DOUBLE-FLOAT-VECTOR subtree-decode-double-float-vector)
                         (,$ARRAY               subtree-decode-array)
                         (,$COMPLEX             subtree-decode-complex)
                         (,$RATIONAL            subtree-decode-rational)
                         (,$KW-SYMBOL           subtree-decode-kw-symbol)
                         (,$UNINTERNED-SYMBOL   subtree-decode-uninterned-symbol)
                         (,$SYMBOL              subtree-decode-symbol)
                         (,$PATHNAME            subtree-decode-pathname)
                         (,$STANDARD-OBJECT     subtree-decode-standard-object)
                         (,$HASH-TABLE          subtree-decode-hash-table)
                         (,$0                   subtree-decode-0)
                         (,$1                   subtree-decode-1)
                         (,$2                   subtree-decode-2)
                         (,$3                   subtree-decode-3)
                         (,$4                   subtree-decode-4)
                         (,$5                   subtree-decode-5)
                         (,$6                   subtree-decode-6)
                         (,$7                   subtree-decode-7)
                         (,$8                   subtree-decode-8)
                         (,$9                   subtree-decode-9)
                         (,$10                  subtree-decode-10)
                         (,$-1                  subtree-decode-minus-1)
                         (,$-2                  subtree-decode-minus-2)
                         (,$FUNCTION            subtree-decode-function)
                         (,$GENERIC-FUNCTION    subtree-decode-generic-function)
                         (,$CONDITION           subtree-decode-condition)
                         (,$STANDARD-CLASS      subtree-decode-standard-class)
                         (,$BUILT-IN-CLASS      subtree-decode-built-in-class)
                         (,$STRUCTURE           subtree-decode-structure)
                         (,$UUID                subtree-decode-uuid))
      do
      (add-subtree-decoder name fn))

(defun subtree-decode-object (stream)
  "Decode an object from the stream by dispatching on the prefix type byte-code. Anything
not covered by this dispatch table will generate an error condition."
  (let ((typ (decode-ubyte stream)))
    (subtree-dispatch-decode-object typ stream)))

(defun subtree-dispatch-decode-object (typ stream)
  (let ((fn  (aref *subtree-decoders* typ)))
    (if fn
        (funcall fn stream)
      (error "Unknown encoding: typ = ~A" typ))
    ))
    
;; ---------------------------------------------------------
;; portable array-based input streams
(defclass array-input-stream ()
  ((arr   :accessor ais-arr   :initarg :arr)
   (ix    :accessor ais-ix    :initform 0)
   (limit :accessor ais-limit :initform 0)
   ))

(defmethod initialize-instance :after ((ais array-input-stream) &key &allow-other-keys)
  (setf (ais-limit ais) (length (ais-arr ais))))


(defun make-array-input-stream (arr)
  (make-instance 'array-input-stream
                 :arr arr))
                          
(defmethod stream-read-byte ((ais array-input-stream))
  (if (>= (ais-ix ais) (ais-limit ais))
      (error "EOF on array-input-stream")
    (prog1
        (aref (ais-arr ais) (ais-ix ais))
      (incf (ais-ix ais)))
    ))

(defmethod stream-read-sequence (seq (ais array-input-stream) &key (start 0) end)
  (let* ((nel (min (- (ais-limit ais)
                      (ais-ix    ais))
                   (- (or end
                          (length seq))
                      start)))
         (ais-end  (+ (ais-ix ais) nel))
         (seq-end  (+ start nel)))
    (setf (subseq seq start seq-end) (subseq (ais-arr ais) (ais-ix ais) ais-end))
    (incf (ais-ix ais) nel)
    nel))
        
(defmethod stream-read-sequence ((seq string) (ais array-input-stream) &key (start 0) end)
  (let ((nel (min (- (ais-limit ais)
                     (ais-ix    ais))
                  (- (or end
                         (length seq))
                     start))))
    (dotimes (ix nel)
      (setf (char seq (+ ix start))
            (code-char (stream-read-byte ais)) ))
    nel))
        
;; ---------------------------------------------------------

(defun encode-ubyte (v arr)
  (vector-push-extend v arr))

(defun encode-char (c arr)
  (encode-ubyte (char-code c) arr))

(defun decode-ubyte (stream)
  "The stream has element type '(unsigned-byte 8) already.
Get the next 8-bit unsigned byte from the stream."
  (stream-read-byte stream))

(defun decode-byte (stream)
  "The stream is a base-char (8-bit) stream. Convert the char to an equivalend signed-byte value."
  (let ((bv (decode-ubyte stream)))
    (if (>= bv 128)
        (- 256 bv)
      bv)))

(defun decode-char (stream)
  "The stream is a '(unsigned-byte 8) stream. Read the next byte and convert to
a character."
  (code-char (decode-ubyte stream)))

;; ---------------------------------------------
;; 7-bit encoding, used for prefix counts and lengths
;; Most of the time these prefix codes will be rather small.
;; If they are fewer than 128, we can encode in a single prefix byte.
;; Otherwise, we have a chain of 7-bit encodings, where if the #x80 bit
;; of the code is set, we have these 7 bits followed by another 7-bit
;; encoding.
;;
;; Example: 0-127 = #x00 - #x7F
;;          128 = #x81,#x00
;;          129-255 = #x81,#x00-#x7F,
;;          256 = #x82,#x00, etc

(defun convert-unsigned-integer-to-ubytes7-list (v)
  "Produce a list of the byte encoding of an integer value. Indefinite length. (Remember
the case of the 3,000 place value for Pi?)"
  (declare (type integer v))
  (labels ((iter (v bytes)
             (let ((new-v     (ash v -7))
                   (new-bytes (cons (logand v #x07F) bytes)))
             (if (plusp new-v)
                 (iter new-v new-bytes)
               new-bytes))))
    (iter v nil)
    ))

(defun encode-ubytes7 (v stream)
  (let ((bytes (convert-unsigned-integer-to-ubytes7-list v)))
    (do ((lst bytes (rest lst)))
        ((endp lst))
      (encode-ubyte (if (rest lst)
                        (logior #x080 (first lst))
                      (first lst))
                    stream))
    ))

(defun decode-ubytes7 (stream)
  (labels ((iter (val)
             (let* ((byte    (decode-ubyte stream))
                    (new-val (logior (ash val 7) (logand byte #x7f))))
               ;; (format t "~&Byte = ~2,'0x, val = ~D" byte new-val)
               (if (zerop (logand #x080 byte))
                   new-val
                 (iter new-val))
               )))
    (iter 0)))

;; ---------------------------------------------

(defun encode-count (ct arr)
  (encode-ubytes7 ct arr))

(defun decode-count (stream)
  (decode-ubytes7 stream))

(defun subtree-decode-count (stream)
  (list :CT (decode-count stream)))

;; -------------------------------------------

(defmethod encode-object ((c character) arr)
  (encode-ubyte $CHARACTER arr)
  (vector-push-extend c arr))

(defun decode-character (stream)
  (decode-char stream))

(defun subtree-decode-character (stream)
  (list :CHARACTER (decode-char stream)))

;; -------------------------------------------

(defmethod encode-object ((n integer) arr)
  "Integers are encoded in several ways. Positive or Negative values
are indicated by the prefix type-byte. The following bytes represent the
unsigned absolute value.

If the integer can be represented in fewer than 256 bytes,
then we encode it as a positive or negative COUNT object with a 1-byte prefix count
of how many bytes follow for the actual encoding.

If the integer is larger than 255 bytes then we encode the prefix count as a COUNT object
that specifies how many bytes make up the following integer value."
  (encode-ubyte (if (minusp n)
                    $NEGATIVE-INTEGER
                  $POSITIVE-INTEGER)
                arr)
  (encode-count (abs n) arr))

(defun decode-integer (stream)
  "We already know we have a large integer that uses a COUNT prefix for its size."
  (decode-count stream))

(defun subtree-decode-integer (stream)
  (list :INTEGER (decode-count stream)))

;; -------------------------------------------
(defmethod encode-object ((obj (eql nil)) arr)
  (encode-ubyte $NIL arr))

(defun decode-nil (stream)
  (declare (ignore stream))
  nil)

(defun subtree-decode-nil (stream)
  (declare (ignore stream))
  :NIL)


;; -------------------------------------------
(defmethod encode-object ((obj (eql t)) arr)
  (encode-ubyte $T arr))

(defun decode-t (stream)
  (declare (ignore stream))
  t)

(defun subtree-decode-t (stream)
  (declare (ignore stream))
  :T)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 0)) arr)
  (encode-ubyte $0 arr))

(defun decode-0 (stream)
  (declare (ignore stream))
  0)

(defun subtree-decode-0 (stream)
  (declare (ignore stream))
  0)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 1)) arr)
  (encode-ubyte $1 arr))

(defun decode-1 (stream)
  (declare (ignore stream))
  1)

(defun subtree-decode-1 (stream)
  (declare (ignore stream))
  1)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 2)) arr)
  (encode-ubyte $2 arr))

(defun decode-2 (stream)
  (declare (ignore stream))
  2)

(defun subtree-decode-2 (stream)
  (declare (ignore stream))
  2)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 3)) arr)
  (encode-ubyte $3 arr))

(defun decode-3 (stream)
  (declare (ignore stream))
  3)

(defun subtree-decode-3 (stream)
  (declare (ignore stream))
  3)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 4)) arr)
  (encode-ubyte $4 arr))

(defun decode-4 (stream)
  (declare (ignore stream))
  4)

(defun subtree-decode-4 (stream)
  (declare (ignore stream))
  4)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 5)) arr)
  (encode-ubyte $5 arr))

(defun decode-5 (stream)
  (declare (ignore stream))
  5)

(defun subtree-decode-5 (stream)
  (declare (ignore stream))
  5)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 6)) arr)
  (encode-ubyte $6 arr))

(defun decode-6 (stream)
  (declare (ignore stream))
  6)

(defun subtree-decode-6 (stream)
  (declare (ignore stream))
  6)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 7)) arr)
  (encode-ubyte $7 arr))

(defun decode-7 (stream)
  (declare (ignore stream))
  7)

(defun subtree-decode-7 (stream)
  (declare (ignore stream))
  7)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 8)) arr)
  (encode-ubyte $8 arr))

(defun decode-8 (stream)
  (declare (ignore stream))
  8)

(defun subtree-decode-8 (stream)
  (declare (ignore stream))
  8)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 9)) arr)
  (encode-ubyte $9 arr))

(defun decode-9 (stream)
  (declare (ignore stream))
  9)

(defun subtree-decode-9 (stream)
  (declare (ignore stream))
  9)

;; -------------------------------------------
(defmethod encode-object ((obj (eql 10)) arr)
  (encode-ubyte $10 arr))

(defun decode-10 (stream)
  (declare (ignore stream))
  10)

(defun subtree-decode-10 (stream)
  (declare (ignore stream))
  10)

;; -------------------------------------------
(defmethod encode-object ((obj (eql -1)) arr)
  (encode-ubyte $-1 arr))

(defun decode-minus-1 (stream)
  (declare (ignore stream))
  -1)

(defun subtree-decode-minus-1 (stream)
  (declare (ignore stream))
  -1)

;; -------------------------------------------
(defmethod encode-object ((obj (eql -2)) arr)
  (encode-ubyte $-2 arr))

(defun decode-minus-2 (stream)
  (declare (ignore stream))
  -2)

(defun subtree-decode-minus-2 (stream)
  (declare (ignore stream))
  -2)

;; -------------------------------------------

(defvar *stored-values*)
(defvar *stored-counter*)

(defvar *grouped-store-hash*)
(defvar *grouped-restore-hash*)

(defun create-serialize-hash ()
  (make-hash-table :test #'eql :size 50))

(defun get-store-hash ()
  (if (boundp '*grouped-store-hash*)
      (clrhash *grouped-store-hash*)
    (create-serialize-hash)))

(defun get-restore-hash ()
  (if (boundp '*grouped-restore-hash*)
      (clrhash *grouped-restore-hash*)
    (create-serialize-hash)))

(deftype not-circ ()
  '(or integer character))

(defun needs-checkp (obj)
  (not (typep obj 'not-circ)))

(defun seen (obj)
  (incf *stored-counter*)
  (gethash obj *stored-values*))

(defun update-seen (obj)
  (setf (gethash obj *stored-values*) *stored-counter*))

(defun get-ref (obj)
  (if (needs-checkp obj)
      (multiple-value-bind (val win) (seen obj)
        (if (or val win)
            val
          (update-seen obj)))
    nil))

;; -------------------------------------------

(defun encode-float-parts (v arr)
  "Float objects (reals) are encoded with a prefix type byte, indicating what kind
of float (short, single, double). Then the integer decoding of the float value is
encoded as two successive signed integers that represent the base-2 exponent, and the
integerized mantissa."
  (multiple-value-bind (ipart exp isign) (integer-decode-float v)
    (encode-object exp arr)
    (encode-object (if (minusp isign)
                       (- ipart)
                     ipart)
                   arr)))

(defun decode-float-number (float-archetype stream)
  "Get the signed integers representing the base-2 exponent and the
integerized mantissa. Then reconstruct as a float and then coerce to the
archetype float value which is a short, single, or double float."
  (let* ((expon (decode-object stream))
         (mant  (decode-object stream)))
    (* (signum mant) (scale-float (float (abs mant) float-archetype) expon))
    ))

(defun subtree-decode-float-number (stream)
  (let ((expon (decode-object stream))
        (mant  (decode-object stream)))
    (list :expon expon :mant mant)))

;; -------------------------------------------

#+:LISPWORKS
(defmethod encode-object ((v short-float) arr)
  (encode-ubyte $SHORT-FLOAT arr)
  (encode-float-parts v arr))

(defun decode-short-float (stream)
  (decode-float-number 1s0 stream))

(defun subtree-decode-short-float (stream)
  (list :SHORT-FLOAT (subtree-decode-float-number stream)))

;; -------------------------------------------

(defmethod encode-object ((v single-float) arr)
  (encode-ubyte $SINGLE-FLOAT arr)
  (encode-float-parts v arr))

(defun decode-single-float (stream)
  (decode-float-number 1e0 stream))

(defun subtree-decode-single-float (stream)
  (list :SINGLE-FLOAT (subtree-decode-float-number stream)))

;; -------------------------------------------

(defmethod encode-object ((v double-float) arr)
  (encode-ubyte $DOUBLE-FLOAT arr)
  (encode-float-parts v arr))

(defun decode-double-float (stream)
  (decode-float-number 1d0 stream))

(defun subtree-decode-double-float (stream)
  (list :DOUBLE-FLOAT (subtree-decode-float-number stream)))

;; -------------------------------------------

(defun encode-raw-string (str arr)
  "Raw strings are used to represent various objects, e.g., Strings, Lisp readable objects,
etc. Raw strings are encoded with an integer prefix count, followed by the characters of
the string."
  (encode-count (length str) arr)
  (dotimes (ix (length str))
    (encode-char (char str ix) arr)))

(defmethod encode-object ((str string) arr)
  "Strings are encoded as a prefix type byte, a prefix count integer,
and the characters of the string."
  (encode-ubyte $STRING arr)
  (encode-raw-string str arr))

(defun decode-string (stream)
  (let* ((nc  (decode-count stream))
         (str (make-string nc)))
    (stream-read-sequence str stream)
    str))

(defun subtree-decode-raw-string (stream)
  (let* ((nc  (subtree-decode-count stream))
         (str (make-string (second nc))))
    (stream-read-sequence str stream)
    (list nc str)))

(defun subtree-decode-string (stream)
  (list :STRING (subtree-decode-raw-string stream)))

;; -------------------------------------------

(defmethod encode-object ((fn pathname) arr)
  (encode-ubyte $PATHNAME arr)
  (encode-raw-string (namestring fn) arr))

(defun decode-pathname (stream)
  (pathname (decode-string stream)))

(defun subtree-decode-pathname (stream)
  (list :PATHNAME (subtree-decode-raw-string stream)))

;; -------------------------------------------

(defun encode-lisp-readable (obj arr)
  "Encode an object as a Lisp readable object. This is the default encoding
for all types that are not covered by explicit encoders. A Lisp readable object
is really just a string encoding but the prefix type byte indicates that the Lisp
reader should be used to interpret that string and build whatever it represents."
  (let ((str (with-output-to-string (s)
               (with-standard-io-syntax
                 (let ((*package*      (find-package :common-lisp))
                       (*print-circle* t))
                   (prin1 obj s) ))) ))
    (encode-ubyte $LISP-READABLE arr)
    (encode-raw-string str arr)))

(defmethod encode-object (obj arr)
  "The default encoder -- encodes as a Lisp readable object."
  (dbg "Encoding unknown object type: ~A obj: ~S"
       (type-of obj) obj)
  (encode-lisp-readable obj arr))

(defun decode-lisp-readable (stream)
  "Decode a Lisp readable object by first decoding the following string. Then have the Lisp
reader interpret that string to construct whatever it represents. This is the default encoding
used for objects not covered by specialized encoders."
  (let ((str (decode-string stream)))
    (with-input-from-string (s str)
      (with-standard-io-syntax
        (let ((*package* (find-package :common-lisp)))
          (read s nil $EOF))
        ))))

(defun subtree-decode-lisp-readable (stream)
  (list :LISP-READABLE (subtree-decode-raw-string stream)))

;; ----------------------------------------

(defmethod encode-object ((sym symbol) arr)
  "Symbols are problematic across remote session connections. Only keyword symbols
have an unambiguous meaning. Keywords are encoded explicitly. Other kinds of symbols
are encoded as Lisp readable objects.

The decoder at the other end knows exactly
what to do with keyword symbols. But how it treats other symbols is unknown. Don't do that
in general, since the meaning is highly ambiguous."
  (cond ((keywordp sym)
         (encode-ubyte $KW-SYMBOL arr)
         (encode-raw-string (symbol-name sym) arr))

        ((null (symbol-package sym))
         (encode-ubyte $UNINTERNED-SYMBOL arr)
         (encode-raw-string (symbol-name sym) arr))

        (t (encode-ubyte $SYMBOL arr)
           (encode-raw-string (package-name (symbol-package sym)) arr)
           (encode-raw-string (symbol-name sym) arr))
        ))

(defun decode-kw-symbol (stream)
  (let ((str (decode-string stream)))
    (um:intern-symbol str :keyword)))

(defun subtree-decode-kw-symbol (stream)
  (list :KEYWORD (subtree-decode-raw-string stream)))

(defun decode-uninterned-symbol (stream)
  (make-symbol (decode-string stream)))

(defun subtree-decode-uninterned-symbol (stream)
  (list :UNINTERNED-SYMBOL (subtree-decode-raw-string stream)))

(defun decode-symbol (stream)
  (let ((package-name (decode-string stream))
        (symbol-name  (decode-string stream)))
    (let ((package (or (find-package package-name)
                       (cerror "Create the package and continue."
                               "Can't find a package called ~S to intern a symbol with name ~S."
                               package-name
                               symbol-name)
                       (make-package package-name))))
      (intern symbol-name package))))

(defun subtree-decode-symbol (stream)
  (list :SYMBOL
        (subtree-decode-raw-string stream) ;; package name
        (subtree-decode-raw-string stream))) ;; symbol name

;; -------------------------------------------

;; Taken straight from swank.lisp --- public domain
;; and then slightly modified
(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Modified to work on non proper lists."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return (values (/ n 2) list)))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

(defmethod encode-object ((lis cons) arr)
  (encode-ubyte $LIST arr)
  (multiple-value-bind (len last) (safe-length lis)
    (encode-count len arr)
    (loop repeat len
          for x on lis do
          (encode-object (car x) arr))
    (encode-object last arr)))

(defmethod decode-list (stream)
  (let* ((nel  (decode-count stream))
         (ret  ())
         (tail ret))
    (dotimes (ix nel)
      (let ((obj (decode-object stream)))
        (if ret
            (setf (cdr tail) (list obj)
                  tail       (cdr tail))
          (setf ret  (list obj)
                tail (last ret)) )))
    (let ((last1 (decode-object stream)))
      (setf (cdr tail) last1))
    ret))

(defmethod subtree-decode-list (stream)
  (let* ((nel (subtree-decode-count stream))
         (ret ())
         (tail ret))
    (dotimes (ix (second nel))
      (let ((obj (subtree-decode-object stream)))
        (if ret
            (setf (cdr tail) (list obj)
                  tail       (cdr tail))
          (setf ret  (list obj)
                tail (last ret)) )))
    (let ((last1 (subtree-decode-object stream)))
      (setf (cdr tail) last1))
    (list :LIST nel ret)))


;; -------------------------------------------

(defmethod encode-object ((v vector) arr)
  "Vectors are encoded in efficient unboxed encodings when they are
specialized. Otherwise, we just encode a collection of boxed objects."
  (let ((eltyp (array-element-type v)))
    (cond ((subtypep eltyp '(unsigned-byte 8))
           (encode-ubyte-vector v arr))
          
          ((subtypep eltyp '(signed-byte 8))
           (encode-byte-vector v arr))

          ((subtypep eltyp 'single-float)
           (encode-single-float-vector v arr))

          ((subtypep eltyp 'double-float)
           (encode-double-float-vector v arr))

          (t (encode-general-vector v arr))
          )))

;; -------------------------------------------------------------

(defun encode-general-vector (v arr)
  (encode-ubyte $VECTOR arr)
  (encode-count (length v) arr)
  (loop for item across v do
        (encode-object item arr)))

(defun decode-vector (stream)
  (let* ((nel (decode-count stream))
         (v   (make-array nel)))
    (loop for ix from 0 below nel do
          (setf (aref v ix) (decode-object stream)))
    v))

(defun subtree-decode-vector (stream)
  (let ((nel (subtree-decode-count stream)))
    (list :VECTOR nel
          (loop for ix from 0 below (second nel) collect
                (subtree-decode-object stream)))))


;; -------------------------------------------------------------

(defun encode-ubytes-of-vector (v arr)
  (loop for b across v do
        (encode-ubyte b arr)))

(defun decode-ubytes-into-vector (v stream)
  (loop for ix from 0 below (length v) do
        (setf (aref v ix) (decode-ubyte stream))))
    
;; -------------------------------------------------------------

(defun encode-ubyte-vector (v arr)
  (encode-ubyte $ubyte-vector arr)
  (encode-count (length v) arr)
  (encode-ubytes-of-vector v arr))

(defun decode-ubyte-vector (stream)
  (let* ((nel (decode-count stream))
         (v   (make-array nel :element-type '(unsigned-byte 8))))
    (decode-ubytes-into-vector v stream)
    v))

(defun subtree-decode-ubyte-vector (stream)
  (let ((nel (subtree-decode-count stream)))
    (list :UBYTE-VECTOR nel
          (loop for ix from 1 to (second nel) collect
                (decode-ubyte stream)))))


;; -------------------------------------------------------------

(defun encode-byte-vector (v arr)
  (encode-ubyte $BYTE-VECTOR arr)
  (encode-count (length v) arr)
  (loop for b across v do
        (encode-ubyte (if (minusp b) (+ 256 b) b) arr)))

(defun decode-byte-vector (stream)
  (let* ((nel (decode-count stream))
         (v   (make-array nel :element-type '(signed-byte 8))))
    (loop for ix from 0 below nel do
          (setf (aref v ix) (decode-byte stream)))
    v))
    
(defun subtree-decode-byte-vector (stream)
  (let ((nel (subtree-decode-count stream)))
    (list :BYTE-VECTOR nel
          (loop for ix from 1 to (second nel) collect
                (decode-ubyte stream)))))


;; -------------------------------------------------------------

(defun encode-single-float-vector (v arr)
  (encode-ubyte $SINGLE-FLOAT-VECTOR arr)
  (encode-count (length v) arr)
  (loop for n across v do
        (encode-float-parts n arr)))

(defun decode-single-float-vector (stream)
  (let* ((nel (decode-count stream))
         (v   (make-array nel :element-type 'single-float)))
    (loop for ix from 0 below nel do
          (setf (aref v ix) (decode-single-float stream)))
    v))

(defun subtree-decode-single-float-vector (stream)
  (let ((nel (subtree-decode-count stream)))
    (list :SINGLE-FLOAT-VECTOR nel
          (loop for ix from 1 to (second nel) collect
                (subtree-decode-object stream)))))


;; -------------------------------------------------------------

(defun encode-double-float-vector (v arr)
  (encode-ubyte $DOUBLE-FLOAT-VECTOR arr)
  (encode-count (length v) arr)
  (loop for n across v do
        (encode-float-parts n arr)))

(defun decode-double-float-vector (stream)
  (let* ((nel (decode-count stream))
         (v   (make-array nel :element-type 'double-float)))
    (loop for ix from 0 below nel do
          (setf (aref v ix) (decode-double-float stream)))
    v))

    
(defun subtree-decode-double-float-vector (stream)
  (let ((nel (subtree-decode-count stream)))
    (list :DOUBLE-FLOAT-VECTOR nel
          (loop for ix from 1 to (second nel) collect
                (subtree-decode-object stream)))))


;; -------------------------------------------

(defmethod encode-object ((a array) arr)
  "Arrays are encoded as first a number of dimensions (1-byte),
the values of the array-dimensions list as successive integers (most likely COUNT objects),
and then the contents of the array are encoded as for vectors. Specialized homogeneous arrays
can be encoded as efficient unboxed values. But in general, the contents will be represented
as a vector of boxed elements."
  (let* ((dims  (array-dimensions a))
         (eltyp (array-element-type a))
         (v     (make-array (array-total-size a)
                            :element-type eltyp
                            :displaced-to a
                            :displaced-index-offset 0)))
    (encode-ubyte $ARRAY arr)
    (encode-ubyte (length dims) arr)
    (loop for dim in dims do
          (encode-count dim arr))
    (encode-object v arr)
    ))

(defun decode-array (stream)
  "Decode an array by getting the number and values of the dimensions list,
decoding the vector of contents, and then adjusting the vector to become the
dimensioned array result."
  (let* ((ndims (decode-ubyte stream))
         (dims (loop for ix from 1 to ndims collect
                     (decode-count stream)))
         (vec  (decode-object stream)))
    (adjust-array vec dims)
    ))

(defun subtree-decode-array (stream)
  (let* ((ndims (decode-ubyte stream))
         (dims  (loop for ix from 1 to (third ndims) collect
                      (decode-count stream)))
         (vec   (subtree-decode-object stream)))
    (list :ARRAY ndims :dims dims :vec vec)))

;; -------------------------------------------
(defmethod encode-object ((v complex) arr)
  "Complex values are encoded as a real object followed by an imaginary object."
  (encode-ubyte $COMPLEX arr)
  (encode-object (realpart v) arr)
  (encode-object (imagpart v) arr))

(defun decode-complex (stream)
  (let* ((re (decode-object stream))
         (im (decode-object stream)))
    (complex re im)
    ))

(defun subtree-decode-complex (stream)
  (let ((re (subtree-decode-object stream))
        (im (subtree-decode-object stream)))
    (list :COMPLEX re im)))

;; -------------------------------------------
(defmethod encode-object ((v rational) arr)
  "Rationals are encoded as a numerator object followed by a denominator object."
  (encode-ubyte $RATIONAL arr)
  (encode-object (numerator v) arr)
  (encode-object (denominator v) arr))

(defun decode-rational (stream)
  (let* ((num (decode-object stream))
         (den (decode-object stream)))
    (/ num den)
    ))

(defun subtree-decode-rational (stream)
  (let ((num (subtree-decode-object stream))
        (den (subtree-decode-object stream)))
    (list :RATIONAL num den)))

;; -------------------------------------------

(defmethod saved-slots (obj)
  (mapcar #'clos:slot-definition-name
          #+:LISPWORKS (clos:class-effective-slots (class-of obj))
          #-:LISPWORKS (class-slots (class-of obj))))

(defun encode-type-object (object arr)
  (let ((slots (remove-if (complement (um:curry #'slot-boundp object))
                          (saved-slots object))))
    (encode-object (type-of object) arr)
    (encode-count (length slots) arr)
    (dolist (slot-name slots)
      (encode-object slot-name arr)
      (encode-object (slot-value object slot-name) arr))
    ))

(defmethod encode-object ((object standard-object) arr)
  ;; A standard object is encoded as:
  ;; - class name
  ;; - number of slots
  ;; - slot values
  (encode-ubyte $STANDARD-OBJECT arr)
  (encode-type-object object arr))

(defmethod encode-object ((object condition) arr)
  (encode-ubyte $CONDITION arr)
  (encode-type-object object arr))

(defun decode-standard-object (stream)
  (let* ((class      (find-class (decode-object stream)))
         (object     (allocate-instance class))
         (nr-slots   (decode-count stream)))
    (dotimes (ix nr-slots)
      (let ((slot-name  (decode-object stream))
            (slot-value (decode-object stream)))
        (setf (slot-value object slot-name) slot-value)))
    object))
      
(defun subtree-decode-standard-object (stream)
  (let ((class-name (subtree-decode-object stream))
        (nr-slots   (subtree-decode-count stream)))
    (list* :STANDARD-OBJECT class-name nr-slots
           (loop for ix from 0 below (second nr-slots) collect
                 (let ((slot-name (subtree-decode-object stream))
                       (slot-value (subtree-decode-object stream)))
                   (cons slot-name slot-value)))
           )))
      
;; -------------------------------------------

; Generify get-slot-details for customization (from Thomas Stenhaug)
(defgeneric get-slot-details (slot-definition)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot details which can be used 
    as an argument to ensure-class")
  (:method ((slot-definition clos:slot-definition))
   (list :name (clos:slot-definition-name slot-definition)
         :allocation (clos:slot-definition-allocation slot-definition)
         :initargs   (clos:slot-definition-initargs slot-definition)
         :initform   (clos:slot-definition-initform slot-definition)
         :readers    (clos:slot-definition-readers slot-definition)
         :type       (clos:slot-definition-type slot-definition)
         :writers    (clos:slot-definition-writers slot-definition))))

(defmethod encode-object ((obj standard-class) arr)
  (encode-ubyte $STANDARD-CLASS arr)
  (encode-object (class-name obj) arr)
  (encode-object (mapcar #'get-slot-details (clos:class-direct-slots obj)) arr)
  (encode-object (mapcar #'class-name (clos:class-direct-superclasses obj)) arr)
  (encode-object (type-of obj) arr))

(defun decode-standard-class (stream)
  (let* ((class  (decode-object stream))
         (slots  (decode-object stream))
         (supers (decode-object stream))
         (meta   (decode-object stream))
         (kwds   '(:direct-slots :direct-superclasses :metaclass))
         (final  (loop for kwd in kwds
                       for slot in (list slots
                                         (or supers (list 'standard-object))
                                         meta)
                       nconc (list kwd slot))))
    (cond ((find-class class nil))
          (t (apply #'clos:ensure-class class final))
          )))

(defun subtree-decode-standard-class (stream)
  (let* ((class  (subtree-decode-object stream))
         (slots  (subtree-decode-object stream))
         (supers (subtree-decode-object stream))
         (meta   (subtree-decode-object stream)))
    (list :STANDARD-CLASS class slots supers meta)))

;; -------------------------------------------

(defmethod encode-object ((obj built-in-class) arr)
  (encode-ubyte $BUILT-IN-CLASS arr)
  (encode-object (class-name obj) arr))

(defun decode-built-in-class (stream)
  (find-class (decode-object stream)))

(defun subtree-decode-built-in-class (stream)
  (list :BUIT-IN-CLASS (subtree-decode-object stream)))

;; -------------------------------------------

(defmethod encode-object ((obj structure-object) arr)
  (encode-ubyte $STRUCTURE arr)
  (let ((slot-names (structure:structure-class-slot-names (class-of obj))))
    (encode-object (type-of obj) arr)
    (encode-count (length slot-names) arr)
    (dolist (slot-name slot-names)
      (encode-object slot-name arr)
      (encode-object (slot-value obj slot-name) arr)) ))

(defun decode-structure (stream)
  (let* ((class (find-class (decode-object stream)))
         (count (decode-count stream))
         (obj   (structure::allocate-instance class)))
    (loop repeat count do
          (let ((slot-name (decode-object stream)))
            (setf (slot-value obj slot-name) (decode-object stream)) ))
    obj))


(defun subtree-decode-structure (stream)
  (let ((name  (subtree-decode-object stream))
        (count (subtree-decode-count stream)))
    (list :STRUCTURE name
          (loop repeat (second count) collect
                (list (subtree-decode-object stream)
                      (subtree-decode-object stream)))) ))
    
;; -------------------------------------------

(defmethod encode-object ((hash-table hash-table) arr)
  (encode-ubyte $HASH-TABLE arr)
  (encode-object (hash-table-test hash-table) arr)
  (encode-object (hash-table-size hash-table) arr)
  (encode-object (hash-table-rehash-size hash-table) arr)
  (encode-object (hash-table-rehash-threshold hash-table) arr)
  (encode-object (hash-table-count hash-table) arr)
  (maphash (lambda (key value)
             (encode-object key arr)
             (encode-object value arr))
           hash-table))

(defun decode-hash-table (stream)
  (let* ((test (decode-object stream))
         (size (decode-object stream))
         (rehash-size (decode-object stream))
         (rehash-threshold (decode-object stream))
         (count (decode-object stream))
         (table (make-hash-table :test test
                                 :size size
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold)))
    (loop repeat count
          do (let* ((key (decode-object stream))
                    (val (decode-object stream)))
               (setf (gethash key table) val)))
    table))

(defun subtree-decode-hash-table (stream)
  (let* ((test (decode-object stream))
         (size (decode-object stream))
         (rehash-size (decode-object stream))
         (rehash-threshold (decode-object stream))
         (count (decode-object stream)))
    (list* :HASH-TABLE test size rehash-size rehash-threshold count
           (loop repeat count
                 collect (let* ((key (decode-object stream))
                                (val (decode-object stream)))
                           (cons key val))
                 )) ))

;; -------------------------------------------
;; Function storing hack.
;; This just stores the function name if we can find it
;; or signal a store-error.

(defun get-function-name (obj)
  (multiple-value-bind (l cp name) (function-lambda-expression obj)
    (declare (ignore l cp))
    (cond ;; normal names and (setf name)
          ((and name (or (symbolp name) (consp name))) name)
          (t (store-error "Unable to determine function name for ~A."
                          obj)))))  

(defmethod encode-object ((obj function) arr)
  (encode-ubyte $FUNCTION arr)
  (encode-object (get-function-name obj) arr))

(defun decode-function (stream)
  (fdefinition (decode-object stream)))

(defun subtree-decode-function (stream)
  (list :FUNCTION (subtree-decode-object stream)))

(defmethod encode-object ((obj generic-function) arr)
  (encode-ubyte $GENERIC-FUNCTION arr)
  (encode-object (get-function-name obj) arr))

(defun decode-generic-function (stream)
  (fdefinition (decode-object stream)))

(defun subtree-decode-generic-function (stream)
  (list :GENERIC-FUNCTION (subtree-decode-object stream)))


;; ---------------------------------------------------------

;; conditions
;; From 0.2.3 all conditions which are signalled from 
;; store or restore will signal a store-error or a 
;; restore-error respectively inside a handler-bind.
(defun sdle-store-report (condition stream)
  (let ((it (caused-by condition)))
    (if it
        (format stream "~A" it)
      (apply #'format stream (format-string condition) 
             (format-args condition)))))

(define-condition sdle-store-error (error)
  ((caused-by :accessor caused-by :initarg :caused-by 
              :initform nil)
   (format-string :accessor format-string :initarg :format-string 
                  :initform "Unknown")
   (format-args :accessor format-args :initarg :format-args :initform nil))
  (:report sdle-store-report)
  (:documentation "Root sdle-store condition"))

(define-condition store-error (sdle-store-error)
  ()
  (:documentation "Error thrown when storing an object fails."))

(define-condition restore-error (sdle-store-error)
  ()
  (:documentation "Error thrown when restoring an object fails."))

(defun store-error (format-string &rest args)
  (error 'store-error :format-string format-string :format-args args))

(defun restore-error (format-string &rest args)
  (error 'restore-error :format-string format-string :format-args args))


;; ---------------------------------------------------------

(defmethod encode-object ((v uuid:uuid) arr)
  "Encode UUID's as big integers"
  (encode-ubyte $UUID arr)
  (encode-ubytes-of-vector (uuid:uuid-to-byte-array v) arr))

(defun decode-uuid (stream)
  "UUID's are coming in as COUNTs"
  (uuid:byte-array-to-uuid
   (let ((v (make-array 16 :element-type '(unsigned-byte 8))))
     (decode-ubytes-into-vector v stream)
     v)))

(defun subtree-decode-uuid (stream)
  (let ((v (make-array 16 :element-type '(unsigned-byte 8))))
    (decode-ubytes-into-vector v stream)
    (list :UUID v)))

;; ---------------------------------------------------------

(defun encode-prefix-length (arr)
  "The total output encoding for a message has a 4-byte prefix count in
network byte-order (big endian). Once the message has been encoded, we can fill in this count,
where the encoding string had reserved the front 4 bytes for this count. The prefix
count does not include itself."
  (let ((len   (- (length arr) 4)))
    (do ((ix 3 (1- ix))
         (v  len (ash v -8)))
        ((minusp ix))
      (setf (aref arr ix) (logand v #x0ff)))
    ))

;; -------------------------------------------

(defun decode-integer-bytes (nb stream)
  (labels ((iter (n val)
             (if (plusp n)
                 (iter (1- n) (logior (ash val 8)
                                      (decode-ubyte stream)))
               val)))
    (iter nb 0)))

(defun decode-prefix-length (arr)
  "Get the message prefix count from the first 4 bytes in the message, which was
transmitted in network byte-order (big-endian)."
  (let ((ais (make-array-input-stream arr)))
    (decode-integer-bytes 4 ais)))

(defun subtree-decode-prefix-length (arr)
  (list :PREFIX-LENGTH (decode-prefix-length arr)))

;; ----------------------------------------------------------
;; routines for testing...

#|
(defun make-encoder-array ()
  (make-array 4
              :element-type 'base-char
              :fill-pointer t
              :adjustable   t))

(defun encode (obj)
  (let ((arr (make-encoder-array)))
    (encode-object obj arr)
    (encode-prefix-length arr)
    arr))

(defun encode-as-lisp (obj)
  (let ((arr (make-encoder-array)))
    (encode-lisp-readable obj arr)
    (encode-prefix-length arr)
    arr))

(defun decode (arr &optional (start 0))
  (with-input-from-string (s arr :start start)
    (decode-object s)))

|#

;; ---------------------------------------------------------------

(defun serialize (msg stream)
  "Serialize a message to the output stream (usually a socket output port). The
message is encoded as a long string of unsigned bytes, or base-chars, with a 4-byte
length prefix in network byte-order (big-endian). The prefix count does not include
the length of itself.

We use the buffer pool of stretchy vectors to hold our encodings. We start by guessing
that we will need 128 bytes. It may grow beyond that size. We leave it to the buffer manager
to sort out how to recycle it when we're finished with it.

Once the message and its 4-byte prefix count is encoded in the buffer we peform a single
I/O call to write the entire stream to the output port."
  (dbg "Send: ~S" msg)
  (let ((*stored-counter* 0)
        (*stored-values*  (get-store-hash)))
    (let ((buf (mgdbuf:get-buffer 128))) ;; 128 is just a guess
      (setf (fill-pointer buf) 4)
      (encode-object msg buf)
      (encode-prefix-length buf)
      (if stream
          (progn
            (write-sequence buf stream)
            (mgdbuf:recycle-buffer buf))
        buf))))

;; ---------------------------------------------------------------

(defun early-eof ()
  (error "Unexpected EOF on input stream"))

;; ---------------------------------------------------------------

(defmethod deserialize ((stream stream))
  "Deserialize an encoded message from the input stream. The message will have a 4-byte
prefix length in network byte-order (big-endian). We strip that count out into a stack
allocated 4-byte array, then decode that length and request a buffer of at least that size
from the buffer manager.

A single I/O read operation then reads the entire encoded message into that buffer, and then
we recursively decode the contents of the buffer. The final decoded message object is returned after
recycling that buffer for another use later. This is an attempt to avoid generating too much garbage."
  (let ((lenseq (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent lenseq))
    (let ((nb (read-sequence lenseq stream)))
      (cond ((zerop nb)  $EOF)
            ((< nb 4)    (early-eof))
            (t  (let* ((nb  (decode-prefix-length lenseq))
                       (buf (mgdbuf:get-buffer nb)))
                  (unwind-protect
                      (if (< (read-sequence buf stream) nb)
                          (early-eof)
                        (let ((ais (make-array-input-stream buf)))
                          (decode-object ais)))
                    (mgdbuf:recycle-buffer buf))
                  ))
            ))))

(defmethod deserialize ((stream array-input-stream))
  "Deserialize an encoded message from the input stream. The message will have a 4-byte
prefix length in network byte-order (big-endian). We strip that count out into a stack
allocated 4-byte array, then decode that length and request a buffer of at least that size
from the buffer manager.

A single I/O read operation then reads the entire encoded message into that buffer, and then
we recursively decode the contents of the buffer. The final decoded message object is returned after
recycling that buffer for another use later. This is an attempt to avoid generating too much garbage."
  (let ((lenseq (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent lenseq))
    (let ((nb (read-sequence lenseq stream)))
      (cond ((zerop nb)  $EOF)
            ((< nb 4)    (early-eof))
            (t  (let* ((nb  (decode-prefix-length lenseq))
                       (buf (mgdbuf:get-buffer nb)))
                  (unwind-protect
                      (if (< (read-sequence buf stream) nb)
                          (early-eof)
                        (let ((ais (make-array-input-stream buf)))
                          (decode-object ais)))
                    (mgdbuf:recycle-buffer buf))
                  ))
            ))))

;; ---------------------------------------------------------------

(defmethod deserialize-to-tree ((stream stream))
  "Deserialize an encoded message from the input stream. The message will have a 4-byte
prefix length in network byte-order (big-endian). We strip that count out into a stack
allocated 4-byte array, then decode that length and request a buffer of at least that size
from the buffer manager.

A single I/O read operation then reads the entire encoded message into that buffer, and then
we recursively decode the contents of the buffer. The final decoded message object is returned after
recycling that buffer for another use later. This is an attempt to avoid generating too much garbage."
  (let ((lenseq (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent lenseq))
    (let ((nb (read-sequence lenseq stream)))
      (cond ((zerop nb)  $EOF)
            ((< nb 4)    (early-eof))
            (t  (let* ((nb  (subtree-decode-prefix-length lenseq))
                       (buf (mgdbuf:get-buffer (second nb))))
                  (unwind-protect
                      (if (< (read-sequence buf stream) (second nb))
                          (early-eof)
                        (let ((ais (make-array-input-stream buf)))
                          (list nb (subtree-decode-object ais))))
                    (mgdbuf:recycle-buffer buf))
                  ))
            ))))

  
(defmethod deserialize-to-tree ((stream array-input-stream))
  "Deserialize an encoded message from the input stream. The message will have a 4-byte
prefix length in network byte-order (big-endian). We strip that count out into a stack
allocated 4-byte array, then decode that length and request a buffer of at least that size
from the buffer manager.

A single I/O read operation then reads the entire encoded message into that buffer, and then
we recursively decode the contents of the buffer. The final decoded message object is returned after
recycling that buffer for another use later. This is an attempt to avoid generating too much garbage."
  (let ((lenseq (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent lenseq))
    (let ((nb (stream-read-sequence lenseq stream)))
      (cond ((zerop nb)  $EOF)
            ((< nb 4)    (early-eof))
            (t  (let* ((nb  (subtree-decode-prefix-length lenseq))
                       (buf (mgdbuf:get-buffer (second nb))))
                  (unwind-protect
                      (if (< (stream-read-sequence buf stream) (second nb))
                          (early-eof)
                        (let ((ais (make-array-input-stream buf)))
                          (list nb (subtree-decode-object ais))))
                    (mgdbuf:recycle-buffer buf))
                  ))
            ))))

  
;; ---------------------------------------------------------------

(defun encoding-tree (obj)
  (let* ((buf (serialize obj nil))
         (ais (make-array-input-stream buf)))
    (prog1
        (deserialize-to-tree ais)
      (mgdbuf:recycle-buffer buf))
    ))
|#