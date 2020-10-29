;; lisp-object-encoder.lisp
;; --------------------------------------------------------------------------------------
;; Network and file portable encoding / decoding for Lisp objects
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
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

;; ---------------------------------------------------------------
(in-package #:lisp-object-encoder)
;; ---------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (sdle-store:find-backend 'loe-back-end)
    (sdle-store:defbackend loe-back-end
                           :magic-number (um:magic-word "RALE") ;; RAL Encoding
                           :extends      (sdle-store:sdle-store)
                           )))

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

(defmacro make-buffer! (nel)
  `(make-array ,nel :element-type '(unsigned-byte 8)))

(defmacro with-stack-buffer ((buffer-name nel) &body body)
  `(let ((,buffer-name (make-buffer! ,nel)))
     (declare (dynamic-extent ,buffer-name))
     ,@body))

;; ---------------------------------------------------------------

(defconstant +UUID-code+ (register-code 127 'uuid))

(defstore (obj UUID:UUID stream)
  (output-type-code +UUID-code+ stream)
  (write-sequence (uuid:uuid-to-byte-array obj) stream))

(defrestore (uuid stream)
  (with-stack-buffer (arr 16)
    (read-sequence arr stream)
    (uuid:byte-array-to-uuid arr)))

;; ---------------------------------------------------------------

(defconstant +LZW-code+ (register-code 126 'lzw))

(defstore (obj LZW:COMPRESSED stream)
  (output-type-code +LZW-code+ stream)
  (let ((data (lzw:compressed-data obj)))
    (store-count (length data) stream)
    (write-sequence data stream)))

(defrestore (lzw stream)
  (let* ((nb  (read-count stream))
         (vec (make-array nb :element-type '(unsigned-byte 8))))
    (read-sequence vec stream)
    (lzw:make-compressed
     :data vec)))

;; -------------------------------------------

(defun normalize-prefix-length (prefix-length)
  (ecase prefix-length
    (:8-bit    1)
    (:16-bit   2)
    (:32-bit   4)
    (:64-bit   8)
    (:128-bit 16)
    ((1 2 4 8 16 nil) prefix-length)))

;; ---------------------------------------------------------

(defun encode-prefix-length (buf prefix-length)
  "The total output encoding for a message has a prefix-length byte prefix count in
network byte-order (big endian). Once the message has been encoded, we can fill in this count,
where the encoding string had reserved the front prefix-length bytes for this count. The prefix
count does not include itself.

All data is expected to be an even number of bytes in length, using tail padding if necessary.
Encrypted data is marked as such by making the prefix count odd."
  (do ((ix (1- prefix-length)             (1- ix))
       (v  (- (length buf) prefix-length) (ash v -8)))
      ((minusp ix))
    (setf (aref buf ix) (logand v #x0ff))) )

;; -------------------------------------------

(defun encode (msg &key
                   use-magic
                   prefix-length
                   (align 1)
                   (reserve 0)
                   align-includes-prefix
                   use-buffer
                   (backend 'loe-back-end))
  "Serialize a message to a buffer of unsigned bytes."
  ;; preflen will be one of 1,2,4,8,16 or 0
  (let* ((preflen (or (normalize-prefix-length prefix-length) 0))
         (buf     (ubstream:with-output-to-ubyte-stream (s use-buffer)
                    (labels ((pad-null (nel)
                               (loop repeat nel do
                                     (write-byte 0 s))))
                      
                      ;; reserve room for prefix count and encryption signature
                      (setf (ubstream:stream-file-position s) (+ preflen reserve))

                      (let ((bknd (if use-magic
                                      (sdle-store:copy-backend backend
                                                               :magic-number use-magic)
                                    backend)))
                        (sdle-store:store msg s bknd))
                      
                      ;; pad data to an even number of bytes
                      (let* ((data-len (- (ubstream:stream-file-position s)
                                          (if align-includes-prefix
                                              0
                                            preflen)))
                             (npad     (- (um:align-pwr2 data-len align)
                                          data-len)))
                        (pad-null npad))
                      ))))
    
    (when prefix-length
      (encode-prefix-length buf prefix-length))
    buf))

;; -------------------------------------------

(defun serialize (msg stream &key
                      use-magic
                      prefix-length
                      use-buffer
                      (backend 'loe-back-end))
  "Serialize a message to the output stream (usually a socket output port). The
message is encoded as a long string of unsigned bytes, or base-chars, with a
length prefix in network byte-order (big-endian). The prefix count does not include
the length of itself.

Once the message and its prefix count is encoded in the buffer we peform a single
I/O call to write the entire stream to the output port."
  (write-sequence (encode msg
                          :use-magic     use-magic
                          :prefix-length prefix-length
                          :use-buffer    use-buffer
                          :backend       backend)
                  stream))

;; ---------------------------------------------------------------
;; ---------------------------------------------------------------

(defun early-eof ()
  (error "Unexpected EOF on input stream"))

;; ----------------------------------------------------------

(defun read-raw-bytes (f nb &optional buf)
  ;; may return stream f on EOF
  (let* ((bytes (or buf (make-buffer! nb)))
         (rdlen (read-sequence bytes f)))
    (cond ((zerop rdlen) f)
          ((< rdlen nb)  (early-eof))
          (t bytes)) ))

(defun must-read-raw-bytes (f nb &optional buf)
  ;; error if EOF while reading
  (let ((bytes (read-raw-bytes f nb buf)))
    (if (eq f bytes)
        (early-eof)
      bytes)))

;; ----------------------------------------------------------

(defmacro or-eofp (f tst-val &body body)
  ;; if tst-val is stream f (EOF), then return strean f (for EOF),
  ;; else execute body
  (let ((g!f (gensym)))
    `(let ((,g!f  ,f))
       (if (eq ,g!f ,tst-val)
           ,g!f
         (progn
           ,@body)))
    ))

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

(defun read-prefix-length (f nb)
  (with-stack-buffer (arr nb)
    (or-eofp f (read-raw-bytes f nb arr)
      (decode-prefix-length arr nb)) ))

;; ----------------------------------------------------------

(defmethod skip-data (f prefix-length)
  ;; given that we have a byte prefix length encoding,
  ;; read that length and then skip that many bytes ahead
  (let ((data-len (read-prefix-length f prefix-length)))
    (or-eofp f data-len
      (file-position f (+ (file-position f)
                          data-len)) )))

;; As a concession to Allegro, which does not export
;; stream:stream-file-position we provide two methods here.
(defmethod skip-data ((f ubstream:ubyte-input-stream) prefix-length)
  ;; given that we have a byte prefix length encoding,
  ;; read that length and then skip that many bytes ahead
  (let ((data-len (read-prefix-length f prefix-length)))
    (or-eofp f data-len
      (setf (ubstream:stream-file-position f)
	    (+ (ubstream:stream-file-position f)
	       data-len)) )))

;; ----------------------------------------------------------

(defun read-data (f &key prefix-length)
  (let ((data-len (read-prefix-length f prefix-length)))
    (or-eofp f data-len
      (must-read-raw-bytes f data-len)) ))

;; ----------------------------------------------------------

(defun deserialize-for-length (stream nb &key (backend 'loe-back-end))
  (mgdbuf:with-temporary-buffer (buf nb)
    (must-read-raw-bytes stream nb buf)
    (ubyte-streams:with-input-from-ubyte-stream (s buf)
      (sdle-store:restore s backend))))

(defun deserialize-prefixed-stream (stream prefix-length &key (backend 'loe-back-end))
  (let ((nb (read-prefix-length stream prefix-length)))
    (or-eofp stream nb
      (deserialize-for-length stream nb :backend backend)) ))

;; -----------------------------------------------------------------------------

(defun deserialize (stream &key
                           use-magic
                           prefix-length
                           length
                           (backend 'loe-back-end))
  "Deserialize an encoded message from the input stream. The message will have a 4-byte
prefix length in network byte-order (big-endian). We strip that count out into a stack
allocated 4-byte array, then decode that length and request a buffer of at least that size
from the buffer manager.

A single I/O read operation then reads the entire encoded message into that buffer, and then
we recursively decode the contents of the buffer. The final decoded message object is returned after
recycling that buffer for another use later. This is an attempt to avoid generating too much garbage."
  (let ((preflen (normalize-prefix-length prefix-length))
        (bknd    (if use-magic
                     (sdle-store:copy-backend backend
                                              :magic-number use-magic)
                   backend)))
    (cond (preflen
           (deserialize-prefixed-stream stream preflen :backend bknd))
            
          (length
           (deserialize-for-length stream length :backend bknd))
          
          (t  (sdle-store:restore stream bknd))
          )))

;; -----------------------------------------------------------------------------

(defun decode (arr &key
                   (start 0)
                   use-magic
                   prefix-length
                   length
                   (reader 'aref)
                   (backend 'loe-back-end))
  (ubstream:with-input-from-ubyte-stream (s arr :start start :reader reader)
    (deserialize s
                 :use-magic     use-magic
                 :prefix-length prefix-length
                 :length        length
                 :backend       backend)))

;; -----------------------------------------------------------------------------

(defun ensure-portable-condition (err)
  ;; sometimes Condition objects are not portable
  ;; across a network connection. But strings always are.
  ;; So we interpret the error on this end, if necessary.
  (multiple-value-bind (v e)
      (ignore-errors (loenc:encode err))
    (declare (ignore v))
    (if e
        (um:format-error err)
      err)))

;; -----------------------------------------------------------------------------
