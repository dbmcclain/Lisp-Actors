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

;; AONT -- All Or Nothing Transformation
;; ---------------------------------------------------------------

(defun aont-ctr-ecb-enc-dec (key ivec ovec nel)
  (declare (type (vector (unsigned-byte 8)) key ivec ovec)
           (type fixnum nel))
  
  (let ((cvec  (make-array 16
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
        (idvec (make-array 0
                           :element-type '(unsigned-byte 8)
                           :displaced-to ivec
                           :adjustable   t))
        (odvec (make-array 0
                           :element-type '(unsigned-byte 8)
                           :displaced-to ovec
                           :adjustable   t))
        (ecb   (make-ecb-cipher :aes key)))
    
    (declare (type (simple-array (unsigned-byte 8) (16)) cvec)
             (type (vector (unsigned-byte 8)) idvec odvec)
             (dynamic-extent cvec idvec odvec ecb))

    (loop for off from 0 below nel by 16 do
          (let* ((nb    (min 16 (- nel off)))
                 (idvec (adjust-array idvec nb
                                      :displaced-to ivec
                                      :displaced-index-offset off))
                 (odvec (adjust-array odvec nb
                                      :displaced-to ovec
                                      :displaced-index-offset off)))
            
            (declare (dynamic-extent nb idvec odvec)
                     (type fixnum nb)
                     (type (vector (unsigned-byte 8)) idvec odvec))
            
            (replace cvec (convert-int-to-nbytes off 4) :start1 12)
            (safe-encrypt-in-place ecb cvec)
            (map-into odvec 'logxor cvec idvec)))
    ))

(defconstant $aont-canary$
  (uuid:uuid-to-byte-array  #/uuid/{6F7C24A8-1154-11E5-A006-129ADD578F77}))

(defun aont-transform (vec)
  (let* ((nel    (length vec))
         (cnel   (+ nel 16))
         (ovec   (make-cipher-block (+ cnel 32)))
         (key    (ctr-drbg 256)))

    
    ;;
    ;; encrypt the vector and accumulate the hash of the encryption
    ;;
    (replace ovec vec)
    (replace ovec $aont-canary$ :start1 nel)
    (aont-ctr-ecb-enc-dec key ovec ovec cnel)
    ;;
    ;; xor the hash with the key
    ;; storing the xor sum at the tail of the output
    ;;
    (let ((odvec (make-displaced-cipher-block ovec cnel 32)))
      (map-into odvec 'logxor
                key
                (ironclad:digest-sequence :sha256 ovec
                                          :end cnel)))
    
    ovec))


(defun aont-untransform (vec)
    
    (let* ((nel  (length vec))
           (onel (- nel 32))
           (ovec (make-cipher-block onel)))
      ;;
      ;; compute the hash of the body, xor with final 32 bytes
      ;; to find the key, and then create a cipher with that key
      ;;
      (aont-ctr-ecb-enc-dec 
       (let ((key (make-cipher-block 32)))
         (map-into key 'logxor
                   (make-displaced-cipher-block vec onel 32)
                   (ironclad:digest-sequence :sha256 vec
                                             :end onel))
         key)
       vec ovec onel)

      (unless (every '=
                     $aont-canary$
                     (subseq ovec (- onel 16)))
        (error "AONT: corrupt transform"))

      ovec))


;; ------------------------------------------------------------

(defun file-vector (fname)
  (with-open-file (f fname
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (let* ((nel (file-length f))
           (vec (make-cipher-block nel)))
      (read-sequence vec f)
      vec)))

;; ------------------------------------------------------------
;; for LZW Compression of plaintext

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
                  (um:nlet iter ((x     x)
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

;; ----------------------------------------------------------------

(defun write-16u (w s)
  (write-byte (ldb (byte 8 8) w) s)
  (write-byte (ldb (byte 8 0) w) s))

(defun read-16u (s)
  (let* ((b1 (read-byte s))
         (b2 (read-byte s)))
    (dpb b1 (byte 8 8) (dpb b2 (byte 8 0) 0))))

(defun write-32u (w s)
  (write-16u (ldb (byte 16 16) w) s)
  (write-16u (ldb (byte 16  0) w) s))

(defun read-32u (s)
  (let* ((w1 (read-16u s))
         (w2 (read-16u s)))
    (dpb w1 (byte 16 16) (dpb w2 (byte 16 0) 0))))

(defun write-chunk (v s)
  (let ((vc (cvt-intvec-to-octets (lzw-compress v))))
    (write-32u (length vc) s)
    (write-sequence vc s)))

(defun read-chunk (s)
  (let* ((nel (read-32u s))
         (v   (make-array nel
                          :element-type '(unsigned-byte 8))))
    (read-sequence v s)
    (lzw-decompress (cvt-octets-to-intvec v))))

;; -----------------------------------------------------------------
;;  Compressed Vector Store - #data octets = #short + 64K * #chunks
;;     +---------------+
;;     | #chunks (U32) | #full chunks (each chunk = 64KB decompressed data octets)
;;     +---------------+
;;     | #rem (U32)    | #decompressed octets in last (short) chunk, or zero
;;     +---------------+
;;     | #octets (U32) | First Chunk - #octets in LZW compression (might be different from 64KB)
;;     +---------------+
;;     | chunk octets  | LZW octets (7-bit enc)
;;     |       .       |
;;     /               /
;;     /               /
;;     |               |
;;     +---------------+
;;     | #octets (U32) | Second Chunk - #octets in LZW compression
;;     +---------------+
;;     | chunk octets  | LZW octets (7-bit enc)
;;     |       .       |
;;
;; 7-bit integer encoding - 7 bits of data, MSB if more
;; Big-endian write order
;; Data is chunked into <= 64KB sections, then LZW compressed
;; Indeterminate number of LZW octets in 7-bit encoding for each chunk
;;
;; ----------------------------------------------------------------

(defconstant $max-chunk  65536)

(defun write-compression (v s)
  (let ((nb (length v)))
    (multiple-value-bind (nchunks nshort)
        (floor nb $max-chunk)
      (write-32u nchunks s)
      (write-32u nshort  s))
    (do ((pos 0  (+ pos $max-chunk)))
        ((>= pos nb) v)
      (write-chunk (subseq v pos (min (+ pos $max-chunk) nb)) s))
    ))

(defun read-compression (s)
  (let* ((nchunks (read-32u s))
         (nshort  (read-32u s))
         (v       (make-array (+ nshort (* nchunks $max-chunk))
                              :element-type '(unsigned-byte 8))))
    (loop repeat (if (zerop nshort)
                     nchunks
                   (1+ nchunks))
          for pos from 0 by $max-chunk
          for chunk = (read-chunk s)
          do
          (replace v chunk :start1 pos))
    v))

#|
(ubstream:with-output-to-ubyte-stream (s)
  (let* ((v (file-vector "aont.exe")))
    (write-compression v s)))

(let* ((str (hcl:file-string "VTuning/crypto/tools/aont.lisp"))
       (h   (huffman-encode (map 'vector 'char-code str))))
  (map 'string 'code-char (huffman-decode (first h) (third h) (second h))))


(let* ((str (hcl:file-string "VTuning/crypto/tools/aont.lisp"))
       (lzstr (lzw-compress str))
       (h   (huffman-encode lzstr))
       (d   (huffman-decode (first h) (third h) (second h))))
  (lzw-decompress-to-string d))

(let* ((str "This is a test")
       (enc (ubstream:with-output-to-ubyte-stream (s)
              (write-compression str s)))
       (dec (ubstream:with-input-from-ubyte-stream (s enc)
              (read-compression s))))
  dec)

|#

;; ----------------------------------------------------------------

(defmethod aont-encode ((x vector))
  ;; x is string or vector
  (encode-bytes-to-base64
   (aont-transform
    (ubstream:with-output-to-ubyte-stream (s)
      (write-compression x s))
    )))

(defmethod aont-encode ((s string))
  (aont-encode (babel:string-to-octets s)))

(defun aont-decode (str)
  (ubstream:with-input-from-ubyte-stream (s (aont-untransform
                                             (decode-bytes-from-base64 str)))
    (read-compression s)))

(defun aont-decode-to-string (vec)
   (babel:octets-to-string (aont-decode vec)))

;; ----------------------------------------------------------------
#|
(defun tst (s)
  (let ((buf (ubstream:with-output-to-ubyte-stream (buf)
               (write-compression (babel:string-to-octets s) buf))))
    (babel:octets-to-string
     (ubstream:with-input-from-ubyte-stream (s buf)
       (read-compression s)))))
  
(let ((tstv (file-vector "VTuning/crypto/tools/aont.lisp")))
  (map 'string 'code-char (cvt-intvec-to-octets (lzw-compress tstv))))

(let* ((v       (file-vector "VTuning/crypto/tools/aont.lisp"))
       (etstv   (aont-transform v))
       (etstv64 (encode-bytes-to-base64 etstv))
       (dtstv64 (decode-bytes-from-base64 etstv64))
       (dtstv   (aont-untransform dtstv64)))
  (list (equalp etstv dtstv64)
        (equalp dtstv v)
        etstv64))


(let* ((v       (file-vector "VTuning/crypto/tools/aont.lisp"))
       (lzv     (lzw-compress v))
       (lzvx    (cvt-intvec-to-octets lzv))
       (lzvxx   (cvt-octets-to-intvec lzvx))
       (dv      (lzw-decompress lzvxx)))
  (list (equalp dv v)
        dv))

(let* ((v       (file-vector "VTuning/crypto/tools/aont.lisp"))
       (lzv     (lzw-compress v))
       (lzve    (cvt-intvec-to-octets lzv))
       (etstv   (aont-transform lzve))
       (etstv64 (encode-bytes-to-base64 etstv))
       (dtstv64 (decode-bytes-from-base64 etstv64))
       (dtstv   (aont-untransform dtstv64))
       (dlzv    (cvt-octets-to-intvec dtstv))
       (dv      (lzw-decompress dlzv))
       )
  (list (equalp etstv dtstv64)
        (equalp dtstv lzve)
        (equalp dlzv  lzv)
        (equalp dv    v)
        etstv64))


(let* ((tstv (file-vector "VTuning/crypto/tools/aont.lisp"))
       (raw  (encode-bytes-to-base64 (aont-transform tstv)))
       (cmp  (aont-encode tstv)))
  (list (length raw) (length cmp)))

|#