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

(in-package :lzw)

;; ----------------------------------------------------------------------------
;; This portion shamelessly lifted from some unknown source...

(declaim (ftype (function (vector vector &optional fixnum fixnum) vector)
                vector-append))
(defun vector-append (old new &optional (start2 0) end2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (prog1 old                     
    (let* ((old-fill (fill-pointer old))
           (new-fill (+ old-fill (length new))))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (replace old new :start1 old-fill :start2 start2 :end2 end2))))
 
(declaim (ftype (function (vector t) vector) vector-append1))
(defun vector-append1 (old new)
  (prog1 old                     
    (let* ((old-fill (fill-pointer old))
           (new-fill (1+ old-fill)))
      (when (> new-fill (array-dimension old 0))
        (adjust-array old (* 4 new-fill)))
      (setf (fill-pointer old) new-fill)
      (setf (aref old old-fill) new))))
 
(declaim (ftype (function (&optional t) vector) make-empty-vector))
(defun make-empty-vector (&optional (element-type t))
  (make-array 0 :element-type element-type :fill-pointer 0 :adjustable t))
 
 
(declaim (ftype (function (t &optional t) vector) make-vector-with-elt))
(defun make-vector-with-elt (elt &optional (element-type t))
  (make-array 1 :element-type element-type
                :fill-pointer 1
                :adjustable t
                :initial-element elt))
 
(declaim (ftype (function (vector t) vector) vector-append1-new))
(defun vector-append1-new (old new)
  (vector-append1 (vector-append (make-empty-vector 'octet) old)
                  new))
 
(declaim (ftype (function (vector vector) vector) vector-append-new))
(defun vector-append-new (old new)
  (vector-append (vector-append (make-empty-vector 'octet) old)
                 new))
 
(deftype octet () '(unsigned-byte 8))
 
(declaim (ftype (function () hash-table) build-dictionary))
(defun build-dictionary ()
  (let ((dictionary (make-hash-table :test #'equalp)))
    (loop for i below 256
          do (let ((vec (make-vector-with-elt i 'octet)))
               (setf (gethash vec dictionary) vec)))
    dictionary))
 
(declaim (ftype (function ((vector octet)) (vector octet))
                lzw-compress-octets))
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

(declaim (ftype (function (vector) (vector octet)) lzw-decompress))
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
  (:method ((string string))
    (lzw-compress
     (babel:string-to-octets string)))
  (:method ((octets vector))
    (lzw-compress-octets octets)))
 
(defun lzw-decompress-to-string (octets)
  (babel:octets-to-string (lzw-decompress octets)))

;; end of shameless lifting...
;; -------------------------------------------------------------

#|
(defun test (string)
  (assert (equal #2=(lzw-decompress-to-string (lzw-compress string)) string) ()
          "Can't compress ~S properly, got ~S instead" string #2#)
  t)
|#

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
                  (um:nlet-tail iter ((x     x)
                                      (hibit 0)
                                      (ans   nil))
                    (let ((acc  (cons (logior hibit (logand x 127))
                                      ans))
                          (xshf (ash x -7)))
                      (if (plusp xshf)
                          (iter xshf #x80 acc)
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

(defstruct compressed
  data)

(defmethod compress (x)
  ;; Compress an arbitrary Lisp object to a byte vector.  Overt
  ;; byte-vec are used because these serialize more efficiently than
  ;; general arrays of T.
  (make-compressed
   :data (cvt-intvec-to-octets
          (lzw-compress
           (loenc:encode x)))))

(defmethod compress ((x compressed))
  x)

(defmethod decompress (x)
  x)

(defmethod decompress ((x compressed))
  ;; decompress a byte vector back to an arbitrary Lisp object
  (loenc:decode
   (lzw-decompress
    (cvt-octets-to-intvec
     (compressed-data x)))))

;; -----------------------------------------------------------
;; Compression based on Zlib - better compression

(defstruct zl-compressed
  data)

(defun zl-compress (x)
  (make-zl-compressed
   :data (zlib:compress (loenc:encode x) :fixed)))

(defmethod decompress ((x zl-compressed))
  (loenc:decode
   (zlib:uncompress
    (zl-compressed-data x))))

