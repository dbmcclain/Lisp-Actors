;;; ZLIB
;;;
;;; Copyright (C) 2001-2004  Harald Musum (musum@pvv.org)
;;; Copyright (C) 2004-2006  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:xzlib)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 0) (space 1) (debug 1)
                     #+cmu (ext:inhibit-warnings 3))))

(defparameter *debug-level* 0)
(defparameter *debug-stream* t)

(defmacro debug-format-1 (&body body)
  `(case *debug-level*
     (0 nil)
     (1 (format *debug-stream* ,@body))
     (2 (format *debug-stream* ,@body))))

(defmacro debug-format-2 (&body body)
  `(case *debug-level*
     (0 nil)
     (1 nil)
     (2 (format *debug-stream* ,@body))))

(defconstant +zlib-major-version+ 0)
(defconstant +zlib-minor-version+ 2)

(defvar +fixed-huffman-code-lengths+
  (let ((array (make-array 288)))
    (loop for i from 0 to 143 do (setf (aref array i) 8))
    (loop for i from 144 to 255 do (setf (aref array i) 9))
    (loop for i from 256 to 279 do (setf (aref array i) 7))
    (loop for i from 280 to 287 do (setf (aref array i) 8))
    array)
  "The number of bits used to represent a code length")

(defvar +fixed-huffman-codes+
  (let ((array (make-array 288))
	(start1 #b00110000)
	(start2 #b110010000)
	(start3 #b0000000)
	(start4 #b11000000))
    (loop for i from 0 to 143 do (setf (aref array i) (+ start1 i)))
    (loop for i from 144 to 255 do (setf (aref array i) (+ start2 (- i 144))))
    (loop for i from 256 to 279 do (setf (aref array i) (+ start3 (- i 256))))
    (loop for i from 280 to 287 do (setf (aref array i) (+ start4 (- i 280))))
    array))

(defvar +fixed-huffman-code-bitlist+
  (loop with array = (make-array (length +fixed-huffman-codes+))
	for i from 0 below (length +fixed-huffman-codes+)
	do (loop with length fixnum = (aref +fixed-huffman-code-lengths+ i)
                 with result = '()
                 with code = (aref +fixed-huffman-codes+ i)
                 for j fixnum from 0 below length
                 do (push (ldb (byte 1 j) code) result)
                 finally (setf (aref array i) result))
	finally (return array)))

(defconstant +huffman-end-of-block-symbol+ 256)

(defvar +dynamic-huffman-code-lengths-order+
  '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defconstant +max-non-compressed-block-size+ 65535)

;; According to RFC 1951, minimum distance is 1 and minimum length is
;; 3, so a distance less than 3 does not make sense.  What am I
;; missing?
(defconstant +min-distance+ 3)
(defconstant +max-distance+ 32768)

(defconstant +min-length+ 3)
(defconstant +max-length+ 258)

(defconstant +deflate-max-hash-table-contents-length+ 5)

(defconstant +adler-base+ 65521)

(defvar +length-encoding+
  #((0 3) (0 4) (0 5) (0 6) (0 7) (0 8)
    (0 9) (0 10) (1 11) (1 13) (1 15) (1 17)
    (2 19) (2 23) (2 27) (2 31) (3 35) (3 43)
    (3 51) (3 59) (4 67) (4 83) (4 99) (4 115)
    (5 131) (5 163) (5 195) (5 227) (0 258))
  "Gives the relationship between a code, and extra bits and length")

(defvar +distance-encoding+
  #((0 1) (0 2) (0 3) (0 4) (1 5) (1 7)
    (2 9) (2 13) (3 17) (3 25) (4 33) (4 49)  
    (5 65) (5 97) (6 129) (6 193) (7 257) (7 385)
    (8 513) (8 769) (9 1025) (9 1537) (10 2049) (10 3073)
    (11 4097) (11 6145) (12 8193) (12 12289) (13 16385) (13 24577))
  "Gives the relationship between a code, and extra bits and distance")

(defstruct bit-stream
  (bytes)  ;; may be a Lisp vector or a scatter vector
  (position 0 :type fixnum))

;;; Functions that operate on a bit-stream struct

(defun bit-stream-read-reversed-bit (bit-stream)
  (prog1
      (ldb (byte 1 (- 7 (mod (bit-stream-position bit-stream) 8)))
           (xaref (bit-stream-bytes bit-stream)
                  (ash (bit-stream-position bit-stream) -3)))
    (incf (bit-stream-position bit-stream))))

(defun bit-stream-read-reversed-bits (bit-stream n)
  (loop with result = 0
	for i from 0 below n
	do (setf result (logior result
                                (ash (bit-stream-read-bit bit-stream)
                                     (1- (- n i)))))
	finally (return result)))

(defun bit-stream-read-bit (bit-stream)
  "Return the next bit from BIT-STREAM"
  (declare (optimize speed))
  (let ((position (bit-stream-position bit-stream))
	(array (bit-stream-bytes bit-stream)))
    ;; FIXME. Is position always a fixnum?
    (declare (type fixnum position)
	     ;; (type (vector (unsigned-byte 8)) array)
             )
    (prog1
	(ldb (byte 1 (mod position 8))
	     (xaref array (ash position -3)))
      (incf (bit-stream-position bit-stream)))))

(defun bit-stream-read-bits (bit-stream n)
  "Read N bits from BIT-STREAM"
  (declare (optimize speed))
  (loop with result fixnum = 0
	for i of-type (unsigned-byte 8) from 0 below n
	for bit of-type (unsigned-byte 1) = (bit-stream-read-bit bit-stream)
	do
	(setq result (+ result (ash bit i)))
	finally (return result)))

(defun bit-stream-read-byte (bit-stream)
  "Read the next byte (8 bits) from BIT-STREAM"
  (prog1 (xaref (bit-stream-bytes bit-stream)
                (ash (bit-stream-position bit-stream) -3))
    (incf (bit-stream-position bit-stream) 8)))

(defun bit-stream-read-n-bytes (bit-stream n)
  "Read N bytes from BIT-STREAM"
  (loop with length = 0
        for i from 0 below n
        do (incf length (ash (bit-stream-read-byte bit-stream)
                             (* 8 (- (1- n) i))))
        finally (return length)))

(defun bit-stream-read-length-and-distance (bit-stream code
					    &optional
                                            (distance-huffman-tree nil))
  "Find the length and distance for CODE from BIT-STREAM and return them"
  (check-type code (integer 0 285))
  (values (+ (first (rest (aref +length-encoding+ (- code 257))))
             (bit-stream-read-bits bit-stream
                                   (first (aref +length-encoding+
                                                (- code 257)))))
          (let ((distance-code (if distance-huffman-tree
				   (bit-stream-read-symbol
				    bit-stream
				    distance-huffman-tree)
                                   (bit-stream-read-reversed-bits
                                    bit-stream 5))))
            (+ (first (rest (aref +distance-encoding+ distance-code)))
               (bit-stream-read-bits
		bit-stream
		(first (aref +distance-encoding+ distance-code)))))))

(defun bit-stream-read-symbol (bit-stream huffman-tree)
  "Read bits from BIT-STREAM and find the corresponding symbol in HUFFMAN-TREE"
  (loop until (atom huffman-tree)
        do (setf huffman-tree (if (zerop (bit-stream-read-bit bit-stream))
                                  (car huffman-tree)
                                  (cdr huffman-tree)))
        finally (return huffman-tree)))

(defun bit-stream-write-bit (bit-stream bit)
  "Write BIT to BIT-STREAM"
  (declare (type (unsigned-byte 1) bit)
           (optimize speed))
  (setf (aref (bit-stream-bytes bit-stream)
              (ash (bit-stream-position bit-stream) -3))
        (dpb bit (byte 1 (mod (bit-stream-position bit-stream) 8))
             (aref (bit-stream-bytes bit-stream)
                   (ash (bit-stream-position bit-stream) -3))))
  (incf (bit-stream-position bit-stream)))

(defun bit-stream-write-byte (bit-stream byte)
  "Write BYTE to BIT-STREAM"
  (setf (aref (bit-stream-bytes bit-stream)
	      (ash (bit-stream-position bit-stream) -3))
	byte)
  (incf (bit-stream-position bit-stream) 8))

(defun bit-stream-write-bits (bit-stream bit-list)
  "Write bits from BIT-LIST to BIT-STREAM"
  (loop for bit in bit-list
	do (bit-stream-write-bit bit-stream bit)))

(defun bit-stream-write-bits2 (bit-stream bit-vector)
  (loop for bit across bit-vector
	do (bit-stream-write-bit bit-stream bit)))

(defun bit-stream-pad-to-byte-boundary (bit-stream)
  "If necessary, pads the current byte in BIT-STREAM with zeroes"
  (unless (zerop (mod (bit-stream-position bit-stream) 8))
    (loop for i from (mod (bit-stream-position bit-stream) 8) below 8
	  do (bit-stream-write-bit bit-stream 0))))

;;; Huffman coding utility functions

;; See section 3.2.2 in RFC 1951 for a description of the algorithm.
;; Some of the variable names are also from this section
(defun make-huffman-tree (huffman-code-lengths)
  "Create a Huffman tree from HUFFMAN-CODE-LENGTHS"
  (let* ((max-bits (reduce #'max huffman-code-lengths))
	 (huffman-tree '())
	 (bl-count (make-array (1+ max-bits) :initial-element 0))
	 (next-code (make-array (1+ max-bits) :initial-element 0))
	 (max-code (1- (length huffman-code-lengths))))
    (loop for i from 0 below (length huffman-code-lengths)
	  for code-length = (aref huffman-code-lengths i)
	  do (unless (zerop code-length)
               (incf (aref bl-count code-length))))
    (loop for bits fixnum from 1 to max-bits
	  with code = 0
	  do (setf code (ash (+ code (aref bl-count (1- bits))) 1)
                   (aref next-code bits) code))
    (loop for i fixnum from 0 to max-code
	  for len = (aref huffman-code-lengths i)
	  do (unless (zerop len)
               (setq huffman-tree (huffman-insert-element huffman-tree
                                                          len
                                                          (aref next-code len)
                                                          i))
               (incf (aref next-code len))))
    huffman-tree))

(defun huffman-insert-element (tree length code symbol)
  "Insert SYMBOL into TREE"
  (cond ((= 0 length)
	 (assert (null tree))
	 symbol)
	((logbitp (1- length) code)
	 (unless (consp tree)
	   (setq tree (cons nil nil)))
	 (setf (cdr tree)
	       (huffman-insert-element (cdr tree) (1- length) code symbol))
	 tree)
	(t
	 (unless (consp tree)
	   (setq tree (cons nil nil)))
	 (setf (car tree)
	       (huffman-insert-element (car tree) (1- length) code symbol))
	 tree)))

(defun read-huffman-code-lengths (bit-stream huffman-tree items)
  (loop with result = (make-array items :initial-element 0)
	with i fixnum = 0
	with symbol
	until (= i items)
	do (setq symbol (bit-stream-read-symbol bit-stream huffman-tree))
	   (case symbol
             (16 (let ((count (+ 3 (bit-stream-read-bits bit-stream 2))))
                   (loop for j from 0 below count
                         do (setf (aref result (+ i j))
                                  (aref result (- i 1))))
                   (incf i count)))
             (17 (let ((count (+ 3 (bit-stream-read-bits bit-stream 3))))
                   (loop for j from 0 below count
                         do (setf (aref result (+ i j)) 0))
                   (incf i count)))
             (18 (let ((count (+ 11 (bit-stream-read-bits bit-stream 7))))
                   (loop for j from 0 below count
                         do (setf (aref result (+ i j)) 0))
                   (incf i count)))
             (otherwise
              (setf (aref result i) symbol)
              (incf i)))
        finally (return result)))

(defun fixed-huffman-code (symbol)
  "Return the code for the given SYMBOL."
  (declare (type fixnum symbol)
	   (optimize speed))
  (loop with length fixnum = (aref +fixed-huffman-code-lengths+ symbol)
	with result = '()               ;MV: collect?
	with code = (aref +fixed-huffman-codes+ symbol)
	for i fixnum from 0 below length
	do (push (ldb (byte 1 i) code) result)
	finally (return result)))

(defun fixed-huffman-code2 (symbol)
  (declare (type fixnum symbol)
	   (optimize speed))
  (loop with length fixnum = (aref +fixed-huffman-code-lengths+ symbol)
	with result = (make-array length :element-type '(unsigned-byte 1))
	with code = (aref +fixed-huffman-codes+ symbol)
	for i fixnum from 0 below length
	do (setf (aref result i) (ldb (byte 1 i) code))
	finally (return result)))


;;; DECISION-TREE macro
;;; Thanks to Pascal Bourguignon

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun infix-to-tree (sequence)
    (labels ((itt (items start end)
               (cond
                 ((= start end)       nil)
                 ((= (1+ start) end)  (list (aref items start)))
                 (t (let ((pivot (truncate (/ (+ start end) 2))))
                      (list (aref items pivot)
                            (itt items start pivot)
                            (itt items (1+ pivot) end)))))))
      (let ((vect (coerce sequence 'vector)))
        (itt vect 0 (length vect)))))
    
  (defun map-tree-postfix (fun tree)
    (if (null tree)
        nil
        (funcall fun
                 (first tree)
                 (map-tree-postfix fun (second tree))
                 (map-tree-postfix fun (third  tree))))))

(defmacro decision-tree (expression &rest clauses)
  "
CLAUSES:  Each clause is of the forms:
          (less|:less . <body>) ; must be the first clause if present.
          (<real> . <body>)
DO:       Evaluate the expression, which must be a real,
          and generate a binary decision tree to select the <body>
          of the clause whose limit is <= the expression and
          the next clause limit is > the expression.
"
  (let ((vexpr (gensym))
        (less (when (and (symbolp (first (first clauses)))
                         (string-equal 'less (first (first clauses))))
                (pop clauses)))
        (clauses (sort (coerce clauses 'vector) (function <)
                       :key (function car))))
    `(let ((,vexpr ,expression))
       ,(map-tree-postfix
         (let ((index -1))
           (flet ((gen-case ()
                    (incf index)
                    (if (zerop index)
                       `(progn ,@(cdr less))
                       `(progn ,@(cdr (aref clauses (1- index)))))))
             (lambda (node left right)
               (if (and (null left) (null right))
                   `(if (< ,vexpr ,(car node))
                        ,(gen-case)
                        ,(gen-case))
                   `(if (< ,vexpr ,(car node))
                        ,left
                        ,(if (null right)
                             (gen-case)
                             right))))))
         (infix-to-tree clauses)))))

(defun distance-code (distance)
  "Return the distance-code for a given DISTANCE"
  (decision-tree distance
                 (less (1- distance))
                 (5 4)
                 (7 5)
                 (9 6)
                 (13 7)
                 (17 8)
                 (25 9)
                 (33 10)
                 (49 11)
                 (65 12)
                 (97 13)
                 (129 14)
                 (193 15)
                 (257 16)
                 (385 17)
                 (513 18)
                 (769 19)
                 (1025 20)
                 (1537 21)
                 (2049 22)
                 (3073 23)
                 (4097 24)
                 (6145 25)
                 (8193 26)
                 (12289 27)
                 (16385 28)
                 (24577 29)
                 (32769 (error "A distance larger than 32768 is illegal"))))


(defun length-code (length)
  "Return the length-code for a given LENGTH"
  (decision-tree length
                 (less (+ 254 length))
                 (11 265)
                 (13 266)
                 (15 267)
                 (17 268)
                 (19 269)
                 (23 270)
                 (27 271)
                 (31 272)
                 (35 273)
                 (43 274)
                 (51 275)
                 (59 276)
                 (67 277)
                 (83 278)
                 (99 279)
                 (115 280)
                 (131 281)
                 (163 282)
                 (195 283)
                 (227 284)
                 (258 285)
                 (259 (error "A length larger than 258 is illegal"))))

(defun distance-code-bits (code)
  "Return a list with 5 elements that are the binary representation of CODE."
  (loop with length = 5                 ;MV: collect?
	with result = '()
	for i from 0 below length
	do (push (ldb (byte 1 i) code) result)
	finally (return result)))

(defun distance-code-bits2 (code)
  (loop with result = (make-array 5 :element-type '(unsigned-byte 1))
	for i from 0 below 5
	do (setf (aref result i) (ldb (byte 1 i) code))
	finally (return result)))

(defun extra-distance-bits (distance)
  "The number of extra distance bits that are needed for a given DISTANCE."
  (max (- (integer-length (- distance 1)) 2) 0))

(defun extra-length-bits (length)
  "The number of extra length bits that are needed for a given DISTANCE."
  (max (- (integer-length (- length 3)) 3) 0))

(defun bit-list-from-value (value length)
  "Return a list with a bit representation of VALUE. The list has
LENGTH elements"
  (loop with result = '()              ;MV: collect?
	for i from 0 below length
	do (push (ldb (byte 1 i) value) result)
	finally (return (nreverse result))))

;; FIXME. Shouldn't the result be reversed here too?
(defun bit-vector-from-value (value length)
  (loop with bit-vector = (make-array length :element-type '(unsigned-byte 1))
	for i from 0 below length
	do (setf (aref bit-vector i) (ldb (byte 1 i) value))
	finally (return bit-vector)))

(defun read-32-bits-from-array (array &optional (start 0))
  "Read a 32-bit word from ARRAY, MSB first starting from position START"
  ;; array may be a scatter-vector or a Lisp vector
  (declare (type integer start))
  (loop with length = 0
        for i from 0 below 4
        do (incf length (ash (xaref array (+ start i)) (* 8 (- 3 i))))
        finally (return length)))

;;; Checksum functions

(defun update-adler-32 (adler buffer)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) adler)
           (optimize speed))
  (loop for i fixnum from 0 below (length buffer)
	with s1 of-type (unsigned-byte 32) = (logand adler #xffff)
	with s2 of-type (unsigned-byte 32) = (logand (ash adler -16) #xffff)
	do (setf s1 (mod (+ s1 (aref buffer i)) +adler-base+)
                 s2 (mod (+ s2 s1) +adler-base+))
	finally (return (+ (ash s2 16) s1))))

(defun adler-32 (buffer)
  "Compute Adler-32 checksum of BUFFER.  Based on the sample code in
appendix C of RFC 1950. update-adler-32 does all the work"
  (update-adler-32 1 buffer))

(defun uncompress (buffer &key
                          (uncompressed-size nil)
                          (output-buffer nil)
                          (start 0)
                          (end (xlength buffer)))
  "Uncompresses BUFFER. Returns a vector of bytes containing the uncompressed
data, and the length of the uncompressed data.
UNCOMPRESSED-SIZE is a hint to set the result buffer size.
If UNCOMPRESSED-SIZE is not specified it is set as double of comressed BUFFER
size.
If OUTPUT-BUFFER is specified, the result will be written to this buffer,
and the UNCOMPRESSED-SIZE is ignored.
Otherwise (default) a new array will be created of size UNCOMPRESSED-SIZE
START specifies the start position in the BUFFER (0 by default)
END specifies the end position in the BUFFER (length of BUFFER by default)"
  ;; buffer may be a Lisp vector or a scatter-vector
  (declare (optimize speed))
  (unless output-buffer
    (setf output-buffer (make-array
                         (if (null uncompressed-size) (* 2 (xlength buffer))
                             uncompressed-size)
                         :adjustable t
                         :fill-pointer 0
                         :element-type '(unsigned-byte 8))))
  (loop with bit-stream = (make-bit-stream :bytes buffer :position start)
	with cmf fixnum = (bit-stream-read-byte bit-stream)
	with flg fixnum = (bit-stream-read-byte bit-stream)
	for bfinal fixnum = (bit-stream-read-bits bit-stream 1)
	for btype fixnum = (bit-stream-read-bits bit-stream 2)
	with adler-32 = (read-32-bits-from-array buffer (- end 4))
	do (check-type (ldb (byte 4 0) cmf) (integer 8))
	   (check-type (ldb (byte 1 5) flg) (integer 0))
           (assert (zerop (mod (+ (* 256 cmf) flg) 31)))
           (cond
             ((= btype 0)
              (debug-format-1 "~&Data in non-compressed format.~%")
              (decode-non-compressed-block bit-stream output-buffer))
             ((= btype 1)
              (debug-format-1 "~&Compressed with fixed Huffman codes.~%")
              (decode-fixed-huffman-block bit-stream output-buffer))
             ((= btype 2)
              (debug-format-1 "~&Compressed with dynamic Huffman codes.~%")
              (decode-dynamic-huffman-block bit-stream output-buffer))
             (t (error "Data compression method not recognized.")))
           (debug-format-2 "~&End of block found. Deflate block processed.~%")
           (when (= bfinal 1)
             (unless (= (adler-32 output-buffer) adler-32)
               (error "Adler-32 checksum error"))
             (return (values output-buffer (fill-pointer output-buffer))))))

(defun decode-non-compressed-block (bit-stream result)
  "Decode one non-compressed block in BIT-STREAM and store the result
in RESULT"
  (let ((junk (bit-stream-read-bits bit-stream 5))
	(len (+ (bit-stream-read-byte bit-stream)
		(ash (bit-stream-read-byte bit-stream) 8)))
	(nlen (+ (bit-stream-read-byte bit-stream)
		 (ash (bit-stream-read-byte bit-stream) 8))))
    (declare (ignore junk)
	     (type (vector (unsigned-byte 8)) result)
	     (optimize speed))
    (assert (= (logand #xffff (lognot nlen)) len))
    (dotimes (i len)
      (vector-push-extend (bit-stream-read-byte bit-stream) result))))

(defun decode-fixed-huffman-block (bit-stream result)
  "Decode one block in BIT-STREAM with fixed Huffman coding and store the
result in RESULT."
  (declare (type (vector (unsigned-byte 8)) result)
           (optimize speed))
  (loop with huffman-tree = (make-huffman-tree +fixed-huffman-code-lengths+)
	with i fixnum = (fill-pointer result)
	for symbol fixnum = (bit-stream-read-symbol
			     bit-stream
			     huffman-tree)
	until (= symbol +huffman-end-of-block-symbol+)
	do (if (<= symbol 255)
               (progn 
                 (vector-push-extend symbol result)
                 (incf i))
               (multiple-value-bind (length distance)
                   (bit-stream-read-length-and-distance bit-stream symbol)
                 (declare (type fixnum length distance))
                 (loop for j fixnum from 0 below length
                       with source-index fixnum = (- i distance)
                       do (vector-push-extend
                           (logand (aref result (+ (mod j distance)
                                                   source-index))
                                   #xff)
                           result)
                          (incf i))))))

(defun decode-dynamic-huffman-block (bit-stream result)
  "Decode one block in BIT-STREAM with dynamic Huffman coding and
store the result in RESULT"
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (let ((hlit (+ (bit-stream-read-bits bit-stream 5) 257))
	(hdist (+ (bit-stream-read-bits bit-stream 5) 1))
	(hclen (+ (bit-stream-read-bits bit-stream 4) 4))
        (code-lengths (make-array 19 :initial-element 0 :element-type 'fixnum))
	literal-huffman-tree
        distance-huffman-tree
	code-length-huffman-tree)
    (declare (type fixnum hlit hdist hclen))
    (loop for i fixnum from 1 to hclen
	  for j fixnum in +dynamic-huffman-code-lengths-order+
	  do (setf (aref code-lengths j) (the fixnum (bit-stream-read-bits bit-stream 3))))
    (setq code-length-huffman-tree (make-huffman-tree code-lengths))
    (setq literal-huffman-tree (make-huffman-tree
				(read-huffman-code-lengths
				 bit-stream
				 code-length-huffman-tree
				 hlit)))
    (setq distance-huffman-tree (make-huffman-tree
				 (read-huffman-code-lengths
				  bit-stream
				  code-length-huffman-tree
				  hdist)))
    (loop with i fixnum = (fill-pointer result)
          for symbol fixnum = (the fixnum (bit-stream-read-symbol bit-stream literal-huffman-tree))
	  until (= symbol +huffman-end-of-block-symbol+)
	  do (if (<= symbol 255)
                 (progn
                   (vector-push-extend symbol result)
                   (incf i))
                 (multiple-value-bind (length distance)
                     (bit-stream-read-length-and-distance
                      bit-stream
                      symbol
                      distance-huffman-tree)
                   (declare (type fixnum length distance))
                   (loop with source-index fixnum = (- i distance)
                         for j fixnum from 0 below length
                         do (vector-push-extend
                             (the (unsigned-byte 8)
                                  (logand (aref result
                                                (+ (mod j distance)
                                                   source-index))
                                          #xff))
                                  result)
                            (incf i)))))))

(defun compress (buffer btype)
  "Compresses BUFFER using deflate algorithm of type BTYPE.
Recognized types are :NO-COMPRESSION, :FIXED and :DYNAMIC."
  (loop with cmf fixnum = 120
	with flg fixnum = 156
	with adler-32 = (adler-32 buffer)
	with blocks = (number-of-blocks buffer btype)
	with buffer-length = (length buffer)
	with block-size fixnum = (ceiling buffer-length blocks)
	;; FIXME. Now chooses the size of the array in bit-stream
	;; based on block-size (assumes worst case of no compression).
	;; For small pictures this is not always enough, because you
	;; need some bytes for DEFLATE header etc.
	with bit-stream = (make-bit-stream
			   :bytes (make-array
				   (if (eq btype :no-compression)
				       (+ +max-non-compressed-block-size+ 11)
                                       (+ block-size 200))
				   :element-type '(unsigned-byte 8))
			   :position 0)
	with hash-table = (unless (eq btype :no-compression)
                            (make-hash-table :test 'equalp))
	with bfinal = 0
	with result = (make-array buffer-length
				  :adjustable t
				  :fill-pointer 0
				  :element-type '(unsigned-byte 8))
	for i fixnum from 0 below blocks
	for start = (* i block-size)
        for end = (* (1+ i) block-size)
	with last-bits = nil
	do (when (= i (1- blocks))
             (setq bfinal 1))
           (check-type (ldb (byte 4 0) cmf) (integer 8))
           (check-type (ldb (byte 1 5) flg) (integer 0))
           (assert (zerop (mod (+ (* 256 cmf) flg) 31)))	
           (when (= i 0)
             (bit-stream-write-byte bit-stream cmf)
             (bit-stream-write-byte bit-stream flg))
           (when last-bits
             (bit-stream-write-bits bit-stream last-bits)
             (setq last-bits nil))
           (bit-stream-write-bits bit-stream (list bfinal))
           (ecase btype
             (:no-compression
              (bit-stream-write-bits bit-stream (list 0 0))
              (bit-stream-pad-to-byte-boundary bit-stream)
              (encode-non-compressed-block bit-stream
                                           buffer
                                           start
                                           (min end buffer-length)))
             (:fixed
              (bit-stream-write-bits bit-stream (list 1 0))
              (encode-fixed-huffman-block bit-stream
                                          buffer
                                          hash-table
                                          start (min end buffer-length)))
             (:dynamic
              (bit-stream-write-bits bit-stream (list 0 1))
              (encode-dynamic-huffman-block bit-stream buffer)))
           (when (= bfinal 1)
             (bit-stream-pad-to-byte-boundary bit-stream)
             (loop for j fixnum from 0 to 3
                   do (bit-stream-write-byte bit-stream
                                             (ldb (byte 8 (* 8 (- 3 j)))
                                                  adler-32)))
             (loop for byte across (subseq (bit-stream-bytes bit-stream) 0
                                           (ash (bit-stream-position
                                                 bit-stream) -3))
                   do (vector-push-extend byte result))
             (return (values result (fill-pointer result))))
           (loop for byte across (subseq (bit-stream-bytes bit-stream) 0
                                         (ash (bit-stream-position
                                               bit-stream) -3))
                 do (vector-push-extend byte result))
           (setq last-bits (bit-list-from-value
                            (aref (bit-stream-bytes bit-stream)
                                  (ash (bit-stream-position bit-stream) -3))
                            (mod (bit-stream-position bit-stream) 8)))
           (setf (bit-stream-position bit-stream) 0)))

(defun number-of-blocks (buffer btype)
  "Return the number of blocks that should be used to encode the BUFFER."
  (ecase btype
    ;; FIXME. This is a requirement, might adjust it later (the blocks
    ;; can be smaller)
    (:no-compression
     (ceiling (length buffer) +max-non-compressed-block-size+))
    ;; FIXME. This is just a value to test with
    (:fixed
     (ceiling (length buffer) (floor +max-non-compressed-block-size+ 4)))
    (:dynamic 1)))

(defun find-best-match (buffer match index)
  "Searches all elements in MATCH to find the one with the lowest
position. INDEX is the index to the current position in BUFFER"
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type fixnum index)
           (optimize speed))
  (unless match                   ;MV: ???
    (values nil nil))
  (let ((length 0)
        (temp-length nil)
        (temp-position nil)
        (final-position nil))
    (dolist (position match)
      (setq temp-position
            (mismatch buffer buffer
                      :start1 position
                      :end1 (min index (+ position +max-length+))
                      :start2 index
                      :test 'equal))
      ;; If the length is less than 3 I can discard this match anyway,
      ;; regardless if it's a wrong match or the length is too short.      
      (if temp-position
          (when (>= (- temp-position position) 3)
            (setq temp-length (- temp-position position))
            (when (> temp-length length)
              (setf length temp-length
                    final-position position)))
        ;; Full match.  Calculate length.
          (progn
            (setq temp-length (min (- (length buffer) index)
                                   (- (min index (+ position +max-length+))
                                      position)))
            ;; (format t "~&Full match")
            (when (and (> temp-length length) (>= temp-length 3))
              (setf length temp-length
                    final-position position))))
      (when (>= length +max-length+)
        (return-from find-best-match (values final-position length))))
    (values final-position length)))

(defun add-hash-value (hash-table hash-value index)
  "Remove oldest hash-value index from HASH-TABLE (if necessary) and
push INDEX into HASH-TABLE"
  (declare (type fixnum index)
	   (optimize speed))
  (let ((values (gethash hash-value hash-table)))
    (if (>= (length values) +deflate-max-hash-table-contents-length+)
	(progn
	  (setq values (subseq values
                               0
                               (1- +deflate-max-hash-table-contents-length+)))
	  (push index values)
	  (setf (gethash hash-value hash-table) values))
        (push index (gethash hash-value hash-table)))))

(defun encode-non-compressed-block (bit-stream buffer start end)
  "Encode a DEFLATE block using the non-compressing method"
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type fixnum start end)
	   (optimize speed))
  (let* ((len (- end start))
	 (nlen (logand #xffff (lognot len))))
    (declare (type (unsigned-byte 32) len nlen)
             (optimize speed))
    (assert (= (logand #xffff (lognot nlen)) len))
    (bit-stream-write-byte bit-stream (ldb (byte 8 0) len))
    (bit-stream-write-byte bit-stream (ldb (byte 8 8) len))
    (bit-stream-write-byte bit-stream (ldb (byte 8 0) nlen))
    (bit-stream-write-byte bit-stream (ldb (byte 8 8) nlen))
    (dotimes (i len)
      (bit-stream-write-byte bit-stream (aref buffer (+ i start))))))

(defun encode-fixed-huffman-block (bit-stream buffer hash-table start end)
  "Encode a DEFLATE block using the fixed Huffman code method"
  (declare (type (vector (unsigned-byte 8)) buffer)
           (optimize speed))
  (loop with i = start
;	with buffer-length = (- end start)
	with values = (make-array 3 :element-type '(unsigned-byte 8))
	with symbol 
	with bit-list
        with position
        with length
        with distance
	with length-code
	with distance-code
	with hash-value
	with match
	until (>= i (- end 2))
;	for symbol = (aref buffer i)
;	for bit-list = (fixed-huffman-code symbol)
;	for bit-list = (aref +fixed-huffman-code-bitlist+ symbol)
;       for hash-value = (replace values buffer :start2 i :end2 (+ i 3))
;       for match = (gethash hash-value hash-table)
;	for position = (first match)
;       for position = (find-best-match buffer match i)
;       for distance = (when position (- i position))
	do (setq symbol (aref buffer i))
  	   (setq bit-list (aref +fixed-huffman-code-bitlist+ symbol))
           (setq hash-value (replace values buffer :start2 i :end2 (+ i 3)))
           (setq match (gethash hash-value hash-table))		
           (multiple-value-setq (position length)
             (find-best-match buffer match i))
           (when position
             (setq distance (- i position)))
           (when (and match (not position))
	  ;; We didn't have a match after all!
             (setq match nil))
           (add-hash-value hash-table hash-value i)
           (if (and match
                    (>= distance +min-distance+)
                    (<= distance +max-distance+))
               (progn (when (or (null length) (> length +max-length+))
                        (setq length +max-length+))
                      (when (> length (- end i))
                        (setq length (- end i)))
                      ;; Find the code representing the length
                      (setq length-code (length-code length))
                      ;; Write the length
                      (bit-stream-write-bits bit-stream
                                             (aref
                                              +fixed-huffman-code-bitlist+
                                              length-code))
                      ;; Write out extra length bits if there are any
                      (when (<= 265 length-code 284)
                        (bit-stream-write-bits
                         bit-stream
                         (bit-list-from-value
                          (- length (first (rest (aref +length-encoding+
                                                       (- length-code 257)))))
                          (extra-length-bits length))))
                      ;; Write 5 bits that represent the distance code
                      (setq distance-code (distance-code distance))
                      (bit-stream-write-bits
                       bit-stream
                       (distance-code-bits distance-code))
                      ;; Write extra distance bits if there are any
                      (when (>= distance-code 4)
                        (bit-stream-write-bits
                         bit-stream
                         (bit-list-from-value
                          (- distance
                             (first (rest (aref +distance-encoding+
                                                distance-code))))
                          (extra-distance-bits distance))))
                      (incf i length))
               (progn
                 (bit-stream-write-bits bit-stream bit-list)	  
                 (incf i)))
           ;; Check if we wrote out all bytes from buffer, else write out
           ;; the remaining ones.
           finally (when (< i end)
                     (loop for j fixnum from 0 below (- end i)
                           for bit-list = (fixed-huffman-code (aref buffer (+ i j)))
                           do (bit-stream-write-bits bit-stream bit-list))))
  (bit-stream-write-bits
   bit-stream (fixed-huffman-code +huffman-end-of-block-symbol+)))

(defun encode-dynamic-huffman-block (bit-stream buffer)
  (error "Not implemented yet."))

