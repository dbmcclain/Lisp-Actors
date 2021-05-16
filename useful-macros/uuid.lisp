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

;;;; Author Boian Tzonev <boiantz@gmail.com>
;;;; 2007, All Rights Reserved 
;;;;
;;;; This software may be distributed and used according to the terms of the
;;;; Lisp Lesser GNU Public License (LLGPL)
;;;; (http://opensource.franz.com/preamble.html).

;;;; Shamelessly hacked by DM/SD 2008, 2009

(defpackage #:uuid
  (:use #:common-lisp)
  (:shadow #:random)
  (:import-from :um
   :defmonitor
   :with-exclusive-access
   :critical-section)
  (:import-from #:ironclad
   #:make-digest
   #:update-digest
   #:produce-digest
   #:ascii-string-to-byte-array)
  (:export
   #:uuid
   #:make-null-uuid
   #:make-uuid-from-string
   #:make-v1-uuid
   #:make-v3-uuid 
   #:make-v4-uuid
   #:make-v5-uuid
   #:+namespace-dns+
   #:+namespace-url+
   #:+namespace-oid+ 
   #:+namespace-x500+
   #:print-bytes
   #:uuid-to-byte-array
   #:byte-array-to-uuid

   #:uuid=
   ;; the following only make sense for Version 1 UUID's
   #:uuid<
   #:uuid-time
   #:uuid-mac
   #:uuid-time<
   #:max-uuid
   #:uuid-to-universal-time
   #:uuid-to-integer
   #:integer-to-uuid
   #:compare-uuid
   #:compare-uuid-time
   #:copy-v1-uuid-replacing-time
   #:when-created
   #:uuid-string
   #:one-of-mine?
   ))

(in-package #:uuid)

(defmonitor random-engine
    ((state (load-time-value
             #+:LISPWORKS (lw:make-mt-random-state t)
             #-:LISPWORKS (cl:make-random-state t))))
  (defun random (arg)
    (critical-section
      #+:LISPWORKS (lw:mt-random arg state)
      #-:LISPWORKS (cl:random arg state))))

(defvar *clock-seq* 0 
  "Holds the clock sequence. Is is set when a version 1 uuid is 
generated for the first time and remains unchanged during a whole session.")

(defvar *node* nil 
  "Holds the IEEE 802 MAC address or a random number 
  when such is not available")

#|
;; adjusted to 10 ticks per count for TOD in usec (100 ns units) ;; DM/RAL
(defconstant +ticks-per-count+ 10
  "Holds the amount of ticks per count. The ticks per count determine the number 
of possible version 1 uuids created for one time interval. Common Lisp provides 
INTERNAL-TIME-UINITS-PER-SECOND which gives the ticks per count for the current system so 
+ticks-per-count+ can be set to INTERNAL-TIME-UINITS-PER-SECOND")
|#

(um:eval-always
  #+:sbcl
  (setf *random-state* (make-random-state t))
  
  (defclass uuid ()
    ((time-low :accessor time-low :initarg :time-low :initform 0)
     (time-mid :accessor time-mid :initarg :time-mid :initform 0)
     (time-high-and-version :accessor time-high :initarg :time-high :initform 0)
     (clock-seq-and-reserved :accessor clock-seq-var :initarg :clock-seq-var :initform 0)
     (clock-seq-low :accessor clock-seq-low :initarg :clock-seq-low :initform 0)
     (node :accessor node :initarg :node :initform 0))
    (:documentation "Represents an uuid"))

  (defun make-uuid-from-string (uuid-string)
    "Creates an uuid from the string represenation of an uuid. (example input string 6ba7b810-9dad-11d1-80b4-00c04fd430c8)"
    (setf uuid-string (string-trim "{}" uuid-string))
    (make-instance 'uuid
		   :time-low (parse-integer uuid-string :start 0 :end 8 :radix 16)
		   :time-mid (parse-integer uuid-string :start 9 :end 13 :radix 16)
		   :time-high (parse-integer uuid-string :start 14 :end 18 :radix 16)
		   :clock-seq-var (parse-integer uuid-string :start 19 :end 21 :radix 16)
		   :clock-seq-low (parse-integer uuid-string :start 21 :end 23 :radix 16)
		   :node (parse-integer uuid-string :start 24 :end 36 :radix 16))))

;; Those should be constants but I couldn't find a way to define a CLOS object to be constant
(um:defconstant+ +namespace-dns+ (make-uuid-from-string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  "The DNS Namespace. Can be used for the generation of uuids version 3 and 5")
(um:defconstant+ +namespace-url+ (make-uuid-from-string "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
  "The URL Namespace. Can be used for the generation of uuids version 3 and 5")
(um:defconstant+ +namespace-oid+ (make-uuid-from-string "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
  "The OID Namespace. Can be used for the generation of uuids version 3 and 5")
(um:defconstant+ +namespace-x500+ (make-uuid-from-string "6ba7b814-9dad-11d1-80b4-00c04fd430c8")
  "The x500+ Namespace. Can be used for the generation of uuids version 3 and 5")

#+(AND :MACOSX :LISPWORKS)
(defun get-node-id ()
  "Get MAC address of first ethernet device"
  (let ((node
         (let ((txt (with-output-to-string (s)
                      (sys:call-system-showing-output
                       "/sbin/ifconfig"
                       :output-stream s)) ))
           (multiple-value-bind (start end starts ends)
               (cl-ppcre:scan 
                ";[ \\t]*ether[ \\t]+(([0-9a-fA-F]{2}:){5}[0-9a-fA-F]{2})[ \\t]" 
                txt)
             (declare (ignore start end))
             (when starts
               (parse-integer (delete #\: (subseq txt
                                                  (aref starts 0)
                                                  (aref ends 0)))
                              :radix 16)) ))))
    (or node
        (dpb #b01 (byte 8 0) (random #xffffffffffff)))
    ))

#+(AND :MACOSX :ALLEGRO)
(defun get-node-id ()
  "Get MAC address of first ethernet device"
  (let ((node
         (let ((txts (excl.osi:command-output
                      "/sbin/ifconfig")
                     ))
           (loop for txt in txts
                 do
                 (multiple-value-bind (start end starts ends)
                     (cl-ppcre:scan 
                      "[ \\t]*ether[ \\t]+(([0-9a-fA-F]{2}:){5}[0-9a-fA-F]{2})[ \\t]"
                      txt)
                   (declare (ignore start end))
                   (when starts
                     (return (parse-integer (delete #\: (subseq txt
                                                                (aref starts 0)
                                                                (aref ends 0)))
                                            :radix 16)) ))) )))
    (or node
        (dpb #b01 (byte 8 0) (random #xffffffffffff)))
    ))

#-:MACOSX
(defun get-node-id ()
  "Get MAC address of first ethernet device"
  (let ((node
	 #+(and :linux (or :cmu :sbcl))
	 ;; todo this can be simplified a bit
	 (let ((proc #+(and :linux :cmu)
		     (ext:run-program "/sbin/ifconfig"
				      nil
				      :pty nil 
				      :wait t 
				      :output :stream 
				      :error t
				      :if-error-exists nil)
		     #+(and :linux :sbcl)
		     (sb-ext:run-program "/sbin/ifconfig" 
					 nil
					 :output :stream
					 :error t
					 :if-error-exists nil
					 :wait nil)
		     ))
	   (loop for line = (read-line #+(and :linux :cmu)
				       (extensions:process-output proc) 
				       #+(and :linux :sbcl)
				       (sb-ext:process-output proc)
				       nil) 
		 while line 
		 when (search "HWaddr" line :test 'string-equal)
		 return (parse-integer (delete #\: (subseq line 38))
				       :radix 16)))
	 #+(and :windows :clisp)
         (let ((output (ext:run-program "ipconfig" 
                                        :arguments (list "/all")
                                        :input nil
                                        :output :stream
                                        :wait t)))
           (loop for line = (read-line output nil) while line 
                 when (search "Physical" line :test 'string-equal)
                 return (parse-integer (delete #\- (subseq line 37)) :radix 16)))

         #+:ALLEGRO
         (dolist (line (excl.osi:command-output "ipconfig /all"))
           (multiple-value-bind (start end starts ends)
              (cl-ppcre:scan "[ \\t]*Physical.+(([0-9a-fA-F]{2}-){5}[0-9a-fA-F]{2})"
                             line)
             (declare (ignore end))
             (when start
               (return (parse-integer (delete #\- (subseq line
                                                          (aref starts 0)
                                                          (aref ends 0)))
                                      :radix 16)))))

         #+(and :WINDOWS :LISPWORKS)
         (let ((output (with-output-to-string (s)
                         (sys:call-system-showing-output "ipconfig /all"
                                                         :output-stream s))))
           (multiple-value-bind (start end starts ends)
               (cl-ppcre:scan ";[ \\t]*Physical.+(([0-9a-fA-F]{2}-){5}[0-9a-fA-F]{2})"
                              output)
             (declare (ignore start end))
             (parse-integer (delete #\- (subseq output
                                                (aref starts 0)
                                                (aref ends 0)))
                            :radix 16)))

         #+(and :linux :lispworks)
         (let ((output (with-output-to-string (s)
                         (sys:call-system-showing-output "ifconfig -a"
                                                         :output-stream s))))
           (let ((pos (search "HWaddr" output :test 'string-equal)))
             (when pos
               (parse-integer (delete #\: (subseq output (+ pos 6) (+ pos 37 6)))
                              :radix 16
                              :junk-allowed t))))

         ))
    (when (not node)
      (setf node (dpb #b01 (byte 8 0) (random #xffffffffffff))))
    node))

	    
(defun load-bytes (b-array &key (byte-size 8) (start 0) end)
  "Helper function to load bytes from a byte-array returning them as integer"
  (let ((ret-val 0))
    (loop for i from start to end
	  for pos from (- end start) downto 0 
	  do (setf ret-val (dpb (aref b-array i) (byte byte-size (* pos byte-size)) ret-val)))
    ret-val))

(defmonitor timestamping 
    ((uuids-this-tick 0)
     (ticks-per-count 10) ;; 10 ticks/count for 100 ns granularity
     (last-time 0))
   (declare (integer uuids-this-tick ticks-per-count last-time))
   (defun get-timestamp ()
     "Get timestamp, compensate nanoseconds intervals"
     (critical-section
       (um:nlet iter ()
         (let ((time-now ;; (+ (* (get-universal-time) 10000000) 100103040000000000)
                         (+ (* 10 (the integer (usec:get-universal-time-usec)))
                            #N|02_208_988_800_0000000|  ;; time offseet between 1970-01-01 and 1900-01-01
                            #N|10_010_304_000_0000000|) ;; time offset in 100ns increments
                         ))  ;; 10_010_304_000 is time between 1582-10-15 and 1900-01-01 in seconds
           (declare (integer time-now))
           (cond ((/= last-time time-now)
                  (setf uuids-this-tick 0
                        last-time time-now)
                  ;; (return-from get-timestamp time-now)
                  time-now)
                 
                 (T 
                  (incf uuids-this-tick)
                  (cond ((< uuids-this-tick ticks-per-count)
                         (+ time-now uuids-this-tick))
                        
                        (T
                         (go-iter))
                        ))
                 ))))))

(defun format-v3or5-uuid (hash ver)
  "Helper function to format a version 3 or 5 uuid. Formatting means setting the appropriate version bytes."
  (when (or (= ver 3) (= ver 5))
    (make-instance 'uuid
		   :time-low (load-bytes hash :start 0 :end 3)
		   :time-mid (load-bytes hash :start 4 :end 5)
		   :time-high (cond ((= ver 3)
				     (dpb #b0011 (byte 4 12) (load-bytes hash :start 6 :end 7)))
				    ((= ver 5)
				     (dpb #b0101 (byte 4 12) (load-bytes hash :start 6 :end 7))))
		   :clock-seq-var (dpb #b10 (byte 2 6) (aref hash 8))
		   :clock-seq-low (aref hash 9)
		   :node (load-bytes hash :start 10 :end 15))))

(defmethod print-object ((id uuid) stream)
  "Prints an uuid in the string represenation of an uuid. (example string 6ba7b810-9dad-11d1-80b4-00c04fd430c8)"
  (format stream "~({~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X}~)" 
          (time-low id)
          (time-mid id)
          (time-high id)
          (clock-seq-var id)
          (clock-seq-low id)
          (node id)))

(defmethod print-object :around ((id uuid) stream)
  (if *print-readably*
      (progn
        ;; (format stream "#$(UUID ")
        (format stream "#.(~S \"" 'uuid)
        (call-next-method)
        (format stream "\")"))
    (call-next-method)))


(defun print-bytes (stream uuid)
  "Prints the raw bytes in hex form. (example output 6ba7b8109dad11d180b400c04fd430c8)"
  (format stream "~({~8,'0X~4,'0X~4,'0X~2,'0X~2,'0X~12,'0X}~)" 
	  (time-low uuid)
	  (time-mid uuid)
	  (time-high uuid)
	  (clock-seq-var uuid)
	  (clock-seq-low uuid)
	  (node uuid)))

(defun make-null-uuid ()
  "Generates a NULL uuid (i.e 00000000-0000-0000-0000-000000000000)"
  (make-instance 'uuid))


(defun make-v1-uuid ()
  "Generates a version 1 (time-based) uuid."
  (let ((timestamp (get-timestamp)))
    (when (zerop *clock-seq*)
      (setf *clock-seq* (random 10000)))
    (when (null *node*)
      (setf *node* (get-node-id)))
    (make-instance 'uuid
                   :time-low (ldb (byte 32 0) timestamp)
                   :time-mid (ldb (byte 16 32) timestamp)
                   :time-high (dpb #b0001 (byte 4 12) (ldb (byte 12 48) timestamp))
                   :clock-seq-var (dpb #b10 (byte 2 6) (ldb (byte 6 8)  *clock-seq*))
                   :clock-seq-low (ldb (byte 8 0) *clock-seq*) 
                   :node *node*)))

(defun one-of-mine? (uuid)
  (and (= 1 (ldb (byte 4 12) (time-high uuid))) ;; type 1?
       (= (node uuid) *node*)))

(defun make-v3-uuid (namespace name)
  "Generates a version 3 (named based MD5) uuid."
  (format-v3or5-uuid 
   (digest-uuid 3 (get-bytes (print-bytes nil namespace)) name)
   3))

(defun make-v4-uuid ()
  "Generates a version 4 (random) uuid"
  (let ((v (random #.(ash 1 128))))
    (make-instance 'uuid
                   :time-low (ldb (byte 32 0) v) ;; (random #xffffffff)
                   :time-mid (ldb (byte 16 32) v) ;; (random #xffff)
                   :time-high (dpb #b0100 (byte 4 12) (ldb (byte 16 48) v #|(random #xffff)|#))
                   :clock-seq-var (dpb #b10 (byte 2 6) (ldb (byte 8 64) v #|(random #xff)|#))
                   :clock-seq-low (ldb (byte 8 72) v) ;; (random #xff)
                   :node (ldb (byte 48 80) v)) #|(random #xffffffffffff)|#
    ))

(defun make-v5-uuid (namespace name)
  "Generates a version 5 (name based SHA1) uuid."
  (format-v3or5-uuid 
   (digest-uuid 5 (get-bytes (print-bytes nil namespace)) name)
   5))

(defun uuid-to-byte-array (uuid)
  "Converts an uuid to byte-array"
  (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
    (with-slots (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
		uuid
		(loop for i from 3 downto 0
		      do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) time-low)))
		(loop for i from 5 downto 4
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
		(loop for i from 7 downto 6
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) time-high-and-version)))
		(setf (aref array 8) (ldb (byte 8 0) clock-seq-and-reserved))
		(setf (aref array 9) (ldb (byte 8 0) clock-seq-low))
		(loop for i from 15 downto 10
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) node)))
    array)))

(defmacro arr-to-bytes (from to array)
  "Helper macro used in byte-array-to-uuid."
  `(loop with res = 0
	 for i from ,from to ,to
	 do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (aref ,array i))
	 finally (return res)))

(defun byte-array-to-uuid (array)
  "Converts a byte-array generated with uuid-to-byte-array to an uuid."
  (assert (and (= (array-rank array) 1)
	       (= (array-total-size array) 16))
	  (array)
	  "Please provide a one-dimensional array with 16 elements of type (unsigned-byte 8)")
  (make-instance 'uuid
		 :time-low (arr-to-bytes 0 3 array)
		 :time-mid (arr-to-bytes 4 5 array)
		 :time-high (arr-to-bytes 6 7 array)
		 :clock-seq-var (aref array 8)
		 :clock-seq-low (aref array 9)
		 :node (arr-to-bytes 10 15 array)))

(defun digest-uuid (ver uuid name)
  "Helper function that produces a digest from a namespace and a string. Used for the 
generation of version 3 and 5 uuids."
  (let ((digester (make-digest (cond ((= ver 3) 
                                      :md5)
                                     ((= ver 5)
                                      :sha1 )))))
   (update-digest digester (ascii-string-to-byte-array uuid))
   (update-digest digester (ascii-string-to-byte-array name))
   (produce-digest digester)))

(defun get-bytes (uuid-string)
  "Converts a uuid-string (as returned by print-bytes) to a string of characters 
built according code-char of each number in the uuid-string"
  (setf uuid-string (string-trim "{}" uuid-string))
  (with-output-to-string (out)
			 (loop with max = (- (length uuid-string) 2)
			       for i = 0 then (+ i 2)
			       as j = (+ i 2)
			       as cur-pos = (parse-integer uuid-string :start i :end j :radix 16)
			       do (format out "~a" (code-char cur-pos))
			       while (< i max))
			 out))

;; ---------------------------------------------------------------
;; More shameless hacks by DM/SD 2008

(defmethod uuid= (a b)
  (eql a b))

(defmethod uuid= ((a uuid) (b uuid))
  "Comparison operation for testing equality of two UUIDs."
  (and  (= (time-low a)      (time-low b))
        (= (time-mid a)      (time-mid b))
        (= (time-high a)     (time-high b))
        (= (clock-seq-var a) (clock-seq-var b))
        (= (clock-seq-low a) (clock-seq-low b))
        (= (node a)          (node b))
        ))

(defun uuid-to-integer (id)
  "Convert a UUID into a (big) integer... only in Lisp!
  Preserves the time order in a V1 UUID."
  (um:->> (time-high id)
          (um:ash-dpb _ 16 (time-mid id))
          (um:ash-dpb _ 32 (time-low id))
          (um:ash-dpb _  8 (clock-seq-var id))
          (um:ash-dpb _  8 (clock-seq-low id))
          (um:ash-dpb _ 48 (node id))))

(defun integer-to-uuid (id)
  "Convert an integer into a UUID.
  Preserves the time order in a V1 UUID."
  (make-instance 'uuid
                 :time-low      (ldb (byte 32 64) id)
                 :time-mid      (ldb (byte 16 96) id)
                 :time-high     (ldb (byte 16 112) id)
                 :clock-seq-var (ldb (byte  8 56) id)
                 :clock-seq-low (ldb (byte  8 48) id)
                 :node          (ldb (byte 48  0) id)))

(defun uuid< (a b)
  ;; really only meaningful for a V1 time-based UUID
  (< (uuid-to-integer a) (uuid-to-integer b)))

(defun uuid-time (id)
  (um:->> 0
          (um:ash-dpb _ 12 (time-high id))
          (um:ash-dpb _ 16 (time-mid id))
          (um:ash-dpb _ 32 (time-low id))))

(defun copy-v1-uuid-replacing-time (new-time uuid)
  (make-instance 'uuid
                 :time-low      (ldb (byte 32  0) new-time)
                 :time-mid      (ldb (byte 16 32) new-time)
                 :time-high     (dpb #b0001 (byte 4 12) (ldb (byte 12 48) new-time))
                 :clock-seq-var (clock-seq-var uuid)
                 :clock-seq-low (clock-seq-low uuid)
                 :node          (node uuid)))
  
(defun uuid-mac (id)
  (node id))

(defun uuid-time< (a b)
  (< (uuid-time a) (uuid-time b)))

(defun compare-uuid (a b)
  (- (uuid-to-integer a) (uuid-to-integer b)))

(defun compare-uuid-time (a b)
  (- (uuid-time a) (uuid-time b)))

(defun max-uuid (a b)
  (if (uuid< a b)
      b
    a))

(defun uuid-to-universal-time (id)
  (truncate (- (uuid-time id)
               #N|02_208_988_800_0000000|
               #N|10_010_304_000_0000000|)
            10000000))

(defmethod make-load-form ((uuid uuid) &optional environment)
  (declare (ignore environment))
  `(make-uuid-from-string ,(format nil "~S" uuid)))

(um:set-/-dispatch-reader "uuid"
                          (lambda (stream)
                            (uuid:make-uuid-from-string
                             (let ((ch (read-char stream)))
                               (if (digit-char-p ch 16)
                                   (um:read-chars-till-delim stream "/" ch)
                                 (um:read-chars-till-delim stream
                                                           (case ch
                                                             (#\{ "}")
                                                             (#\[ "]")
                                                             (#\( ")")
                                                             (#\< ">")
                                                             (t (list ch))) ))))))
#|
(um:set-$-dispatch-reader :uuid
                          (lambda (sym)
                            ;; symbol or string acceptable
                            (uuid:make-uuid-from-string (string sym))))
|#

(defmethod uuid (str)
  ;; string or symbol acceptable
  (make-uuid-from-string (string str)))

(defmethod when-created ((uuid uuid))
  ;; for Type-1 UUID's
  (multiple-value-bind (utc frac)
      (uuid-to-universal-time uuid)
    (multiple-value-bind (ss mm hh d m y dow)
        (decode-universal-time utc 0)
      (let ((mac (uuid-mac uuid))
            (day (aref #.#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                       dow)))
        (format nil
                "~A ~4D~{-~2,'0D~} ~{~2,'0D~^\:~}.~7,'0D UTC ~{~2,'0X~^\:~}"
                day y (list m d) (list hh mm ss)
                frac
                (loop for pos from 40 downto 0 by 8 collect
                      (ldb (byte 8 pos) mac))
                )))))

(defun uuid-string (uuid)
  (princ-to-string uuid))

