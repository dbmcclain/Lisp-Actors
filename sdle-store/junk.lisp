

(defun save-ratio (ratio)
  (gen-object (numerator ratio))
  (gen-object (denominator ratio))
  (gen :make-ratio))

(defun save-positive-integer (int)
  ())

;; ------------------------------

(deftype ubyte ()
  '(unsigned-byte 8))

(defun num->bits (num arr nb)
  (declare (integer num)
           ((array ubyte) arr)
           (fixnum nb))
  (loop for ix fixnum from 0 below (the fixnum (* 8 nb)) by 8
        for jx fixnum from (1- nb) by -1
        do (setf (aref arr jx) (the ubyte (ldb (byte 8 ix) num)) )))

(disassemble #'num->bits)

(defun store-count (count stream)
  (cond
   ((<= count 128)
    (write-byte count stream))

  ((<= count 255)
   (write-byte #x81 stream)
   (write-byte count stream))

  (t
   (let* ((nb (ceiling (integer-length count) 8))
          (arr (make-array nb :element-type 'ubyte)))
     (declare (fixnum nb)
              ((array ubyte) arr))
     (num->bits count arr nb)
     (if (< nb 127)
         (progn
           (write-byte (logior #x80 nb) stream)
           (write-sequence arr stream))
       ;; else
       (progn
         (write-byte #xff stream)
         (store-count nb stream)
         (write-sequence arr stream))
       )))
  ))

(ubstream:with-output-to-ubyte-stream (s)
  (store-count 1234567890 s))

(defun read-count (stream)
  (let ((x (read-byte stream)))
    (declare (type ubyte x))

    (cond
     ((<= x 128) x)
     ((= x 129) (read-byte stream))
     (t
      (let* ((nel (if (= x #xff)
                      (read-count stream)
                    ;; else
                    (logand x #x7f)))
             (arr (make-array nel :element-type 'ubyte)))
        (declare (dynamic-extent arr))
        (read-sequence arr stream)
        (bits->num nel arr)))
     )))

(disassemble #'read-count)

(defun bits->num (nel arr)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (array ubyte) arr)
           (type fixnum nel))
           
  (um:perform iter ((ix  (* 8 (1- nel)))
                    (jx  0)
                    (val 0))
    (declare (fixnum  ix)
             (integer val))
    (if (>= jx nel)
        val
      (iter (the fixnum (- ix 8)) (1+ jx)
            (dpb (the ubyte (aref arr jx))
                 (byte 8 ix)
                 val))
      )))
        
(disassemble #'bits->num)
(compile 'bits->num)
(inspect #'bits->num)

(ubstream:with-input-from-ubyte-stream (s #(131 1 0 0))
  (read-count s))

(ubstream:with-input-from-ubyte-stream (s
                                        (ubstream:with-output-to-ubyte-stream (s)
                                          (store-count 1234567890 s)))
  (read-count s))

;; -----------------------------------------------------------

(defun store-count (count stream)
  (cond
   ((<= count 128)
    (write-byte count stream))

  ((<= count 255)
   (write-byte #x81 stream)
   (write-byte count stream))

  (t
   (let* ((nb (ceiling (integer-length count) 8)))
     (declare (fixnum nb))
     (if (< nb 127)
         (write-byte (logior #x80 nb) stream)
       ;; else
       (progn
         (write-byte #xff stream)
         (store-count nb stream)))
     
     (um:perform iter ((ix (* 8 (1- nb))))
       (write-byte (ldb (byte 8 ix) count) stream)
       (if (> ix 0)
           (iter (- ix 8))))
     ))
  ))

(ubstream:with-input-from-ubyte-stream (s
                                        (ubstream:with-output-to-ubyte-stream (s)
                                          (store-count 1234567890 s)))
  (read-count s))

(ubstream:with-output-to-ubyte-stream (s)
  (store-count 512 s))

(defun read-count (stream)
  (let ((x (read-byte stream)))
    (declare (type ubyte x))

    (cond
     ((<= x 128) x)
     ((= x #x81) (read-byte stream))
     (t
      (let* ((nb (if (= x #xff)
                     (read-count stream)
                   ;; else
                   (logand x #x7f))))
        (um:perform iter ((ix (* 8 (1- nb)))
                          (val 0))
          (if (< ix 0)
              val
            (iter (- ix 8) (dpb (read-byte stream)
                                (byte 8 ix)
                                val))
            ))))
     )))

