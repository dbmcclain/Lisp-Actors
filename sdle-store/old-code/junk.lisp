#|
(defun num->bits (num )
  (if (zerop num)
      '(0)
    (nreverse
     (loop for val = num then (ash val -7)
           for bit-8 = #x00 then #x80
           until (zerop val)
           collect (logior bit-8 (logand val #X7F)) into bits
           finally (return bits)))
    ))

(defun num->bits (num)
  (if (zerop num)
      '(0)
    (let ((bits nil))
      (loop for val = num then (ash val -7)
            for bit-8 = #x00 then #x80
            until (zerop val)
            do (push (logior bit-8 (logand val #X7F)) bits)
            finally (return bits)))
    ))

(defun num->bits (num)
  (declare (type fixnum num)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))

  (labels ((low7 (val)
             (declare (type fixnum val))
             (the fixnum (logand val #x7F)))
           
           (shr7 (val)
             (declare (type fixnum val))
             (the fixnum (ash val -7))))

    (declare (type fixnum low7 shr7)
             (inline low7 shr7))

    (let ((bits (list (low7 num))))
      (loop for val fixnum = (the fixnum (shr7 num)) then (the fixnum (shr7 val))
            until (= 0 val)
            do (push (logior #x80 (the fixnum (low7 val))) bits))
      bits)))

(defun num->bits (num)
  (let ((bits (list (logand num #x7f))))
    (loop for val = (ash num -7) then (ash val -7)
          until (zerop val)
          do (push (logior #x80 (logand val #x7F)) bits))
    bits))
|#

;; --------------------------------------------------------

;; --------------------------------------------------------------
;; counts = implicit unsigned integers
;; use 7-bit variable length encoding. Bit 8 = read/write another byte
;; last byte has bit-8 = 0.
#|
(defun num->bits (num)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  
  (if (typep num 'fixnum)

      (labels ((low7 (val)
                 (declare (type fixnum val))
                 (the fixnum (logand val #x7F)))
               
               (shr7 (val)
                 (declare (type fixnum val))
                 (the fixnum (ash val -7))))
        
        (declare (inline low7 shr7))
        
        (let ((bits (list (low7 num))))
          (loop for val fixnum = (the fixnum (shr7 num)) then (the fixnum (shr7 val))
                until (zerop val)
                do (push (logior #x80 (the fixnum (low7 val))) bits))
          bits))
  ;; else
  (let ((bits (list (logand num #x7f))))
        (loop for val = (ash num -7) then (ash val -7)
              until (zerop val)
              do (push (logior #x80 (logand val #x7F)) bits))
        bits)))

(defun store-count (count stream)
  (declare (type integer count)
           (type stream stream)
           ;; (xoptimize speed)
	   )
  (write-sequence (num->bits count) stream))

(defun read-count (stream)
  (do* ((x (read-byte stream) (read-byte stream))
        (val (logand x #x7f)  (+ (ash val 7) (logand x #x7f))))
       ((zerop (logand x #x80)) val) ))
|#

;; --------------------------------------------------------------
#|
(deftype ubyte () '(unsigned-byte 8))

(defun num->bits (num arr nb)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type integer num)
           (type (array ubyte) arr)
           (type fixnum nb))
  (loop for ix fixnum from 0 below (the fixnum (* 8 nb)) by 8
        for jx fixnum from (1- nb) by -1
        do (setf (aref arr jx) (the ubyte (ldb (byte 8 ix) num)) )))

(defun bits->num (nel arr)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (array ubyte) arr)
           (type fixnum nel))
           
  (um:perform iter ((ix  0)
                    (jx  (* 8 (1- nel)))
                    (val 0))
    (declare (fixnum  ix jx)
             (integer val))
    (if (>= ix nel)
        val
      (iter (the fixnum (1+ ix))
            (the fixnum (- jx 8))
            (the integer (dpb (the ubyte (aref arr jx))
                              (byte 8 ix)
                              val)))
      )))

(defun store-count (count stream)
  (declare (type integer count))
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
|#

