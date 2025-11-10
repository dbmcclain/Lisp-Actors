;; read-raw-vector.lisp
;;
;; Read/Write Raw Binary Vectors (8, 16, 32, 64-bits) without respect
;; to any network ordering. Just a raw write of vector contents in
;; stored memory byte order.
;;
;; For two machines of same endian order on opposite ends of a network
;; connection, both supporting single- and double-precision IEEE
;; floating point numbers, this is an expedient way to send data.
;;
;; Actual transmission occurs as a stream of :UINT8 octets.
;;
;; DM/RAL  2025/11/09 07:00:50 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.read-raw-vector
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.read-raw-vector)

;; ----------------------------------

(defun %read/write-raw-vector (ctyp direction sequence stream start end)
  ;; Perform actual I/O as :UINT8 octets, up to 16 at a time.
  #F
  (declare (fixnum start))
  (let* ((csiz                (fli:size-of ctyp))
         (nr-elts-per-buf     (truncate (max csiz 16) csiz))
         (buf                 (make-array (* csiz nr-elts-per-buf)
                                          :element-type '(unsigned-byte 8)))
         (end                 (or end (length sequence)))
         (pos                 start))
    (declare (dynamic-extent buf)
             (fixnum csiz end pos nr-elts-per-buf))
    (fli:with-dynamic-foreign-objects ((pb :uint8 :nelems (length buf)))
      (fli:with-coerced-pointer (pel :type ctyp) pb
        (case direction
          
            (:input
             (while (< pos end)
               (let* ((nr-bytes-to-read (* csiz (min nr-elts-per-buf (- end pos))))
                      (nr-bytes-read    (read-sequence buf stream :end nr-bytes-to-read))
                      (nr-elts-read     (truncate nr-bytes-read csiz)))
                 (declare (fixnum nr-bytes-to-read nr-bytes-read nr-elts-read))
                 (when (plusp nr-elts-read)
                   (fli:replace-foreign-array pb       buf
                                              :end1    nr-bytes-read)
                   (fli:replace-foreign-array sequence pel
                                              :start1  pos
                                              :end2    nr-elts-read)
                   (incf pos nr-elts-read))
                 (when (< nr-bytes-read nr-bytes-to-read)
                   (setf end pos))
                 ))
             pos)
            
            (:output
             (while (< pos end)
               (let* ((nr-elts-to-write  (min nr-elts-per-buf (- end pos)))
                      (nr-bytes-to-write (* csiz nr-elts-to-write)))
                 (fli:replace-foreign-array pel sequence
                                            :start2 pos
                                            :end1   nr-elts-to-write)
                 (fli:replace-foreign-array buf pb
                                            :end2   nr-bytes-to-write)
                 (write-sequence buf stream :end nr-bytes-to-write)
                 (incf pos nr-elts-to-write)
                 ))
             sequence)
            )))
    ))

;; --------------------------------------------

(defun c-type-for (lisp-type)
  ;;
  ;; Return the FLI C type corresponding to a Lisp type. Only those
  ;; types are listed here which are compatible with
  ;; FLI:REPLACE-FOREIGN-ARRAY.
  ;;
  (let ((pair (assoc lisp-type '(( single-float       . :float  )
                                 ( double-float       . :double )
                                 ( cl:base-char       . :uint8  )
                                 ( lw:bmp-char        . :uint16 )
                                 ( (unsigned-byte  8) . :uint8  )
                                 ( (unsigned-byte 16) . :uint16 )
                                 ( (unsigned-byte 32) . :uint32 )
                                 ( (unsigned-byte 64) . :uint64 )
                                 ( (  signed-byte  8) . :int8   )
                                 ( (  signed-byte 16) . :int16  )
                                 ( (  signed-byte 32) . :int32  )
                                 ( (  signed-byte 64) . :int64  ))
                     :test #'equal) ))
    (if pair
        (cdr pair)
      (error "No corresponding C-Type"))
    ))

;; --------------------------------------------
;; Switch between standard :UINT8 reading/writing, and otherwise

(defun rv-dispatch (direction uint8-fn sequence &rest args)
  (let ((ctyp (c-type-for (array-element-type sequence))))
    (if (eql ctyp :uint8)
        (funcall uint8-fn)
      (apply #'%read/write-raw-vector ctyp direction sequence args))
    ))

;; --------------------------------------------

(defgeneric read-raw-vector (sequence stream &key start end)
  (:method ((sequence vector) stream &key (start 0) end)
   ;;
   ;; Read elements into a raw vector whose element type has
   ;; 8, 16, 32, or 64 bits. This includes :FLOAT and :DOUBLE.
   ;;
   ;; Stream should have element type '(UNSIGNED-BYTE 8).
   (rv-dispatch :input
                (lambda ()
                  (read-sequence sequence stream
                                 :start start
                                 :end end))
                sequence stream start end)))
   
(defgeneric write-raw-vector (sequence stream &key start end)
  (:method ((sequence vector) stream &key (start 0) end)
   ;;
   ;; Write elements from a raw vector whose element type has
   ;; 8, 16, 32, or 64 bits. This includes :FLOAT and :DOUBLE.
   ;;
   ;; Stream should have element type '(UNSIGNED-BYTE 8).
   (rv-dispatch :output
                (lambda ()
                  (write-sequence sequence stream
                                  :start start
                                  :end   end))
                sequence stream start end)))

;; --------------------------------------------

(defclass raw-vector-augmented-stream-mixin ()
  ())

(defun make-raw-vector-augmented-stream (stream)
  (let ((class-name   (concatenate 'string
                                   (string :RV-AUGM-)
                                   (string (class-name (class-of stream))))
                      ))
    (wrap-instance-with-mixin stream
                              :mixin-class 'raw-vector-augmented-stream-mixin
                              :class-name  class-name)
    ))

;; --------------------------------------------

(defmethod stream:stream-read-sequence ((stream raw-vector-augmented-stream-mixin) (sequence vector) start end)
  ;;
  ;; Read elements into a raw vector whose element type has
  ;; 8, 16, 32, or 64 bits. This includes :FLOAT and :DOUBLE.
  ;;
  ;; Stream should have element type '(UNSIGNED-BYTE 8).
  (rv-dispatch :input
               (lambda ()
                 (call-next-method))
               sequence stream start end))

(defmethod stream:stream-write-sequence ((stream raw-vector-augmented-stream-mixin) (sequence vector) start end)
  ;;
  ;; Write elements from a raw vector whose element type has
  ;; 8, 16, 32, or 64 bits. This includes :FLOAT and :DOUBLE.
  ;;
  ;; Stream should have element type '(UNSIGNED-BYTE 8).
  (rv-dispatch :output
               (lambda ()
                 (call-next-method))
               sequence stream start end))

