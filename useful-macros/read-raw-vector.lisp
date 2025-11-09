;; read-raw-vector.lisp
;;
;; DM/RAL  2025/11/09 07:00:50 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.read-raw-vector
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.read-raw-vector)

;; ----------------------------------

(defun %read-raw-vector (ctyp sequence stream start end)
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
        pos ))
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
;; Switch between standard :UINT8 reading, and otherwise

(defun rv-dispatch (sequence stream start end uint8-fn)
  (let ((ctyp (c-type-for (array-element-type sequence))))
    (if (eql ctyp :uint8)
        (funcall uint8-fn)
      (%read-raw-vector ctyp sequence stream start end))
    ))

;; --------------------------------------------

(defun read-raw-vector (sequence stream &key (start 0) end)
  ;;
  ;; Read elements into a raw vector whose element type has
  ;; 8, 16, 32, or 64 bits. This includes :FLOAT and :DOUBLE.
  ;;
  ;; Stream should have element type '(UNSIGNED-BYTE 8).
  (rv-dispatch sequence stream start end
               (lambda ()
                 (read-sequence sequence stream
                                :start start
                                :end end))
               ))

;; --------------------------------------------

(defclass raw-vector-augmented-stream-mixin ()
  ())

(defun make-raw-vector-augmented-stream (stream)
  (let* ((mixin-class  (find-class 'raw-vector-augmented-stream-mixin))
         (stream-class (class-of stream))
         (bridge-class-name (intern (concatenate 'string
                                                 (string :RV-Augm-)
                                                 (string (class-name stream-class)))
                                    (find-package :com.ral.useful-macros)))
         (bridge-class (clos:ensure-class bridge-class-name
                                          :direct-superclasses (list mixin-class stream-class)
                                          :metaclass           (class-of stream-class))))
    (change-class stream bridge-class)))

;; --------------------------------------------

(defmethod stream:stream-read-sequence ((stream raw-vector-augmented-stream-mixin) sequence start end)
  ;;
  ;; Read elements into a raw vector whose element type has
  ;; 8, 16, 32, or 64 bits. This includes :FLOAT and :DOUBLE.
  ;;
  ;; Stream should have element type '(UNSIGNED-BYTE 8).
  (rv-dispatch sequence stream start end
               (lambda ()
                 (call-next-method))))

