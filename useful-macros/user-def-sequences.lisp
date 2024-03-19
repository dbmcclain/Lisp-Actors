;; user-def-sequences.lisp
;;
;; DM/RAL  2024/03/18 14:41:55 UTC
;; ----------------------------------

(defpackage #:user-def-sequences
  (:use #:common-lisp #:com.ral.useful-macros))

(in-package #:user-def-sequences)

;; ----------------------------------

(defclass user-defined-sequence ()
  ())

(defgeneric user-defined-sequence-element-type (x))
(defgeneric user-defined-sequence-length (x))
(defgeneric user-defined-sequence-fetch (x ix))
(defgeneric user-defined-sequence-to-vector (x)
  (:method ((obj user-defined-sequence))
   (let* ((nel (user-defined-sequence-length obj))
          (ans (make-array nel
                           :element-type (user-defined-sequence-element-type obj))
              ))
    (dotimes (ix nel)
      (setf (aref ans ix) (user-defined-sequence-fetch obj ix)))
    ans)))

;; ---------------------------------------------------------------------

(defclass ovly-vec (user-defined-sequence)
  ;; User overlay vectors atop a SYS:TYPED-AREF vector
  ((arr  :reader ovly-vec-arr  :initarg :arr)
   (off  :reader ovly-vec-off  :initarg :off)
   (typ  :reader user-defined-sequence-element-type :initarg :element-type)
   (siz  :reader ovly-vec-elt-size)
   (nel  :reader user-defined-sequence-length :initarg :nel)
   ))

(defmethod initialize-instance :after ((obj ovly-vec) &key element-type &allow-other-keys)
  (with-slots (arr off siz nel) obj
    (setf siz
          (cond
           ((or (eql element-type 'sys:int32)
                (eql element-type 'single-float)
                (equalp element-type '(signed-byte 32))
                (equalp element-type '(unsigned-byte 32)))
            4)
           ((or (eql element-type 'sys:int64)
                (eql element-type 'double-float)
                (equalp element-type '(signed-byte 64))
                (equalp element-type '(unsigned-byte 64)))
            8)
           ((or (equalp element-type '(signed-byte 16))
                (equalp element-type '(unsigned-byte 16)))
            2)
           ((or (equalp element-type '(signed-byte 8))
                (equalp element-type '(unsigned-byte 8)))
            1)
           (t
            (error "Unrecognized element type: ~A" element-type))
           ))
    (assert (<= (ceiling (+ off (* nel siz)) 8) (length arr)))))
             
(defmethod user-defined-sequence-fetch ((obj ovly-vec) (ix fixnum))
  #F
  (with-slots (arr off typ siz nel) obj
    (assert (and (<= 0 ix)
                 (< ix nel)))
    (sys:typed-aref typ arr
                    (the fixnum (+ (the fixnum off)
                                   (the fixnum (* ix (the fixnum siz)))))
                    )))

#|
(let* ((arr  (sys:make-typed-aref-vector (* 4 12 4)))
       (xarr (make-instance 'ovly-vec
                            :arr arr
                            :off (* 3 12 4)
                            :element-type 'single-float
                            :nel 12)))
  (user-defined-sequence-length xarr)
  (inspect xarr))
  
|#
