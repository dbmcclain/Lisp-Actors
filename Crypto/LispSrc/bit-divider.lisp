;; bit-divider.lisp
;;
;; DM/RAL  2024/03/07 08:38:12 UTC
;; ----------------------------------

(defpackage #:bit-divider
  (:use #:common-lisp #:def*))

(in-package #:bit-divider)

;; ----------------------------------

(defun get-bits (lev start nbits)
  ;; LEV is a little-endian byte vector
  ;; START is bit number into the vector
  ;; NBITS is how many you want
  (let+ ((:mvb (off bit-start) (truncate start 8))
         (:mvb (end bit-end)   (truncate (+ start nbits) 8))
         (limit (length lev)))
    (um:nlet iter ((ans     0)
                   (pos     off)
                   (bit-pos bit-start)
                   (kbits   0))
      (if (< pos limit)
          (if (< pos end)
              (let* ((nel  (- 8 bit-pos))
                     (bits (byte nel bit-pos)))
                (iter (+ ans (ash (ldb bits (aref lev pos)) kbits))
                      (1+ pos)
                      0
                      (+ kbits nel)))
            ;; else
            (when (> bit-end bit-pos)
              (let* ((nel  (- bit-end bit-pos))
                     (bits (byte nel bit-pos))) 
                (+ ans (ash (ldb bits (aref lev pos)) kbits) )) ))
        ;; else
        ans))
    ))

(defun store-bits (lev start nbits val)
  ;; LEV is a little-endian byte vector
  ;; START is bit number into the vector
  ;; NBITS is how many you want
  (let+ ((:mvb (off bit-start) (truncate start 8))
         (:mvb (end bit-end)   (truncate (+ start nbits) 8))
         (limit (length lev)))
    (um:nlet iter ((pos     off)
                   (bit-pos bit-start)
                   (kbits   0))
      (when (< pos limit)
        (if (< pos end)
            (let* ((dst-val   (aref lev pos))
                   (nel       (- 8 bit-pos))
                   (dst-bits  (byte nel bit-pos))
                   (src-bits  (byte nel kbits)))
              (setf (ldb dst-bits dst-val) (ldb src-bits val)
                    (aref lev pos) dst-val)
              (iter (1+ pos)
                    0
                    (+ kbits nel)))
          ;; else
          (when (plusp bit-end)
            (let* ((dst-val  (aref lev pos))
                   (nel      (- bit-end bit-pos))
                   (dst-bits (byte nel bit-pos))
                   (src-bits (byte nel kbits)))
              (setf (ldb dst-bits dst-val) (ldb src-bits val)
                    (aref lev pos) dst-val))))
        ))))
  
(defun get-bits-vec (lev vec bpw nvec)
  (loop for ix from 0 below nvec
        for bit from 0 by bpw
        do
          (setf (aref vec ix) (get-bits lev bit bpw))))

(defun store-bits-vec (lev vec bpw nvec)
  (loop for ix from 0 below nvec
        for bit from 0 by bpw
        do
        (store-bits lev bit bpw (aref vec ix))))

#|
(setf *print-base* 16)
(setf *print-base* 10)
(setf *print-length* 100)
(setf *print-length* 10)

(let* ((lev (vec-repr:lev-vec (hash:hash/256 :test)))
       (vec (make-array 5 :initial-element 0))
       (bpw 53))
  (get-bits-vec lev vec bpw 5)
  (list lev vec))

(let* ((lev (vec-repr:lev-vec (hash:hash/256 :test)))
       (ref (copy-seq lev))
       (bpw 53)
       (vec (make-array 5 :initial-element 0)))
  (get-bits-vec ref vec bpw 5)
  (fill lev 0)
  (store-bits-vec lev vec bpw 5)
  (or (assert (equalp lev ref))
      (list vec lev ref)))

 |#