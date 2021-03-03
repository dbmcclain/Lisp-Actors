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

;; -----------------------------------------------------
(in-package #:ubyte-streams)
;; -----------------------------------------------------

(defclass ubyte-stream ()
  ())

(defmethod cl:stream-element-type ((stream ubyte-stream))
  '(unsigned-byte 8))

;; ------------------------------------------------------

#+:LISPWORKS
(defclass ubyte-output-stream (stream:fundamental-binary-output-stream
                               ubyte-stream)
  ((arr  :accessor uos-arr
         :initarg  :buffer
         :initform (mgdbuf:make-buffer 1024))))

#+:ALLEGRO
(defclass ubyte-output-stream (excl:fundamental-binary-output-stream
                               ubyte-stream)
  ((arr  :accessor uos-arr
         :initarg  :buffer
         :initform (mgdbuf:make-buffer 1024))))

#+:CLOZURE
(defclass ubyte-output-stream (stream:fundamental-binary-output-stream
                               ubyte-stream)
  ((arr  :accessor uos-arr
         :initarg  :buffer
         :initform (mgdbuf:make-buffer 1024))))


#+:sbcl
(defclass ubyte-output-stream (sb-gray:fundamental-binary-output-stream
                               ubyte-stream)
  ((arr  :accessor uos-arr
         :initarg  :buffer
         :initform (mgdbuf:make-buffer 1024))))

(defun make-ubyte-output-stream (&optional use-buffer)
  (if use-buffer
      (make-instance 'ubyte-output-stream
                     :buffer use-buffer)
    (make-instance 'ubyte-output-stream)))

#+(or :LISPWORKS :CLOZURE :SBCL)
(defmethod stream:stream-write-byte ((stream ubyte-output-stream) val)
  (vector-push-extend val (uos-arr stream))
  val)

#+:ALLEGRO
(defmethod excl:stream-write-byte ((stream ubyte-output-stream) val)
  (vector-push-extend val (uos-arr stream))
  val)

(defmethod stream-bytes ((stream ubyte-output-stream))
  (uos-arr stream))

(defun do-with-output-to-ubyte-stream (fn use-buffer)
  (let ((s (make-ubyte-output-stream use-buffer)))
    (funcall fn s)
    (copy-seq (uos-arr s))))

(defmacro with-output-to-ubyte-stream ((stream-name &optional use-buffer) &body body)
  `(do-with-output-to-ubyte-stream (lambda (,stream-name) ,@body) ,use-buffer))

(defmethod stream-file-position ((stream ubyte-output-stream))
  (fill-pointer (uos-arr stream)))

(defmethod (setf stream-file-position) (pos (stream ubyte-output-stream))
  (let ((arr (uos-arr stream)))
    (if (array-in-bounds-p arr pos)
        (setf (fill-pointer arr) pos)
      (adjust-array arr (max (* 2 (array-total-size arr))
                             (+ pos 128))
                    :fill-pointer pos))))

;; ------------------------------------------------------

#+(or :LISPWORKS :CLOZURE)
(defclass ubyte-input-stream (stream:fundamental-binary-input-stream 
                              ubyte-stream)
  ((arr    :reader   uis-arr    :initarg :arr)
   (ix     :accessor uis-ix     :initarg :start)
   (end    :reader   uis-end    :initarg :end)
   (reader :reader   uis-reader :initarg :reader :initform 'xaref)
   ))

#+(or sbcl)
(defclass ubyte-input-stream (sb-gray:fundamental-binary-input-stream 
                              ubyte-stream)
  ((arr    :reader   uis-arr    :initarg :arr)
   (ix     :accessor uis-ix     :initarg :start)
   (end    :reader   uis-end    :initarg :end)
   (reader :reader   uis-reader :initarg :reader :initform 'xaref)
   ))

#+:ALLEGRO
(defclass ubyte-input-stream (excl:fundamental-binary-input-stream 
                              ubyte-stream)
  ((arr    :reader   uis-arr    :initarg :arr)
   (ix     :accessor uis-ix     :initarg :start)
   (end    :reader   uis-end    :initarg :end)
   (reader :reader   uis-reader :initarg :reader :initform 'xaref)
   ))

(defun make-ubyte-input-stream (arr &key (start 0) end (reader 'xaref))
  (make-instance 'ubyte-input-stream
                 :arr    arr
                 :start  start
                 :end    end
                 :reader reader))

(defclass scatter-vector ()
  ((frags      :accessor scatter-vector-frags      :initform (make-array 1
                                                                         :adjustable   t
                                                                         :fill-pointer 0))
   (last-ix    :accessor scatter-vector-last-ix    :initform 0)
   (last-base  :accessor scatter-vector-last-base  :initform 0)
   (length     :accessor scatter-vector-length     :initform 0)
   ))

(defgeneric in-bounds-p (vec ix)
  (:method ((vec vector) ix)
   (array-in-bounds-p vec ix))
  (:method ((vec scatter-vector) ix)
   (and (not (minusp ix))
        (< ix (scatter-vector-length vec))
        )))

(defgeneric xlength (vec)
  (:method ((vec vector))
   (length vec))
  (:method ((vec scatter-vector))
   (scatter-vector-length vec)))

(defgeneric xaref (vec ix)
  (:method ((vec vector) ix)
   (aref vec ix))
  (:method ((vec scatter-vector) ix)
   ;; written to allow possibility of scatter-vector elements also
   ;; being scatter-vectors
   (let ((ix-rem (- ix (scatter-vector-last-base vec))))
     (when (minusp ix-rem)
       ;; start over from front
       (setf (scatter-vector-last-ix vec)   0
             (scatter-vector-last-base vec) 0
             ix-rem  ix))
     (um:nlet iter ((cur-ix (scatter-vector-last-ix vec))
                    (ix-rem ix-rem))
       (let* ((cur-frag (aref (scatter-vector-frags vec) cur-ix))
              (nel      (xlength cur-frag)))
         (if (< ix-rem nel)
             (xaref cur-frag ix-rem)
           (progn
             (incf (scatter-vector-last-ix vec))
             (incf (scatter-vector-last-base vec) nel)
             (go-iter (1+ cur-ix) (- ix-rem nel)))
           )))
     )))

(defgeneric xdefrag (vec)
  ;; return vector copy of vec defragmented
  (:method ((vec vector))
   (copy-seq vec))
  (:method ((vec scatter-vector))
   (xsubseq vec 0)))

(defun within (low ix hi)
  ;; test for ix in half-open interval [low, hi)
  (and (<= low ix)
       (< ix hi)))

(defgeneric xsubseq (vec start &optional end)
  ;; return a subseq vector copy of vec
  (:method ((vec vector) start &optional end)
   (subseq vec start end))
  (:method ((vec scatter-vector) start &optional end)
   (let* ((frags  (scatter-vector-frags vec))
          (nfrags (length frags)))
     (case nfrags
       ((0)  (subseq #() start end))
       ((1)  (let ((fragv (xdefrag (aref frags 0))))
               (if (and (zerop start) ;; try to avoid double copying
                        (or (null end)
                            (= end (length fragv))))
                   fragv ;; fragv is already a copy
                 ;; else
                 (subseq fragv start end))))
       (t
        (let* ((end  (or end (xlength vec)))
               (ans  (make-array (- end start)
                                 :element-type '(unsigned-byte 8))))
          (um:nlet outer ((base    0)
                          (frag-ix 0))
            ;; first - search for start frag
            (if (< base end)
                (let* ((frag (aref frags frag-ix))
                       (nb   (xlength frag)))
                  (when (within base start (+ base nb))
                    (um:nlet inner ((pos     0)
                                    (base    base)
                                    (frag-ix frag-ix))
                      (if (< base end)
                          (let* ((fragv      (xdefrag (aref frags frag-ix)))
                                 (nfrag      (length fragv))
                                 (frag-start (max 0 (- start base)))
                                 (frag-end   (- (min (+ base nfrag) end)
                                                base))
                                 (nb         (- frag-end frag-start)))
                            (replace ans fragv
                                     :start1 pos
                                     :start2 frag-start
                                     :end2   frag-end)
                            (go-inner (+ pos nb) (+ base nfrag) (1+ frag-ix)))
                        ;; else
                        ans)))
                  (go-outer (+ base nb) (1+ frag-ix)))
              ;; else
              ans))
          ))
       ))))

(defmethod scatter-vector-add-fragment ((sv scatter-vector) frag-vec)
  (vector-push-extend frag-vec (scatter-vector-frags sv))
  (incf (scatter-vector-length sv) (xlength frag-vec)))

#+(or :LISPWORKS :CLOZURE)
(defmethod stream:stream-read-byte ((stream ubyte-input-stream))
  (with-accessors ((arr    uis-arr)
                   (ix     uis-ix )
                   (end    uis-end)
                   (reader uis-reader)) stream
    (if (or (and end
                 (>= ix end))
            (not (in-bounds-p arr ix)))
        :eof
      (prog1
          (funcall reader arr ix)
        (incf ix))
      )))

#+sbcl 
(defmethod sb-gray:stream-read-byte ((stream ubyte-input-stream))
  (with-accessors ((arr    uis-arr)
                   (ix     uis-ix )
                   (end    uis-end)
                   (reader uis-reader)) stream
    (if (or (and end
                 (>= ix end))
            (not (in-bounds-p arr ix)))
        :eof
      (prog1
          (funcall reader arr ix)
        (incf ix))
      )))


#+:ALLEGRO
(defmethod excl:stream-read-byte ((stream ubyte-input-stream))
  (with-accessors ((arr    uis-arr)
                   (ix     uis-ix )
                   (end    uis-end)
                   (reader uis-reader)) stream
    (if (or (and end
                 (>= ix end))
            (not (in-bounds-p arr ix)))
        :eof
      (prog1
          (funcall reader arr ix)
        (incf ix))
      )))

(defun do-with-input-from-ubyte-stream (fn arr &rest args)
  (let ((s (apply 'make-ubyte-input-stream arr args)))
    (funcall fn s)))

(defmacro with-input-from-ubyte-stream ((stream-name arr &rest args) &body body)
  `(do-with-input-from-ubyte-stream (lambda (,stream-name) ,@body) ,arr ,@args))

(defmethod stream-file-position ((stream ubyte-input-stream))
  (uis-ix stream))

(defmethod (setf stream-file-position) (pos (stream ubyte-input-stream))
  (setf (uis-ix stream) pos))
