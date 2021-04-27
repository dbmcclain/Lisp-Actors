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


(defpackage #:cow
  (:use #:common-lisp)
  (:export
   #:cow-changed-p
   #:cow-ref
   #:make-cow-vector
   #:make-cow-array
   #:as-cow
   #:deref))

(in-package #:cow)

;; -----------------------------------------------------

(defclass <cow> ()
  ((lock    :reader   cow-lock
            :initform (mp:make-lock
                       :sharing t))

   (cow     :accessor cow-copy-p
            :initarg :cow
            :initform t)

   (changed :accessor cow-changed-p
            :initform nil)
   
   (ref     :accessor cow-ref
            :initarg :ref)
   ))

;; -----------------------------------------------------

(defmethod make-load-form ((obj <cow>) &optional environment)
  `(make-instance ,(class-of obj) :ref ,(cow-ref obj)))

;; -----------------------------------------------------

(defmethod as-cow (obj &key (cow t))
  ;; use :COW NIL if you just want locked access to the vector
  (make-instance '<cow>
                 :ref  obj
                 :cow  cow))

(defmethod as-cow ((obj <cow>) &key (cow t))
  (as-cow (cow-ref obj) :cow cow))
                   
;; -----------------------------------------------------

(defun make-cow-vector (nel &rest args)
  (make-instance '<cow>
                 :ref (apply #'make-array nel args)))

(defun make-cow-array (dims &rest args)
  (make-instance '<cow>
                 :ref (apply #'make-array dims args)))

;; -----------------------------------------------------

(defun iterated-deref (obj index)
  (um:nlet-tail iter ((obj   obj)
                      (index index))
    (if index
        (iter (deref obj (car index)) (cdr index))
      obj)))


(defgeneric deref (obj index)

  (:method ((obj <cow>) index)
   (mp:with-sharing-lock ((cow-lock obj))
     (deref (cow-ref obj) index)))

  (:method ((obj vector) index)
   (if (consp index)
       (iterated-deref obj index)
     (aref obj index)))

  (:method ((obj array) index)
   (apply #'aref obj index))

  (:method ((obj structure-object) slot)
   (if (consp slot)
       (iterated-deref obj slot)
     (slot-value obj slot))))


;; -----------------------------------------------------

(defgeneric setref (obj index val)
  
  (:method ((obj <cow>) index val)
   (mp:with-exclusive-lock ((cow-lock obj))
     (when (cow-copy-p obj)
       (setf (cow-ref obj)       (copy-obj (cow-ref obj))
             (cow-copy-p obj)    nil
             (cow-changed-p obj) t))
     (setref (cow-ref obj) index val)))

  (:method ((obj vector) index val)
   (setf (aref obj index) val))

  (:method ((obj array) index val)
   (setf (apply #'aref obj index) val))

  (:method ((obj structure-object) slot val)
   (setf (slot-value obj slot) val)))


(defsetf deref setref)

;; -----------------------------------------------------

(defgeneric copy-obj (obj)
  ;; used internally
  (:method ((obj vector))
   (copy-seq obj))

  (:method ((obj array))
   (let* ((elt-type (array-element-type obj))
          (tsize    (array-total-size obj))
          (new (make-array (array-dimensions obj)
                           :element-type elt-type))
          (rmaj (make-array tsize
                            :element-type elt-type
                            :displaced-to obj))
          (new-rmaj (make-array tsize
                                :element-type elt-type
                                :displaced-to new)))
     (replace new-rmaj rmaj)
     new))

  (:method ((obj structure-object))
   (copy-structure obj)))

;; -----------------------------------------------------

#|
(defstruct thing
  a b c)

(let* ((x (as-cow (make-thing
                   :a 1
                   :b (as-cow (vector 1 2 3))
                   :c 3)))
       (xx (cow-ref (deref x 'b))))
  (setf (deref (deref x 'b) 2) 15)
  (fac:pr xx (cow-ref (deref x 'b)))
  (assert (not (eq xx (cow-ref (deref x 'b))))))
 |#
#|
(setf x (vector 1 2 3))

(setf (aref x 2) 15)

x

(defstruct thing a b c)

(let* ((x  (as-cow (make-thing :a 1 :b (as-cow (vector 1 2 3)) :c 3)))
      (xx  (cow-ref (deref x 'b))))
  (setf (deref (deref x 'b) 2) 15)
  xx)

(setf x (vector 1 2 3))

(setf (aref x 2) 15)

(identity x)
(identity 32)
x
(print x)

|#
;; ------------------------------------------------------

(defclass <immut> ()
  ((lock   :reader  immut-lock
           :initform (mp:make-lock))

   (map    :accessor immut-map
           :initform (maps:empty))

   (changed :accessor immut-changed-p
            :initform nil)))

(defmethod make-immut ()
  (make-instance '<immut>))

(defmethod as-immut ((obj <immut>))
  obj)

(defmethod deref ((obj <immut>) index)
  (maps:find index (immut-map obj)))

(defmethod setref ((obj <immut>) index val)
  (mp:with-lock ((immut-lock obj))
    (setf (immut-map obj) (maps:add index val (immut-map obj))
          (immut-changed-p obj) t)
    val))

;; ------------------------------------------------------

(defclass <immutv> (<immut>)
  ((ref   :accessor immutv-ref
          :initarg  :ref
          :initform nil)))

(defmethod as-immut ((obj vector))
  (make-instance '<immutv>
                 :ref obj))

(defmethod deref ((obj <immutv>) index)
  (or (call-next-method)
      (aref (immutv-ref obj) index)))

(defmethod setref ((obj <immutv>) index val)
  (assert (and (integerp index)
               (< -1 index (length (immutv-ref obj)))))
  (call-next-method))

(defmethod as-vector ((obj <immutv>))
  (let ((vec  (copy-seq (immutv-ref obj))))
    (maps:iter (lambda (k v)
                 (setf (aref vec k) v))
               (immut-map obj))
    vec))

#|
(let ((x (as-immut (vector 1 2 3))))
  (setf (deref x 2) 15)
  (inspect x)
  (as-vector x))

(let ((x (as-immut (make-array 127))))
  (um:lc ((:do
              (setf (deref x ix) ix))
          (ix <.. 0 126)))
  (sets:view-set (immut-upd x))
  (as-vector x))
 |#
