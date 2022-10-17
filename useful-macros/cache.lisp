;; cache.lisp -- Make cached objects and functions
;;
;; DM/HMSC  04/09
;; -----------------------------------------------------------
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

(defpackage #:com.ral.useful-macros.cache
  (:use #:common-lisp #:um)
  (:export
   #:cache
   #:cacheize
   #:uncacheize

   #:2-way-cache
   #:check-cache
   #:update-cache
   #:clear-cache
   ))

(in-package com.ral.useful-macros.cache)

;; -----------------------------------------------------------

(um:defconstant+ +uniq+ #())

(defun cache (fn &key (test #'equal))
  ;; provide a simple 2-way associative cache on function fn
  (let* ((cache (vector (list +uniq+) (list +uniq+)))
         (ix    0)) ;; MRU index
    (declare (fixnum ix)
             ((vector cons 2) cache))
    (labels ((tst (x cx)
               (and (not (eq cx +uniq+)) ;; can't allow +uniq+ to be abused
                    (funcall test x cx)))
             (zap (cell)
               (setf (car cell) +uniq+
                     (cdr cell) nil))
             (find-cell (args)
               (let ((cell (aref cache ix)))
                 (declare (cons cell))
                 (if (tst args (car cell))
                     (values cell t)
                   (aref cache (setf ix (logxor ix 1))))
                 )))
      (dlambda*
        (:clear ()
         (map nil #'zap cache))
        
        (:set-values (vals &rest args)
         (assert (listp vals))
         ;; check MRU cell first as most likely
         (values-list
          (let ((cell (find-cell args)))
            (declare (cons cell))
            (setf (car cell) args
                  (cdr cell) vals)
            )))
        
        (:set (val &rest args)
         (apply #':set-values (list val) args))
        
        (t (&rest args)
           ;; check MRU cell first as most likely
           (values-list
            (multiple-value-bind (cell checked)
                (find-cell args)
              (declare (cons cell))
              (if checked
                  (cdr cell)
                (if (tst args (car cell))
                    (cdr cell)
                  (let ((ans (multiple-value-list (apply fn args))))
                    (setf (car cell) args
                          (cdr cell) ans))
                  )))))
        ))))

(defun cacheize (fn-name)
  (unless (get fn-name 'cacheized)
    (let ((fn (symbol-function fn-name)))
      (setf (get fn-name 'cacheized)  fn
            (symbol-function fn-name) (cache fn))
      )))

(defun un-cacheize (fn-name)
  (um:when-let (fn (get fn-name 'cacheized))
    (setf (symbol-function fn-name) fn)
    (remprop fn-name 'cacheized)))
            
;; -----------------------------------------------------------
#|
(defclass 2-way-cache ()
  ((cache   :accessor 2-way-cache       :initform (vector (cons +uniq+ nil)
                                                          (cons +uniq+ nil)))
   (ix      :accessor 2-way-cache-ix    :initform 0)
   (test    :accessor 2-way-cache-test  :initarg :test :initform 'eql)
   ))

(defun cache-oper (obj key found-fn not-found-fn)
  (let* ((ix      (2-way-cache-ix obj))
         (cache   (2-way-cache obj))
         (v1      (aref cache ix))
         (k1      (car v1))
         (test-fn (2-way-cache-test obj)))
    (declare (fixnum ix)
             ((vector t 2) cache)
             (cons v1))
    
    ;; must check for +uniq+ cache cell, since
    ;; we have no idea what the user's test-fn will try to do...
    ;; e.g., it might ask for the (string key) and +uniq+ cannot be coerced to string
    (labels ((test-key (k)
               (and (not (eq +uniq+ k))
                    (funcall test-fn key k))))
    
      (if (test-key k1)
          (funcall found-fn v1)
        ;; else
        (let* ((ixp (logxor ix 1))
               (v2  (aref cache ixp))
               (k2  (car v2)))
          (declare (fixnum ixp)
                   (cons v2))
          (setf (2-way-cache-ix obj) ixp)
          (if (test-key k2)
              (funcall found-fn v2)
            ;; else
            (funcall not-found-fn v2))
          )))))

(defmethod check-cache ((obj 2-way-cache) key)
  (cache-oper obj key
              'cdr
              'false))

(defmethod update-cache ((obj 2-way-cache) key val)
  (cache-oper obj key
              (lambda (v)
                (declare (cons v))
                (setf (cdr v) val))
              (lambda (v)
                (declare (cons v))
                (setf (car v) key
                      (cdr v) val))
              ))

(defmethod clear-cache ((obj 2-way-cache))
  (let ((cache (2-way-cache obj)))
    (declare ((vector t 2) cache))
    (setf (car (the cons (aref cache 0))) +uniq+  ;; reset the key (car) into something unique
          (car (the cons (aref cache 1))) +uniq+)
    ))
|#
;; ---------------------------------------------------

(defclass 2-way-cache ()
  ((cache-lines :reader cache-lines)
   (test-fn     :reader cache-test-fn   :initarg :test   :initform 'eql)
   (row-fn      :reader cache-row-fn)
  ))

(defmethod initialize-instance :after ((obj 2-way-cache)
                                &key
                                (nlines 16)
                                (hashfn 'sxhash)
                                &allow-other-keys)
  (assert (plusp nlines))
  (let ((cache (make-array nlines)))
    (dotimes (ix nlines)
      (setf (aref cache ix) (vector 1
                                    (list +uniq+)
                                    (list +uniq+)) ))
    (setf (slot-value obj 'cache-lines) cache
          (slot-value obj 'row-fn)      (if (= 1 nlines)
                                            (constantly (aref cache 0))
                                          ;; else
                                          (lambda (k)
                                            ;; mod is always non-negative,
                                            ;; while rem could produce negative results
                                            (aref cache (mod (funcall hashfn k)
                                                             nlines)))))
    ))

(defun cache-oper (obj key found-fn not-found-fn)
  #F
  (let* ((line    (funcall (cache-row-fn obj) key))
         (ix      (aref line 0))
         (v1      (aref line ix))
         (test-fn (cache-test-fn obj)))
    (declare (fixnum ix)
             ((vector t 3) line)
             (cons v1))

    ;; must check for +uniq+ cache cell, since we have
    ;; no idea what the user's test-fn will try to do...
    ;; e.g., it might ask for the (string key) and +uniq+ cannot be coerced to string
    (labels ((test-key (k)
               (and (not (eq +uniq+ k))
                    (funcall test-fn key k))))
      
      (if (test-key (car v1))
          (funcall found-fn v1)
        ;; else
        (let* ((ixp (logxor 3 ix))
               (v2  (aref line ixp)))
          (declare (fixnum ixp)
                   (cons v2))
          (setf (aref line 0) ixp)
          (if (test-key (car v2))
              (funcall found-fn v2)
            ;; else
            (funcall not-found-fn v2))
          )))))
      
(defmethod check-cache ((obj 2-way-cache) key)
  #F
  (cache-oper obj key
              'cdr
              'false))

(defmethod update-cache ((obj 2-way-cache) key val)
  #F
  (cache-oper obj key
              (lambda (v)
                (declare (cons v))
                (setf (cdr v) val))
              (lambda (v)
                (declare (cons v))
                (setf (car v) key
                      (cdr v) val))
              ))

(defmethod clear-cache ((obj 2-way-cache))
  #F
  (loop for line of-type (vector t 3) across (the vector (cache-lines obj)) do
        (setf (aref line 1) (list +uniq+)  ;; make the keys unique
              (aref line 2) (list +uniq+))))
         
;; ----------------------------------------------------------
