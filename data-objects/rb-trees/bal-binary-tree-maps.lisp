;; --------------------------------------------
(in-package #:com.ral.rb-trees.maps)
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

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------
#|
(defstruct thing)
(clos:compute-applicable-methods-using-classes #'ord:compare (mapcar #'class-of (list (make-thing) (make-thing))))
(clos:compute-discriminating-function #'ord:compare)
(compute-applicable-methods #'ord:compare (list (make-thing) (make-thing)))

(clos:compute-effective-method #'ord:compare clos::*standard-method-combination*
                               (compute-applicable-methods #'ord:compare (list (make-thing) (make-thing))))

(CLOS:GENERIC-FUNCTION-METHOD-COMBINATION #'ord:compare)
(closer-mop:compute-effective-method #'ord:compare
                                     (CLOS:GENERIC-FUNCTION-METHOD-COMBINATION #'ord:compare)
                                     (compute-applicable-methods #'ord:compare (list (make-thing) (make-thing))))
(ord:compare (make-thing) (make-thing))
(type-of (ser:encode (make-thing)))
|#
;; ----------------------------------------------

(defmethod add (map key val &key (replace t))
  (sets:with-replacement-p (and replace 'true)
    (sets:add map key val)
    ))

(define-modify-macro addf (key value &rest args)
  add)

(defmethod find (map key &optional default)
  ;; eval with contant stack space - S(1)
  (multiple-value-bind (found v) (mem map key)
    (if found
        (values v t)
      (values default nil))))

(defmethod mapi (map f)
  ;; eval with S(Log2(N))
  (let ((new-map (empty)))
    (sets:iter map
               #'(lambda (key val)
                   (setf new-map (add new-map key (funcall f key val))
                         )))
    new-map))

(defmethod map (map f)
  (mapi map
        #'(lambda (k v)
            (declare (ignore k))
            (funcall f v))
        ))

(defmethod add-plist (map plist)
  ;; plist = alternating key,value pairs
  (um:nlet iter ((lst plist))
    (when lst
      (addf map (car lst) (cadr lst))
      (go-iter (cddr lst)))
    map))

(defmethod add-alist (map alist)
  ;; alist list of pairs (key value)
  (um:nlet iter ((lst alist))
    (when lst
      (addf map (caar lst) (cadar lst))
      (go-iter (cdr lst)))
    map))

(defmethod add-hashtable (map hashtable)
  (maphash (um:curry 'addf map) hashtable)
  map)

(defmethod add-keys-vals (map keys vals)
  ;; keys and vals are lists of correspoinding key/val pairs
  (um:nlet iter ((ks keys)
                 (vs vals))
    (when (and ks vs)
      (addf map (car ks) (car vs))
      (go-iter (cdr ks) (cdr vs)))
    map))

;; ----------------------------------------------

#|
;; test code for map
(let ((x (empty)))
  (setf x (add 'this 15 x))
  (setf x (add 'that 32 x))
  (setf x (add 'thother 64 x))
  (inspect x)
  (sets:iter x #'print)
  (find 'else x :not-found)
  (fold x
        (lambda (k v acc)
          (cons (print (list k (+ v 3))) acc))
        nil))
  |#
#||#
;; ----------------------------------------------

#+:LISPWORKS
(unless (vectorp (sets::singleton-node nil nil))
  (defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'list-form)))
    (declare (ignore mode))
    (let* ((elts (sets:elements map))
           (keys (mapcar #'car elts))
           (vals (mapcar #'cdr elts)))
      (values keys vals)))
  
  (defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'graph-form)))
    (declare (ignore mode))
    (values :graph (sets:view-set map))))
