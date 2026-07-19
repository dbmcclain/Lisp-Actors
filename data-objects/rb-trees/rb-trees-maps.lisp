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

(defclass map (tree)
  ()
  (:metaclass
   #+:LISPWORKS clos:funcallable-standard-class
   #+:SBCL      sb-mop:funcallable-standard-class))

;; --------------------------------------------

(defun make-map-type (&rest args &key compare-fn replace-p-fn)
  (declare (ignore compare-fn replace-p-fn))
  (apply #'make-tree-type args))

(defmethod map-type-compare-fn ((m map))
  (tree-type-compare-fn m))

(defmethod map-type-replace-p-fn ((m map))
  (tree-type-replace-p-fn m))

;; --------------------------------------------

(defun make-map (&rest args &key map-type compare-fn replace-p-fn)
  ;; Args can be :MAP-TYPE, :COMPARE-FN, :REPLACE-P-FN, or none for defaults.
  (declare (ignore compare-fn replace-p-fn))
  (apply #'make-instance 'map (if map-type
                                  (list* :tree-type map-type args)
                                args)))

(defmethod make-map-like ((m map))
  (make-tree-like m))

(defmethod map-type ((m map))
  (tree-type m))

;; --------------------------------------------

(defmethod add ((map map) key value)
  (trees:add map key value))

(define-modify-macro addf (key val)
  add)

(defmethod find ((map map) key &optional default)
  ;; eval with contant stack space - S(1)
  (multiple-value-bind (found v) (mem map key)
    (if found
        (values v t)
      default)))

(defmethod mapi ((map map) f)
  ;; eval with S(Log2(N))
  (let ((new-map (make-map-like map)))
    (sets:iter map
               #'(lambda (key val)
                   (addf new-map key (funcall f key val))))
    new-map))

(defmethod map ((map map) f)
  (mapi map
        #'(lambda (k v)
            (declare (ignore k))
            (funcall f v))
        ))

(defmethod add-plist ((map map) plist)
  ;; plist = alternating key,value pairs
  (um:nlet iter ((lst plist))
    (when lst
      (addf map (car lst) (cadr lst))
      (go-iter (cddr lst)))
    map))

(defmethod add-alist ((map map) alist)
  ;; alist list of pairs (key value)
  (um:nlet iter ((lst alist))
    (when lst
      (addf map (caar lst) (cdar lst))
      (go-iter (cdr lst)))
    map))

(defmethod add-hashtable ((map map) hashtable)
  (maphash (lambda (k v)
             (addf map k v))
           hashtable)
  map)

(defmethod add-keys-vals ((map map) keys vals)
  ;; keys and vals are lists of correspoinding key/val pairs
  (um:nlet iter ((ks keys)
                 (vs vals))
    (when (and ks vs)
      (addf map (car ks) (car vs))
      (go-iter (cdr ks) (cdr vs)))
    map))

#+:LISPWORKS
(defmethod view-map ((m map) &rest args
                     &key key layout)
  (declare (ignore key layout))
  (apply #'view-tree m args))

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
