;; --------------------------------------------
(in-package :maps)
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

(defstruct map-cell
  key val)

(defmethod ord:compare ((a map-cell) (b map-cell))
  ;; for comparing two map cells
  ;; used by sets:add
  (ord:compare (map-cell-key a) (map-cell-key b)))

(defmethod ord:compare (a (b map-cell))
  ;; for comparing keys against map-cells
  (ord:compare a (map-cell-key b)))

(defmethod ord:compare ((a map-cell) b)
  ;; for comparing map-cells against keys
  ;; allows us to perform (set:diff map set) to remove keys
  (ord:compare (map-cell-key a) b))

;; ----------------------------------------------

(defmethod add ((map tree) key val &key (replace t))
  (sets:with-replacement-p replace
    (sets:add map
              (make-map-cell
               :key key
               :val val))
    ))

(define-modify-macro addf (key value &rest args)
  add)

(defmethod find ((map tree) key &optional default)
  ;; eval with contant stack space - S(1)
  (multiple-value-bind (found cell) (mem map key)
    (if found
        (values (map-cell-val cell) t)
      (values default nil))))

(defmethod fold ((map tree) f accu)
  ;; eval with S(Log2(N))
  (sets:fold map
             #'(lambda (cell accu)
                 (funcall f (map-cell-key cell) (map-cell-val cell) accu))
             accu))

(defmethod mapi ((map tree) f)
  ;; eval with S(Log2(N))
  (let ((new-map (empty)))
    (sets:iter map
               #'(lambda (cell)
                 (let ((key (map-cell-key cell)))
                   (setf new-map (add new-map key (funcall f key (map-cell-val cell)))
                         ))))
    new-map))

(defmethod map ((map tree) f)
  (mapi map
        #'(lambda (k v)
            (declare (ignore k))
            (funcall f v))
        ))

(defmethod iter ((map tree) f)
  (sets:iter map
             #'(lambda (cell)
                 (funcall f (map-cell-key cell) (map-cell-val cell)))
             ))

(defmethod add-plist ((map tree) plist)
  ;; plist = alternating key,value pairs
  (um:nlet iter ((lst plist))
    (when lst
      (addf map (car lst) (cadr lst))
      (go-iter (cddr lst)))
    map))

(defmethod add-alist ((map tree) alist)
  ;; alist list of pairs (key value)
  (um:nlet iter ((lst alist))
    (when lst
      (addf map (caar lst) (cadar lst))
      (go-iter (cdr lst)))
    map))

(defmethod add-hashtable ((map tree) hashtable)
  (maphash (um:curry 'addf map) hashtable)
  map)

(defmethod add-keys-vals ((map tree) keys vals)
  ;; keys and vals are lists of correspoinding key/val pairs
  (um:nlet iter ((ks keys)
                 (vs vals))
    (when (and ks vs)
      (addf map (car ks) (car vs))
      (go-iter (cdr ks) (cdr vs)))
    map))


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
#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'list-form)))
  (declare (ignore mode))
  (let* ((elts (sets:elements map)))
    (if (every #'map-cell-p elts)
        (let ((keys (mapcar #'map-cell-key elts))
              (vals (mapcar #'map-cell-val elts)))
          (values keys vals)) ;; :entries (list (mapcar #'cons keys vals))))
      (values :elements (list elts)))))

(defmethod key-fn ((cell map-cell))
  (map-cell-key cell))

#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'graph-form)))
  (declare (ignore mode))
  (values :graph (sets:view-set map)))
#||#  
