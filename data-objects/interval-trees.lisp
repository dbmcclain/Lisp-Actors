;; interval-trees.lisp -- RB Trees used for holding intervals (start len)
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

(in-package #:interval-trees)

(defmethod find-containing ((map maps:tree) key &optional default)
  ;; key should be a pair (start len)
  ;; returns the object from the cell that has an interval key
  ;; which contains the interval lookup key
  #f
  (labels
      ((find-containing (map key default)
         (if map
             (destructuring-bind (kstart klen) key
               (sets:with-node-bindings (l k v r) map
                 (let ((c (ord:compare key k)))
                   (cond ((zerop c)  (values v t))
                         ((minusp c)
                          ;; key < interval
                          ;; can happen if (1) kstart < istart => try left branch
                          ;;            or (2) kstart = istart && kend < iend => we found it
                          (let ((istart (car k)))
                            (cond ((= kstart istart)                  ;; K |---|
                                   (values v t))                      ;; I |-----|
                                  (t                                  ;; K |----|
                                                                      (find-containing l key default))   ;; I   |---|
                                  )))
                         (t
                          ;; key > interval
                          ;; can happen if (1) kstart = istart && kend > iend => try right brancch
                          ;;            or (2) kstart > istart 
                          ;;
                          (destructuring-bind (istart ilen) k
                            (cond ((<= (+ kstart klen)                  ;; K    |--|
                                       (+ istart ilen))                 ;; I   |-----|
                                   (values v t))
                                  (t                                    ;; K |------| or    |-----|
                                                                        (find-containing r key default))     ;; I |---|       |-----|
                                  )))
                         ))))
           ;; else
           default)))
    (find-containing (maps:tree-nodes map) key default)
    ))
       
#|
(let ((m (um:-> (maps:empty)
                (maps:add '(0 4) 'one)
                (maps:add '(3 4) 'two)
                (maps:add '(5 10) 'three))))
  (find-containing m '(5 3)))
|#

;; ---------------------------------------------------------------------------------

