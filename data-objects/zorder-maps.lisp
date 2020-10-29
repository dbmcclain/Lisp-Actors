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

(defpackage #:zorder-maps
  (:use #:common-lisp)
  (:export
   #:zorder-key
   #:make-zorder-key
   #:zorder-key-p
   #:zorder-key-coord
   ))

(in-package #:zorder-maps)

(defun less-msb (x y)
  (and (< x y)
       (< x (logxor x y))))

(defun cmp-zorder(a b)
  (let ((ndim  (length a)))
    ;; a, b are lists of equal length representing coordinates in N dimensions
    (assert (= ndim (length b)))
    (let ((j  0)
          (x  0))
      (loop for k from 0 below ndim do
            (let ((y (logxor (elt a k) (elt b k))))
              (when (less-msb x y)
                (setf j k
                      x y))
              ))
      (- (elt a j) (elt b j))
      )))

(defstruct zorder-key
  coord)

(defmethod ord:compare ((a zorder-key) (b zorder-key))
  (cmp-zorder (zorder-key-coord a) (zorder-key-coord b)))

#|
(let* ((coords (loop for x from 0 below 8 nconc
                     (loop for y from 0 below 8 collect
                           (list x y))))
       (sorted (sort coords (um:compose #'minusp #'cmp-zorder)))
       (zmap   (maps:empty)))
  (loop for coord in coords do
        (setf zmap (maps:add zmap
                             (make-zorder-key
                              :coord coord)
                             coord)))
  (sets::view-set zmap)
  (plt:plot 'plt
            (mapcar #'first sorted)
            (mapcar #'second sorted)
            :clear t
            :symbol :circle
            :plot-joined t)
  (inspect sorted))
|#
