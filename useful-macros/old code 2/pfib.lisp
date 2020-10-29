#|
(defparameter *fibs* (make-array 33))
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

(defun direct-fib (n)
  (if (< n 2)
      n
    (um:nlet-tail iter ((f1 0)
                        (f2 1)
                        (ix 2))
      (if (> ix n)
          f2
        (iter f2 (+ f1 f2) (1+ ix)))
      )))

(defun basic-fib (n)
  (if (< n 2)
      n
    (let* ((x (basic-fib (- n 2)))
           (y (basic-fib (1- n))))
      (+ x y))
    ))

(defun fib (n)
  (labels ((basic-fib (n)
             (let* ((x (fib (- n 2)))
                    (y (fib (1- n))))
               (+ x y))))
    (cond ((< n 2) n)
          ((< n 33)
           (or (aref *fibs* n)
               (setf (aref *fibs* n) (basic-fib n))))
          (t (basic-fib n))
          )))

(defun pfib (n)
  (labels ((%pfib (n)
             (if (< n 33)
                 (constantly (basic-fib n))
               (let* ((x (unsafe-future (%pfib (1- n))))
                      (y (%pfib (- n 2))))
                 (lambda ()
                   (+ (funcall (touch x)) (funcall y))))
               )))
    (funcall (%pfib n))
    ))

  |#

