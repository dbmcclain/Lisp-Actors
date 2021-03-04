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

(in-package :ecc-crypto-b571)

;; ---------------------------------------------------------------------------------

;; ----------------------------------------------------------

(defun make-linear-lagrange-interpolator (shares)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (* prod (- x2 x)))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum share)
                 (destructuring-bind (x y) share
                   (let ((xs (mapcar 'car (remove share shares))))
                     (+ sum
                        (* y (/ (lprod x0 xs)
                                (lprod x xs)) )) ))) )
        (reduce #'term shares
                :initial-value 0))) ))

(defun solve-linear-lagrange (shares &optional (x 0))
  (let ((fn (make-linear-lagrange-interpolator shares)))
    (funcall fn x)))

;; -------------------------------------------------------------

#|
(let* ((shares1 '((0 15) (1 32) (7 76)))
       (xs  '(0 1 2 3 4 7))
       (y1s (mapcar (um:curry 'solve-linear-lagrange shares1) xs))
       (shares2 '((0 37) (1 32) (7 76)))
       (y2s (mapcar (um:curry 'solve-linear-lagrange shares2) xs))
       (shares1 (um:take 3 (um:drop 2 (mapcar 'list xs y1s))))
       (shares2 (um:take 3 (um:drop 2 (mapcar 'list xs y2s)))))
  (plt:fplot 'plt '(0 10) (lambda (x)
                            (solve-linear-lagrange shares1 x))
             :clear t)
  (plt:fplot 'plt '(0 10) (lambda (x)
                            (solve-linear-lagrange shares2 x))
             :color :red)
  (plt:plot 'plt
            (mapcar 'car shares1)
            (mapcar 'cadr shares1)
            :symbol :circle)
  (plt:plot 'plt
            (mapcar 'car shares2)
            (mapcar 'cadr shares2)
            :color :red
            :symbol :circle)
  (solve-linear-lagrange (mapcar (lambda (sh1 sh2)
                                   (list (car sh1)
                                         (+ (* 1/2 (cadr sh1))
                                            (* 1/2 (cadr sh2)))))
                                 shares1 shares2)))

 |#