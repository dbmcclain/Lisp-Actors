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

(in-package :um)

(let ()
  (labels ((doit-1 ()
             (print "Starting doit-1")
             (sleep 3)))
    (with-serial-dispatch ()
      (doit-1))
    (print "Just launched doit-1")
    (with-serial-dispatch ()
      (doit-1))
    (print "Just launched doit-1 again...")
    (wait-for-serial-dispatch)
    (print "Finished...")
    (values)))


(let ()
  (labels ((doit-1 ()
             (print "Starting doit-1")
             (sleep 3)))
    (with-parallel-dispatch ()
      (doit-1))
    (print "Just launched doit-1")
    (with-parallel-dispatch ()
      (doit-1))
    (print "Just launched doit-1 again...")
    (wait-for-parallel-dispatch)
    (print "Finished...")
    (values)))


(defun aitken (a b c)
  (declare (double-float a b c))
  #F
  (declare (optimize #+:LISPWORKS (float 0)))
  (declare (:explain :variables :all-calls :all-calls-with-arg-types))
  (let* ((c-b (- c b))
         (den (- c-b (- b a))))
    (declare (double-float c-b den))
    (if (zerop den)
        c
      (- c (/ (* c-b c-b) den)))))
