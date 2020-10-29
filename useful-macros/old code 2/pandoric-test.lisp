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

(in-package :cl-user)

(defun make-stats-counter
       (&key (count 0)
             (sum   0)
             (ssum  0))
  (um:plambda (n) (sum count ssum)
    (incf ssum (* n n))
    (incf sum  n)
    (incf count)))

(um:defpan stats-counter-mean (sum count)
  (/ sum count))

(um:defpan stats-counter-variance (ssum sum count)
  (if (< count 2)
      0
    (/ (- ssum
          (* sum (stats-counter-mean self)))
       (- count 1))))

(um:defpan stats-counter-stddev ()
  (sqrt (stats-counter-variance self)))
