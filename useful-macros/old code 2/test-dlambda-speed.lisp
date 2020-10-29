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

(defun tst (&key (nkey 4) (niter #N1_000_000))
  (let* ((keys   (lc ((symb n) (n <.. 1 nkey))))
         (plist  (mapcan #'identity (lc ((list n n) (n <- keys)))))
         (map    (reduce #'(lambda (map n)
                             (maps:add n n map))
                         keys
                         :initial-value (maps:empty)))
         (arr    (coerce keys 'vector)))
    (print "Compute Overhead")
    (time
     (loop repeat niter do
           (let* ((ix (random nkey))
                  (key (aref arr ix)))
             )))
    (print "Compute Map")
    (time
     (loop repeat niter do
           (let* ((ix (random nkey))
                  (key (aref arr ix)))
             (maps:find key map))))
    (print "Compute PList")
    (time
     (loop repeat niter do
           (let* ((ix (random nkey))
                  (key (aref arr ix)))
             (getf plist key))))
    
    ))
         
  