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


(in-package :fac)

(let ((actor (make-actor
              (let ((count 0))
                (dlambda
                  (:test (n)
                   (setf count n)
                   (um:nlet iter ()
                     (with-future ()
                         (sleep 1)
                       (pr :count count)
                       (when (plusp (decf count))
                         (iter))))))))
             ))
  (spawn actor :test 5))

(spawn (let ((count 0))
         (dlambda
           (:test (n)
            (setf count n)
            (um:nlet iter ()
              (with-future ()
                  (sleep 1)
                (pr :count count)
                (when (plusp (decf count))
                  (iter)))))))
       :test 5)

(let ((x (future #'one))
      (y (future #'two)))
  (when-all (list x y)
            body))

(progn
  (with-futures (&rest answers)
      ((progn
         (sleep 1) :ok-1)
       (progn
         (sleep 2) :ok-2)
       (progn
         (sleep 3) :ok-3))
    (pr answers))
  (pr :after-after))

(with-futures (x y z)
    ((sin 0.1)
     (sin 0.2)
     (sin 0.3))
  (pr (list x y z)))

(funcall (dlambda
           (:one () 1)
           (:two () 2)
           (t (&rest args)
              (break)
              args)))
              
(defun tst (&optional (n #N1_000))
  (time
   (loop repeat n do
         (spawn (lambda ()
                  nil)))))

(defun tst2 (&optional (N #N1_000))
  (time
   (loop repeat n do
         (mpcompat:process-run-function "test" '() (lambda ()
                                              nil)))))