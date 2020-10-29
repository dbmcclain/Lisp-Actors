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

(defun do-suspend (fn)
  (let (old-fn)
    (setf old-fn (become
                  (dlambda
                    (:resume (&rest args)
                     (become old-fn)
                     (apply fn args))
                    
                    (t (&rest msg)
                       (apply old-fn msg)))))
    ))

(defmacro suspend (args &body body)
  `(do-suspend (lambda ,args
                 ,@body)))

#|
(spawn (lambda ()
         (pr :Hello)
         (let ((self (current-actor)))
           (spawn (lambda ()
                    (sleep 2)
                    (send self :Doit!)
                    (sleep 1)
                    (send self :resume :A-message)
                    (sleep 2)
                    (send self :What?))))
         (become
          (dlambda
            (t (&rest msg)
               (pr msg))))
         (suspend (msg)
           (pr :Yes msg))))
 |#