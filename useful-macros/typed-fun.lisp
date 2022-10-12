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

(in-package :useful-macros)

;; ------------------------------------------------------

#+:LISPWORKS
(defmacro! def-typed-fn (type-name params fn &environment env)
  (let ((maker-name (symb 'make- type-name))
    (um:ensure-thread-eval-def maker-name
      `(progn
         (defclass ,type-name ()
           ()
           (:metaclass clos:funcallable-standard-class))
         
         (defmethod initialize-instance :after ((fn ,type-name) &key behavior &allow-other-keys)
           (clos:set-funcallable-instance-function fn behavior))
         
         (defun ,maker-name ,params
           ;; params serve to parameterize the function body
           ;; function body can refer to itself as SELF
           (let (,a!self)
             (declare (ignorable ,a!self))
             (setf ,a!self
                   (make-instance ',type-name
                                  :behavior ,fn)))
           ))
      env)))

#+:LISPWORKS
(editor:setup-indent "def-typed-fn" 2)

#|
;; Example:
(def-typed-fn diddly-fn (a b) (lambda (x) (+ a (* b x))))
(let ((fn (make-diddly-fn 1 2)))
  (funcall fn 15)) ;; => 31
|#
