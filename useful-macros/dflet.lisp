;; dflet.lisp -- Dynamically bound functions
;;
;; Adapted from P. Costanza
;;
;; DM/RAL  04/17
;; -----------------------------------------
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

(defpackage #:aop
  (:use #:common-lisp)
  (:export
   #:dflet
   #:call-next-function
   #:get-dynsym
   #:make-dynsym
   #:ensure-dynsym
   #:defdynfun
   #:redefdynfun
   #:get-defined-dynsym
   ))

(in-package #:aop)

(defvar *dynsyms* (make-hash-table :test #'equal))

(defmacro get-dynsym (fname)
  `(gethash ,fname *dynsyms*))

(defun make-dynsym (fname)
  (setf (get-dynsym fname) (make-symbol (format nil "*~A*" fname))))

(defmacro ensure-dynsym (fname default)
 `(or (get-dynsym ,fname)
       ,default))

(defun ensure-dynfun-form (fname &rest rest)
  (let ((dynsym (ensure-dynsym fname (make-dynsym fname))))
    `(progn
       (setf (get-dynsym ',fname) ',dynsym)
       (defparameter ,dynsym
         ,(if rest
              `(lambda ,@rest)
            `(if (fboundp ',fname)
                 (fdefinition ',fname)
               (lambda (&rest args)
                 (cerror "Retry applying ~A to ~A."
                         "Undefined dynamic function ~A called with arguments ~A."
                         ',fname args)
                 (apply ',fname args)))))
       (defun ,fname (&rest args)
         (apply ,dynsym args)))))

(defmacro defdynfun (fname args &body body)
  (apply #'ensure-dynfun-form fname args body))

(defmacro redefdynfun (fname)
  (ensure-dynfun-form fname))

(defun get-defined-dynsym (fname)
  (ensure-dynsym fname (progn
                         (cerror "Make ~A a dynamic function."
                                 "Function ~A is not dynamic."
                                 fname)
                         (eval `(redefdynfun ,fname))
                         (get-dynsym fname))))

(defmacro dflet1 ((fname args &body funbody) &body dflbody)
  (let ((dynsym (get-defined-dynsym fname)))
    (lw:with-unique-names (orgfun orgargs newargs)
        `(let* ((,orgfun  ,dynsym)
                (,dynsym  (lambda (&rest ,orgargs)
                            (flet ((call-next-function (&rest ,newargs)
                                     (apply ,orgfun (if ,newargs ,newargs ,orgargs))))
                              (destructuring-bind ,args
                                  ,orgargs
                                ,@funbody)))))
           (declare (ignorable ,orgfun))
           ,@dflbody))))

(defmacro dflet ((&rest decls) &body body)
  (reduce (lambda (decl result)
            `(dflet1 ,decl ,result))
          decls
          :initial-value `(progn ,@body)
          :from-end      t))

#+:LISPWORKS
(editor:setup-indent "dflet" 1 2 4 'flet)

#|
(defun f (x) (print x))
(redefdynfun f)
(progn
  (fmakunbound 'f)
  (remhash 'f *dynsyms*))
(dflet ((f (x)
           (print "entering f")
           (call-next-function)
           (print "leaving f")))
  (f 5))
 |#

