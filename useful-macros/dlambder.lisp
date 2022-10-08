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

#|
(defpackage #:dlam
  (:use #:common-lisp)
  (:export
   #:dlam-fun
   #:dlam-keys
   #:dlam-fns
   #:dlam-def
   #:dlambda
   #:dcase
   #:replace-handlers
   #:replace-handler
   ))
|#

(in-package #:dlam)

;; ----------------------------------------------------------------------
#|
(defun invsel (msg)
  (error "Invalid selector ~A" (car msg)))
|#

(defun is-underscore? (x)
  (and (symbolp x)
       (string= "_" (string x))))

(defun decl-us (args)
  (when (is-underscore? args)
      `((declare (ignore _)))))

(defun us-conv (args)
  (if (is-underscore? args)
      `(&rest _)
    args))

(defun dlam-parser (gargs)
  (lambda* ((sel args &rest body))
    (cond ((eq t sel)
           (if (eq 'when (car body))
               `((apply (lambda* ,args
                          (declare (ignorable ,@args))
                          ,(second body))
                        ,gargs)
                 (apply (lambda* ,args ,@(cddr body)) ,gargs))
             ;; else
             `(t (apply (lambda* ,(us-conv args) ,@(decl-us args) ,@body) ,gargs))
             ))
          ((eq 'when (car body))
           `((and (eql ',sel (car ,gargs))
                  (apply (lambda* ,args
                           (declare (ignorable ,@args))
                           ,(second body))
                         (cdr ,gargs)))
             (apply (lambda* ,args ,@(cddr body)) (cdr ,gargs))))
          (t
           `((eql ',sel (car ,gargs))
             (apply (lambda* ,(us-conv args) ,@(decl-us args) ,@body) (cdr ,gargs))))
          )))

(defmacro dlambda (&rest clauses)
  (um:with-unique-names (args)
    (let ((parser (dlam-parser args)))
      `(lambda (&rest ,args)
         (cond
          ,@(mapcar parser clauses)))
      )))

(defmacro dcase (args &rest clauses)
  `(apply (dlambda
            ,@clauses)
          ,args))

#|
(dlambda
  (:x (a) when (eq a 'this)
   (doit a))
  (:y (b)
   (doit b))
  (t _
     (x)))

 |#
;; -------------------------------------------

(defun dlam*-parser (gargs &optional (resfn #'identity))
  (lambda* ((sel args &rest body))
    (cond ((eq t sel)
           (if (eq 'when (car body))
               `((apply (lambda* ,args
                          (declare (ignorable ,@args))
                          ,(second body))
                        ,gargs)
                 ,(funcall resfn `(apply #',sel ,gargs)))
             ;; else
             `(t ,(funcall resfn `(apply #',sel ,gargs)))
             ))
          ((eq 'when (car body))
           `((and (eql ',sel (car ,gargs))
                  (apply (lambda* ,args
                           (declare (ignorable ,@args))
                           ,(second body))
                         (cdr ,gargs)))
             ,(funcall resfn `(apply #',sel (cdr ,gargs)))))
          (t
           `((eql ',sel (car ,gargs))
             ,(funcall resfn `(apply #',sel (cdr ,gargs)))))
          )))

(defun filter-when (clauses)
  (mapcan (lambda* ((sel args &body body))
            (if (eql (car body) 'when)
                `((,sel ,(us-conv args) ,@(decl-us args) ,@(cddr body)))
              `((,sel ,(us-conv args) ,@(decl-us args) ,@body))
              ))
          clauses))

(defmacro dlambda* (&rest clauses)
  ;; The advantage provided by this LABELS implementation is that each
  ;; clause can directly call on each other when needed.
  ;; But each tag must be a symbol.
  (um:with-unique-names (args)
    (let ((parser (dlam*-parser args)))
      (if (notevery (um:compose 'symbolp 'car) clauses)
          `(dlambda ,@clauses)
        ;; else
        `(labels*
             ,(filter-when clauses)
           (lambda (&rest ,args)
             (cond
              ,@(mapcar parser clauses))))
        ))))

(defmacro dcase* (args &rest clauses)
  `(apply (dlambda*
            ,@clauses)
          ,args))

#|
(dlambda*
  (:x (a) when (eq a 'this)
   (doit a))
  (:y (b)
   (doit b))
  (t _
     (x)))

 |#
;; -------------------------------------------

(defun tlam-parser (gargs)
  (dlam*-parser gargs (lambda (form)
                       `(lambda () ,form))))

(defmacro tlambda (&rest clauses)
  ;; a variant on DLAMBDA - instead of executing a matching clause, it
  ;; returns a closure that can do so later, or NIL of no clauses
  ;; match.
  (um:with-unique-names (args)
    (let ((parser (tlam-parser args)))
      `(labels*
           ;; using LABELS allows any clause to invoke another by name
           ,(filter-when clauses)
         (lambda (&rest ,args)
           (cond
            ,@(mapcar parser clauses))))
      )))

(defmacro tcase (args &rest clauses)
  `(apply (tlambda
           ,@clauses)
          ,args))

#|
(tlambda
  (:x (a) when (eq a 'this)
   (doit a))
  (:y (b)
   (doit b))
  (t _
     (x)))

 |#
;; ----------------------------------------------------
#+:LISPWORKS
(progn
  (editor:indent-like 'tlambda 'progn)
  (editor:indent-like 'tcase   'case)
  (editor:indent-like 'dlambda 'progn)
  (editor:indent-like 'dlambda* 'progn)
  (editor:indent-like 'dcase  'case)
  (editor:indent-like 'dcase* 'case))
;; ----------------------------------------------------------------------
#|
;; equiv to #F
;; (declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))
;; (declaim  (OPTIMIZE (SPEED 3) #+:LISPWORKS (FLOAT 0)))

(defun make-dlambda-dispatcher (fns def)
  (lambda (&rest args)
    (apply (getf fns (car args) def) args)))

(defun dlam-no-default (&rest args)
  (declare (ignore args))
  (error "No default action specified"))

(defmacro dlambda (&rest clauses)
  (let* ((def   ''dlam-no-default)
         (dummy (gensym))
         (lams  (mapcan (lambda (clause)
                          (and dummy
                               (destructuring-bind (key args . body) clause
                                 (if (eql t key)
                                     (setf def `(lambda ,args ,@body)
                                           dummy nil)
                                   `(',key (lambda ,(cons dummy args) ,@body)))
                                 )))
                        clauses)))
    `(make-dlambda-dispatcher (list ,@lams) ,def)))

(defmacro dcase (args &rest clauses)
  ;; make a once-only cached dlambda in case the dcase is in a loop
  ;; this speeds things up, and also preserves internal state between invocations
  `(apply (dlambda ,@clauses) ,args))

#+:LISPWORKS
(editor:setup-indent "DCASE" 1 nil nil 'flet)
#+:LISPWORKS
(editor:setup-indent "DLAMBDA" 0 nil nil 'flet)

|#
;; -----------------------------------------------------------------

#|
(let ((fn  (dlambda
            (:one () 1)
            (:two () 2)
            (t  (&rest args) :What?))))
  ;; (inspect fn)
  (funcall fn :zero))

(dcase '(:three)
  (:one () 1)
  (:two () 2)
  (t (&rest args) :Huh?))

(loop for ix in '(:one :two :three :four :five) do
      (print
       (funcall (let ((ct 0))
                  (lambda (&rest args)
                    (incf ct)
                    (apply (dlambda
                             (:one () 1)
                             (:two () 2)
                             (:five () 5)
                             (t (&rest args)
                                (declare (ignore args))
                                :Huh?
                                (print ct)))
                           args)))
                ix)))
  
(let ((fn  (dhandler
            (:one () 1)
            (:two () 2)
            (t  (&rest args) :What?))))
  ;; (inspect fn)
  (funcall fn :zero))

(dlambda
  (:x (a) :when (eq a 'this)
   (doit a))
  (:y (b)
   (doit b)))

 |#
