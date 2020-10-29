
(defpackage :cps-test
  (:use :cl)
  (:export
   ))

(in-package :cps-test)

(defun exec-form (&rest forms)
  (let ((g!block (gensym))
        (g!args  (gensym))
        (g!ans   (gensym)))
    `(block ,g!block
       (macrolet ((=values (&rest ,g!args)
                    `(return-from ,',g!block (values ,@,!args))))
         ,@forms))
    ))

(defun =cont1 (fn)
  (lambda (&rest args)
    (when fn
      (apply (shiftf fn nil) args))))

(defun cont-fn (args body)
  (if (member '&rest args)
      `(lambda ,args
         ,@body)
    ;; else - as in multiple-value-bind
    `(lambda (&optional ,@args &rest #1=#:ignored)
       (declare (ignore #1#))
       ,@body)))

(defmacro =bind ((&rest args) expr &body body)
  `(let ((%sk (=cont1 ,(cont-fn args body))))
     ;; expr should return via =values
     ,expr))

(defmacro =values (&rest retvals)
  ;; invoke a continuation. This should generally be in tail position
  ;; continuation can only be invoked once
  (let ((g!ans (gensym)))
    `(let ((,g!ans (list ,@retvals)))
       (apply (shiftf %sk #'values) ,g!ans))
    ))

