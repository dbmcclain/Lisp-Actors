
(in-package :ac)


#||#
(defun =basic-cont (fn)
  ;; make fn into a one-shot execute
  (lambda (&rest args)
    (when fn
      (apply (shiftf fn nil) args))))
#||#
#|
(defun =basic-cont (fn)
  fn)
|#

(defun =cont (fn)
  ;; will be augmeted with advice in Actors package
  (=basic-cont fn))

(defun cont-fn (args body)
  (if (member '&rest args)
      `(lambda ,args
         ,@body)
    ;; else - as in multiple-value-bind
    `(lambda (&optional ,@args &rest #1=#:ignored)
       (declare (ignore #1#))
       ,@body)))

#|
(defun closure (free-args fn-form)
  `(let ,(mapcar #`(,a1 ,a1) free-args)
     ,fn-form))

(defmacro =bind* (args free-args expr &body body)
  ;; can only be used in the context of a continuation, or WITH-CONT
  `(let ((%sk (=cont (progn ;; um:with-tail-pure-code
                         ,(closure (cons '%sk free-args) (cont-fn args body))))))
     ;; expr should return via =values
     ,expr))
|#

(defmacro =bind* ((&rest args) expr &body body)
  `(let ((%sk (=cont ,(cont-fn args body))))
     ;; expr should return via =values
     ,expr))
