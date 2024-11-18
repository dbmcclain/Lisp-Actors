
(in-package #:com.ral.aquaterm)

(defmacro by-proxy (&body body)
  `(aqt::proxy-request #'(lambda () 
                           ,@body)))
  
(defmacro def-proxy-fli-function ((name &rest args)
                                  user-args &rest other-args)
  (let ((cname       (um:intern-symbol (format nil "_~A" name)))
        (caller-args (delete :constant 
                             (mapcar #'first user-args)))
        (coercions   (delete nil 
                             (mapcar #'um:coerce-fli-arg user-args))))
    `(progn
       (fli:define-foreign-function (,cname ,@args)
           ,user-args
         ,@other-args)
       (defun ,name ,caller-args
         (let ,coercions
           (aqt::by-proxy (,cname ,@caller-args)))))
    ))