
(defpackage :com.ral.useful-macros.nlet
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.nlet)

(defun %i-nlet (whole go-name name bindings decls+body)
  (let* ((gs    (gensyms bindings))
         (gn    (gensym))
         (vars  (mapcar #'car bindings))
         (xvars (gensyms vars)))
    (multiple-value-bind (body decls doc)
        (parse-body decls+body :documentation t :whole whole)
      `(labels ((,name ,xvars
                  ,@doc
                  (macrolet ((,go-name ,gs
                               `(progn
                                  (psetq
                                   ,@(mapcan 'list
                                             ',xvars
                                             (list ,@gs)))
                                  (go ,',gn))
                               ))
                    (tagbody
                     ,gn
                     (return-from ,name
                       (symbol-macrolet ,(mapcar #'list vars xvars)
                         ,@decls
                         ,@body)))
                    )))
         (,name ,@(mapcar #'second bindings)))
      )))

(defmacro nlet (&whole whole name bindings &body decls+body)
  (%i-nlet whole (symb :go- name) name bindings decls+body))

#|
 ;; deprecated...
(defmacro nlet-tail (&whole whole name bindings &body decls+body)
  (%i-nlet whole name name bindings decls+body))
|#

#|
(nlet iter ((a a-init)
            (b b-init))
  (declare (fixnum a b))
  (clause1)
  (go-iter xx yy))

(defun tst ()
  (nlet iter ((x   1)
               (ans 1))
    "A test of NLET"
    (declare (fixnum x ans))
    (if (> x 10)
        ans
      (go-iter (1+ x) (* x ans))
      )))
(tst)  

 |#
