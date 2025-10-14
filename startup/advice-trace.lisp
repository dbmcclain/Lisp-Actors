
(defpackage #:com.ral.advice-trace
  (:use #:common-lisp)
  (:export
   #:*advised*
   #:show-advised))

(in-package #:com.ral.advice-trace)

(defvar *advised* '())

(lw:defadvice (compiler::define-around-advice watch-advised :after) (n &rest junk)
 (declare (ignore junk))
 (pushnew (list n *load-pathname*) *advised* :test #'equalp))

(lw:defadvice (compiler::define-before-advice watch-advised :after) (n &rest junk)
 (declare (ignore junk))
 (pushnew (list n *load-pathname*) *advised* :test #'equalp))

(lw:defadvice (compiler::define-after-advice watch-advised :after) (n &rest junk)
 (declare (ignore junk))
 (pushnew (list n *load-pathname*) *advised* :test #'equalp))


(defun show-advised ()
  (with-standard-io-syntax
    (pprint *advised*))
  (values))
