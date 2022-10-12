;; clc.lisp -- check/lock/check pattern
;;
;; CHECK/LOCK/CHECK [aka CLC] is a very common pattern in SMP
;; multiprocessing. You first check for a value and return it if
;; present. Otherwise you lock a shared lock, check again, or else
;; execute some body of code.
;;
;; DM/RAL 04/21
;; -------------------------------------------------------

(defpackage :com.ral.useful-macros.clc
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.clc)

(defun do-clc (chkfn lock bodyfn)
  (or (funcall chkfn)
      (mpcompat:with-lock (lock)
        (or (funcall chkfn)
            (funcall bodyfn)))
      ))

(defmacro check/lock/check (chkform lock &body body)
  `(do-clc (lambda ()
             ,chkform)
           ,lock
           (lambda ()
             ,@body)))
