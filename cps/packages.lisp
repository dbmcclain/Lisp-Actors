
(in-package :cl-user)

(defpackage :cps
  (:use :cl)
  (:import-from :alexandria
   :parse-ordinary-lambda-list)
  (:import-from :useful-macros
   :symb
   :if-let
   :when-let
   :split
   :lambda*
   :defun*
   :labels*
   :flet*
   :nlet
   :nlet-tail)
  (:export
   :=bind
   :=future
   :=values
   :=defun
   :=defgeneric
   :=defmethod
   :=labels
   :=flet
   :=lambda
   :=funcall
   :=apply
   :with-cont
   
   :=bind-cont
   :=wait-cont
   :=wait

   :=nlet
   :=nlet-tail

   :prep-wait
   :do-wait

   :=tlet
   :thunk
   :trampoline
   :=cont
   :=cont1
   :with-cps
   :once-only
   :=handler-bind
   :=handler-case
   :=catch
   :=let
   :=let*
   :=restart-case
   :=restart-bind
   :=with-simple-restart
   :=ignore-errors
   :=handler-bind*
   :=handler-case*
   :=handler-bind-case
   :=unwind-protect
   ))

