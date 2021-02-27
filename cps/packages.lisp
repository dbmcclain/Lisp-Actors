
(in-package #:cl-user)

(defpackage #:cps
  (:use #:cl)
  (:import-from #:alexandria
   #:parse-ordinary-lambda-list)
  (:import-from #:useful-macros
   #:symb
   #:if-let
   #:when-let
   #:split
   #:lambda*
   #:defun*
   #:labels*
   #:flet*
   #:nlet)
  (:export
   #:=bind
   #:=future
   #:=values
   #:=defun
   #:=defgeneric
   #:=defmethod
   #:=labels
   #:=flet
   #:=lambda
   #:=funcall
   #:=apply
   #:with-cont
   
   #:=bind-cont
   #:=wait-cont
   #:=wait

   #:=nlet
   #:=nlet-tail

   #:prep-wait
   #:do-wait

   #:=tlet
   #:trampoline
   #:with-trampoline
   #:=cont
   #:=fut
   #:with-cps
   #:=handler-bind
   #:=handler-case
   #:=catch
   #:=restart-case
   #:=restart-bind
   #:=with-simple-restart
   #:=ignore-errors
   #:=handler-bind*
   #:=handler-case*
   #:=handler-bind-case
   #:=unwind-protect
   #:=let
   #:=let*
   ))

