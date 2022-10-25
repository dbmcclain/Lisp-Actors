
(in-package #:cl-user)

(defpackage #:com.ral.cps
  (:use #:cl #:def*)

  (:import-from #:um
   #:symb
   #:if-let
   #:when-let
   #:split
   #:lambda*
   #:defun*
   #:labels*
   #:flet*
   #:nlet)

  (:import-from #:um
   #:dynamic-wind
   #:proceed
   #:capture-dynamic-environment
   #:with-dynamic-environment
   #:call-with-dynamic-environment)
  (:export
   #:dynamic-wind
   #:proceed
   #:capture-dynamic-environment
   #:with-dynamic-environment
   #:call-with-dynamic-environment)

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

