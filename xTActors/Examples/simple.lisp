;; Very simple example on how to use actors

(defpackage #:simple
  (:use :common-lisp)
  (:export
   #:echo
   #:receiver
   #:run
   ))

(in-package #:simple)


(defparameter receiver
  (actors:create
   (lambda (msg)
     (print (format nil "~a: ~a~%" :receiver msg)))))

(defparameter echo
  (actors:create
   (lambda (msg)
     (print (format nil "~a: ~a~%" :echo msg))
     (actors:send receiver msg))))

(defun run ()
  (actors:send echo :hello))
