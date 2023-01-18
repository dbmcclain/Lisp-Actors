;; Computationally heavy usage of actors. All CPUs are used.

(defpackage #:stresstest
  (:use :common-lisp)
  (:export
   #:echo
   #:receiver
   #:run
   ))

(in-package #:stresstest)

(defparameter *large-number* 1000000000)

(defparameter receiver
  (actors:create
   (lambda (idx msg)
     (format t "idx: ~a, msg: ~a~%" idx msg))))

(defun create-worker ()
  (actors:create
   (lambda (idx cnt)
     (let ((answer (loop for i from 1 to cnt count (oddp i))))
       (actors:send receiver idx answer)))))

(defparameter main
  (actors:create
   (lambda (idx cnt)
     (let ((worker (create-worker)))
       (actors:send worker idx cnt)))))

(defun run ()
  (dotimes (n 20)
    (actors:send main n *large-number*)))
