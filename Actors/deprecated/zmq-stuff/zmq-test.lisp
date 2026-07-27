;; zmq-test.lisp
;;
;; DM/RAL  2023/09/26 11:27:19
;; ----------------------------------

(ql:quickload :zeromq)

(defpackage #:zmq-test
  (:use #:common-lisp #|#:zmq-intf|#))

(in-package #:zmq-test)

;; ----------------------------------

#|
;; using my adaptation of libzmq
(defun zmq-hw-client-test ()
  (let* ((ctx  (zmq-ctx-new))
         (req  (zmq-socket ctx +zmq-req+)))
    (fli:with-dynamic-foreign-objects ()
      (let ((ans (fli:allocate-dynamic-foreign-object :type :char
                                                      :nelems 10)
                 ))
        (zmq-connect req "tcp://localhost:5555")
        (dotimes (ix 10)
          (format t "~%Sending Hello ~D" ix)
          (zmq-str-send req "Hello" 5 0)
          #|
          (loop for c across "Hello"
                  for jx from 0
                  do
                  (setf (fli:dereference ans :index jx) (char-code c)))
          (zmq-send req ans 5 0)
          |#
          (let ((nel (zmq-recv req ans 10 0)))
            (format t "~%Received ~A ~D" (fli:convert-from-foreign-string ans :length nel) ix)))
        (zmq-close req)
        (zmq-ctx-destroy ctx)
        ))
    ))
|#

;; using zeromq
(defun zmq-hw-client-test ()
  (zeromq:with-context (ctx)
    (let ((req (zeromq:socket ctx :REQ)))
      (zeromq:connect req "tcp://zircon.local:5555")
      (dotimes (ix 10)
        (format t "~%Sending Hello ~D" ix)
        (zeromq:send req "Hello")
        (format t "~%Received ~A ~D" (zeromq:recv req 10) ix))
      (zeromq:close req))
    ))

#|
(zmq-hw-client-test)
 |#