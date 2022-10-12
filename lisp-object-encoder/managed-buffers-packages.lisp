
(in-package :user)

(defpackage :managed-buffers
  (:use #:common-lisp #:priq)
  ;; (:nicknames #:mgdbuf)
  (:local-nicknames (#:um  #:com.ral.useful-macros))
  (:export
   #:make-buffer
   #:init-buffer-queues
   #:get-buffer
   #:recycle-buffer
   #:with-temporary-buffer))

