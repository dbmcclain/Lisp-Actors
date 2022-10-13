
(in-package :user)

(defpackage :com.ral.managed-buffers
  (:use #:common-lisp #:com.ral.prio-queue)
  ;; (:nicknames #:mgdbuf)
  (:local-nicknames (#:um  #:com.ral.useful-macros))
  (:export
   #:make-buffer
   #:init-buffer-queues
   #:get-buffer
   #:recycle-buffer
   #:with-temporary-buffer))

