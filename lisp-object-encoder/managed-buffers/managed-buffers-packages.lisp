
(in-package :common-lisp-user)

(defpackage :com.ral.managed-buffers
  (:use #:common-lisp #:priq) ;; #:com.ral.prio-queue)
  ;; (:nicknames #:mgdbuf)
  #+nil
  (:local-nicknames (#:um  #:com.ral.useful-macros))
  (:export
   #:make-buffer
   #:init-buffer-queues
   #:get-buffer
   #:recycle-buffer
   #:with-temporary-buffer))

