
(in-package :user)

(defpackage #:com.ral.self-sync
  (:use #:common-lisp #:um) ;; #:com.ral.useful-macros)
  #+nil
  (:local-nicknames (#:um #:com.ral.useful-macros))
  (:export
   #:write-record
   #:make-reader
   #:make-reader-fsm
   #:encode
   #:decode))

