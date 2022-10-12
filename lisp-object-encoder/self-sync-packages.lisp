
(in-package :user)

(defpackage #:self-sync
  (:use #:common-lisp #:com.ral.useful-macros)
  (:local-nicknames (#:um #:com.ral.useful-macros))
  (:export
   #:write-record
   #:make-reader
   #:make-reader-fsm
   #:encode
   #:decode))

