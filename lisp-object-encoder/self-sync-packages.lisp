
(in-package :user)

(defpackage #:self-sync
  (:local-nicknames (#:um #:useful-macros))
  (:export
   #:write-record
   #:make-reader
   #:make-reader-fsm
   #:encode
   #:decode))

