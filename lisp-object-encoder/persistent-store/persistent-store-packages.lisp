
(in-package :user)

(defpackage #:persistent-store
  (:use #:common-lisp)
  ;; (:nicknames #:persist)
  (:export
   #:make-persistent-store
   #:retrieve
   #:retrieve-store
   #:persist
   #:persist-in-store
   #:mark-dirty
   #:unpersist
   #:commit
   #:commit-store
   #:revert
   ))

