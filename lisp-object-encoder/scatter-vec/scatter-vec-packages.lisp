
(in-package :common-lisp-user)

(defpackage #:com.ral.scatter-vec
  (:use :common-lisp)
  #+nil
  (:local-nicknames (#:um  #:com.ral.useful-macros))
  (:export
   #:scatter-vector
   #:make-scatter-vector
   #:in-bounds-p
   #:xlength
   #:xaref
   #:xdefrag
   #:xsubseq
   #:xposition
   #:xdovec
   #:xupdate-digest
   #:xwrite-sequence
   #:add-fragment
   ))

