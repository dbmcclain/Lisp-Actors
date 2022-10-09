
(in-package :user)

(defpackage #:scatter-vec
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

