
(in-package :common-lisp-user)

(defpackage #:com.ral.ubyte-streams
  (:use #:common-lisp #:scatter-vec) ;; #:com.ral.scatter-vec)
  ;; (:nicknames #:ubstream)
  #+nil
  (:local-nicknames
   (#:um     #:com.ral.useful-macros)
   (#:mgdbuf #:com.ral.managed-buffers))
  #+:LISPWORKS
  (:import-from #:stream
   #:stream-file-position)
  (:export
   #:ubyte-stream
   #:ubyte-input-stream
   #:ubyte-output-stream
   #:make-ubyte-input-stream
   #:make-ubyte-output-stream
   #:stream-bytes
   #:with-input-from-ubyte-stream
   #:with-output-to-ubyte-stream
   ;; Allegro does not export stream:stream-file-position
   #:stream-file-position

   #:scatter-vector
   #:scatter-vector-add-fragment
   #:in-bounds-p
   #:scatter-vector-length
   #:xaref
   #:xlength
   #:xdefrag
   #:xsubseq
   ))

