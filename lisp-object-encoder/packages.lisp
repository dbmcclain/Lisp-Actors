
(in-package :user)

(defpackage #:xzlib
  (:use #:cl #:scatter-vec)
  (:export
   #:compress
   #:uncompress))

(defpackage :lzw
  (:use :cl)
  (:local-nicknames
   (#:um       #:com.ral.useful-macros)
   (#:ubstream #:ubyte-streams))
  (:export
   :compressed
   :compressed-data
   :make-compressed
   :lzw-compress
   :lzw-decompress
   :make-empty-vector
   :vector-append1
   :vector-append
   :cvt-octets-to-intvec
   :cvt-intvec-to-octets
   :compress
   :decompress
   :zl-compress
   :zl-compressed
   :make-zl-compressed
   :zl-compressed-data
   ))

(defpackage :lisp-object-encoder
  (:use #:common-lisp #:scatter-vec)
  (:local-nicknames
   (#:ubstream  #:ubyte-streams)
   (#:mgdbuf    #:managed-buffers)
   (#:uuid      #:com.ral.uuid)
   (#:um        #:com.ral.useful-macros))
  (:nicknames #:loenc)
  (:import-from #:sdle-store
   #:output-type-code
   #:store-object
   #:store-count
   #:read-count
   #:restore-object
   #:register-code
   #:next-available-code
   #:$unbound-marker
   #:after-retrieve
   #:rawbytes
   #:rawbytes-bytes
   #:make-rawbytes)
  (:export
   #:register-code
   #:next-available-code
   #:defstore
   #:defrestore
   #:store-count
   #:store-object
   #:read-count
   #:restore-object
   #:skip-data
   #:read-data
   #:decode-prefix-length
   #:read-prefix-length
   #:read-raw-bytes
   #:must-read-raw-bytes
   #:encode-prefix-length
   #:encode
   #:decode
   #:serialize
   #:deserialize
   #:after-retrieve
   #:early-eof
   #:$unbound-marker
   #:rawbytes
   #:rawbytes-bytes
   #:make-rawbytes
   #:with-stack-buffer
   #:decode-prefix-length
   #:loe-back-end
   #:ensure-portable-condition
   ))

