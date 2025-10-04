
(in-package :common-lisp-user)

(project:defproject
 (#:xzlib         #:com.ral.xzlib)
 (#:lzw           #:com.ral.lzw))
 
(defpackage #:com.ral.xzlib
  (:use #:cl #:scatter-vec)
  (:export
   #:compress
   #:uncompress))

(defpackage :com.ral.lisp-object-encoder
  (:use #:common-lisp #:scatter-vec)
  (:import-from #:sdle-store
   #:output-type-code
   #:store-object
   #:store-count
   #:read-count
   #:restore-object
   #:register-code
   #:next-available-code
   #:$unbound-marker
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
   #:early-eof
   #:$unbound-marker
   #:rawbytes
   #:rawbytes-bytes
   #:make-rawbytes
   #:with-stack-buffer
   #:decode-prefix-length
   #:loe-back-end
   #:ensure-portable-condition

   #:before-store
   #:after-restore
   #:unshared-list

   #:dump
   ))

(defpackage #:com.ral.lzw
  (:use #:cl)
  (:export
   #:compressed
   #:compressed-data
   #:make-compressed
   #:lzw-compress
   #:lzw-decompress
   #:make-empty-vector
   #:vector-append1
   #:vector-append
   #:cvt-octets-to-intvec
   #:cvt-intvec-to-octets
   #:compress
   #:decompress
   #:zl-compress
   #:zl-compressed
   #:make-zl-compressed
   #:zl-compressed-data
   ))

