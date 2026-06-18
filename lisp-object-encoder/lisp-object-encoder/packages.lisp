
(in-package :common-lisp-user)

(project:defproject
 (#:xzlib         #:com.ral.xzlib)
 (#:lzw           #:com.ral.lzw))
 
(defpackage #:com.ral.xzlib
  (:use #:cl #:scatter-vec)
  (:export
   #:compress
   #:uncompress))

(defpackage #:com.ral.loenc-private
  (:use #:cl)
  (:export
   #:ex-encode
   #:ex-decode
   #:ex-serialize
   #:ex-deserialize))

(defpackage #:serializer
  (:use #:common-lisp #:com.ral.loenc-private)
  #+:lispworks (:import-from #:clos
                #:generic-function-name
                #:slot-definition-allocation
                #:compute-slots
                #:slot-definition
                #:slot-definition-initform
                #:slot-definition-initargs
                #:slot-definition-name
                #:slot-definition-readers
                #:slot-definition-type
                #:slot-definition-writers
                #:class-direct-default-initargs
                #:class-direct-slots
                #:class-slots
                #:class-direct-superclasses
                #:ensure-class)
  #+sbcl (:import-from #:sb-mop
          #:generic-function-name
          #:slot-definition-allocation
          #:slot-definition
          #:compute-slots
          #:slot-definition-initform
          #:slot-definition-initargs
          #:slot-definition-name
          #:slot-definition-readers
          #:slot-definition-type
          #:slot-definition-writers
          #:class-direct-default-initargs
          #:class-direct-slots
          #:class-direct-superclasses
          #:class-slots
          #:ensure-class)
  (:export
   #:encode
   #:decode
   #:serialize
   #:deserialize

   #:make-serializable
   #:deserialize-type
   ))

(defpackage :com.ral.lisp-object-encoder
  (:use #:common-lisp #:scatter-vec #:com.ral.loenc-private)
  (:import-from #:serializer
   #:encode
   #:decode
   #:serialize
   #:deserialize)
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

