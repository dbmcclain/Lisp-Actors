
(in-package :user)

(defpackage #:scatter-vec
  (:export
   #:scatter-vector
   #:in-bounds-p
   #:xlength
   #:xaref
   #:xdefrag
   #:xsubseq
   #:xposition
   #:add-fragment
   ))

(defpackage #:self-sync
  (:export
   #:write-self-sync
   #:read-self-sync))

(defpackage #:xzlib
  (:use #:cl #:scatter-vec)
  (:export
   #:compress
   #:uncompress))

(defpackage :lzw
  (:use :cl)
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

(defpackage :managed-buffers
  (:use #:common-lisp #:priq)
  (:nicknames #:mgdbuf)
  (:export
   #:make-buffer
   #:init-buffer-queues
   #:get-buffer
   #:recycle-buffer
   #:with-temporary-buffer))

(defpackage #:persistent-store
  (:use #:common-lisp)
  (:nicknames #:persist)
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

(defpackage :prevalent-object
  (:use #:common-lisp)
  (:nicknames #:prevo)
  (:import-from :loenc
   :store-count
   :store-object
   :read-count
   :restore-object
   :loe-back-end
   :serialize
   :deserialize
   :register-code
   :defrestore
   :defstore)
  (:import-from :sdle-store
   :find-backend
   :defbackend)
  (:import-from :um
   :if-let
   :when-let
   :magic-word
   :format-error)
  (:export
   #:*prevalent-system*
   #:*default-prevalent-filename*
   #:system

   #:open-system
   #:save-system
   #:restore-system
   #:close-system
   
   #:add-root-object
   #:remove-root-object
   #:get-root-object
   #:root-keys
   #:touch-slot
   
   #:prevalent-class
   #:prevalent-object
   
   #:remove-object
   #:mutate-object
   ))

(defpackage #:ubyte-streams
  (:use #:common-lisp #:scatter-vec)
  (:nicknames #:ubstream)
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

