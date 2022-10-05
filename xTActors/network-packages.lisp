
(defpackage :com.ral.actors.encoding.self-sync
  (:use #:cl #:com.ral.actors)
  (:export
   #:encode
   #:decode
   #:stream-decoder
   ))

(defpackage :com.ral.actors.encoding
  (:use
   #:common-lisp
   #:com.ral.actors
   #:def*)
  (:export
   #:encrypt/decrypt
   #:make-auth-key
   #:make-auth
   #:check-auth
   #:make-signature
   #:check-signature
   #:list-imploder
   #:list-exploder
   #:printer
   #:writer
   #:marker
   #:marshal-encoder
   #:marshal-decoder
   #:fail-silent-marshal-decoder
   #:simple-compress
   #:simple-uncompress
   #:marshal-compressor
   #:marshal-decompressor
   #:fail-silent-marshal-decompressor
   #:noncer-beh
   #:noncer
   #:encryptor
   #:decryptor
   #:authentication
   #:check-authentication
   #:signing
   #:signature-validation
   #:self-sync-encoder
   #:checksum
   #:verify-checksum
   #:chunker
   #:dechunker
   #:chunk-monitor
   #:netw-encoder
   #:netw-decoder
   #:disk-encoder
   #:disk-decoder
   #:self-sync-stream-writer
   #:read-self-sync-stream
   #:+AONT-FILE-TYPE-ID+
   #:aont-encoder
   #:aont-decoder
   #:aont-file-writer
   #:aont-file-reader
   ))

(defpackage :com.ral.actors.network
  (:use
   #:common-lisp
   #:com.ral.actors
   #:def*
   #:com.ral.actors.encoding)
  (:export
   #:*default-port*
   #:*socket-timeout-period*
   #:start-tcp-server
   #:terminate-server
   #:*default-port*
   #:+MAX-FRAGMENT-SIZE+
   #:client-connector
   #:connections
   ))

(defpackage :com.ral.actors.secure-comm
  (:use
   #:common-lisp
   #:com.ral.actors
   #:core-crypto
   #:edec #:def*
   #:com.ral.actors.encoding)
  (:import-from #:com.ral.actors.network
   #:connections
   #:client-connector)
  (:export
   #:make-local-services
   #:global-services
   #:server-crypto-gateway
   #:client-gateway
   #:remote-service
   #:+server-connect-id+
   #:server-skey
   #:start-server-gateway
   ))

