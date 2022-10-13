
(defpackage :com.ral.actors.encoding.self-sync
  (:use #:cl #:com.ral.actors)
  (:local-nicknames
   (#:ubyte-streams #:com.ral.ubyte-streams)
   (#:scatter-vec   #:com.ral.scatter-vec)
   (#:um  #:com.ral.useful-macros))
  (:export
   #:encode
   #:decode
   #:stream-decoder
   ))

(defpackage :com.ral.actors.encoding
  (:use
   #:common-lisp
   #:com.ral.actors)
  (:local-nicknames
   (#:loenc     #:com.ral.lisp-object-encoder)
   (#:sets      #:com.ral.rb-tree.sets)
   (#:um        #:com.ral.useful-macros)
   (#:uuid      #:com.ral.uuid)
   (#:modmath   #:crypto/modular-arith)
   (#:self-sync #:com.ral.actors.encoding.self-sync))
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

(defpackage :com.ral.actors.secure-comm
  (:use
   #:common-lisp
   #:com.ral.actors
   #:core-crypto
   #:edec
   #:com.ral.actors.encoding)
  (:local-nicknames
   (#:loenc #:com.ral.lisp-object-encoder)
   (#:uuid  #:com.ral.uuid)
   (#:sets  #:com.ral.rb-tree.sets)
   (#:aop   #:com.ral.useful-macros.dflet)
   (#:um    #:com.ral.useful-macros))
  (:import-from #:com.ral.useful-macros
   #:capture-ans-or-exn
   #:call-capturing-ans-or-exn
   #:recover-ans-or-exn)
  (:export
   #:make-local-services
   #:global-services
   #:server-crypto-gateway
   #:client-gateway
   #:remote-service
   #:+server-connect-id+
   #:server-skey
   #:start-server-gateway
   #:connections
   #:client-connector
   ))

(defpackage :com.ral.actors.network
  (:use
   #:common-lisp
   #:com.ral.actors
   #:com.ral.actors.encoding)
  (:local-nicknames
   (#:self-sync #:com.ral.actors.encoding.self-sync))
  (:import-from #:com.ral.useful-macros
   #:when-let
   #:wr
   #:copy-with)
  (:import-from #:vec-repr
   #:bevn
   #:vec
   #:int)
  (:import-from #:com.ral.actors.secure-comm
   #:connections
   #:client-connector
   #:make-local-services
   #:server-crypto-gateway
   #:+server-connect-id+)
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

