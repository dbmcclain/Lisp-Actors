
(um:eval-always
  (project:defproject
   (#:self-synca  #:com.ral.actors.encoding.self-sync)
   (#:secure-comm #:com.ral.actors.secure-comm)
   (#:encoding    #:com.ral.actors.encoding)
   (#:eccke       #:com.ral.crypto.ecc-key-exchange)))

(defpackage :com.ral.actors.encoding.self-sync
  (:use #:cl #:actors)
  (:export
   #:encode
   #:decode
   #:stream-decoder

   #:+long-count-base+
   #:+max-short-count+
   #:+max-long-count+
   #:+start-sequence+

   #:int-to-vec-le4
   #:vec-le4-to-int
   #:crc32
   ))

(defpackage :com.ral.actors.encoding
  (:use
   #:common-lisp
   #:actors)
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
   #:non-destructive-encryptor
   #:non-destructive-decryptor
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
   #:client-ratchet-manager
   #:server-ratchet-manager
   #:ratchet-encryptor
   #:ratchet-decryptor
   ))

(defpackage :com.ral.actors.secure-comm
  (:use
   #:common-lisp
   #:com.ral.actors
   #:core-crypto
   #:edec
   #:encoding)
  (:import-from #:um
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
   #:actors
   #:encoding)
  (:import-from #:um
   #:when-let
   #:wr
   #:copy-with)
  (:import-from #:vec-repr
   #:bevn
   #:vec
   #:int)
  (:import-from #:secure-comm
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

#|
(asdf :doctools)
(doctools:gen-docs
 :asdf-system-name :com.ral.actors.secure-channel
 :package-name     :com.ral.actors.secure-comm
 :directory        (translate-logical-pathname "PROJECTS:LISP;xTActors;secure-channel")
 :subtitle         "Secure Sockets and Remote Actors")
|#