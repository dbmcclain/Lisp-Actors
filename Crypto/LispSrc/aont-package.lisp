
(defpackage #:aont
  (:use
   #:common-lisp
   #:com.ral.crypto.crypto-utils
   #:ciphers
   #:prng
   #:kdf)
  (:export
   #:aont-ctr-ecb-enc-dec
   #:$aont-canary$
   #:aont-transform
   #:aont-untransform

   #:file-vector
   #:cvt-intvec-to-octets
   #:cvt-octets-to-intvec
   #:write-16u
   #:read-16u
   #:write-32u
   #:read-32u
   #:write-chunk
   #:read-chunk
   #:$max-chunk
   #:write-compression
   #:read-compression
   #:aont-encode
   #:aont-decode
   #:aont-decode-to-string
   ))

(defpackage #:tolstoy-aont
  (:use
   #:common-lisp
   #:aont
   #+:ACTORS #:actors)
  (:export
   #:make-aont-messaging-intf
   ))
