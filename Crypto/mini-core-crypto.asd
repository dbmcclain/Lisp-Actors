
(defsystem "mini-core-crypto"
  :description "mini-core-crypto: core cryptography subset functions"
  :version     "1.0.1"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :in-order-to ((test-op (test-op "core-crypto-test")))
  :serial       t
  :components  ((:file "mini-ecc-package")
                (:file "cached-var")
                (:file "modular-arith")
                (:file "utilities")
                (:file "vec-repr-2")
                (:file "hash")
                (:file "ctr-hash-drbg")
                ;; (:file "primes")
                ;; (:file "startup")
                ;; (:file "lib-loads")
                ;; (:file "edwards")
                ;; (:file "lagrange-4-square")
                ;; (:file "pbc-cffi")
                ;; (:file "crypto-safe-reader")
                ;; (:file "pbc")
                ;; (:file "subkey-derivation")
                ;; (:file "proofs")
                ;; (:file "init-crypto")
                )
  :depends-on   ("ironclad"
                 "com.ral.useful-macros"
                 "com.ral.lisp-object-encoder"
                 "s-base64"
                 "cl-base58"
                 ))

