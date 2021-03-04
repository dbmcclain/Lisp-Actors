#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defsystem "core-crypto"
  :description "core-crypto: core cryptography functions"
  :version     "1.0.1"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :in-order-to ((test-op (test-op "core-crypto-test")))
  :serial       t
  :components  ((:file "ecc-package")
                (:file "cached-var")
                (:file "modular-arith")
                (:file "utilities")
                (:file "vec-repr-2")
                (:file "hash")
                (:file "ctr-hash-drbg")
                (:file "primes")
                (:file "startup")
                (:file "lib-loads")
                (:file "edwards")
                (:file "lagrange-4-square")
                (:file "pbc-cffi")
                (:file "crypto-safe-reader")
                (:file "pbc")
                (:file "subkey-derivation")
                (:file "proofs")
                (:file "init-crypto")
                (:file "keying")
                (:file "ed-keying")
                (:file "my-keying")
                (:file "schnorr")
                (:file "data-check"))
  :depends-on   ("ironclad"
                 "useful-macros"
                 "mpcompat"
                 "lisp-object-encoder"
                 "s-base64"
                 "cl-base58"
                 ;; "emotiq"
                 ;; "emotiq/delivery"
                 "cffi"
		 "core-crypto/libraries"
                 ))

(defsystem "core-crypto/libraries"
  :perform
  (prepare-op
   :before (o c)
   (let ((wildcard-for-libraries
          (make-pathname :defaults 
                         (asdf:system-relative-pathname
                          :core-crypto "./var/local/lib/libLispCurve1174")
                         :type :wild)))
     (unless (directory wildcard-for-libraries)
       (format *standard-output*
               "~&Failed to find libraries matching~&~t~a~&~
~&Attempting to build native libraries... hang on for a minute, please..."
               wildcard-for-libraries)
       (run-program `("bash"
                      ,(namestring (system-relative-pathname
                                    :core-crypto "./etc/build-crypto-ecc.bash")))
                    :output :string :error :string)
       (run-program `("bash"
                      ,(namestring (system-relative-pathname
                                    :core-crypto "./etc/build-crypto-pairings.bash")))
                    :output :string :error :string)
       (format *standard-output* "~tWhew!  Finished.~&")))))

