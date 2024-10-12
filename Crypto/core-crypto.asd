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
                (:file "primes")
                ;; (:file "startup")                  ;; edwards-ecc-v2
                ;; #-:WINDOWS (:file "lib-loads")     ;; edwards-ecc-v2
                ;; (:file "proofs-ecc")
                (:file "lagrange-4-square")
                ;; #-:WINDOWS (:file "pbc-cffi")
                ;; #-:WINDOWS (:file "crypto-safe-reader")
                ;; #-:WINDOWS (:file "pbc")
                ;; #-:WINDOWS (:file "subkey-derivation")
                ;; #-:WINDOWS (:file "proofs")
                ;; (:file "init-crypto")              ;; edwards-ecc-v2
                ;; (:file "keying")                   ;; edwards-ecc-v2
                ;; (:file "ed-keying")                ;; edwards-ecc-v2
                ;; (:file "my-keying")                ;; edwards-ecc-v2
                ;; (:file "schnorr")                  ;; edwards-ecc-v2
                ;; (:file "data-check")               ;; edwards-ecc-v2
                )
  :depends-on   (;; "edwards-ecc-v2"
                 ;; "edwards-ecc"
                 ;; "ironclad"
                 ;; "useful-macros"
                 ;; "mpcompat"
                 ;; "lisp-object-encoder"
                 ;; "s-base64"
                 ;; "cl-base58"
                 ;; "emotiq"
                 ;; "emotiq/delivery"
                 ;; "cffi"
		 ;; #-:WINDOWS "core-crypto/libraries"
                 ))

#|
#-:WINDOWS
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
       #|
       (run-program `("bash"
                      ,(namestring (system-relative-pathname
                                    :core-crypto "./etc/build-crypto-pairings.bash")))
                    :output :string :error :string)
       |#
       (format *standard-output* "~tWhew!  Finished.~&")))))

|#
