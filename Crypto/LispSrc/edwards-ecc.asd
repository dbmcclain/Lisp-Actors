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


(defsystem "edwards-ecc"
  :description "edwards-ecc: Single-curve ECC crypto along Edwards Curves"
  :version     "1.0.1"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :in-order-to ((test-op (test-op "core-crypto-test")))
  :serial       t
  :components  (
                #|
                (:file "primes")
                (:file "startup")
                #-:WINDOWS (:file "lib-loads")
                |#
                (:file "startup")
                #-:WINDOWS (:file "lib-loads")

                (:file "ed-types")
                (:file "ed-curves")
                #-:WINDOWS (:file "edwards-metal")
                (:file "edwards")
                (:file "elligator")
                #|
                (:file "proofs-ecc")
                (:file "lagrange-4-square")
                #-:WINDOWS (:file "pbc-cffi")
                #-:WINDOWS (:file "crypto-safe-reader")
                #-:WINDOWS (:file "pbc")
                #-:WINDOWS (:file "subkey-derivation")
                #-:WINDOWS (:file "proofs")
                |#
                (:file "keying")
                (:file "ed-keying")
                (:file "my-keying")
                (:file "schnorr")
                (:file "init-crypto")
                (:file "data-check"))
  
  :depends-on   ("mini-core-crypto"
                 "cffi"
		 #-:WINDOWS "edwards-ecc/libraries"
                 ))

#-:WINDOWS
(defsystem "edwards-ecc/libraries"
  :perform
  (prepare-op
   :before (o c)
   (let ((wildcard-for-libraries
          (make-pathname :defaults 
                         (asdf:system-relative-pathname
                          :core-crypto "../var/local/lib/libLispCurve1174")
                         :type :wild)))
     (unless (directory wildcard-for-libraries)
       (format *standard-output*
               "~&Failed to find libraries matching~&~t~a~&~
~&Attempting to build native libraries... hang on for a minute, please..."
               wildcard-for-libraries)
       (run-program `("bash"
                      ,(namestring (system-relative-pathname
                                    :core-crypto "../etc/build-crypto-ecc.bash")))
                    :output :string :error :string)
       (format *standard-output* "~tWhew!  Finished.~&")))))

