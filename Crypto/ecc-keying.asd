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

(in-package :cl-user)

(asdf:defsystem "ecc-keying"
  :description "ecc-keying: encryption based on NIST B-571 Elliptic Curve Cryptography"
  :version     "1.0"
  :author      "D.McClain <david@acudora.com>"
  :license     "Copyright (c) 2011 by Acudora, Inc. All rights reserved."
  :components  ((:file "ecc-utils")
                (:file "crypto-le")
                (:file "gf-571")
                (:file "ecc-B571")
                (:file "kdf")
                (:file "crypto-environ")
                (:file "passwds")
                (:file "ecc-keys")
                (:file "encryption")
                (:file "gfc-encryption")
                (:file "ctr-hmac-encryption")
                (:file "3ctr-hmac-encryption"))
  :serial       t
  :depends-on   ("ironclad"
                 "com.ral.useful-macros"
                 "com.ral.lisp-object-encoder"
                 "s-base64"
                 ;; "plotter"
                 "data-objects"
                 "core-crypto"
                 ))

