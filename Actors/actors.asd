#|
MIT License Terms:

Copyright (c) 2017, Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

;; ... or feel free to not use ECC (uses RSA instead)
(pushnew :using-ecc-crypto *features*)
;; On MacOSX - use Grand Central Dispatch (or not...)
#+:MACOSX
(pushnew :using-mac-gcd *Features*)

(asdf:defsystem "actors"
  :description "Actors Multiplexed on an OS Thread Pool"
  :version     "2.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2017 by Refined Audiometrics Laboratory, LLC. MIT License terms apply."
  :components  ((:file "packages")
                (:file "adapters")
                #-:lispworks (:file "ansi-timer")
                ;; (:file "actors")
                (:file "actors-lf")
                (:file "watch")
                (:file "actor-class")
                #-:USING-MAC-GCD (:file "executives")
                #+:USING-MAC-GCD (:file "mac-executives")
                (:file "par-exec")
                (:file "actors-schedule")
                (:file "actors-machines")
                (:file "actors-futures")
                (:file "promises")
                (:file "actors-directory")
                (:file "actors-lfm")
                ;; (:file "actor-bridge")
                (:file "bridge-v2")
                (:file "network-security")
                #+:USING-ECC-CRYPTO (:file "srp6-ecc")
                #-:USING-ECC-CRYPTO (:file "srp6-rsa")
                (:file "network-connection")
                (:file "erl-proc")
                (:file "subscribe")
                ;; (:file "reactor")
                )
  :SERIAL T
  :depends-on   ("data-objects"
                 "trivia"
		 "mpcompat"
                 "cps"
                 "lisp-object-encoder"
                 #+:USING-ECC-CRYPTO "core-crypto"
                 ))

