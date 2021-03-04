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

(asdf:defsystem "actors"
  :description "Actors Multiplexed on an OS Thread Pool"
  :version     "2.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2017 by Refined Audiometrics Laboratory, LLC. MIT License terms apply."
  :components  ((:file "packages")
                (:file "adapters")
                #-:lispworks (:file "ansi-timer")
                (:file "actors")
                (:file "actor-class")
                (:file "executives")
                ;; (:file "erl-proc")
                (:file "par-exec")
                (:file "actors-schedule")
                (:file "actors-machines")
                (:file "actors-futures")
                (:file "actors-directory")
                (:file "actors-lfm")
                (:file "actor-bridge")
                (:file "network-security")
                #+:COM.RAL (:file "srp6-ecc")
                (:file "network-connection")
                (:file "reactor"))
  :SERIAL T
  :depends-on   ("data-objects"
                 "trivia"
		 "mpcompat"
                 "cps"
                 "lisp-object-encoder"
                 #+:COM.RAL "core-crypto"
                 ))

