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

;; Let's see how little we actually need before we can bring up core Actors...
(asdf:defsystem "com.ral.useful-macros"
  :description "useful-macros: a collection of widely useful macros and functions"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "eval-always")
                (:file "def-extensions")

                #+:CLOZURE (:file "clozure-compat")
  	        #+:SBCL    (:file "sbcl-compat")
		#+:ALLEGRO (:file "allegro-compat")
                
                (:file "sharp-f")
                (:file "stub-functions")
                (:file "basic-useful")

                (:file "nbr-cpus")

                ;; (:file "freev")
                ;; (:file "package-aliases")

                (:file "nlet")
                (:file "sharp-quasiquote-reader")
                (:file "bang-macros")
                (:file "ppcre-reader")
                (:file "reader-macros")
                (:file "list-match")
                #+(AND :COM.RAL :LISPWORKS) (:file "ctypes")
                (:file "useful-macros")
                #+:LISPWORKS (:file "editor-extensions")
                (:file "timeout")
		(:file "usec")
                #+(AND :LISPWORKS :MACOSX) (:file "OSX-UUID-Generate")
                #+(AND :ALLEGRO :MACOSX)   (:file "OSX-UUID-Generate-Allegro")
                #-(OR (AND :MACOSX :LISPWORKS)
                      (AND :MACOSX :ALLEGRO)) (:file "OSX-UUID-Generate"))
  :serial       t
  :depends-on   ("optima"
                 "cl-ppcre"
                 "alexandria"
                 "cffi"
                 "com.ral.mpcompat"
                 ))

;; Actors can be invoked after first loading "useful-macros"

(asdf:defsystem "com.ral.useful-macros/ext"
  :description "useful-macros/extensions: a collection of widely useful macros and functions"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "dlambder")
                (:file "bb")
                
                (:file "encstr")
                (:file "rmw-v2")
                (:file "capture")
                ;; (:file "scraps")
                (:file "pandoric")
                (:file "typed-fun")
                ;; (:file "monads")
                (:file "critical-section")
                ;; (:file "dispatch-queues") ;; what do we need these for?
                
                ;; (:file "useful-macros-old")
                ;; (:file "match-macro")

                ;; these match-macro-ex were the ones in use before optima
                ;; (:file "match-macro-ex")
                ;; (:file "match-macro-ex-opt")

                ;; (:file "match-macro-ex3")
                ;; (:file "monitor-macros")
                ;; (:file "lazy") ;; supplanted by a better, simpler, version
                #+:LISPWORKS (:file "underscore")
               	(:file "uuid")
                ;; (:file "xfli")
		;; (:file "rubber-objects")
                

                #+:LISPWORKS (:file "abbrev-bignums")

                (:file "sep")
                (:file "handlers")
                #+:LISPWORKS (:file "defalias")
                (:file "wordlist")
                (:file "call-fwd")

                #+:LISPWORKS (:file "fixmes")
                #+:LISPWORKS (:file "safe-streams")
                #+:LISPWORKS (:file "safe-read-patch")
                (:file "safe-read-from-string")
		#+(AND :LISPWORKS :MACOSX) (:file "objc")

                #+:LISPWORKS (:file "my-complete-symbol") ;; fix problem in LW for hierarchical package support

                #+:LISPWORKS (:file "fpctl")
                (:file "encaps-type")
                (:file "clc")
		(:file "comprehensions")
                (:file "ffs")
                (:file "engfmt")
                (:file "memoize")
                #-:ALLEGRO (:file "cache")
                #+:WIN32 (:file "exec")
                (:file "computed-metaclass")
                (:file "dynamic-wind")
                (:file "lazy-v2") ;; not supplanted by Actors
                #+(AND :COM.RAL :LISPWORKS) (:file "remembered-filenames")
                #+(AND :COM.RAL :LISPWORKS) (:file "lexb4")
                #+(AND :COM.RAL :LISPWORKS) (:file "safe-call-system")
                #-:relative-package-names (:file "hierarchical-packages")
                (:file "dflet")
                )
  :serial       t
  :depends-on   ("com.ral.useful-macros"
                 "ironclad"
                 "safe-read" ;; thanks Michal!
                 ))

