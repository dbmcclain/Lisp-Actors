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

(asdf:defsystem "useful-macros"
  :description "useful-macros: a collection of widely useful macros and functions"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  (#+:CLOZURE (:file "clozure-compat")
  	        #+:SBCL    (:file "sbcl-compat")
		#+:ALLEGRO (:file "allegro-compat")
                #+:LISPWORKS (:file "my-complete-symbol") ;; fix problem in LW for hierarchical package support
                (:file "packages")
                ;; (:file "lambda-parsing")
                (:file "nbr-cpus")
                (:file "eval-always")
                (:file "dflet")
                (:file "xlambda")
                (:file "timeout")
                (:file "dynamic-wind")
		(:file "comprehensions")
                #+(AND :COM.RAL :LISPWORKS) (:file "lexb4")
                ;; (:file "freev")
                #+(AND :COM.RAL :LISPWORKS) (:file "safe-call-system")
                (:file "hierarchical-packages")
                (:file "package-aliases")
                (:file "basic-useful")
                (:file "sharp-quasiquote-reader")
                (:file "bang-macros")
                (:file "ppcre-reader")
                (:file "reader-macros")
                (:file "safe-read-from-string")
                #+(AND :COM.RAL :LISPWORKS) (:file "ctypes")
                (:file "dlambder")
                (:file "bb")
                (:file "useful-macros")
                (:file "rmw")
                (:file "capture")
                ;; (:file "scraps")
                (:file "pandoric")
                (:file "typed-fun")
                ;; (:file "monads")
                (:file "critical-section")
                ;; (:file "dispatch-queues") ;; what do we need these for?
                
                (:file "lazy-v2") ;; not supplanted by Actors
                
                #+(AND :COM.RAL :LISPWORKS) (:file "remembered-filenames")
                ;; (:file "useful-macros-old")
                ;; (:file "match-macro")

                ;; these match-macro-ex were the ones in use before optima
                ;; (:file "match-macro-ex")
                ;; (:file "match-macro-ex-opt")

                ;; (:file "match-macro-ex3")
                ;; (:file "monitor-macros")
                (:file "memoize")
                #-:ALLEGRO (:file "cache")
                #+:WIN32 (:file "exec")
                ;; (:file "lazy") ;; supplanted by a better, simpler, version
                (:file "engfmt")
		(:file "usec")
               	(:file "uuid")
                (:file "computed-metaclass")
                #+(AND :LISPWORKS :MACOSX) (:file "OSX-UUID-Generate")
                #+(AND :ALLEGRO :MACOSX)   (:file "OSX-UUID-Generate-Allegro")
                #-(OR (AND :MACOSX :LISPWORKS)
                      (AND :MACOSX :ALLEGRO)) (:file "OSX-UUID-Generate")
                ;; (:file "xfli")
		;; (:file "rubber-objects")
                (:file "abbrev-bignums")
                (:file "sep")
                (:file "handlers")
                )
  :serial       t
  :depends-on   (#| "compiled-ml-matcher" |#
                 "optima"
                 "cl-ppcre"
                 "ironclad"
                 "alexandria"
                 "cffi"
                 ))

