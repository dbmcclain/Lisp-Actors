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

(asdf:defsystem "data-objects"
  :description "data-objects: a collection of widely useful data types"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "orderable")
                (:file "ref-v4")
                (:file "ord")
                (:file "bankers-queue")
                (:file "finger-tree")
                (:file "bal-binary-trees")
                ;; (:file "bal-binary-trees-cps")
                (:file "bal-binary-tree-maps")
                (:file "prio-queue-lf")
                (:file "resource")
                (:file "locked-resource")
                ;; (:file "mi-rubber-objects-2")
		;; (:file "rubber-objects-maps")
                #+:LISPWORKS (:file "rubber-objects-lock-free")
                #+(AND :COM.RAL :LISPWORKS) (:file "mcas-v4")
                #+:LISPWORKS (:file "multilock")
                #+:LISPWORKS (:file "tl2")
                #+(AND :COM.RAL :LISPWORKS) (:file "fstm-v4")
                #+(AND :COM.RAL :LISPWORKS) (:file "lf-bag")
                #+(AND :COM.RAL :LISPWORKS) (:file "lw-rwgate")
                #+(AND :COM.RAL :LISPWORKS) (:file "progress-bar")
                #+(AND :COM.RAL :LISPWORKS) (:file "debug-stream")
                #+(AND :COM.RAL :LISPWORKS) (:file "collector2")
                (:file "interval-trees")
                (:file "zorder-maps")
                (:file "rpsx")
                (:file "btree-clos")
                (:file "memory-btrees-clos")
                (:file "trie")
		#+(AND :COM.RAL :LISPWORKS) (:file "protocols")
                )
  :serial t
  :depends-on   ("useful-macros"
                 "cps"
                 "mpcompat"
                 "optima"
                 "trivia"
                 ))


