;; rb-packages.lisp
;; DM/RAL  02/09
;; ------------------------------------------------------------------
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

(defpackage #:com.ral.rb-trees.fwds
  (:use #:common-lisp)
  (:export
   #:compare
   ))

(defpackage #:com.ral.orderable
  (:use #:common-lisp)
  (:local-nicknames
   (#:ord        #:com.ral.rb-trees.fwds)
   (#:mpcompat   #:com.ral.mpcompat))
  (:export
   #:<orderable-mixin>
   #:order-id
   ))

(defpackage #:com.ral.ord
  (:use #:common-lisp)
  (:local-nicknames
   (#:um   #:com.ral.useful-macros)
   (#:uuid #:com.ral.uuid))
  (:shadow #:equal)
  (:import-from #:com.ral.orderable
   #:<orderable-mixin>
   #:order-id)
  (:import-from #:com.ral.rb-trees.fwds
   #:compare)
  (:export
   #:compare
   #:compare<
   #:compare<=
   #:compare=
   #:compare>=
   #:compare>
   #:minval
   #:maxval
   #:make-ci-char
   #:make-ci-string
   #:equal
   #:less
   #:greater
   ))

(defpackage #:com.ral.rb-trees.sets
  (:use #:common-lisp)
  (:local-nicknames
   (#:ord   #:com.ral.ord)
   (#:sets  #:com.ral.rb-trees.sets)
   (#:um    #:com.ral.useful-macros))
  (:shadow #:remove #:union #:intersection #:every #:some)
  #+:LISPWORKS
  (:import-from #:lw
   #:false
   #:true)
  #+nil
  (:import-from #:cps
   #:with-cont
   #:=defun
   #:=values
   #:=bind
   #:=nlet
   #:=tlet
   #:trampoline)
  (:export
   #:make-shared-set
   #:make-unshared-set
   #:erase
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:tree
   #:empty
   #:node
   #:is-empty
   #:singleton
   #:height
   #:mem
   #:add
   #:addf
   #:with-replacement
   #:without-replacement
   #:with-replacement-p
   #:remove
   #:removef
   #:remove-min-elt
   #:remove-max-elt
   #:union
   #:intersection
   #:diff
   #:subset
   #:iter
   #:fold
   #:every
   #:some
   #:filter
   #:split
   #:partition
   #:cardinal
   #:elements
   #:min-elt
   #:max-elt
   #:choose
   #:view-set

   ;; privately exported for derivative packages
   #:with-node-bindings
   #:with-list-bindings
   #:key-fn
   ))

(defpackage #:com.ral.rb-trees.maps
  (:use #:common-lisp)
  (:local-nicknames
   (#:ord   #:com.ral.ord)
   (#:sets  #:com.ral.rb-trees.sets)
   (#:maps  #:com.ral.rb-trees.maps)
   (#:um    #:com.ral.useful-macros))
  (:shadow #:find #:map)
  (:import-from #:com.ral.rb-trees.fwds
   #:compare)
  (:import-from #:com.ral.rb-trees.sets
   #:tree
   #:empty
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:erase
   #:singleton
   #:is-empty
   #:mem
   #:diff
   #:cardinal
   #:view-set
   #:with-node-bindings
   #:key-fn
   #:removef)
  (:shadowing-import-from #:com.ral.rb-trees.sets
   #:remove
   #:union
   #:intersection)
  (:export
   #:make-shared-map
   #:make-unshared-map
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:erase
   #:map-cell
   #:map-cell-key
   #:map-cell-val
   #:empty
   #:singleton
   #:is-empty
   #:add
   #:addf
   #:find
   #:remove
   #:removef
   #:mem
   #:diff
   #:intersection
   #:union
   #:iter
   #:map
   #:mapi
   #:fold
   #:cardinal
   #:view-set
   #:with-node-bindings
   #:add-plist
   #:add-alist
   #:add-hashtable
   #:add-keys-vals
   ))

