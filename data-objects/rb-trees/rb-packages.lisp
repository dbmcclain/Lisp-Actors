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

(defpackage #:com.ral.ord
  (:use #:common-lisp)
  (:export
   #:compare
   #:compare<
   #:compare<=
   #:compare=
   #:compare>=
   #:compare>
   #:minval
   #:maxval
   #:ci-char
   #:ci-string
   #:make-ci-char
   #:make-ci-string
   #:equals
   #:less
   #:greater
   ))

(defpackage #:com.ral.orderable
  (:use #:common-lisp)
  (:import-from #:com.ral.ord
   #:compare)
  (:export
   #:<orderable-mixin>
   #:order-id
   ))

(defpackage #:com.ral.rb-trees.sets
  (:use #:common-lisp)
  (:shadow #:remove #:union #:intersection #:every #:some #:copy-tree)
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
   #:/eql
   #:make-shared-set
   #:make-unshared-set
   #:erase
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:tree
   #:tree-type
   #:tree-type-compare-fn
   #:tree-type-replace-p-fn
   #:tree-nodes
   #:make-tree
   #:make-tree-type
   #:empty
   #:node
   #:node-p
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
   #:partition
   #:cardinal
   #:elements
   #:min-elt
   #:max-elt
   #:choose
   #:view-set

   #:UD
   #:UE
   #:UE?
   #:SD
   #:SE
   #:SE?
   
   ;; privately exported for derivative packages
   #:with-node-bindings
   #:with-list-bindings
   #:key-fn
   ))

(defpackage #:com.ral.rb-trees.maps
  (:use #:common-lisp)
  (:shadow #:find #:map)
  (:import-from #:ord ;; #:com.ral.ord
   #:compare)
  (:import-from #:sets ;; #:com.ral.rb-trees.sets
   #:/eql
   #:tree
   #:tree-type
   #:tree-type-compare-fn
   #:tree-type-replace-p-fn
   #:tree-nodes
   #:make-tree
   #:make-tree-type
   #:empty
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:erase
   #:singleton
   #:add
   #:addf
   #:mem
   #:diff
   #:cardinal
   #:iter
   #:fold
   #:filter
   #:partition
   #:elements
   #:view-set
   #:with-node-bindings
   #:key-fn
   #:is-empty
   #:removef)
  (:shadowing-import-from #:sets ;; #:com.ral.rb-trees.sets
   #:remove
   #:union
   #:intersection
   #:every
   #:some)
  (:export
   #:/eql
   #:tree
   #:tree-type
   #:tree-type-compare-fn
   #:tree-type-replace-p-fn
   #:tree-nodes
   #:make-tree
   #:make-tree-type
   #:make-shared-map
   #:make-unshared-map
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:erase
   #:map-cell
   #:map-cell-p
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
   #:filter
   #:partition
   #:elements
   #:every
   #:some
   #:cardinal
   #:view-set
   #:with-node-bindings
   #:add-plist
   #:add-alist
   #:add-hashtable
   #:add-keys-vals

   #:UD
   #:UE
   #:UE?
   #:SD
   #:SE
   #:SE?
))

(defpackage #:com.ral.rb-trees.hashtable
  (:use #:common-lisp)
  (:shadow
   #:hash-table
   #:make-hash-table
   #:hash-table-p
   #:hash-table-test
   #:gethash
   #:remhash
   #:clrhash
   #:maphash)
  (:export
   #:hash-table
   #:make-hash-table
   #:hash-table-p
   #:hash-table-test
   #:gethash
   #:remhash
   #:clrhash
   #:maphash
   #:add
   #:addf
   #:trim
   #:trimf
   #:stats
   ))

(project:defproject
 (#:rbht #:com.ral.rb-trees.hashtable)
 (#:pfht #:rbht))

