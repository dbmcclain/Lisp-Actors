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

(defpackage #:com.ral.rb-trees
  (:use #:common-lisp)
  (:shadow #:remove #:union #:intersection #:every #:some #:copy-tree #:set #:map)
  (:export
   #:/eql
   #:tree-type
   #:tree-type-compare-fn
   #:tree-type-replace-p-fn
   #:make-tree-type
   #:tree
   #:tree-nodes
   #:make-tree
   #:make-tree-like
   #:set
   #:map
   #:empty
   #:node
   #:node-p
   #:is-empty
   #:height
   #:mem
   #:add
   #:addf
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
   #:view-tree
   #:with-node-bindings
   #:with-list-bindings
   ))

(defpackage #:com.ral.rb-trees.sets
  (:use #:common-lisp)
  (:shadowing-import-from #:com.ral.rb-trees
   #:remove #:union #:intersection #:every #:some #:set
   #:some #:every)
  (:import-from #:com.ral.rb-trees
   #:/eql
   #:make-tree-type
   #:tree-type
   #:tree-type-compare-fn
   #:make-tree-like
   #:empty
   #:node
   #:node-p
   #:is-empty
   #:height
   #:mem
   #:removef
   #:remove-min-elt
   #:remove-max-elt
   #:diff
   #:subset
   #:iter
   #:fold
   #:filter
   #:partition
   #:cardinal
   #:elements
   #:min-elt
   #:max-elt
   #:choose)
  (:export
   #:/eql
   #:set
   #:set-type
   #:make-set-type
   #:set-type-compare-fn
   #:make-set
   #:make-set-like
   #:make-shared-set
   #:make-unshared-set
   #:erase
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:empty
   #:is-empty
   #:height
   #:mem
   #:add
   #:addf
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
   ))

(defpackage #:com.ral.rb-trees.maps
  (:use #:common-lisp)
  (:shadow #:find)
  (:shadowing-import-from #:com.ral.rb-trees
   #:remove #:union #:intersection #:every #:some #:map
   #:some #:every)
  (:import-from #:com.ral.rb-trees
   #:/eql
   #:make-tree-type
   #:tree-type-compare-fn
   #:tree-type-replace-p-fn
   #:make-tree-like
   #:tree-type
   #:empty
   #:is-empty
   #:height
   #:mem
   #:removef
   #:remove-min-elt
   #:remove-max-elt
   #:diff
   #:subset
   #:iter
   #:fold
   #:filter
   #:partition
   #:cardinal
   #:elements
   #:min-elt
   #:max-elt
   #:choose)
  (:export
   #:/eql
   #:map
   #:map-type
   #:make-map-type
   #:map-type-compare-fn
   #:map-type-replace-p-fn
   #:make-map
   #:make-map-like
   #:copy
   #:copy-as-shared
   #:copy-as-unshared
   #:erase
   #:empty
   #:is-empty
   #:add
   #:addf
   #:find
   #:remove
   #:removef
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
   #:view-map
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
 (#:trees #:com.ral.rb-trees)
 (#:rbht  #:com.ral.rb-trees.hashtable)
 (#:pfht  #:rbht))

