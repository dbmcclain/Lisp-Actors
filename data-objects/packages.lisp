
;; packages.lisp
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

(defpackage #:orderable
  (:use #:common-lisp)
  (:export
   #:<orderable-mixin>
   #:order-id
   ))

#| ;; for V2
(defpackage #:ref
  (:use #:common-lisp)
  (:export
   #:ref
   #:refp
   #:ref-place
   #:ref-value
   #:basic-ref-value
   #:set-ref-value
   #:basic-set-ref-value
   #:make-ref
   #:copy-ref
   #:cas
   #:basic-cas
   #:atomic-exch
   #:atomic-incf
   #:atomic-decf
   #:ref-cell
   #:cow
   #:cowp
   #:clone
   #:deref
   #:cref
   #:crefp
   #:wref
   #:wrefp
   ))
|#

(defpackage #:ref ;; for V3
  (:use #:common-lisp)
  (:import-from :um
   #:rmw
   #:rd
   #:wr
   #:rd-object
   #:wr-object
   #:rmw-object
   #:cas-object
   #:atomic-exch-object)
  (:export
   #:ref
   #:ref-val
   #:val
   #:wval
   #:ref-p
   #:rd
   #:wr
   #:rmw
   #:rd-object
   #:wr-object
   #:rmw-object
   #:cas-object
   #:atomic-exch-object
   #:cow
   #:cow-p
   #:clone
   #:ielt
   #:islot-value
   #:iaref
   #:alist
   #:alist-lst
   #:tree
   #:tree-lst
   ))

#| ;; for V2
(defpackage #:mcas
  (:use #:common-lisp)
  (:import-from #:orderable
   #:<orderable-mixin>
   #:order-id)
  (:import-from #:ref
   #:ref
   #:ref-value
   #:basic-ref-value
   #:cas
   #:basic-cas
   #:ref-cell)
  (:export
   #:mcas-ref
   #:mcas-ref-p
   #:cas
   #:ref-value
   #:set-ref-value
   #:mcas
   #:mcas-read
   #:make-mcas-ref
   ))
|#

(defpackage #:mcas ;; for V3
  (:use #:common-lisp #:def*)
  (:import-from #:ref
   #:ref
   #:val
   #:ref-val)
  (:import-from #:useful-macros
   #:cas)
  (:export
   #:mcas-ref
   #:mcas-ref-p
   #:mcas
   #:mcas-read
   #:val
   #:cas
   ))

(defpackage :multilock
  (:use #:cl)
  (:import-from #:timeout
   *timeout*
   timeout)
  (:import-from #:orderable
   #:<orderable-mixin>
   #:order-id)
  (:export
   #:multilock
   #:sharing-multilock
   #:make-multilock
   #:make-sharing-multilock
   #:with-multilocks
   #:with-sharing-multilocks
   #:with-exclusive-multilocks
   ))

(defpackage #:priq
  (:use #:common-lisp #:def*)
  #-OPENMCL (:import-from :mpcompat
   :CAS)
  (:export
   #:unsafe-lifo
   #:lifo
   #:unsafe-fifo
   #:fifo
   #:unsafe-priq
   #:priq
   #:mailbox
   #:prio-mailbox

   #:make-unsafe-lifo
   #:make-lifo
   #:make-unsafe-fifo
   #:make-fifo
   #:make-unsafe-priq
   #:make-priq
   #:addq
   #:popq
   #:peekq
   #:emptyq-p
   #:contents
   #:findq
   #:lastq
   #:countq

   #:make-mailbox
   #:make-prio-mailbox
   #:mailbox-send
   #:mailbox-read
   #:mailbox-peek
   #:mailbox-empty-p
   #:mailbox-not-empty-p
   #:mailbox-discard
   
   #:locked-exec
   ))

#|
(defpackage #:dstm
  (:use #:common-lisp)
  (:import-from #:ref
   #:ref
   #:mref
   #:ref-value
   #:set-ref-value
   #:cas)
  (:export
   #:var
   #:make-var
   #:atomic
   #:open-for-read
   #:open-for-write
   #:release-read
   #:abort-transaction
   #:retry
   #:clone
   ))
|#

#| ;; for V2 REF
(defpackage #:fstm
  (:use #:common-lisp)
  (:import-from :orderable
   #:<orderable-mixin>
   #:order-id)
  (:import-from #:ref
   #:ref
   #:refp
   #:ref-cell
   #:ref-value
   #:set-ref-value
   #:basic-ref-value
   #:cas
   #:atomic-incf
   #:cow
   #:copy-ref)
  (:export
   #:var
   #:make-var
   #:atomic
   #:orelse
   #:check
   #:open-for-read
   #:open-for-write
   #:release-read
   #:release-write
   #:retry
   #:var-val
   #:var-ref
   #:validate
   ))
|#

(defpackage #:fstm ;; for V3 REF
  (:use #:common-lisp #:def*)
  (:import-from :orderable
   #:<orderable-mixin>
   #:order-id)
  (:import-from :ref
   #:ref
   #:val
   #:wval
   #:cow)
  (:export
   #:var
   #:atomic
   #:orelse
   #:check
   #:open-for-read
   #:open-for-write
   #:release-read
   #:release-write
   #:retry
   #:ref
   #:val
   #:wval
   #:validate
   ))

(defpackage #:topgui
  (:use #:common-lisp)
  (:export
   #:define-toplevel-app-interface
   #:run-toplevel-app-interface))

#|
(defpackage #:data-objects
  (:use #:common-lisp)
  (:nicknames #:dobj)
  (:export
   #:get-item
   #:put-item
   #:data-available-p
   #:basic-data-object
   #:basic-fifo-queue
   #:basic-lifo-stack
   #:mp-shared-mixin
   #:mp-shared-data-object
   #:mp-shared-fifo-queue
   #:mp-shared-lifo-stack
   ;; #:with-lock
   #:with-locked-access
   #:queue-data
   #:stack-data

   ;; the following needed to overcome bug in hqn 4.1x
   #:my-process-wait
   #:my-process-wait-with-timeout
   #:my-process-lock
   #:my-mailbox-read
   #:my-with-lock))

(defpackage #:queue
  (:use #:common-lisp)
  (:shadow
   #:push
   #:pop
   #:length
   #:delete
   #:delete-if
   #:find
   #:find-if
   #:map
   #:every
   #:some
   #:position
   #:position-if
   #:nth
   #:count
   #:count-if
   #:reduce
   #:member
   #:last
   )
  (:export
   #:queue
   #:create
   #:clear
   #:add
   #:push
   #:peek
   #:top
   #:take
   #:pop
   #:copy
   #:is-empty
   #:not-empty
   #:length
   #:map
   #:iter
   #:fold
   #:transfer
   #:contents
   #:tail
   #:last
   #:delete
   #:delete-if
   #:find
   #:find-if
   #:every
   #:some
   #:list-of
   #:position
   #:position-if
   #:count
   #:count-if
   #:nth
   #:reduce
   #:member
   #:do-queue
   ))

(defpackage #:stack-on-list
  (:use #:common-lisp)
  (:shadow #:push #:pop)
  (:nicknames #:stackl)
  (:export
   #:stack
   #:create
   #:clear
   #:copy
   #:push
   #:top
   #:pop
   #:is-empty
   #:depth
   #:iter
   ))

(defpackage #:stack-on-vector
  (:use #:common-lisp)
  (:shadow #:push #:pop)
  (:nicknames #:stackv)
  (:export
   #:stack
   #:create
   #:clear
   #:copy
   #:push
   #:top
   #:pop
   #:is-empty
   #:depth
   #:iter
   ))

(defpackage #:single-reader-mailbox
  (:use #:common-lisp)
  (:nicknames #:srmb)
  (:import-from #:queue
   #:peek
   #:is-empty
   #:not-empty)
  (:export
   #:mailbox
   #:create
   #:send
   #:receive
   #:peek
   #:is-empty
   #:not-empty
   #:selective-receive
   ))

(defpackage #:com.ral.biqueue
  (:use #:COMMON-LISP)
  (:nicknames #:BIQUEUE)
  (:export
   #:biqueue
   #:enqueue-fore
   #:enqueue-aft
   #:dequeue
   ))

(defpackage #:multiple-reader-mailbox
  (:use #:common-lisp)
  (:nicknames #:mrmb)
  (:import-from #:queue
   #:peek
   #:is-empty
   #:not-empty)
  (:export
   #:mailbox
   #:create
   #:send
   #:receive
   #:peek
   #:is-empty
   #:not-empty
   ))
|#

(defpackage #:btree
  (:use #:common-lisp)
  (:export
   #:node
   #:btree
   
   #:btree-protocol
   #:make-btree

   #:items-count
   #:root-node
   #:compare-fn
   #:key-fn
   #:make-node
   #:discard-node

   #:node-height
   #:node-fill-pointer
   #:node-list-cell
   #:node-capacity
   #:copy-node-list-cells
   #:coerce-to-object
   
   #:first-item
   #:last-item
   #:map-tree
   #:find-item
   #:insert-item
   #:add/update-item
   #:delete-item

   #:create-cursor
   #:cursor-next
   #:cursor-previous

   #:check-cache
   #:update-cache
   #:clear-cache
   #:cache-id

   #:btree-lock
   #:with-locked-btree
   #:get-cache
   ))

(defpackage :memory-btrees
  (:use #:common-lisp)
  (:export
   #:make-btree
   ))

(defpackage #:protocol
  (:use #:common-lisp)
  (:export
   #:define-protocol
   #:implements-protocol
   ))

(defpackage #:rps
  (:use #:common-lisp)
  (:shadow #:signal)
  (:export
   #:with-noticed-mutations
   #:make-noticed-mutable-object
   #:make-ephemeral-cell
   #:value
   #:*environment*
   #:make-environment
   #:add-dependent
   #:remove-dependent
   #:clear-dependents
   #:clear-all-dependents
   #:add-observer
   #:remove-observer
   #:clear-observers
   #:clear-all-observers
   #:register-notification-action
   #:remove-notification-action
   #:clear-notification-actions
   #:notify
   #:enqueue-action
   #:enqueue-after-action
   #:enqueue
   #:enqueue-after
   #:define-monitored-class
   #:noticed-slots-metalevel-class
   #:noticed-slots-root-class
   ))

(defpackage #:ord
  (:use #:common-lisp)
  (:shadow #:equal)
  (:import-from #:orderable
   #:<orderable-mixin>
   #:order-id)
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

(defpackage #:sets
  (:use #:common-lisp)
  (:shadow #:remove #:union #:intersection #:every #:some)
  #+:LISPWORKS
  (:import-from #:lw
   #:false
   #:true)
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

(defpackage #:maps
  (:use #:common-lisp)
  (:shadow #:find #:map)
  (:import-from #:sets
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
  (:shadowing-import-from #:sets
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

#|
(defpackage #:com.ral.priority-queue
  (:use #:common-lisp)
  (:nicknames #:prioq)
  (:export
   #:priority-queue
   #:is-empty
   #:add-item
   #:remove-item

   #:priority-dispatch-queue
   #:priority-dispatch-queue-p
   #:make-priority-dispatch-queue
   #:dispatch-priority-sync
   #:dispatch-priority-async
   #:wait-for-priority-dispatch
   #:with-priority-dispatch

   #:dispatch-priority-evt
   #:wait-for-dispatch-priority-evt
   ))

(defpackage #:dispatch
  (:use #:common-lisp)
  (:nicknames #:dspq)
  (:import-from #:prioq
   #:make-priority-dispatch-queue
   #:dispatch-priority-sync
   #:dispatch-priority-async
   #:wait-for-priority-dispatch
   #:with-priority-dispatch)
  (:import-from #:um
   #:make-serial-dispatch-queue
   #:dispatch-serial-sync
   #:dispatch-serial-async
   #:wait-for-serial-dispatch
   #:with-serial-dispatch
   
   #:dispatch-parallel-sync
   #:dispatch-parallel-async
   #:wait-for-parallel-dispatch
   #:with-parallel-dispatch

   #:with-dispatch-timeout
   #:wait-for-dispatch-group)
  (:export
   #:make-priority-dispatch-queue
   #:dispatch-priority-sync
   #:dispatch-priority-async
   #:wait-for-priority-dispatch
   #:with-priority-dispatch

   #:make-serial-dispatch-queue
   #:dispatch-serial-sync
   #:dispatch-serial-async
   #:wait-for-serial-dispatch
   #:with-serial-dispatch
   
   #:dispatch-parallel-sync
   #:dispatch-parallel-async
   #:wait-for-parallel-dispatch
   #:with-parallel-dispatch

   #:with-dispatch-timeout
   #:wait-for-dispatch-group
   ))
|#

#|
(defpackage #:rb-tree
  (:use #:common-lisp)
  (:shadow #:merge #:equal #:remove #:union #:intersection #:some #:every)
  (:import-from #:sets
   #:*test-fn*
   #:*key-fn*
   #:compare-key-with-node-val
   #:compare-node-vals
   #:with-test-key
   #:with-list-bindings)
  (:export
   #:<tree>
   #:<empty-tree>
   #:<node>
   #:tree-height
   #:node-left
   #:node-val
   #:node-right
   
   #:empty
   #:is-empty
   #:singleton
   #:create
   #:add
   #:join
   #:concat
   #:mem
   #:remove
   #:min-elt
   #:max-elt
   #:remove-min-elt
   #:remove-max-elt
   #:union
   #:intersection
   #:diff
   #:compare
   #:equal
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
   #:choose

   #:view-tree
   ))

(defpackage #:rb-tree-maps
  (:use #:common-lisp)
  (:shadow #:find #:map)
  (:shadowing-import-from #:rb-tree #:equal #:remove)
  (:import-from #:rb-tree
   #:<tree>
   #:empty
   #:singleton
   #:is-empty
   #:view-tree
   #:mem
   #:compare)
  (:export
   #:empty
   #:singleton
   #:is-empty
   #:add
   #:find
   #:remove
   #:mem
   #:iter
   #:map
   #:mapi
   #:fold
   #:compare
   #:equal
   #:view-tree
   ))
|#
#|
(defpackage #:lw6-stm
  (:use #:common-lisp)
  (:nicknames #:lwstm)
  (:export
   #:def-var
   #:def-accessor
   #:def-mutator

   #:with-mutation
   #:with-accessing
   ))
|#

(defpackage #:debug-stream
  (:use #:common-lisp)
  (:nicknames #:dbgstrm #:dbgw)
  (:export
   #:make-debug-stream
   #:debug-print
   #:pr
   #:clear
   #:cls))

(defpackage #:progress-bar
  (:use #:common-lisp)
  (:nicknames #:pbar)
  (:export
   #:with-progress-bar
   #:incr-value
   #:set-value
   #:user-cancel))

#|
(defpackage #:simple-vstm
  (:use #:common-lisp)
  (:nicknames #:svstm)
  (:export
   #:var
   #:make-var
   #:var-val
   #:rmw
   ))
|#

(defpackage #:interval-trees
  (:use #:common-lisp)
  (:nicknames #:itree)
  (:export
   #:find-containing
   ))

(defpackage #:trie
  (:use #:common-lisp)
  (:export
   #:*trie*
   #:make-trie
   #:trie-find
   #:trie-insert
   #:trie-delete
   #:trie-catalog
   #:trie-autocomplete
   #:view-trie
   ))
