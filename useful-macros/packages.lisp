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

(in-package "CL-USER")

(defpackage #:com.ral.useful-macros
  (:use #:common-lisp)
  #+:LISPWORKS
  (:import-from #:lispworks
   #:true
   #:false
   #:do-nothing
   #:nconcf
   #:removef
   #:appendf
   #:if-let
   #:when-let
   #:when-let*
   #:with-unique-names
   #:rebinding
   #:whitespace-char-p
   #:push-end)

  (:import-from #:alexandria
   #:curry
   #:rcurry
   #:compose)

  (:export
   #:match
   #:match-fail)
  (:export

   #:firsts
   #:seconds
   #:thirds
   
   #:*timeout*
   #:timeout
   #:with-timeout
   
   #:defconstant+

   #:lc
   #:def-typed-fn

   #|
   #:with-lexical-closures
   #:llet
   #:llet*
   #:lexical
   |#
   
   #:test-special
   #:?specials
   #:ensure-lexical-bindings
   #:mp-lambda
   #:lexical
   #:ensure-lexical
   #:mp-labels
   #:mp-flet
   
   #:single-eval
   #:accum
   #:circular-list-p
   
   #:mv-constantly
   #:pairs
   #:triples
   #:tuples
   
   #:nlet-cps
   #:=values
   #:=bind

   #:<collector>
   #:collector-reset
   #:collector-discard-contents
   #:collector-stuff-contents
   #:collector-nstuff-contents
   #:collector-ncontents
   #:collector-length
   #:collector-empty-p
   #:collector-append1
   #:collector-append
   #:collector-nappend
   #:collector-prepend
   #:collector-nprepend
   #:collector-push
   #:collector-pop

   #:<monitored-object-mixin>
   #:mark-changed
   #:nchanged-p
   
   #:<mpsafe-mixin>
   #:with-locked-instance

   #:make-collector
   #:make-monitored-collector
   #:make-mpsafe-collector
   #:make-mpsafe-monitored-collector

   #:make-list-builder
   #:make-lbs
   #:lbs-reset
   #:lbs-append
   #:lbs-get

   #:stack-probe
   #:safe-call-system ;; LWM bug fix DM/RAL 02/17
   
   #:defmonitor
   #:critical-section
   #:critical-or
   #:with-exclusive-access
   #:with-shared-access
   #:static
   #:let-static
   #|
   :def-excl
   :lambda-excl
   :labels-excl
   :flet-excl
   :def-shared
   :lambda-shared
   :labels-shared
   :flet-shared
   |#

   #:fegetround
   #:fesetround
   #:+fe_tonearest+
   #:+fe_downward+
   #:+fe_upward+
   #:+fe_towardzero+

   #:with
   #:letp
   #:wholepart
   #:fracpart
   #:copy-struct
   #:make-struct-copy
   #:slot-names
   
   #:read-extended-number-syntax
   #:hz-to-nn
   #:nn-to-hz
   #:q-for-bw
   #:bw-for-q
   
   #:symbol-gensym
   #:with-gensyms
   #:allf
   #:nilf
   #:tf
   #:conc1f
   #:addf ;; same as incf
   #:subf
   #:mulf
   #:divf
   #:ashf
   #:appendf
   #:nconcf
   #:deletef
   #:deletef-if
   #:removef
   #:removef-if
   #:aconsf
   #:or-setf
   #:mv-or-setf
   #:ensure-assoc
   #:make-setter
   
   #:while
   #:foreach
   #:until
   #:if-let
   #:when-let
   #:if-let*
   #:when-let*
   
   #:append1
   #:conc1
   #:single
   #:last1
   #:mklist
   #:safe-list-length
   #:longer
   #:filter
   #:filter-map
   #:group
   #:flatten
   #:shallow-flatten
   #:prune
   #:find2
   #:before
   #:after
   #:duplicate
   #:split-if
   #:most
   #:best
   #:mostn
   #:mapn
   #:map0-n
   #:map1-n
   #:mapa-b
   #:map->
   #:mappend
   #:mapcars
   #:rmapcar
   #:readlist
   #:prompt
   #:break-loop
   #:mkstr
   #:raw-mkstr
   #:symb
   #:kwsymb
   #:kwsymbol
   #:reread
   #:explode
   #:correct-for-symbol-character-case
   #:intern-symbol
   
   #:constituent
   #:tokens
   #:tokens-if
   #:tokens-if-not
   #:split-string
   #:paste-strings
   ;; #:getenv
   ;; #:winexec
   #:pickfile
   #:get-time-string
   #:map-from-to
   #:map-from-below
   #:map-from-for
   #:collect->
   #:subselector
   #:where
   #:where-if
   #:where-if-not
   #:subselect
   #:subselect-if
   #:subselect-if-not
   #:row-major-vector
   #:indgen
   #:collect-where
   #:collect-where-not
   #:collect-if
   #:collect-if-not
   #:keep-if
   #:keep-if-not
   #:with-cstring
   #:with-cstrings
   #:in-cstring
   #:out-cstring
   #:uchar
   #:def-enum
   #:ez-define-foreign-function

   #:compose
   #:curry
   #:rcurry
   #:combine
   #:gensyms
   #:currym
   #:rcurrym
   #:named-lambda
   
   #:expanded-compose
   #:expanded-curry
   #:expanded-rcurry
   #:expanded-combine

   #:curried-lambda

   #:eqlcond
   #:foldl
   #:foldr
   #:coerce-fli-arg
   #:def-safe-fli-function

   #:with-tail-pure-code
   #:true
   #:false
   #:do-nothing

   #||#
   ;; lazy eval and once-functions
   #:with-spin
   #:without-spin
   #:deferred
   #:lazy
   #:once-only
   #:once-thereafter
   #:future
   #:unsafe-future
   #:force

   ;; futures based parallel mapping
   #:pmap
   #:npmap
   #:pvmap
   #:npvmap
   #:par
   #||#

   ;; #:safe-function-p

   #:make-range
   #:range

   #:drop
   #:take
   #:lastn
   #:split
   #:zip
   #:interleave

   ;; #:make-once-thereafter

   #:largest-abs-value
   #:max-abs
   #:make-list-reducer

   #:post-incf
   #:post-decf

   #:with-slot-values

   #:fn
   #:if*
   #:alambda
   #:aif
   #:it
   #:aif*
   #:awhen
   #:alet
   #:alet*
   #:alet-fsm
   #:arun-fsm
   #:ichain-before
   #:ichain-after
   #:ichain-intercept
   #:alet-hotpatch
   #:let-hotpatch
   #:let-binding-transform
   #:sublet
   #:sublet*
   #:this  ;; this, sym, val needed to support external extensions of pandoric macros
   #:pinspect
   #:pandoric-body
   #:pandoriclet
   #:pandoriclet-get
   #:pandoriclet-set
   #:get-pandoric
   #:with-pandoric
   #:pandoric-hotpatch
   #:pandoric-recode
   #:plambda
   #:defpan
   #:defpan-method
   #:pandoric-eval
   #:this
   #:penv
   #:make-pandoric-state-machine
   #:defstates

   #:copy-with
   
   #:make-rubber-vector
   #:firsts-of
   #:slice
   #:left-part
   #:right-part

   #:*match-case-sensitive-p*
   #:match
   #:match2
   #:rt-match
   #:match-fail
   #:match-failure
   #:encode-match-body
   #:encode-match-bodies
   #:eql-tree
   
   #:move
   #:pwr2-q
   #:pwr2
   #:unchecked-pwr2
   #:ceiling-pwr2
   #:unchecked-ceiling-pwr2
   #:ceiling-log2
   #:unchecked-ceiling-log2
   #:floor-pwr2
   #:unchecked-floor-pwr2
   #:floor-log2
   #:unchecked-floor-log2
   
   #:align-pwr2
   #:unchecked-align-pwr2
   
   #:format-error
   #:separate-declares-and-documentation
   #:define-monitor
   #:with-monitor
   #:let-monitor
   #:lock-mixin
   #:let-locking

   #:with-slot-accessors
   #:bind*
   #:define-bind*-handler
   #:perform

   #:binsearch

   #:computed-metalevel-class

   #:nif
   #:g!-symbol-p
   #:defmacro/g!
   #:parse-body ;; Alexandria replacement
   #:nlet
   #:o!-symbol-p
   #:o!-symbol-to-g!-symbol
   #:defmacro!
   #:dlambda
   #:dcase
   #:dlambda*
   #:dcase*
   #:tlambda
   #:tcase
   
   #:segment-reader

   #:make-state-machine
   #:run-state-machine
   #:push-state
   #:pop-state

   #:make-coll
   ;; #:collect-decls
   #:dis

   #:fast-progn
   #:safe-progn

   #:pointer-&
   #:pointer-*
   #:with-fast-stack

   #:make-tlist
   #:tlist-left
   #:tlist-right
   #:tlist-empty-p
   #:tlist-add-left
   #:tlist-add-right
   #:tlist-rem-left
   #:tlist-update
   #:with-conses-counted
   #:with-cons-pool
   #:cons-pool-cons
   #:cons-pool-free
   #:make-cons-pool-stack
   #:make-shared-cons-pool-stack
   #:with-dynamic-cons-pools
   #:fill-cons-pool

   #:hhmmss.ss
   #:hms
   #:ddmmyyyy
   #:dmy
   #:yyyymmdd
   #:ymd


   #:memo
   #:memoize
   #:clear-memoize
   #:un-memoize
   #:defun-memo

   #:cache
   #:cacheize
   #:un-cacheize
   
   #:2-way-cache
   #:check-cache
   #:update-cache
   #:clear-cache

   #:2-way-n-level-cache
   #:check-cache
   #:update-cache
   #:clear-cache
   #:clear-cache-row

   #:set-$-dispatch-reader
   #:get-$-dispatch-reader
   #:set-/-dispatch-reader
   #:get-/-dispatch-reader
   #:read-chars-till-delim

   #:alias
   #:unalias
   #:aliases ;; use (setf aliases) to set new aliases
   #:defaliasfn
   #:defcapture

   #:separate-decls-and-body

   #:with-remembered-filename
   #:remember-filename
   #:remembered-filename
   #:filename-timestamp-string
   #:add-timestamp-to-filename
   
   #:defwrapper

   #:with-transient-mutation
   #:make-unique-object
   #:cant-happen
   #:letrec
   #:length=1
   #:starts-with
   #:symbol-name=
   #:symbol-name-equal
   #:xcond
   #:safe-read-from-string
   #:row-type
   #:sum-type
   #:featurep
   #:magic-word
   #:rot-bits
   #:partition

   #|
   ;; internal use names marked with prefix "%"
   #:<dspq>
   #:dspq-add-item
   #:dspq-read
   #:safe-function-p
   #:add-to-dispatch-queue
   #:serial-dispatch-queue
   #:install-service
   #:find-service
   #:make-serial-dispatch-queue
   #:dispatch-serial-sync
   #:dispatch-serial-async
   #:dispatch-parallel-sync
   #:dispatch-parallel-async
   #:wait-for-serial-dispatch
   #:wait-for-parallel-dispatch
   #:wait-for-dispatch-group
   #:with-dispatch-timeout
   #:with-serial-dispatch
   #:with-parallel-dispatch

   #:%wrap-dispatch-group
   #:%with-dispatch-group
   #:%do-serial-dispatch
   #:%do-serial-dispatch-sync
   #:%do-serial-dispatch-async
   #:*dispatch-timeout*
   #:par
   #:par1
   |#

   #:capture-packet
   #:capture-ans-or-exn
   #:call-capturing-ans-or-exn
   #:recover-ans-or-exn

   #:*top-function*
   #:sfloat
   #:dfloat
   #:chktype
   #:chkarg
   #:chkargs

   #:int32-cnot
   #:int32-rot

   #:in-eval-mode-p
   #:ensure-thread-eval
   #:ensure-thread-eval-def

   #:define-rmw-functions

   #:rd
   #:wr
   #:rmw
   #:cas
   #:atomic-exch
   
   #:rd-object
   #:wr-object
   #:rmw-object
   #:cas-object
   #:atomic-exch-object
   
   #:<lockable-mixin>
   #:<abstract-kv>
   #:<plist>
   #:<alist>
   #:<map>
   #:<hash-table>
   #:<shared-plist>
   #:<shared-alist>
   #:<shared-map>
   #:<shared-hash-table>
   #:get-kv
   #:set-kv
   #:iter-kv
   #:ensure-kv
   #:remove-key
   #:set-kvs
   #:merge-kvs

   #:nest
   #:fst
   #:snd

   #:*print-bignum-abbrev*
   #:without-abbrev
   #:abbrev-str

   #:sepi
   #:sepfp

   #|
   #:define
   #:define*
   #:define-macro
   #:define-generic
   #:define-method
   |#
   #:lambda*
   #:flet*
   #:labels*
   #:defun*
   #:λ
   #:µ
   #:∂
   #:parse-body
   #:is-underscore?
   #:is-lambda-list-keyword?
   #:deflex
   
   #:->
   #:_>
   #:=>
   
   #:read-mailbox-with-timeout
   #:eval-always
   #:->>

   #:handler-bind*
   #:handler-case*
   #:handler-bind-case
   #:call-with-handler
   #:call-with-restart

   #:get-number-of-processors

   #:dynamic-wind
   #:proceed
   #:capture-dynamic-environment
   #:call-with-dynamic-environment
   #:with-dynamic-environment

   #:doseq
   #:vbind
   #:vbind*
   #:with-velems

   #:utf-8-encoding
   #:utf-8-code-chars
   #:encstr
   #:sbs
   #:ucs
   #:dostring
   #:dovector

   #:defalias
   #:redirect

   #:convert-int-to-wordlist
   #:convert-wordlist-to-int

   #:cx-dspec-def

   #:with-fwd
   #:call-fwd
   #:apply-fwd

   #:within

   #:defstub
   #:stub-function-p
   #:with-fast-impl
   #:use-slow-code
   #:reset-error

   #:ash-dpb
   #:ash-dpbf

   #:check/lock/check

   #:make-encapsulated-type

   #:addnew-to-plist
   #:reapply

   #:merge-alist
   #:merge-plist
   #:string-interp

   #+(AND :LISPWORKS :MACOSX) #:st-to-objc
   #+(AND :LISPWORKS :MACOSX) #:objc-invoke-st

   #:with-unique-names
   #:rebinding
   #:whitespace-char-p
   #:push-end

   #:mappings
   #:map-filename
   ))
