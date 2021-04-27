;; packages.lisp
;; DM/RAL  02/09
;; ------------------------------------------------------------------

(in-package :user)

(defpackage #:actors-globals
  (:use #:common-lisp
   #:priq)
  (:export
   #:+nbr-execs+
   #:+max-pool+
   #:+heartbeat-interval+
   #:+maximum-age+

   #:*executive-processes*
   #:*executive-counter*
   #:*heartbeat-timer*
   #:*last-heartbeat*
   #:*current-actor*
   #:*actor-ready-queue*
   #:*actor-directory-manager*
   #:*shared-printer-actor*
   
   #:current-actor
   #:blind-print
   ))

(defpackage #:actors-macros
  (:use #:common-lisp
   #:actors-globals)
  (:export
   #:gensym-like
   #:anaphor
   #:make-lock
   #:with-lock
   #:split-bindings
   #:in-eval-mode-p
   #:self-visible-p
   #:ensure-self-binding
   #:ensure-thread-eval
   #:def-alias))

(defpackage #:actors-base
  (:use #:common-lisp
   #:ref
   #:priq
   #:actors-macros
   #:actors-globals)
  (:import-from #:useful-macros
   #:defmacro!
   #:this
   #:pandoriclet
   #:with-pandoric
   #:defpan
   #:pandoric-hotpatch
   #:pandoric-recode
   #:curry
   #:rcurry
   #:dlambda
   #:when-let
   #:if-let
   #:nlet-tail)
  (:import-from #:optima
   #:match
   #:ematch)
  (:import-from #:optima.extra
   #:lambda-match)
  (:export
   #:*current-actor*
   #:exec-actor
   #:current-actor
   #:def-factory
   #:make-actor
   #:spawn
   
   #:become
   #:revert
   
   #:kill-executives

   #:send
   #:ask
   #:actor-alive-p
   #:find-actor
   #:find-actor-name
   #:get-actors
   #:pr

   #:with-actor-values
   #:actor-behavior
   #:with-locked-actor
   #:ensure-executives

   #:actor
   #:actor-priority
   #:actor-behavior
   #:actor-messages
   #:actor-running-p-ref
   #:actor-properties
   #:actor-lambda-list
   #:has-messages-p
   #:has-behavior-p
   
   #:priority-send
   
   #:get-actor-property
   #:set-actor-property
   #:set-actor-priority
   
   #:register-actor
   #:unregister-actor

   #:add-to-ready-queue
   #:ready-queue-empty-p
   #:empty-ready-queue
   #:pop-ready-queue

   #:make-rpc-query

   #:defmacro!
   #:defpan
   #:with-pandoric
   #:pandoriclet
   #:pandoric-hotpatch
   #:pandoric-recode
   #:dlambda
   #:match
   #:ematch
   #:lambda-match
   #:when-let
   #:if-let
   #:curry
   #:rcurry
   #:nlet-tail
   ))

(defpackage #:actors-data-structs
  (:use #:common-lisp #:actors-base)
  (:export
   #:make-shared-queue
   #:make-shared-stack
   #:make-shared-map
   #:make-shared-set
   #:make-shared-hash-table
   ))

(defpackage #:actors-components
  (:use #:common-lisp #:actors-base)
  (:export
   #:broadcast
   #:make-timestamper
   #:make-tee
   #:make-splay
   #:make-partitioner
   ))

(defpackage #:actors-machines
  (:use #:common-lisp
   #:priq
   #:actors-macros
   #:actors-base)
  (:export
   #:schedule-timeout
   #:unschedule-timeout
   #:recv
   #:become-recv
   #:make-state-machine
   ))

(defpackage #:actors
  (:use #:common-lisp
   #:actors-macros
   #:actors-globals
   #:actors-base
   #:actors-data-structs
   #:actors-components
   #:actors-machines)
  (:nicknames #:ac)
  (:export
   #:actor
   #:*current-actor*
   #:exec-actor
   #:current-actor

   #:def-factory
   #:make-actor
   #:spawn
   
   #:become
   #:revert
   
   #:kill-executives
   #:send
   #:ask
   #:priority-send
   #:actor-alive-p
   #:find-actor
   #:get-actors
   
   #:make-shared-queue
   #:make-shared-stack
   #:make-shared-map
   #:make-shared-set
   #:make-shared-hash-table

   #:broadcast
   #:make-timestamper
   #:make-tee
   #:make-splay
   #:make-partitioner

   #:schedule-timeout
   #:unschedule-timeout

   #:recv
   #:become-recv
   #:make-state-machine

   #:make-rpc-query
   
   #:set-actor-priority
   #:get-actor-property
   #:set-actor-property
   #:actor-lambda-list
   
   #:pr

   #:register-actor
   #:unregister-actor

   #:defpan
   #:dlambda
   #:match
   #:ematch
   #:lambda-match
   #:when-let
   #:if-let
   #:curry
   #:rcurry
   #:nlet-tail
   ))

(defpackage #:actors-user
  (:use #:common-lisp #:actors)
  (:export
   ))
