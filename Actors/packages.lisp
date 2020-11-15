;; packages.lisp - Actors packages
;;
;; DM/RAL  12/17
;; -------------------------------------------------------------------
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


(in-package :CL-USER)

#-lispworks
(defpackage #:ansi-timer
  (:use #:common-lisp)
  (:export
   #:timer
   #:make-timer
   #:schedule-timer
   #:schedule-timer-relative
   #:unschedule-timer
   ))

(defpackage #:actor-internal-message
  (:use #:common-lisp)
  (:export
   #:continuation
   #:ask

   #:forwarding-send
   #:forwarding-ask
   #:forwarding-reply
   #:no-service

   #:discard
   #:frag
   #:last-frag
   #:srp-node-id
   #:srp-phase2
   #:srp-phase2-reply
   #:srp-phase3
   #:incoming-msg

   #:rd-incoming
   #:rd-error

   #:wr-fail
   #:wr-done

   #:client-info
   #:server-info
   ))

(defpackage #:actors.directory
  (:use #:common-lisp)
  (:export
   #:clear-directory
   #:register-actor
   #:unregister-actor
   #:get-actors
   #:get-actor-names
   #:find-actor
   #:find-names-for-actor
   ))

(defpackage #:actors.lfm
  (:use #:common-lisp)
  (:export
   #:lfm
   #:log-info
   #:log-warning
   #:log-error
   #:set-printer
   #:set-stream
   #:ensure-system-logger
   ))

(defpackage :actors.network
  (:use #:common-lisp)
  (:export
   #:*default-port*
   #:*socket-timeout-period*
   #:socket-send
   #:open-connection
   #:start-tcp-server
   #:terminate-server
   #:*default-port*
   #:client-request-negotiation
   #:intf-srp-ph2-begin
   #:intf-srp-ph2-reply
   #:intf-srp-ph3-begin
   ))

(defpackage :actors.security
  (:use #:common-lisp)
  (:export
   #:secure-encoding
   #:secure-decoding
   #:insecure-decoding
   #:client-crypto
   #:server-crypto
   #:unexpected
   #:make-u8-vector
   #:convert-vector-to-integer
   #:+MAX-FRAGMENT-SIZE+
   #:server-negotiate-security
   #:client-negotiate-security
   ))
   
(defpackage :actors.bridge
  (:use #:common-lisp)
  (:export
   #:actor-bridge
   #:ip-dest
   #:make-ip-dest
   #:bridge-register
   #:bridge-unregister
   #:bridge-forward-message
   #:bridge-ask-query
   #:bridge-handle-reply
   #:bridge-reset
   ))

(defpackage #:actors.executives
  (:use #:common-lisp)
  (:export
   #:terminate-actor
   #:set-heartbeat-interval
   #:get-heartbeat-interval
   #:set-maximum-age
   #:get-maximum-age
   #:set-executive-pool
   #:get-nbr-execs
   #:kill-executives
   #:add-to-ready-queue
   #:without-watchdog
   #:*watchdog-hook*
   #:default-watchdog-function
   ))

(defpackage #:actors.par
  (:use #:common-lisp)
  (:export
   #:pmapcar
   #:par
   #:parlet
   #:=non-blocking
   ))

(defpackage #:actors.base
  (:use #:common-lisp #:cps
   #-:LISPWORKS
   #:ansi-timer)
  (:import-from #:useful-macros
   #:curry
   #:rcurry
   #:if-let
   #:when-let
   #:nlet-tail
   #:dlambda*
   #:dcase*
   #:defmonitor
   #:critical-section
   #:capture-ans-or-exn
   #:call-capturing-ans-or-exn
   #:recover-ans-or-exn
   #:rmw)
  (:import-from #:actors.executives
   #:add-to-ready-queue)
  (:import-from #:timeout
   #:timeout
   #:*timeout*)
  (:export
   #:send
   #:ask
   #:=ask
   #:get-property
   #:set-property
   #:remove-property
   #:current-actor
   #:self
   
   #:<runnable>
   #:actor-busy
   #:worker
   #:worker-dispatch-wrapper
   #:actor
   #:actor-properties-ref
   #:actor-mailbox
   #:actor-user-fn
   #:limited-actor
   #:actor-as-worker
   #:limited-actor-as-worker
   #:make-actor
   #:make-limited-actor

   #:%run-actor

   #:become
   #:self-call
   #:in-ask-p
   #:whole-message
   #:dispatch-message

   #:spawn
   #:spawn-limited
   #:spawn-worker
   #:spawn-actor-as-worker
   #:spawn-limited-actor-as-worker

   #:invalid-send-target

   #:schedule-after
   #:recv
   #:recv-match
   #:retry-recv
   
   #:inject
   #:exec
   #:perform-in-actor
   #:inject-into-actor
   #:query-actor
   #:with-as-current-actor
   #:hoare-monitor

   #:assemble-ask-message

   #:=async
   #:=async/err

   #:prt
   #:pr
   ))

(defpackage #:actors
  (:use #:common-lisp
   #:cps
   #:actors.base
   #:actors.network
   #:actors.directory
   #:actors.par
   #:actors.executives
   #:actors.lfm)
  (:nicknames #:ac)
  (:import-from #:timeout
   #:timeout
   #:*timeout*)
  (:export
   #:send
   #:ask
   #:=ask
   #:get-property
   #:set-property
   #:remove-property
   #:current-actor
   #:self
   
   #:<runnable>
   #:actor-busy
   #:worker
   #:worker-dispatch-wrapper
   #:actor
   #:actor-properties-ref
   #:actor-mailbox
   #:actor-user-fn
   #:limited-actor
   #:actor-as-worker
   #:limited-actor-as-worker
   #:make-actor
   #:make-limited-actor

   #:become
   #:self-call
   #:in-ask-p
   #:whole-message
   #:dispatch-message

   #:spawn
   #:spawn-limited
   #:spawn-worker
   #:spawn-actor-as-worker
   #:spawn-limited-actor-as-worker

   #:invalid-send-target

   #:schedule-after
   #:recv
   #:recv-match
   #:retry-recv
   
   #:inject
   #:exec
   #:perform-in-actor
   #:inject-into-actor
   #:query-actor
   #:with-as-current-actor
   #:hoare-monitor

   #:clear-directory
   #:register-actor
   #:unregister-actor
   #:find-actor
   #:get-actors
   #:get-actor-names
   #:find-names-for-actor
   
   #:terminate-actor

   #:timeout
   #:*timeout*

   #:par
   #:pmapcar
   #:parlet
   #:=non-blocking
   
   #:set-maximum-age
   #:get-maximum-age
   #:set-heartbeat-interval
   #:get-heartbeat-interval
   #:get-nbr-execs
   #:set-executive-pool

   #:default-watchdog-function
   #:*watchdog-hook*
   #:without-watchdog
   
   #:kill-executives

   #:schedule-after

   #:lfm
   #:log-info
   #:log-warning
   #:log-error
   #:set-printer
   #:set-stream

   #:start-tcp-server
   #:terminate-server

   #:subscribe
   #:unsubscribe
   #:notify
   #:=subscribe
   #:=unsubscribe

   #:=async
   #:=async/err

   #:prt
   #:pr
   ))

(loop for sym being the external-symbols of (find-package :cps) do
  (import sym :actors)
  (export sym :actors))

