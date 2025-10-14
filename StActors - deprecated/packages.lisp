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

(defpackage #:stactors
  (:nicknames #:stac)
  (:use #:common-lisp #:cps)
  #.`(:export
      ,@(loop for sym being the external-symbols of :cps
              collect sym))

  (:import-from #:timeout
   #:timeout
   #:*timeout*)
  (:export
   #:timeout
   #:*timeout*)

  (:export
   #:define-actor-class
   #:send
   #:ask
   #:=ask
   #:get-property
   #:set-property
   #:remove-property
   #:self
   #:self-beh
   
   #:<runnable>
   #:actor-busy
   #:worker
   #:worker-dispatch-wrapper
   #:actor
   #:actor-properties-ref
   #:actor-mailbox
   #:actor-affinity
   #:actor-beh
   #:make-actor
   #:make-limited-actor
   #:make-remote-actor

   #:become
   #:self-call
   #:in-ask-p
   #:whole-message
   #:dispatch-message
   #:repeat-send

   #:spawn
   #:spawn-limited
   #:spawn-worker

   #:invalid-send-target

   #:schedule-after
   #:recv
   #:recv-match
   #:retry-recv
   
   #:inject
   #:exec
   #:query
   #:perform-in-actor
   #:inject-into-actor
   #:query-actor
   #:hoare-monitor

   #:make-actor-directory
   #:clear-directory
   #:register-actor
   #:unregister-actor
   #:directory-find-if
   #:find-actor
   #:get-actors
   #:get-actor-names
   #:find-names-for-actor
   
   #:terminate-actor
   #:terminate-actors

   #:timeout
   #:*timeout*

   #:par
   #:pmapcar
   #:parlet
   #:=non-blocking
   #:par-any
   #:=when-any
   #:=unless-any
   
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

   #|
   #:subscribe
   #:unsubscribe
   #:notify
   #:=subscribe
   #:=unsubscribe
   |#
   
   #:=async
   #:=async/err

   #:prt
   #:pr

   #:usti
   #:usti=
   #:proxy
   #:make-proxy

   #:future
   #:call-future
   #:force
   #:call-forcing
   #:=fwait

   #:send-sync

   #:start

   #:defglobal-var

   #:promise
   #:with-promise
   #:fulfill
   #:realize

   #:get-message-dispatch-handler
   #:self-dispatch

   #:subscribe
   #:notify
   #:unsubscribe

   #:become-remote
   #:become-local
   
   #:watch
   #:unwatch

   #:with-worker
   ))

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

(defpackage #:stactors/internal-message
  (:use #:common-lisp)
  (:export
   #:continuation
   #:ask
   #:send-sync
   #:unwatch
   #:become-local
   ))

(defpackage #:stactors/internal-message/security
  (:use #:common-lisp)
  (:export
   #-:USING-ECC-CRYPTO #:srp-node-id-rsa
   #+:USING-ECC-CRYPTO #:srp-node-id-ecc
   #-:USING-ECC-CRYPTO #:srp-phase2-rsa
   #+:USING-ECC-CRYPTO #:srp-phase2-ecc
   #:srp-phase2-reply
   #:srp-phase3
   ))

(defpackage #:stactors/internal-message/bridge
  (:use #:common-lisp)
  (:export
   #:forwarding-send
   #:forwarding-ask
   #:forwarding-reply
   #:no-service
   ))

(defpackage #:stactors/internal-message/network
  (:use #:common-lisp)
  (:export
   #:discard
   #:frag
   #:last-frag
   #:incoming-msg

   #:rd-incoming
   #:rd-error

   #:wr-fail
   #:wr-done

   #:client-info
   #:server-info
   ))

(defpackage #:stactors/directory
  (:use #:common-lisp #:stactors))

(defpackage #:stactors/lfm
  (:use #:common-lisp #:stactors)
  (:export
   #:ensure-system-logger
   #:kill-system-logger
   ))

(defpackage :stactors/network
  (:use #:common-lisp #:stactors)
  (:export
   #:*default-port*
   #:*socket-timeout-period*
   #:socket-send
   #:open-connection
   #:start-tcp-server
   #:terminate-server
   #:*default-port*
   #-:USING-ECC-CRYPTO #:client-request-negotiation-rsa
   #+:USING-ECC-CRYPTO #:client-request-negotiation-ecc
   #-:USING-ECC-CRYPTO #:intf-srp-ph2-begin-rsa
   #+:USING-ECC-CRYPTO #:intf-srp-ph2-begin-ecc
   #:intf-srp-ph2-reply
   #:intf-srp-ph3-begin
   ))

(defpackage :stactors/security
  (:use #:common-lisp #:stactors)
  (:shadow #:random)
  (:export
   #:$VERSION
   #:random
   #:secure-encoding
   #:secure-decoding
   #:byte-decode-obj
   #:crypto
   #:make-u8-vector
   #:convert-vector-to-integer
   #:integer-of
   #:+MAX-FRAGMENT-SIZE+
   #:assemble-sks
   #:time-to-renegotiate?
   #:with-mod
   #:m+
   #:m-
   #:m*
   #:m/
   #:minv
   #:m^
   ))
   
(defpackage :stactors/bridge
  (:use #:common-lisp #:stactors)
  (:export
   #:actor-bridge
   #:bridge-register
   #:bridge-unregister
   #:bridge-forward-message
   #:bridge-deliver-message
   #:bridge-ask-query
   #:bridge-handle-reply
   #:bridge-reset
   ))

(defpackage #:stactors/executives
  (:use #:common-lisp #:stactors)
  (:import-from #:useful-macros
   #:rmw
   #:rd
   #:wr)
  (:export
   #:nullify
   #:terminate-actor
   #:terminate-actors
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

   ;; MAC GCD stuff
   #:run-actor-direct
   #:run-worker-direct
   ))

(defpackage #:stactors/par
  (:use #:common-lisp #:stactors))

(defpackage #:stactors/base
  (:use #:common-lisp #:stactors
   #-:LISPWORKS #:ansi-timer)
  (:import-from #:useful-macros
   #:curry
   #:rcurry
   #:if-let
   #:when-let
   #:nlet
   #:dlambda*
   #:dcase*
   #:defmonitor
   #:critical-section
   #:capture-ans-or-exn
   #:call-capturing-ans-or-exn
   #:recover-ans-or-exn
   #:rmw
   #:rd
   #:wr)
  (:import-from #:ref
   #:ref
   #:ref-val)
  (:import-from #:stactors/executives
   #:add-to-ready-queue
   #:run-actor-direct
   #:run-worker-direct)
  (:export
   #:%run-actor
   #:assemble-ask-message
   #:no-immediate-answer
   #:prepend-events
   ))

(defpackage #:stactors/rwgate
  (:use #:common-lisp #:stactors))

(defpackage #:stactors/promises
  (:use #:common-lisp #:stactors))


(defpackage #:stactors/erl
  (:use #:common-lisp #:stactors)
  (:export
   #:process
   #:make-process
   #:spawn-link
   #:spawn-monitor
   #:link-between
   #:unlink-between
   #:link-to
   #:unlink-from
   #:link
   #:unlink
   #:exit
   #:trap-exits))

(defpackage #:stactors/notifications
  (:use #:common-lisp #:stactors)
  (:export
   #:subscribe
   #:notify
   #:unsubscribe
   ))

(defpackage #:stactors/user
  (:use #:common-lisp #:stactors))


