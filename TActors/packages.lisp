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

(defpackage #:actors
  (:nicknames #:ac)
  (:use #:common-lisp #:def*)
  #|
  (:use #:common-lisp #:cps)
  #.`(:export
      ,@(loop for sym being the external-symbols of :cps
              collect sym))
  |#
  (:import-from #:timeout
   #:timeout
   #:*timeout*)
  (:export
   #:timeout
   #:*timeout*)

  (:export
   #:ret
   #:ret*
   #:γlambda
   #:γactor
   #:γ
   #:pipe
   #:fork
   #:list-match
   #:actor-nlet

   #:addq
   #:pushq
   #:popq
   #:iterq
   #:do-queue
   
   #:β
   #:beta
   #:beta-beh
   #:beta-gen
   #:alambda
   #:par-safe
   #:io
   #:in-sponsor
   #:current-sponsor
   #:*current-sponsor*
   #:sponsor
   #:make-sponsor
   #:*sponsor*
   #:*slow-sponsor*

   #:send
   #:send*
   #:self
   #:self-beh
   #:self-sponsor
   #:sponsored-actor
   #:sponsored-actor-spon
   #:sponsored-actor-act
   #:hosted-actor
   #:send-now
   #:ask
   
   #:ensure-par-safe-behavior

   #:actor-trait
   #:actor-trait-p
   #:local-actor-trait
   #:local-actor-trait-p
   #:actor
   #:actor-p
   #:actor-beh
   #:make-actor
   #:make-remote-actor
   #:with-worker
   
   #:become
   #:repeat-send
   
   #:invalid-send-target

   #:make-actor-directory
   #:clear-directory
   #:register-actor
   #:unregister-actor
   #:find-actor
   #:get-actors
   #:get-actor-names
   #:find-names-for-actor
   
   #:timeout
   #:*timeout*

   #:start-actors-system
   #:kill-executives

   #:send-after

   #:lfm
   #:log-info
   #:log-warning
   #:log-error
   #:set-printer
   #:set-stream

   #:start
   #:start-tcp-server
   #:terminate-server

   #:usti
   #:usti=
   #:proxy
   #:make-proxy

   #:actors
   #:future
   #:lazy
   #:sink
   #:const
   #:println
   #:once
   #:send-to-all
   #:race
   #:fwd
   #:label
   #:wrapping
   #:tag
   #:ser
   #:par
   #:scheduled-message
   #:schedule-after
   #:serializer
   #:timing
   #:sponsor-switch
   #:sequenced-delivery
   #:prune-self
   #:mbox-sender
   
   #:sink-beh
   #:const-beh
   #:once-beh
   #:race-beh
   #:fwd-beh
   #:label-beh
   #:tag-beh
   #:wrapping-beh
   #:scheduled-message-beh
   #:serializer-beh
   #:timing-beh
   #:pruned-beh
   #:pend-beh
   #:mbox-sender-beh
   
   #:subscribe
   #:notify
   #:unsubscribe

   #:watch
   #:unwatch

   #:suspend
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

(defpackage #:actors/internal-message
  (:use #:common-lisp)
  (:export
   #:continuation
   #:ask
   #:send-sync
   #:unwatch
   #:become-local
   ))

(defpackage #:actors/internal-message/security
  (:use #:common-lisp #:def*)
  (:export
   ))

(defpackage #:actors/internal-message/bridge
  (:use #:common-lisp #:def*)
  (:export
   #:forwarding-send
   #:forwarding-ask
   #:forwarding-reply
   #:no-service
   ))

(defpackage #:actors/internal-message/network
  (:use #:common-lisp #:def*)
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

(defpackage #:actors/macros
  (:use #:common-lisp #:actors #:def*))

(defpackage #:actors/directory
  (:use #:common-lisp #:actors #:def*))

(defpackage #:actors/lfm
  (:use #:common-lisp #:actors #:def*)
  (:export
   #:ensure-system-logger
   #:kill-system-logger
   ))

(defpackage :actors/network
  (:use #:common-lisp #:actors #:def*)
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
   ))

(defpackage :actors/security
  (:use #:common-lisp #:actors #:def*)
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
   
(defpackage :actors/bridge
  (:use #:common-lisp #:actors #:def*)
  (:export
   #:actor-bridge
   #:bridge-register
   #:bridge-pre-register
   #:bridge-unregister
   #:bridge-forward-message
   #:bridge-deliver-message
   #:bridge-ask-query
   #:bridge-handle-reply
   #:bridge-reset
   #:bridge-know-self
   ))

(defpackage #:actors/executives
  (:use #:common-lisp #:actors #:def*)
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

(defpackage #:actors/par
  (:use #:common-lisp #:actors #:def*))

(defpackage #:actors/base
  (:use #:common-lisp #:actors
   #-:LISPWORKS #:ansi-timer
   #:def*)
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
  (:import-from #:actors/executives
   #:add-to-ready-queue
   #:run-actor-direct
   #:run-worker-direct)
  (:export
   #:%run-actor
   #:assemble-ask-message
   #:no-immediate-answer
   #:retry-send
   #:*whole-message*
   ))

(defpackage #:actors/rwgate
  (:use #:common-lisp #:actors #:def*))

(defpackage #:actors/promises
  (:use #:common-lisp #:actors #:def*))


(defpackage #:actors/erl
  (:use #:common-lisp #:actors #:def*)
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

(defpackage #:actors/notifications
  (:use #:common-lisp #:actors #:def*)
  (:export
   #:subscribe
   #:notify
   #:unsubscribe
   ))

(defpackage #:actors/user
  (:use #:common-lisp #:actors #:def*))


