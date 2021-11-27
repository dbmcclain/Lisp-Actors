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
   #:pipe-beh
   #:pipe
   #:sink-pipe
   #:pass
   #:pass-beh
   #:fork
   #:list-match
   #:actor-nlet
   #:is-pure-sink?

   #:make-sponsor
   #:kill-sponsor
   #:restart-sponsor
   #:base-sponsor
   #:slow-sponsor
   #:def-sponsor
   #:in-sponsor
   #:in-this-sponsor
   #:io
   #:ioreq
   #:par-safe
   #:with-sponsor
   #:err
   
   #:+emptyq+
   #:+doneq+
   #:addq
   #:pushq
   #:popq
   #:emptyq?
   #:iterq
   #:do-queue
   
   #:β
   #:beta
   #:beta-beh
   #:beta-gen
   #:alambda

   #:restart-actors-system
   #:kill-actors-system

   #:become   
   #:send
   #:send*
   #:repeat-send
   #:send-combined-msg
   #:ask
   #:maybe-safe-ask
   
   #:self
   #:self-beh
   #:self-sponsor

   #:actor
   #:actor-p
   #:actor-beh
   #:make-actor
   #:make-remote-actor
   #:with-worker
   
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
   #:with-printer
   #:with-ticket
   #:ticketed-perform
   #:println
   #:writeln
   #:fmt-println
   #:logged-beh
   #:logged
   #:logger
   #:atrace
   
   #:once
   #:send-to-all
   #:race
   #:fwd
   #:label
   #:tag
   #:ser
   #:par
   #:scheduled-message
   #:schedule-after
   #:serializer
   #:serializer-abort
   #:timing
   #:sequenced-delivery
   #:prune-self
   #:prunable-alambda
   #:mbox-sender
   #:ioreq
   #:time-tag
   
   #:format-usec
   #:logger-timestamp
   
   #:sink-beh
   #:const-beh
   #:once-beh
   #:race-beh
   #:fwd-beh
   #:label-beh
   #:tag-beh
   #:time-tag-beh
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

   #:self-msg

   #:marshal-encoder
   #:marshal-decoder
   #:marshal-compressor
   #:marshal-decompressor
   #:encryptor
   #:decryptor
   #:signing
   #:signature-validation
   #:self-sync-encoder
   #:self-sync-decoder
   #:chunker
   #:dechunker
   #:printer
   #:writer
   #:marker
   #:logger

   #:netw-encoder
   #:netw-decoder
   #:disk-encoder
   #:disk-decoder
   #:encr-disk-encoder
   #:encr-disk-decoder

   #:acurry-beh
   #:racurry-beh
   #:acurry
   #:racurry

   #:aont-encoder
   #:aont-decoder
   #:aont-file-writer
   #:aont-file-reader

   #:list-imploder
   #:list-exploder

   #:with-timeout
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
   #:foreign-ask
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

(defpackage :actors/network
  (:use #:common-lisp #:actors #:def*)
  (:export
   #:*default-port*
   #:*socket-timeout-period*
   #:start-tcp-server
   #:terminate-server
   #:*default-port*
   #:+MAX-FRAGMENT-SIZE+
   #:client-connector
   #:connections
   ))

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
  (:export
   #:*current-actor*
   #:*current-sponsor*
   #:*evt-queue*
   #:add-evq
   #:*all-sponsors*
   ))

(defpackage :ac-secure-comm
  (:use #:common-lisp #:actors #:core-crypto #:edec)
  (:export
   #:make-local-services
   #:global-services
   #:server-crypto-gateway
   #:client-gateway
   #:remote-service
   #:+server-connect-id+
   #:+server-skey+
   #:start-server-gateway
   ))

(defpackage #:actors/user
  (:use #:common-lisp #:actors #:def*))


