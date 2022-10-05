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

(defpackage #:com.ral.actors
  (:use #:common-lisp #:def*)
  (:nicknames #:ac)
  (:import-from #:timeout
   #:timeout
   #:*timeout*)
  (:export
   #:timeout
   #:*timeout*)
  (:import-from #:list-match
   #:match
   #:match-fail)
  (:export
   #:match
   #:match-fail)
  (:import-from #:um
   #:letrec)
  (:export
   #:letrec)
  (:export
   #:*nbr-pool*
   #:α
   #:αα
   #:ret
   #:ret*
   #:γlambda
   #:γactor
   #:γ
   #:pipe-beh
   #:pipe
   #:sink-pipe
   #:tee
   #:tee-beh
   #:fork
   #:simd
   #:mimd
   #:actor-nlet
   #:is-pure-sink?
   #:deflex
   #:µ
   #:∂
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
   #:def-beh

   #:custodian
   #:running-actors-p
   #:restart-actors-system
   #:kill-actors-system
   #:add-executives

   #:*current-sponsor*
   #:sponsor-beh
   #:make-sponsor
   #:in-sponsor
   #:in-this-sponsor
   #:self-sponsor
   
   #:become
   #:abort-beh
   #:send
   #:send*
   #:repeat-send
   #:send-combined-msg
   #:send-to-pool
   #:ask
   #:stsend
   #:with-single-thread
   #:call-actor
   #:*NO-ANSWER*
   
   #:self
   #:self-beh

   #:actor
   #:actor-p
   #:actor-beh
   #:create
   #:make-remote-actor
   #:concurrently
   
   #:send-after

   #:start
   #:start-tcp-server
   #:terminate-server

   #:future
   #:lazy
   #:sink
   #:const
   #:with-printer

   #:println
   #:writeln
   #:fmt-println
   #:logged-beh
   #:logged
   #:logger
   #:install-atrace
   #:uninstall-atrace
   #:atrace
   
   #:once
   #:send-to-all
   #:race
   #:fwd
   #:label
   #:tag
   #:ser
   #:par

   #:serializer
   #:serializer-sink
   #:blocking-serializer
   #:with-assured-response
   #:def-ser-beh
   #:timing
   ;; #:sequenced-delivery
   ;; #:prune-self
   ;; #:prunable-alambda
   #:mbox-sender
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
   #:timing-beh
   ;; #:pruned-beh
   ;; #:pend-beh
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
   #:fail-silent-marshal-decoder
   #:marshal-compressor
   #:marshal-decompressor
   #:fail-silent-marshal-decompressor
   #:encryptor
   #:decryptor
   #:rep-signing
   #:rep-sig-validation
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
   #:checksum
   #:verify-checksum
   
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

   #:kvdb
   #:deep-copy

   #:fn-actor-beh
   #:fn-actor
   #:reactive-obj-beh
   #:reactive-obj

   #:side-job
   #:guard
   #:close-file
   #:secure-erase
   #:perform

   #:%set-beh

   #:authentication
   #:check-authentication
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

(defpackage #:com.ral.actors.macros
  (:use #:common-lisp #:com.ral.actors #:def*))

(defpackage #:com.ral.actors.base
  (:use #:common-lisp #:com.ral.actors
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
   ))

(defpackage com.ral.actors.kv-database
  (:use #:cl #:def* #:com.ral.actors)
  (:export
   #:kvdb
  ))
   
(defpackage #:com.ral.actors.user
  (:use #:common-lisp #:com.ral.actors #:def*)
  (:nicknames #:ac-user))


