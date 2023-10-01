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
  #.`(:export
      ,@(loop for sym being the external-symbols of '#:def*
              collect sym))
  #+:LISPWORKS
  (:import-from #:lw
   #:do-nothing)
  (:import-from #:um
   #:timeout
   #:*timeout*
   #:with-timeout)
  (:export
   #:timeout
   #:*timeout*
   #:with-timeout)
  (:import-from #:um
   #:match
   #:match-fail
   #:letrec)
  (:export
   #:match
   #:match-fail
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
   #:service
   #:null-service
   #:echo-service
   #:fork
   #:simd
   #:mimd
   #:actor-nlet
   #:is-pure-sink?
   #:deflex
   #:µ
   #:∂
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
   #:actors-running-p
   #:restart-actors-system
   #:kill-actors-system
   #:add-executives

   #:*current-actor*
   #:*current-beh*
   #:*current-message*
   #:*current-env*
   
   #:become
   #:abort-beh
   #:send
   #:send*
   #:repeat-send
   #:send-combined-msg
   #:send-to-pool
   #:ask
   
   #:self
   #:self-beh
   #:self-env
   #:self-msg
   #:screened-beh
   
   #:actor
   #:actor-p
   #:actor-beh
   #:%actor-cas
   #:service
   #:create
   #:%create
   #:actors
   #:make-remote-actor
   #:concurrently
   
   #:send-after

   #:start
   #:start-tcp-server
   #:terminate-server

   #:future
   #:lazy
   #:sink
   #:become-sink
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
   #:send-all-to
   #:race
   #:fwd
   #:label
   #:tag
   #:once-tag
   #:timed-tag
   #:ser
   #:par

   #:serializer-beh
   #:serializer
   #:serializer-sink
   #:blocking-serializer
   #:err-from
   #:timing
   #:sequenced-delivery
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
   #:mbox-sender-beh
   
   #:subscribe
   #:notify
   #:unsubscribe

   #:watch
   #:unwatch

   #:suspend

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

   #:authentication
   #:check-authentication

   #:splay-beh
   #:splay
   #:yield

   #:and-β
   #:or-β
   #:let-β
   #:let*-β
   #:let++
   #:prog1-β
   #:progn-β
   #:with-and-β
   #:with-or-β
   #:if-and-β
   #:if-or-β
   #:when-and-β
   #:when-or-β
   #:unless-and-β
   #:unless-or-β
   #:if-β
   #:when-β
   #:unless-β
   #:true
   #:false
   #:def-actor
   #:define-behavior
   #:sequential-β

   #:unw-prot-beh
   #:unw-prot
   #:open-file-beh
   #:open-file
   #:with-actors

   #:fut-wait-beh
   #:fut
   #:future-become-beh
   
   #:timed-service
   #:with-maximum-io-syntax

   #:recursive-ask
   #:with-recursive-ask
   #:err-chk
   #:referred-error
   #:terminated-ask
   #:ask-timeout
   #:check-for-errors
   #:+timed-out+
   #:warn-timeout
   
   #:restartable
   #:non-idempotent
   #:on-commit
   #:do-nothing
   #:tracing-send
   #:untraced-send
   #:trace-me
   #:dbg-trace
   #:dbg-trace-inspect
   #:tracer

   ;; convenience macros
   #:<<    ;; FUNCALL
   #:<<*   ;; APPLY
   #:>>    ;; SEND
   #:>>*   ;; SEND*
   #:!     ;; SETF
   #:β!    ;; BECOME
   #:!β    ;; BECOME (for us dyslexics)
   #:αλ    ;; ALAMBDA
   ))

#+(OR :ALLEGRO :CCL)
(defpackage #:com.ral.ansi-timer
  (:use #:common-lisp)
  (:export
   #:timer
   #:make-timer
   #:schedule-timer
   #:schedule-timer-relative
   #:unschedule-timer
   ))

(defpackage #:com.ral.actors.base
  (:use #:common-lisp
   #:com.ral.actors
   #+(OR :ALLEGRO :CCL) #:com.ral.ansi-timer
   #:com.ral.usec)
  (:import-from #:um
   #:curry
   #:rcurry
   #:if-let
   #:when-let
   #:nlet
   #:rmw
   #:rd
   #:wr)
  (:export
   #:*current-actor*
   ;; #:*current-env*
   ))

(defpackage #:com.ral.actors.macros
  (:use
   #:common-lisp
   #:com.ral.actors
   #:com.ral.actors.base))

(defpackage #:com.ral.actors.user
  (:use
   #:common-lisp
   #:com.ral.actors))
 
#|
(asdf :doctools)
(doctools:gen-docs
 :asdf-system-name :com.ral.actors
 :package-name     :com.ral.actors
 :directory        (translate-logical-pathname "PROJECTS:LISP;xTActors;actors-base")
 :subtitle         "Transactional Parallel/Concurrent Hewitt Actors in Lisp")
|#
