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

(defpackage :com.ral.mpcompat
  (:use #:common-lisp)
  ;; (:nicknames #:mpcompat)
  #+:SBCL
  (:import-from #:sb-concurrency
   #:mailbox)
  #+:LISPWORKS
  (:import-from #:mp
   #:process-name
   #:process-run-function
   #:process-kill
   #:process-interrupt
   #:without-preemption
   #:process-wait
   #:process-wait-with-timeout
   #:lock-owner
   #:process-lock
   #:process-unlock
   #:with-lock
   #:make-mailbox
   #:mailbox-empty-p
   #:mailbox-not-empty-p
   #:process-sharing-lock
   #:process-sharing-unlock
   #:process-exclusive-lock
   #:process-exclusive-unlock
   #:mailbox
   )
  #+(OR :LISPWORKS6 :LISPWORKS7 :LISPWORKS8)
  (:import-from #:mp
   #:mailbox-send
   #:mailbox-read
   ;; new in Lispworks 6
   #:process-property
   #:process-private-property
   #:process-poke
   #:make-lock
   #:make-condition-variable
   #:condition-variable-wait
   #:condition-variable-signal
   #:lock-owned-by-current-process-p
   #:with-sharing-lock
   #:with-exclusive-lock
   ;; in LW7
   #:process-terminate
   #:funcall-async
   #:current-process-kill
   #:make-timer
   #:schedule-timer-relative
   #:get-current-process
   #:*initial-processes*
   #:with-exclusive-lock
   #:with-sharing-lock
   #:process-allow-scheduling
   )
  #+(OR :LISPWORKS6 :LISPWORKS7 :LISPWORKS8)
  (:import-from #:sys
   #:atomic-incf
   #:atomic-decf
   #:atomic-fixnum-incf
   #:compare-and-swap
   #:ensure-memory-after-store
   #:globally-accessible
   #:atomic-exchange
   #:atomic-push
   #:atomic-pop)
  (:export
   #:current-process-kill
   #:process-name
   #:process-property
   #:process-private-property
   #:process-run-function
   #:process-terminate
   #:process-interrupt
   #:without-preemption
   #:process-wait
   #:process-wait-with-timeout
   #:process-kill
   #:make-lock
   #:lock-owner
   #:process-lock
   #:process-unlock
   #:with-lock
   #:with-spinlock
   #:with-locks
   #:with-spinlocks
   #:with-sharing-lock
   #:with-exclusive-lock
   #:make-mailbox
   #:mailbox-send
   #:mailbox-read
   #:mailbox-empty-p
   #:mailbox-not-empty-p
   
   ;; new in Lispworks 6
   #:process-poke
   #:make-condition-variable
   #:condition-variable-wait
   #:condition-variable-signal
   #:lock-owned-by-current-process-p
   
   #:atomic-incf
   #:atomic-decf
   #:atomic-fixnum-incf
   #:compare-and-swap
   #:ensure-memory-after-store

   #+nil #:generate-uuid
   
   #:critical
   #:spin-critical

   #:CAS
   #:funcall-async

   #:defglobal
   #:make-timer
   #:schedule-timer-relative
   #:get-current-process
   #:*initial-processes*

   #:globally-accessible
   #:atomic-exchange
   #:atomic-push
   #:atomic-pop
   
   #:with-exclusive-lock
   #:with-sharing-lock
   #:process-allow-scheduling
   #:process-sharing-lock
   #:process-exclusive-lock
   #:process-sharing-unlock
   #:process-exclusive-unlock
   #:mailbox
   ))

