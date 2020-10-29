;; clozure-compat.lisp -- compatibility layer for Clozure Lisp
;; DM/RAL  02/09
;; -------------------------------------------------------------
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

#+:CLOZURE
(defpackage :clos
  (:use #:COMMON-LISP)
  (:import-from #:CCL
		#:standard-slot-definition
		#:standard-direct-slot-definition
		#:standard-effective-slot-definition
		#:direct-slot-definition-class
		#:effective-slot-definition-class
		#:compute-effective-slot-definition
		#:slot-value-using-class
		#:class-slots
		#:slot-definition-name
		#:slot-boundp-using-class
		#:slot-makunbound-using-class
		#:validate-superclass
		#:method-specializers
		#:generic-function-methods
		#:compute-class-precedence-list
		#:finalize-inheritance
		)
  (:export
   #:standard-slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-class
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:compute-effective-slot-definition
   #:slot-value-using-class
   #:class-slots
   #:slot-definition-name
   #:slot-boundp-using-class
   #:slot-makunbound-using-class
   #:generic-function-methods
   #:validate-superclass
   #:method-specializers
   #:compute-class-precedence-list
   #:finalize-inheritance
   #:class-name
   ))
  

#+:CLOZURE
(defpackage :mp
  (:use #:COMMON-LISP)
  (:import-from #:CCL
		#:*current-process*
		#:make-process
		#:process-suspend
		#:process-resume
		#:process-suspend-count
		#:process-preset
		#:process-enable
		#:process-run-function
		#:process-interrupt
		#:process-reset
		#:process-kill
		#:process-abort
		#:*ticks-per-second*
		#:process-whostate
		#:process-allow-schedule
		#:process-wait
		#:process-wait-with-timeout
		#:without-interrupts
		#:make-lock
		#:with-lock-grabbed
		#:grab-lock
		#:release-lock
		#:try-lock
		#:make-read-write-lock
		#:with-read-lock
		#:with-write-lock
		#:make-semaphore
		#:signal-semaphore
		#:wait-on-semaphore
		#:timed-wait-on-semaphore
		#:process-input-wait
		#:process-output-wait
		#:with-terminal-input
		#:*request-terminal-input-via-break*
		#:join-process
		#:process-name)
  (:export
   #:*current-process*
   #:make-process
   #:process-suspend
   #:process-resume
   #:process-suspend-count
   #:process-preset
   #:process-enable
   #:process-run-function
   #:process-interrupt
   #:process-reset
   #:process-kill
   #:process-abort
   #:*ticks-per-second*
   #:process-whostate
   #:process-allow-schedule
   #:process-wait
   #:process-wait-with-timeout
   #:without-interrupts
   #:make-lock
   #:with-lock-grabbed
   #:grab-lock
   #:release-lock
   #:try-lock
   #:make-read-write-lock
   #:with-read-lock
   #:with-write-lock
   #:make-semaphore
   #:signal-semaphore
   #:wait-on-semaphore
   #:timed-wait-on-semaphore
   #:process-input-wait
   #:process-output-wait
   #:with-terminal-input
   #:*request-terminal-input-via-break*
   #:join-process
   #:process-name
   ))

#+:CLOZURE
(defpackage :stream
  (:use #:COMMON-LISP)
  (:import-from #:CCL
		#:fundamental-binary-output-stream
		#:fundamental-binary-input-stream
                #:stream-write-byte
                #:stream-read-byte)
  (:export
   #:fundamental-binary-output-stream
   #:stream-write-byte
   #:fundamental-binary-input-stream
   #:stream-read-byte
   ))

