;; mp-compatibility.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
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

;; --------------------------------------------------
(in-package #:com.ral.mpcompat)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))
;; --------------------------------------------------
;; Compatibility Layer

(defmacro defglobal (name val)
  `(sb-ext:defglobal ,name ,val))

(defun get-current-process ()
  "Get the current Lisp process."
  sb-thread:*current-thread*)

;; --------------------------------------------------------------------------

(defun process-name (proc)
  (sb-thread:thread-name proc))

(defun set-process-name (proc name)
  (setf (sb-thread:thread-name proc) name))

;; --------------------------------------------------------------------------

(defvar *process-plists* (make-hash-table :weakness :key :test 'eq))

(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  (gethash proc *process-plists*))

(defun set-process-plist-entry (proc key val)
  (let ((lst (process-plist proc)))
    (if lst
        (setf (getf lst key) val)
      (setf (gethash proc *process-plists*) (list key val)))
    ))

;; --------------------------------------------------------------------------

(defun process-run-function (name flags proc &rest args)
  "Spawn a new Lisp thread and run the indicated function with inital args."
  (declare (ignore flags))
  (sb-thread:make-thread (lambda ()
			   (apply proc args))
			 :name name))

;; --------------------------------------------------------------------------

(defun process-kill (proc)
  "Kill the indicated Lisp process."
  (sb-thread:terminate-thread proc))

(defun current-process-kill ()
  (sb-thread:abort-thread))

;; --------------------------------------------------------------------------

(defun process-interrupt (proc fn &rest args)
  "Interrupt the indicated Lisp process to have it perform a function."
  (declare (type function fn))
  (sb-thread:interrupt-thread proc (lambda ()
				     (apply fn args))))

;; --------------------------------------------------------------------------

(defmacro without-preemption (&body body)
  "Perform the body forms without preemption."
  `(sb-sys:without-interrupts
     ,@body))

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(defun make-lock (&key name important-p (safep t) sharing)
  "Make a Lisp lock."
  (declare (ignorable important-p safep sharing))
  (sb-thread:make-mutex :name name))

;; --------------------------------------------------------------------------

(defun do-with-lock (lock timeout fn)
  (declare (type function fn))
  (cond ((eq sb-thread:*current-thread* (sb-thread:mutex-owner lock))
	 (funcall fn))
	(timeout
	 (tagbody
	    (sb-sys:without-interrupts
	      (handler-case
		  (sb-ext:with-timeout timeout
		    (sb-sys:allow-with-interrupts
                      (sb-thread:grab-mutex lock :waitp t))
		    (go have-lock))
		(sb-ext:timeout (cx)
		  (declare (ignore cx))
		  (go beyond))))
	    have-lock
	    (unwind-protect
		 (funcall fn)
	      (sb-sys:without-interrupts
		(sb-thread:release-mutex lock)))
	    beyond))

	(t (sb-thread:with-mutex (lock)
	     (funcall fn)))
	))

(defmacro with-spinlock ((lock) &body body)
  `(with-lock (,lock) ,@body))

#|
(defmacro with-lock ((lock &optional whostate timeout) &body body)
  "Wait for lock available, then execute the body while holding the lock."
  (declare (ignore whostate)))
  `(do-with-lock ,lock ,timeout (lambda () ,@body))
|#

(defmacro with-lock ((lock &optional whostate timeout) &body body)
  "Wait for lock available, then execute the body while holding the lock."
  (declare (ignore whostate))
  `(sb-thread:with-mutex (,lock :timeout ,timeout) ,@body))

;; --------------------------------------------------------------------------

(defun lock-owner (lock)
  (sb-thread:mutex-owner lock))

;; --------------------------------------------------------------------------

(defun make-mailbox (&key size lock-name)
  "Make a Lisp mailbox."
  (declare (ignorable size lock-name))
  (sb-concurrency:make-mailbox :name lock-name))

;; --------------------------------------------------------------------------

(defun mailbox-send (mbox msg)
  "Send a message to a Lisp mailbox."
  (sb-concurrency:send-message mbox msg))

;; --------------------------------------------------------------------------


(defun mailbox-read (mbox &optional timeout) 
  "Wait with timeout for a message to arrive at the Lisp mailbox and return it.
A null timeout means wait forever."
  (if timeout 
      (sb-concurrency:receive-message mbox :timeout timeout)
      (sb-concurrency:receive-message mbox)))

;; --------------------------------------------------------------------------

(defun mailbox-empty? (mbox)
  "Check if the Lisp mailbox is empty. Return generalized T/F."
  (sb-concurrency:mailbox-empty-p mbox))

(defun mailbox-empty-p (mbox)
  (mailbox-empty? mbox))


;; --------------------------------------------------------------------------

(defun process-wait (wait-reason wait-fn &rest wait-args)
  (declare (ignore wait-reason))
  (sb-ext:wait-for
   (apply wait-fn wait-args)))


;; --------------------------------------------------------------------------

(defun process-wait-with-timeout (wait-reason timeout
				  &optional wait-fn &rest wait-args)
  (if wait-fn
      (sb-ext:wait-for (apply wait-fn wait-args) :timeout timeout)
      (sb-ext:wait-for t :timeout timeout)))


;; --------------------------------------------------------------------------

#+nil
(defun generate-uuid ()
  (uuid:make-v4-uuid))

(defmacro atomic-incf (place)
  `(sb-ext:atomic-incf ,place))

(defmacro atomic-decf (place)
  `(sb-ext:atomic-decf ,place))

(defun nyi (name)
  (error "Not yet implemented: ~S" name))

(defmacro atomic-fixnum-incf (place)
  `(atomic-incf ,place))

(defmacro atomic-exch (a b)
  `(nyi 'atomic-exch))

(defmacro atomic-push (item lst)
  `(nyi 'atomic-push))

(defmacro atomic-pop (lst)
  `(nyi 'atomic-pop))

(defmacro globally-accessible (place)
  `(nyi 'globally-accessible))

(defun process-sharing-lock (&rest args)
  (nyi 'process-sharing-lock))

(defun process-exclusive-lock (&rest args)
  (nyi 'process-exclusive-lock))

(defun process-sharing-unlock (&rest args)
  (nyi 'process-sharing-unlock))

(defun process-exclusive-unlock (&rest args)
  (nyi 'process-exclusive-unlock))

(defun process-lock (&rest args)
  (nyi 'process-lock))

(defun process-unlock (&rest args)
  (nyi 'process-unlock))

(defmacro with-exclusive-lock ((lock &rest args) &body body)
  `(with-lock (,lock) ,@body))

(defmacro with-sharing-lock ((&rest args) &body body)
  `(nyi 'with-sharing-lock))

(defmacro compare-and-swap (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defmacro CAS (place old new)
  `(sb-ext:CAS ,place ,old ,new))


(defun funcall-async (fn &rest args)
  (sb-thread:make-thread fn :arguments args))

(defun make-timer (fn &rest args)
  (sb-ext:make-timer (lambda () (apply fn args))))

(defun schedule-timer-relative (timer dt)
  (sb-ext:schedule-timer timer dt))

