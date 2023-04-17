;; mp-compat-allegro.lisp
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
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))
;; --------------------------------------------------
;; Compatibility Layer

(defmacro defglobal (name val)
  `(defvar ,name ,val))

(defun current-process ()
  "Get the current Lisp process."
  mp:*current-process*)

;; --------------------------------------------------------------------------

(defun process-name (proc)
  (mp:process-name proc))

(defun set-process-name (proc name)
  (setf (mp:process-name proc) name))

;; --------------------------------------------------------------------------

(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  (mp:process-property-list proc))

(defun get-process-plist-entry (proc key &optional default)
  "Set the property named by key in the process' property list to val"
  (getf (process-plist proc) key default))

(defun (setf process-plist-entry) (val proc key &optional default)
  "Set the property named by key in the process' property list to val"
  (let ((plist (process-plist proc)))
    (setf (getf plist key default) val
          (mp:process-property-list proc) plist)))

;; --------------------------------------------------------------------------

(defun process-property (key &optional proc default)
  "Get the property named by key in the process' property list"
  (let ((plist (process-plist (or proc (current-process)))))
    (getf plist key default)))

(defun (setf process-property) (val key &optional proc default)
  (let* ((proc  (or proc (current-process)))
         (plist (process-plist proc)))
    (setf (getf plist key default) val
          (mp:process-property-list proc) plist)
    ))

(defun process-private-property (key &optional default)
  (process-property key nil default))

(defun (setf process-private-property) (val key &optional default)
  (setf (process-property key nil default) val))

;; --------------------------------------------------------------------------

(defun process-run-function (name flags proc &rest args)
  "Spawn a new Lisp thread and run the indicated function with inital args."
  (declare (ignore flags))
  (apply #'mp:process-run-function name proc args))

;; --------------------------------------------------------------------------

(defun process-kill (proc)
  "Kill the indicated Lisp process."
  (mp:process-kill proc))

;; --------------------------------------------------------------------------

(defun process-interrupt (proc fn &rest args)
  "Interrupt the indicated Lisp process to have it perform a function."
  (apply #'mp:process-interrupt proc fn args))

;; --------------------------------------------------------------------------

(defmacro without-preemption (&body body)
  "Perform the body forms without preemption."
  `(mp:without-scheduling ,@body))

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(defun make-lock (&key name important-p (safep t) sharing)
  "Make a Lisp lock."
  (declare (ignorable important-p safep sharing))
  (mp:make-process-lock
   :name name))

;; --------------------------------------------------------------------------

(defmacro with-lock ((lock &optional whostate timeout) &body body)
  "Wait for lock available, then execute the body while holding the lock."
  `(mp:with-process-lock (,lock :whostate ,whostate
                                :timeout  ,timeout)
                         ,@body))

(defmacro with-spinlock (args &body body)
  `(with-lock ,args ,@body))

(defmacro with-sharing-lock (args &body body)
  `(with-lock ,args ,@body))

(defmacro with-exclusive-lock (args &body body)
  `(with-lock ,args ,@body))

;; --------------------------------------------------------------------------

(defun lock-owner (lock)
  (declare (ignorable lock))
  (mp:process-lock-locker lock))

;; --------------------------------------------------------------------------

(defun process-lock (lock &optional whostate timeout)
  (mp:process-lock lock mp:*current-process* whostate timeout))

;; --------------------------------------------------------------------------

(defun process-unlock (lock)
  (mp:process-unlock lock mp:*current-process*))

;; --------------------------------------------------------------------------

(defun make-mailbox (&key size &allow-other-keys)
  "Make a Lisp mailbox."
  (declare (ignorable size))
  (make-instance 'mp:queue))

;; --------------------------------------------------------------------------

(defun mailbox-send (mbox msg)
  "Send a message to a Lisp mailbox."
  (mp:enqueue mbox msg))

;; --------------------------------------------------------------------------

(defun mailbox-read (mbox &optional wait-reason timeout)
  "Wait with timeout for a message to arrive at the Lisp mailbox and return it.
A null timeout means wait forever."
  (if (mp:queue-empty-p mbox)
      (if timeout
          (when (sys::process-wait-with-timeout (or wait-reason
                                                    "Waiting on mailbox")
                                                timeout
                                                (lambda () 
                                                  (not (mp:queue-empty-p mbox))))
            (values (mp:dequeue mbox) t))
        (values (mp:dequeue mbox :wait t) t))
    (values (mp:dequeue mbox) t)))

;; --------------------------------------------------------------------------

(defun mailbox-empty-p (mbox)
  "Check if the Lisp mailbox is empty. Return generalized T/F."
  (mp:queue-empty-p mbox))

(defun mailbox-not-empty-p (mbox)
  (not (mailbox-empty-p mbox)))

;; --------------------------------------------------------------------------

(defun process-wait (wait-reason wait-fn &rest wait-args)
  (apply #'mp:process-wait wait-reason wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun process-wait-with-timeout (wait-reason timeout
				  &optional wait-fn &rest wait-args)
  (if timeout
      (apply #'mp:process-wait-with-timeout wait-reason timeout wait-fn wait-args)
    (progn
      (apply #'mp:process-wait wait-reason wait-fn wait-args)
      t)))

;; --------------------------------------------------------------------------

#+nil
(defun generate-uuid ()
  (uuid:make-v1-uuid))

;; --------------------------------------------------------------------------

(defmacro atomic-incf (place)
  `(excl:incf-atomic ,place))

(defmacro atomic-decf (place)
  `(excl:decf-atomic ,place))

(defmacro atomic-push (item place)
  `(excl:push-atomic ,item ,place))

(defmacro atomic-pop (place)
  `(excl:pop-atomic ,place))

(defun ensure-memory-after-store ()
  t)

(defmacro compare-and-swap (place before after)
  ;; Franz warns that atomic-conditional-setf can only be used as a
  ;; test form in a conditional, a result of a
  ;; technical compiler issue. So here we ensure that happens...
  `(when (excl:atomic-conditional-setf ,place ,after ,before)
     t))

(defmacro CAS (place old new)
  `(compare-and-swap ,place ,old ,new))


(defun funcall-async (fn &rest args)
  (apply #'mp:process-run-function "Async" fn args))

(defun get-current-process ()
  (current-process))

(defun current-process-kill ()
  (process-kill (current-process)))

(defun process-terminate (proc &key join-timeout force-timeout)
  (declare (ignore join-timeout force-timeout))
  (process-kill proc))
