;; mp-compat-lw5.lisp
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
(in-package #:mp-compatibility)
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))
;; --------------------------------------------------
;; Compatibility Layer

(defun process-property (key &optional proc default)
  "Get the property named by key in the process' property list"
  (let* ((proc  (or proc mp:*current-process*))
         (plist (mp:process-plist proc)))
    (getf plist key default)))

(defun (setf process-property) (value key &optional proc default)
  (let* ((proc  (or proc mp:*current-process*))
         (plist (mp:process-plist proc)))
    (setf (getf plist key default) value
          (mp:process-plist proc)  plist)))

;; (defsetf process-property set-process-property)


(defun process-private-property (key &optional default)
  (process-property key nil default))

(defun (setf process-private-property) (value key &optional default)
  (setf (process-property key nil default) value))

;; (defsetf process-private-property set-process-private-property)


;; --------------------------------------------------------------------------

(defun mailbox-send (mbox msg)
  "Send a message to a Lisp mailbox."
  (mp:mailbox-send mbox (list msg)))

;; --------------------------------------------------------------------------

(defun mailbox-read (mbox &optional wait-reason timeout)
  "Wait with timeout for a message to arrive at the Lisp mailbox and return it.
A null timeout means wait forever."
  (let ((ans (mp:mailbox-read mbox wait-reason timeout)))
    (if ans
        (values (car ans) t)
      nil)))

;; --------------------------------------------------------------------------

(defun lock-owned-by-current-process-p (lock)
  (eq (mp:lock-owner lock) (current-process)))




(defmacro atomic-incf (place)
  `(mp:without-interrupts (incf ,place)))

(defmacro atomic-decf (place)
  `(mp:without-interrupts (decf ,place)))

(defun ensure-memory-after-store ()
  t)

(defmacro compare-and-swap (place before after)
  `(mp:without-interrupts
     (when (eq ,before ,place)
       (setf ,place ,after)
       t)))
  


;; ------------------------------------------------
;; Spin-locks

(defun do-with-spinlock (lock fn &aux ans)
  (loop until (mp:with-lock (lock nil 0.01)
                (setf ans (multiple-value-list (funcall fn)))
                t))
  (values-list ans))

(defmacro with-spinlock ((lock) &body body)
  `(do-with-spinlock ,lock (lambda () ,@body)))

#|(defmacro xwith-spinlock ((lock) &body body)
  `(mp:with-lock (,lock) ,@body))|#

(editor:setup-indent "with-spinlock" 1)

;; ----------------------------------------------

(defmacro with-sharing-lock (lock-form &body body)
  `(mp:with-lock ,lock-form
     ,@body))

(defmacro with-exclusive-lock (lock-form &body body)
  `(mp:with-lock ,lock-form
     ,@body))


(defun make-lock (&key (name "Anon") important-p (safep t) &allow-other-keys)
  (mp:make-lock :name name :important-p important-p :safep safep))
