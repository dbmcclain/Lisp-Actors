;; collector.lisp
;; --------------------------------------------------------------
;; Define our own collector objects that perform rapid nconc list
;; accumulation
;;
;; DM/RAL 02/07 -- added a Lock for MP safety
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

(defpackage #:com.ral.useful-macros.collector
  (:use #:common-lisp #:com.ral.useful-macros))

(in-package #:com.ral.useful-macros.collector)

(defclass <collector> ()
  ((hd   :accessor collector-hd :initform nil)
   (tl   :accessor collector-tl :initform nil)))

(defclass <monitored-collector> (<collector>)
  ((changed  :accessor collector-changed-p :initform nil)))

(defclass <mpsafe-monitored-collector> (<monitored-collector>)
  ((lock :accessor mpsafe-lock
         :initform 
	 #+:LISPWORKS (mp:make-lock)
	 #+:ALLEGRO   (mp:make-process-lock))))

;; -------------------------------------------------

(defun make-collector ()
  (make-instance '<collector>))

(defun make-monitored-collector ()
  (make-instance '<monitored-collector>))

(defun make-mpsafe-monitored-collector ()
  (make-instance '<mpsafe-monitored-collector>))

;; ------------------------------------------------

(defun internal-reset (c)
  (setf (collector-hd c) nil
        (collector-tl c) nil))

(defgeneric collector-reset (collector)
  (:method ((collector <collector>))
   (internal-reset collector))
  (:method :after ((collector <monitored-collector>))
   (setf (collector-changed-p collector) nil))
  (:method :around ((collector <mpsafe-monitored-collector>))
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))

(defgeneric collector-discard-contents (collector)
  (:method ((collector <collector>))
   (internal-reset collector))
  (:method :after ((collector <monitored-collector>))
   (setf (collector-changed-p collector) t))
  (:method :around ((collector <mpsafe-monitored-collector>))
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))

(defgeneric collector-nstuff-contents (collector list)
  (:method ((collector <collector>) (list list))
   (setf (collector-tl collector) (last list)
         (collector-hd collector) list))
  (:method :after ((collector <monitored-collector>) list)
   (setf (collector-changed-p collector) t))
  (:method :around ((collector <mpsafe-monitored-collector>) list)
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))
   
(defgeneric collector-stuff-contents (collector list)
  (:method ((collector <collector>) (list list))
   (collector-nstuff-contents collector (copy-list list))))

(defgeneric collector-ncontents (collector &key discard)
  (:method ((collector <collector>) &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  ;;
  ;; If user doesn't want to discard contents,
  ;; then hand him a copy of the contents as they exist at this moment.
  (if discard
      (prog1
          (collector-hd collector)
        (collector-discard-contents collector))
    ;; else
    (copy-list (collector-hd collector))))
  (:method :around ((collector <mpsafe-monitored-collector>) &key &allow-other-keys)
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))
  
(defgeneric collector-length (collector)
  (:method ((collector <collector>))
   (length (collector-hd collector))))

(defgeneric collector-empty-p (collector)
  (:method ((collector <collector>))
   (null (collector-hd collector))))

(defgeneric collector-nappend (collector list)
  (:method ((collector <collector>) (list list))
   (let ((penult (nconc (collector-tl collector) list)))
     (setf (collector-tl collector) (last list))
     (unless (collector-hd collector)
       (setf (collector-hd collector) penult))))
  (:method :after ((collector <monitored-collector>) list)
   (setf (collector-changed-p collector) t))
  (:method :around ((collector <mpsafe-monitored-collector>) list)
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))
   
(defgeneric collector-append (collector list)
  (:method ((collector <collector>) (list list))
   (collector-nappend collector (copy-list list))))

(defgeneric collector-append1 (collector item)
  (:method ((collector <collector>) item)
   (collector-nappend collector (list item))))

(defgeneric collector-nprepend (collector list)
  (:method ((collector <collector>) (list list))
   (setf (collector-hd collector) (nconc list (collector-hd collector)))
   (unless (collector-tl collector)
     (setf (collector-tl collector) (last list))))
  (:method :after ((collector <monitored-collector>) list)
   (setf (collector-changed-p collector) t))
  (:method :around ((collector <mpsafe-monitored-collector>) list)
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))
  
(defgeneric collector-prepend (collector list)
  (:method ((collector <collector>) (list list))
   (collector-nprepend collector (copy-list list))))

(defgeneric collector-push (collector item)
  (:method ((collector <collector>) item)
   (push item (collector-hd collector))
   (unless (collector-tl collector)
     (setf (collector-tl collector) (collector-hd collector))))
  (:method :after ((collector <monitored-collector>) item)
   (setf (collector-changed-p collector) t))
  (:method :around ((collector <mpsafe-monitored-collector>) item)
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))
  
(defgeneric collector-pop (collector)
  (:method ((collector <collector>))
   (prog1
       (pop (collector-hd collector))
     (unless (collector-hd collector)
       (setf (collector-tl collector) nil))))
  (:method :after ((collector <monitored-collector>))
   (setf (collector-changed-p collector) t))
  (:method :around ((collector <mpsafe-monitored-collector>))
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))

(defgeneric mark-changed (collector &optional t/f)
  (:method ((collector <monitored-collector>) &optional (t/f t))
   (setf (collector-changed-p collector) t/f)))

(defgeneric nchanged-p (collector)
  (:method ((collector <monitored-collector>))
   ;; asking if changed also resets the monitor
   (shiftf (collector-changed-p collector) nil))
  (:method :around ((collector <mpsafe-monitored-collector>))
   (mp:with-lock ((mpsafe-lock collector))
     (call-next-method))))


