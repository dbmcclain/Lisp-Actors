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


(in-package :actors)

;; --------------------------------------------------------
;; Shared printer driver... another instance of something better
;; placed into an Actor

(defun blind-print (cmd &rest items)
  (declare (ignore cmd))
  (let ((prfn (get :actors :print-handler)))
    (dolist (item items)
      (funcall prfn item))
    ))

(eval-when (:load-toplevel)
  (unless (get :actors :print-handler)
    (setf (get :actors :print-handler) 'print)))

(defvar *shared-printer-actor*    #'blind-print)

(defun pr (&rest things-to-print)
  (let ((fmt  (first things-to-print)))
    (cond ((and (stringp fmt)
                (find #\~ fmt))
           (send *shared-printer-actor* :print
                 (apply 'format nil fmt (rest things-to-print))))

          (t
           (apply #'send *shared-printer-actor* :print things-to-print))
          )))

(defun install-actor-printer ()
  (unless (typep *shared-printer-actor* 'actor)
    (setf *shared-printer-actor*
          (make-actor
           (dlambda
            (:print (&rest things-to-print)
		    (apply 'blind-print :print things-to-print))
            
            (:quit () (become 'blind-print))
            )))
    (register-actor *shared-printer-actor* :SHARED-PRINTER)))

;; --------------------------------------------------------

(defun install-actor-system (&rest ignored)
  (declare (ignore ignored))
  (install-actor-printer))

#-:lispworks
(eval-when (:load-toplevel)
  (install-actor-system))

#+:lispworks
(eval-when (:load-toplevel)
  ;; 3 choices, if :COM.RAL is true, use lw:define-action; elsif building-binary, don't install actors; else install actors
  ;; Cannot install actor system during DELIVERY (since, multitasking not allowed during DELIVERY), must install actors later.
  ;; *performing-binary-build* is created in delivery.lisp, else it is not created and not BOUNDP

  ;; Trying to avoid the use of *features*.  We use a special, cl-user::*performing-binary-build*, set up
  ;; in emotiq/etc/deliver/deliver.lisp, then write Lisp code to decide which of the 3 cases to perform (at LOAD time).
  ;; This special is UNINTERNED in emotiq/src/startup.lisp/START.

  (let ((com-ral-p #+:COM.RAL t #-:COM.RAL nil)
        (building-binary-p (boundp 'cl-user::*performing-binary-build*)))

    (flet ((create-lispworks-action-to-install-actors ()
        (let ((lw:*handle-existing-action-in-action-list* '(:warn :skip)))
          (lw:define-action "Initialize LispWorks Tools"
                            "Start up Functional Actors"
                            'install-actor-system
                            :after "Run the environment start up functions"
                            :once))))
      
      (format *standard-output* "~&com-ral-p ~A~&building-binary-p ~A~&"
              com-ral-p
              building-binary-p)

      (if com-ral-p
          (create-lispworks-action-to-install-actors)
        (if building-binary-p
	    nil                                            ;; do nothing, esp. don't try to initialize actors
          (install-actor-system))))))                      ;; in all other cases, install actors at LOAD time.


