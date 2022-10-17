;; ------------------------------------------
;; Hoare Monitors, Critical Sections, and static bindings
;;
;; DM/RAL 08/20 (latest update)
;; -------------------------------------------
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

(defpackage #:com.ral.useful-macros.critical-section
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.critical-section)

;; ------------------------------------------------------------------------
;; WARNING!! Any macros that generate embedded (LOAD-TIME-VALUE ...)
;; work properly only in compiled code. Interpreted code, through
;; repeated eval expansion, generates fresh values on every execution.
;;
;; More properly, things like this should be expressed as LOL
;; closures. The use of DEFMONITOR is best, in the case of locks and
;; static bindings, or any shared mutable state in the LET bindings.
;;
;; NOTE: Even LOL is SMP-unsafe if any of the bindings are mutable.
;; They should always be wrapped in a DEFMONITOR, and all access to
;; them should be guarded with CREITICAL-SECTION or
;; WITH-EXCLUSIVE-ACCESS.
;;
;; In the end, it is probably better to forego using STATIC, and just
;; use an overt, but critical-section protected, access to LOL
;; bindings.
;;
;; ------------------------------------------------------------------------
;; Runtime Support

(defstruct mon-parms
  lock     ;; a sharing lock
  bindings ;; a vector of binding values
  static)  ;; a property list of STATIC bindings

(defun %binding (mon ix)
  (svref (mon-parms-bindings mon) ix))

(defun %set-binding (mon ix val)
  (setf (svref (mon-parms-bindings mon) ix) val))

(defsetf %binding %set-binding)

(defun %static (mon id &optional init)
  (mpcompat:with-exclusive-lock ((mon-parms-lock mon))
    ;; use excl locking because first time access has potential to
    ;; structurally mutate the slot
    (let* ((unbound #())
           (val     (getf (mon-parms-static mon) id unbound)))
      (if (eq val unbound)
          (%set-static mon id init)
        val))))

(defun %set-static (mon id val)
  (mpcompat:with-exclusive-lock ((mon-parms-lock mon))
    (setf (getf (mon-parms-static mon) id) val)))

(defsetf %static (mon id &optional init) (val)
  (declare (ignore init))
  `(%set-static ,mon ,id ,val))

(defun %with-shared-access (mon timeout fn)
  (mpcompat:with-sharing-lock ((mon-parms-lock mon) "In Shared Monitor" timeout)
    (funcall fn)))

(defun %with-exclusive-access (mon timeout fn)
  (mpcompat:with-exclusive-lock ((mon-parms-lock mon) "In Exclusive Monitor" timeout)
    (funcall fn)))

;; -------------------------------------------------------------------
;; DEFMONITOR - encloses a scoping region in which function
;; definitions can be made mutually thread safe. Initial bindings are
;; produced with one-time initialization, as with DEFVAR, so that
;; internal state is maintained upon reloading the DEFMONITOR.
;;
;; State bindings can be reinitialized by first calling MAKUNBOUND on
;; the DEFMONITOR symbol and then reloading the DEFMONITOR.
;;
;; Within the monitor region, macros STATIC, CRITICAL-SECTION,
;; WITH-EXCLUSIVE-ACCESS, and WITH-SHARED-ACCESS, become defined for
;; use in the body of the DEFMONITOR. Typically, the body will contain
;; a collection of DEFUN.
;;
;; Any use of shared mutable state among the enclosed functions should
;; be guarded with CRITICAL-SECTION, WITH-EXCLUSIVE-ACCESS, or
;; WITH-SHARED-ACCESS.
;;
;; The use of STATIC is interally guarded as a CRITICAL-SECTION, but
;; any mutable references should be overtly guarded by the programmer
;; using WITH-EXCLUSIVE-ACCESS, WITH-SHARED-ACCESS, or
;; CRITICAL-SECTION.
;;
;; CRITICAL-SECTION and STATIC are guarded by an exclusive lock with
;; infinite timeout. Use enclosing WITH-EXCLUSIVE-ACCESS or
;; WITH-SHARED-ACCESS if you want to exercise control over the locking
;; timeout.
;;
;; Syntax is (DEFMONITOR name bindings [[decl* | doc]] form*)
;; ----------------------------------------------------------------------

(defmacro cx-dspec-def (dspec &body body)
  (declare (ignorable dspec))
  #+:LISPWORKS
  `(dspec:def ,dspec ,@body)
  #-:LISPWORKS
  `(progn ,@body))

(defmacro defmonitor (&whole whole name bindings &body body)
  (multiple-value-bind (forms decls docstr)
      (parse-body body :documentation t :whole whole)
    (let* ((vars  (mapcar 'car bindings))
           (vals  (mapcar 'cadr bindings)))
      `(cx-dspec-def (defmonitor ,name)
         (mpcompat:defglobal ,name (load-time-value
                                    (make-mon-parms
                                     :lock     (mpcompat:make-lock :sharing t)
                                     :bindings ,(when bindings `(vector ,@vals))))
                             ,@docstr)
         (macrolet ((with-shared-access ((&optional timeout) &body body)
                      `(%with-shared-access ,',name ,timeout (lambda ()
                                                               ,@body)))
                    (with-exclusive-access ((&optional timeout) &body body)
                      `(%with-exclusive-access ,',name ,timeout (lambda ()
                                                                  ,@body)))
                    (static (id &optional data)
                      `(%static ,',name ,id (load-time-value ,data))))
           (symbol-macrolet ,(loop for pos from 0
                                   for var in vars
                                   collect
                                   `(,var (%binding ,name ,pos)))
             ,@decls
             ,@forms)))
      )))

#+:LISPWORKS
(progn
  (editor:setup-indent "defmonitor" 2)
  (editor:setup-indent "with-exclusive-access" 1)
  (editor:setup-indent "with-shared-access" 1)
  (editor:setup-indent "let-static" 1))

(defmacro critical-section (&body body)
  `(with-exclusive-access ()
     ,@body))

(defmacro let-static (bindings &body body)
  `(symbol-macrolet ,(mapcar (lambda* ((name id &optional init))
                               `(,name (static ,id ,init)))
                             bindings)
     ,@body))

(defmacro critical-or (&optional clause-1 &rest clauses)
  ;; another variant of the CHECK/LOCK/CHECK pattern for SMP
  ;; multiprocessing.
  (when clause-1
    (let ((g!clause-1  (gensym)))
      `(flet ((,g!clause-1 ()
                ,clause-1))
         (or (,g!clause-1)
             (critical-section
               (or (,g!clause-1)
                   (progn
                     ,@clauses)))))
      )))

;; -----------------------------------------------------

#|
(defmonitor test-monitor ()
  (defun tst ()
    (critical-section
      (let ((c (static 'tst-c (list 0))))
        (incf (car c))))))

(defmonitor test-monitor-2 ()
  (defun tst-2 ()
    (critical-section
      (let-static ((c 'tst-c 0))
        (incf c)))))
(makunbound 'test-monitor-2)

(xdefun tst ((&whole arg &optional hd . tl))
  (list arg hd tl))
|#

