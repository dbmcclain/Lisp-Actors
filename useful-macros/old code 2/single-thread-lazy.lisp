;; Single-thread-lazy.lisp - Single threaded lazy/force
;;
;; DM/RAL 02/17
;; -----------------------------------------
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

(defpackage #:single-thread-lazy
  (:use #:common-lisp)
  (:nicknames #:stlazy)
  (:export
   #:force
   #:lazy
   ))

(in-package #:stlazy)

;; equiv to #F
(proclaim '(optimize (speed  3)
                     ;; (safety 0)
                     (float  0)))

(defstruct lazy-cell
  fn
  (val :eval))

(defmacro lazy (&body body)
  `(make-lazy-cell
    :fn  (lambda () ,@body)))

(defun force (x)
  (if (lazy-cell-p x)
      (with-accessors ((fn  lazy-cell-fn)
                       (val lazy-cell-val)) x
        (values-list
         (if (eq :eval val)
           (setf val (multiple-value-list (funcall fn)))
           val)) )
    ;; else
    x))
    
;; -----------------------------------------
;; Lazy evaluation and once-functions
;;

(defstruct lazy-cell
  fn)

(defun force (x)
  ;; force one level of lazy cells. But note that if any of them are
  ;; lazy*, then they will in turn perform an extra force
  (if (lazy-cell-p x)
      (funcall (lazy-cell-fn x))
    x))

(defun force* (x &rest vals)
  ;; completely force any chains of lazy cells
  (if (lazy-cell-p x)
      (multiple-value-call #'force* (funcall (lazy-cell-fn x)))
    (apply #'values x vals)))

;; ----------------------------------------

(defun %monitored-when (test-fn lock body-fn set-fn)
  (when (funcall test-fn)
    (mp:with-lock (lock)
      (when (funcall test-fn)
        (multiple-value-prog1
            (funcall body-fn)
          (funcall set-fn)))
      )))

(defmacro monitored-when ((place compare lock new-value) &body body)
  ;; force all threads and processors to go through this
  ;; in sequential manner when place EQ compare,
  ;; execute body if place EQ compare after lock,
  ;; leave place set to new-value after executing body
  ;; returns NIL or body result
  `(%monitored-when (lambda ()
                      (eq ,place ,compare))
                    ,lock
                    (lambda ()
                      ,@body)
                    (lambda ()
                      (setf ,place ,new-value))
                    ))

#+:LISPWORKS
(editor:setup-indent "MONITORED-WHEN" 2)

(defun %make-lazy (drilldown-fn)
  ;; drilldown-fn should take no arguments, and return a capture tuple
  ;; of (values-list &optional error-condition)
  (let ((lock (mp:make-lock)) ;; make thread-safe
        vals
        self)
    (labels ((fast-eval ()
               vals)
             (slow-eval ()
               (monitored-when (self #'slow-eval lock #'fast-eval)
                   (setf vals (capture-ans-or-exn
                               (shiftf drilldown-fn nil)))) ;; shiftf for GC
               vals)
             (outer-eval ()
               (recover-ans-or-exn (funcall self))))
      (setf self #'slow-eval)
      (make-lazy-cell
       :fn  #'outer-eval)
      )))
  
;; ----------------------------------------
;; Lazy supports only a single level of laziness. On force, only the
;; immediate object is forced.

(defmacro lazy (&body body)
  ;; lazy makes a lazy closure that will eval on only the first force
  ;; and thereafter return the same value on subsequent forces
  `(%make-lazy (lambda () ,@body)))

;; --------------------
;; Lazy* supports iterated chains of laziness. On force, the entire
;; chain is forced.  A lazy* chain can finally return multiple values

(defun %make-lazy* (thunk)
  ;; the idea is that you force a lazy thunk, and if it yields another
  ;; lazy thunk in return, you force it again, and again, and again...
  ;; Where is this behavior useful?
  (%make-lazy
   (lambda ()
     (labels ((drill-down (x)
                (let ((ans (capture-ans-or-exn x)))  ;; returns tuple (values-list err)
                  (if (and (single ans)              ;; no error?
                           (single (car ans))        ;; values-list is a singleton?
                           (lazy-cell-p (caar ans))) ;; testing for another lazy
                      (capture-ans-or-exn #'force (caar ans))
                    ans))))
       (recover-ans-or-exn (drill-down (shiftf thunk nil))))) ;; shiftf for GC
   ))

(defmacro lazy* (&body body)
  ;; makes a lazy closure that will eval deeply on force.  Thereafter,
  ;; it will return the final value on each subsequent force.
  `(%make-lazy* (lambda () ,@body)))

#+:LISPWORKS
(progn
  (editor:setup-indent "lazy"  2 2)
  (editor:setup-indent "lazy*" 2 2))

