;; fstm-v4.lisp -- Dynamic STM - Software Transactional Memory
;;
;; Adapted from UCAM-CL-TR-579 U.Cambridge Tech Report 579,
;; "Practical lock-freedom" by Keir Fraser, Feb 2004
;;
;; See also paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787&rep=rep1&type=pdf
;;  "Software Transactional Memory for Dynamic-Sized Data Structures",
;;  Herlihy, Luchangco, Moir, Sherer
;;
;; This version is based on the modern MCAS from Guerraoui, et al.
;; Minimum CAS operation count. For N vars, without contention, we
;; perform only N+1 CAS operations.
;;
;; DM/RAL  12/20
;; --------------------------------------------------------
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

(in-package #:fstm)

;; ----------------------------------------------------

(defvar *current-transaction* nil)

(defclass transaction (<orderable-mixin>)
  ((state-ref  :reader   transaction-state-ref  :initform (ref:ref :undecided))
   (ro-list    :accessor transaction-ro-list    :initform nil)
   (rw-map     :accessor transaction-rw-map     :initform (maps:empty))
   ))

(defun make-transaction ()
  (make-instance 'transaction))

(defmethod transaction-p (x)
  nil)

(defmethod transaction-p ((x transaction))
  x)

(defun trans-state (trans)
  (ref:basic-val (transaction-state-ref trans)))

(defun trans-state-p (trans state)
  (eq state (trans-state trans)))

(defvar *ncomms*  0)  ;; cumm nbr successful commits
(defvar *nrolls*  0)  ;; cumm nbr retrys

;; ----------------------------------------------------

(define-condition retry-exn ()
  ())

(defun retry ()
  (sys:atomic-fixnum-incf *nrolls*)
  (error (load-time-value
          (make-condition 'retry-exn)
          t)))

;; ----------------------------------------------------

(defvar *var-index* 0)

(defstruct (var
            (:include ref:ref)
            (:constructor var (ref:val)))
  (id (incf *var-index*) :read-only t))

(defmethod ord:compare ((a var) (b var))
  (- (var-id a) (var-id b)))

;; ----------------------------------------------------

(declaim (inline trans<))

(defun trans< (trans1 trans2)
  (declare (type transaction trans1 trans2))
  (< (the fixnum (order-id trans1))
     (the fixnum (order-id trans2))))

;; -------------------------------------------------------------------

(defstruct word-desc
  parent   ;; points to top-level transaction
  old      ;; points to old value in var
  new-ref) ;; COW ref to hold new value for var

(defun obj-read (var trans)
  ;; only called outside of commit
  (declare (var var))
  (um:nlet-tail retry-read ()
    (let ((val (var-val var))) ;; NOTE: direct low-level access to VAL slot
      (if (word-desc-p val)
          (let ((parent (word-desc-parent val)))
            (if (and (not (eq parent trans))
                     (trans-state-p parent :undecided))
                (progn
                  (commit-transaction parent)
                  (retry-read))
              ;; else
              (values val
                      (if (trans-state-p parent :successful)
                          (ref:basic-val (word-desc-new-ref val)) ;; low-level non-COW read
                        (word-desc-old val)))
              ))
        ;; else
        (values val val)))
    ))

(defun simple-obj-read (var)
  ;; use by OPEN-FOR-READ and OPEN-FOR-WRITE
  (multiple-value-bind (contents value)
      (obj-read var nil)
    (declare (ignore contents))
    value))

(um:defun* check-var ((var . old) trans)
  (multiple-value-bind (contents value)
      (obj-read var trans)
    (declare (ignore contents))
    (eq old value)))

;; --------------------------------------

(defun #1=every-rw-element (trans fn)
  ;; using pre-order scan
  (maps:iter (transaction-rw-map trans)
             (lambda (var wdesc)
               (unless (funcall fn var wdesc)
                 (return-from #1# nil))))
  t)

(defun every-ro-element (trans fn)
  (every fn (transaction-ro-list trans)))

;; ----------------------------------------

(defun validate-transaction (trans)
  (and
   (every-ro-element trans (um:rcurry #'check-var nil))
   (every-rw-element trans
                     (lambda (var wdesc)
                       (check-var (cons var (word-desc-old wdesc)) nil)))
   ))
                      
(defun commit-transaction (trans)
  (declare (transaction trans))
  (labels
      ((acquire (var wdesc)
         (declare (word-desc wdesc))
         (um:nlet-tail retry-word ()
           (multiple-value-bind (content value)
               (obj-read var trans)
             (or (eq content wdesc)
                 (and (eq value (word-desc-old wdesc))
                      (trans-state-p trans :undecided)
                      (or (um:basic-cas var content wdesc)
                          (retry-word))
                      ))
             ))))
    
    (um:basic-cas (transaction-state-ref trans) :undecided
                  (if (and
                       (every-ro-element trans (um:rcurry #'check-var trans))
                       (every-rw-element trans #'acquire))
                      :successful
                    :failed))
    (trans-state-p trans :successful)
    ))

(defun re-parent (trans)
  ;; after absorbing sub-transactions, we need to point all the RW entries
  ;; to this top level transaction, before attempting to commit
  (maps:iter (transaction-rw-map trans)
             (lambda (var wdesc)
               (declare (ignore var))
               (setf (word-desc-parent wdesc) trans))
             ))

;; -------------------------------------------------------------------------
;; operations on *CURRENT-TRANSACTION*

(defun open-for-read (var)
  (declare (var var))
  (check-type *current-transaction* transaction)
  (check-type var var)
  (um:if-let (pair (assoc var (transaction-ro-list *current-transaction*)
                          :test #'eq))
      (cdr pair)
    ;; else
    (um:if-let (wdesc (maps:find (transaction-rw-map *current-transaction*) var))
        (ref:val (word-desc-new-ref wdesc)) ;; read-only val - do not mutate
      ;; else
      (let ((value (simple-obj-read var)))
        (push (cons var value)
              (transaction-ro-list *current-transaction*))
        value))))

(defmethod ref:val ((var var))
  (open-for-read var))

(defun release-read (var)
  ;; releasing a var that was never opened is a benign error
  (check-type *current-transaction* transaction)
  (check-type var var)
  (um:deletef (transaction-ro-list *current-transaction*)
              var
              :key  #'car
              :test #'eq))

(defun open-for-write (var)
  (declare (var var))
  (check-type *current-transaction* transaction)
  (check-type var var)
  ;; check if we already have it opened for writing
  (um:if-let (wdesc (maps:find (transaction-rw-map *current-transaction*) var))
      (word-desc-new-ref wdesc) ;; current new ref
    
      ;; else - new open
      (let* ((val (um:if-let (pair (assoc var (transaction-ro-list *current-transaction*)
                                           :test #'eq))
                       (progn
                         (release-read var)
                         (cdr pair))
                     ;; else
                     (simple-obj-read var)))
             (new-ref (ref:cow val)) ;; be sure to use VAL or WVAL to access value
             (wdesc   (make-word-desc
                       :old     val
                       :new-ref new-ref)))
        (maps:addf (transaction-rw-map *current-transaction*)
                   var wdesc)
        new-ref)))

(defmethod ref ((var var))
  ;; Deliver a REF on the VAR which can be used with VAL (read-only
  ;; value) and WVAL (a clone that can be mutated) to effect COW
  ;; semantics.
  ;; Set with either of (SETF (REF:VAL ref) ...) or (SETF (REF:WVAL ref) ...).
  (open-for-write var))

(defun release-write (var)
  (check-type *current-transaction* transaction)
  (check-type var var)
  (maps:removef (transaction-rw-map *current-transaction*) var))

(declaim (inline check))

(defun check (expr)
  (unless expr
    (retry)))

(defun validate ()
  (check (validate-transaction *current-transaction*)))

(defun commit ()
  (re-parent *current-transaction*)
  (check (commit-transaction *current-transaction*))
  (sys:atomic-fixnum-incf *ncomms*))

;; ------------------------------

(defun do-orelse (&rest fns)
  (labels
      ((absorb-trans (parent trans)
         (setf (transaction-ro-list parent) (transaction-ro-list trans)
               (transaction-rw-map parent)  (transaction-rw-map trans))))

    (let ((parent *current-transaction*))
      (um:nlet-tail iter ((rest-fns fns))
        (if rest-fns
            (let ((*current-transaction* (make-transaction)))
              (when parent
                (absorb-trans *current-transaction* parent))
              (handler-case
                  (multiple-value-prog1
                      (funcall (first rest-fns))
                    (cond (parent
                           (validate)
                           (absorb-trans parent *current-transaction*))
                          
                          (t
                           ;; else - we are parent, so commit
                           (commit))
                          ))
                
                (retry-exn ()
                  (iter (rest rest-fns)))
                ))
          ;; else - retry the whole bunch
          (if parent
              (retry)
            (iter fns))
          )))))

(defmacro atomic (&body body)
  `(do-orelse (lambda ()
                ,@body)))

(defmacro orelse (&rest clauses)
  `(do-orelse ,@(mapcar #`(lambda ()
                            ,a1)
                        clauses)))

#|
(let ((a  (var 1))
      (b  (var 2)))
  (multiple-value-bind (my-a my-b)
      (atomic
        (values (fstm:val a)
                (fstm:val b)))
    (+ my-a my-b)))
|#
#|
(defun do-atomic (fn)
  (if *current-transaction*
      ;; absorb nested atomics into outer one
      (values (funcall fn) t) ;; optimistic success
    ;; else
    (um:nlet-tail iter ()
      (let ((*current-transaction* (make-transaction)))
        (handler-case
            (multiple-value-prog1
                (values (funcall fn) t)
              (commit))
          (retry-exn ()
            (iter))
          (abort-exn (exn)
            (values (abort-exn-retval exn) nil))
          )))
    ))
  
(defmacro atomic (&body body)
  ;; return (values body t) if successful
  ;; else (values nil nil) if aborted
  `(do-atomic (lambda ()
                ,@body)))
|#

;; -------------------------------------------------

;; ---------------------------------------------------
;; Test it out... hopefully lots of contention... yep!
#|
;; Speed Comparison DSTM/FSTM (median of 3 runs)
;;
;; Duration Measurements (1M Iters)
Test      DSTM       FSTM
----      ----       ----
TST1      2.23       5.1
TST2      4.50      10.3
TST3      6.72      15.3
|#

#|
(defun common-code (delta)
    (atomic
      (let* ((refa (open-for-write *a*))
             (refb (open-for-write *b*))
             (a    (+ delta (ref-value refa)))
             (b    (* 2 a)))
        (setf (ref-value refa) a
              (ref-value refb) b)
        )))

(defstruct my-cache
  (modification-count 0)
  a
  b)
 
;; modifier code
(sys:with-modification-change
 (my-cache-modification-count cache)
 (setf (my-cache-a cache) (calculate-a-value xxx)
       (my-cache-b cache) (calculate-b-value yyy)))
 
;; reading code
(loop
  (sys:with-modification-check-macro
   my-cache-did-not-change-p (my-cache-modification-count cache)
   (let ((a (my-cache-a cache))
	 (b (my-cache-b cache)))
     (when (my-cache-did-not-change-p)
       (return (values a b )))
     )))
|#

