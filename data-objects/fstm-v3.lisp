;; fstm.lisp -- Dynamic STM - Software Transactional Memory
;;
;; Adapted from UCAM-CL-TR-579 U.Cambridge Tech Report 579,
;; "Practical lock-freedom" by Keir Fraser, Feb 2004
;;
;; See also paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787&rep=rep1&type=pdf
;;  "Software Transactional Memory for Dynamic-Sized Data Structures",
;;  Herlihy, Luchangco, Moir, Sherer
;;
;; DM/RAL  03/17
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

;; Here, it is known that other-trans is in the midst of a commit.  If
;; it is still in the reading / grabbing stage of commit, then we look
;; to see if we are also committing. If we are, we wouldn't be here
;; unless we had already grabbed all of our write cells. There is a
;; possibility that other-trans may also want some of those same write
;; cells, and the outcome of its commit is uncertain. To avoid an
;; infinite loop, if we are older than other-tran, then we abort other
;; tran.

;; Otherwise, whether we are committing or not, we help him commit, so
;; that we gain a definitive answer to his commit outcome. If he wants
;; some of our write cells, then

;; If it has proceeded past the point of grabbing all its write cells
;; and is now in :READ-PHASE, then we help it along.
;;
;; If trans (which is us) is also in a commit, and if we are older
;; than other-trans, then we attempt to abort other-trans. That will
;; either succeed, or else other-trans has already finished
;; committing. Either way it will have a status of :FAILED or
;; :SUCCEEDED.
;;
;; (NOTE: if we are in a commit, and in this routine, then we must
;; already be in :READ-PHASE)
;;
;; If we aren't in a commit, then we merely help other-trans finish
;; its commit.

(defun obj-reader (var help-fn)
  ;; only called outside of commit
  (declare (var var)
           (function help-fn))
  (let ((val (var-val var))) ;; NOTE: direct low-level access to VAL slot
    (if (transaction-p val)
        (let ((pair (maps:find (transaction-rw-map val) var)))
          (funcall help-fn val)
          (if (trans-state-p val :successful)
              (ref:basic-val (cdr pair)) ;; new, bypass COW cloning
            (car pair))) ;; old
      ;; else
      val)))

(defun obj-verify (trans var old)
  ;; only called during commit :read-phase
  (declare (transaction trans))
  (labels
      ((help (other-trans)
         (declare (transaction other-trans))
         (when (trans-state-p other-trans :read-phase)
           (if (trans< trans other-trans) ;; I'm older
               (ref:basic-cas (transaction-state-ref other-trans) :read-phase :failed)
             ;; else
             (commit-transaction other-trans)))
         ))
    (declare (dynamic-extent #'help))
    (eq old (obj-reader var #'help))
    ))

(defun obj-read (var)
  ;; only called outside of commit
  (labels
      ((help (other-trans)
         (declare (transaction other-trans))
         (when (trans-state-p other-trans :read-phase)
           (commit-transaction other-trans))))
    (declare (dynamic-extent #'help))
    (obj-reader var #'help)))

;; --------------------------------------

(defun #1=validate ()
  (check-type *current-transaction* transaction)
  (labels
      ((check-var (var old)
         (unless (eq old (obj-read var))
           (return-from #1# nil))))
    (declare (dynamic-extent #'check-var))
    (maps:iter
     (transaction-rw-map *current-transaction*)
     (um:lambda* (var (old . new-ref))
       (declare (ignore new-ref))
       (check-var var old)))
    (map nil (um:lambda* ((var . old))
               (check-var var old))
         (transaction-ro-list *current-transaction*))
    t))
                      
(defun #1=commit-transaction (trans)
  (declare (transaction trans))
  (let ((rw-map    (transaction-rw-map trans))
        (state-ref (transaction-state-ref trans)))

    (um:labels*
        ((in-state? (state)
           (eq state (ref:basic-val state-ref)))
         (transition (from-state to-state)
           (ref:basic-cas state-ref from-state to-state)
           (when (member (ref:basic-val state-ref) '(:failed :successful))
             (release)))
         (patchup-success (var (old . new-ref))
           (declare (ignore old))
           (ref:basic-cas var trans (ref:basic-val new-ref))) ;; bypass COW cloning
         (patchup-failure (var (old . new-ref))
           (declare (ignore new-ref))
           (ref:basic-cas var trans old))
         (verify-read ((var . val))
           (unless (obj-verify trans var val)
             (transition :read-phase :failed)))
         (release ()
           (let ((success (in-state? :successful)))
             (maps:iter rw-map
                        (if success
                            #'patchup-success
                          #'patchup-failure))
             (return-from #1# success)))
         (acquire (var (old . new-ref))
           (declare (ignore new-ref))
           (um:nlet-tail iter ()
             (unless (ref:basic-cas var old trans)
               (let ((val (var-val var))) ;; NOTE: low-level direct access to VAL slot
                 (cond ((eq val trans))
                       ;; we were here before... must be helping from another thread

                       ((eq val old)
                        ;; someone else owned it, then gave it back
                        (iter))
                       
                       ((transaction-p val) ;; still in a commit
                        (commit-transaction val) ;; help it along
                        (iter))
                       
                       (t
                        ;; someone else has it now
                        (transition :undecided :failed))
                       )))
             )))
      (declare (dynamic-extent #'in-state? #'transition
                               #'acquire #'verify-read #'release
                               #'patchup-success #'patchup-failure))

      (mp:with-interrupts-blocked
        (when (in-state? :undecided)
          (maps:iter rw-map #'acquire)
          (transition :undecided :read-phase))
        (when (in-state? :read-phase)
          ;; NOTE: this read checking only ensures that the value now
          ;; seen is the same as originally seen, for each individual
          ;; read-only operation.
          (map nil #'verify-read
               (transaction-ro-list trans))
          (transition :read-phase :successful))
        (release))
      )))
        
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
    (um:if-let (pair (maps:find (transaction-rw-map *current-transaction*) var))
        (ref:val (cdr pair)) ;; read-only val - do not mutate
      ;; else
      (let ((data (obj-read var)))
        (push (cons var data)
              (transaction-ro-list *current-transaction*))
        data))))

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
  (um:if-let (pair (maps:find (transaction-rw-map *current-transaction*) var))
      (cdr pair) ;; current new ref
    
      ;; else - new open
      (let* ((val (um:if-let (pair (assoc var (transaction-ro-list *current-transaction*)
                                           :test 'eq))
                       (progn
                         (release-read var)
                         (cdr pair))
                     ;; else
                     (obj-read var)))
             (new-ref (ref:cow val))) ;; be sure to use DEREF to access value
        (maps:addf (transaction-rw-map *current-transaction*)
                   var (cons val new-ref))
        new-ref)))

(defmethod ref ((var var))
  ;; deliver a REF on the VAR which can be used with RDEREF and WDEREF
  ;; to effect COW semantics
  (open-for-write var))
;
(defun release-write (var)
  (check-type *current-transaction* transaction)
  (check-type var var)
  (maps:removef (transaction-rw-map *current-transaction*) var))

(declaim (inline check))

(defun check (expr)
  (unless expr
    (retry)))

(defun commit ()
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
                           (check (validate))
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

