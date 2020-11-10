;; MCAS.lisp -- Multiple CAS on CAR/CDR of ref-cells.
;;
;; Adapted from UCAM-CL-TR-579 U.Cambridge Tech Report 579,
;; "Practical lock-freedom" by Keir Fraser, Feb 2004
;;
;; DM/RAL  02/17
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

(in-package #:mcas)
   
(declaim (optimize (speed 3) #|(safety 0)|# #+:LISPWORKS (float 0)))

;; -------------------------------------------------------------------------------------
;; MCAS can only be used on refs that can be sorted into total order.
;; All simple CAS ops must be performed using MCAS,
;; and all REF-VAL calls must be performed using MCAS-READ.

(defvar *mcas-index*  0)

(defstruct (mcas-ref
            (:include ref:ref)
            (:constructor mcas-ref (ref:val)))
  (id (incf *mcas-index*) :read-only t))

(defmethod ord:compare ((a mcas-ref) (b mcas-ref))
  (- (mcas-ref-id a) (mcas-ref-id b)))

;; ------------------
;; CCAS - Conditional CAS
;;
;; The job of CCAS is to conditionally acquire a reference on behalf
;; of an MCAS operation. The condition is that the MCAS operation must
;; still be in a state of :UNDECIDED.
;;
;; If this condition is met, we either acquire the ref cell with the
;; MCAS descriptor, or we fail because the ref cell did not contain
;; the expected old value
;;
;; If the CCAS acquires, but the condition is not met, it can only be
;; because another thread pushed us along to a :FAILED or :SUCCEEDED
;; resolution already. In other words we have already been through
;; here.  So even if we successfuly CAS again, we have to set the
;; value back to Old value.
;;

;; -------------------------------------------------------------

(defstruct ccas-desc
  old new pred)

(defun ccas-help (ref desc)
  ;; At this point we know that ref contained our desc. It might not
  ;; still contain it, if we got interrupted and another thread nudged
  ;; us to completion. In that case the following CAS will fail, but
  ;; that's okay.
  ;;
  ;; Nudge along a CCAS-DESC to resolve as either an MCAS-DESC, or the
  ;; old value of the ref
  (declare (ccas-desc desc))
  (let ((val  (if (funcall (ccas-desc-pred desc))
                  (ccas-desc-new desc)
                (ccas-desc-old desc))))
    ;;
    ;; If the ref cell still contains our CCAS desc, then this CAS
    ;; will succeeed.
    ;;
    ;; If not, then it is because another thread has already been
    ;; through here nudging our CCAS-DESC and succeeded.
    ;;
    (ref:basic-cas ref desc val)))

(defun ccas (ref old new pred)
  ;; CCAS -- conditional CAS, perform CAS only if PRED returns true.
  ;; Returns true if CAS was successful.
  (let ((desc  (make-ccas-desc
                :old   old
                :new   new
                :pred  pred)))
    (declare (ccas-desc desc))
    (um:nlet-tail iter ()
     (if (ref:basic-cas ref old desc)
         ;; at this point, we know the ref contains our desc, even if
         ;; we get interrupted and another thread performs it for us.
         (progn
           (ccas-help ref desc)
           t)
       ;;
       ;;               CAS succeeded
       ;;                     |  
       ;;              ref-val EQ old -> CCAS desc
       ;;               |     |    |                   
       ;;   MCAS :UNDECIDED   |   MCAS :FAILED
       ;;                     |
       ;;         MCAS :SUCCEEDED && old EQ new
       ;;
       ;; We got it! Either this is the first time
       ;; through with MCAS :UNDECIDED or, since the
       ;; old value was eq the expected old, the MCAS was
       ;; pushed along by another thread and the state
       ;; must now be :FAILED.
       ;;
       ;; (Or else, the planned new value was the same as
       ;; the old value and MCAS :SUCCEEDED. Either way,
       ;; it put back the old value.).
       ;;
       ;; In the first case, we can now replace our CCAS
       ;; descriptor with the caller's MCAS descriptor.
       ;;
       ;; In the second case, we must put back the old value.
       ;;
       
       ;; else
       (let ((v (ref:basic-val ref)))
         (when (ccas-desc-p v)
           (ccas-help ref v)
           (iter)))
       ))))

(defun ccas-read (ref)
  ;; Return either an mcas-desc or an old value.
  ;; This nudges along any ccas-desc that may be claiming the ref.
  (um:nlet-tail iter ()
    (let ((v (ref:basic-val ref)))
      (cond ((ccas-desc-p v)
             (ccas-help ref v)
             (iter))
            
            (t  v)
            ))))

;; ------------------
;; MCAS - Multiple CAS
;;
;; NOTE: Any ref that is used in an MCAS operation should really use
;; MCAS and MCAS-READ, even when not part of an ensemble (as in simple
;; CAS), and for querying the value.  This will detect MCAS in
;; progress and help it along for final resolution.

(defstruct mcas-desc
  triples
  (status   (ref:ref :undecided)))

(defun #1=mcas-help (desc)
  (declare (mcas-desc desc))
  (let ((triples  (mcas-desc-triples desc))
        (status   (mcas-desc-status desc)))

    (um:labels*
        ((undecided-p ()
           ;; can't be declared dynamic-extent
           (eq :undecided (ref:basic-val status)))
         
         (successful-p ()
           (eq :successful (ref:basic-val status)))

         (patch-fail ((ref old new))
           (declare (ignore new))
           (ref:basic-cas ref desc old))

         (patch-succeed ((ref old new))
           (declare (ignore old))
           (ref:basic-cas ref desc new))
         
         (decide (desired-state)
           (ref:basic-cas status :undecided desired-state)
           (let* ((success (successful-p))
                  (patchfn (if success #'patch-succeed #'patch-fail)))
             (map nil patchfn triples)
             (return-from #1# success)))
         
         (acquire ((ref old new))
           (declare (ignore new))
           (um:nlet-tail iter ()
             ;; we might be on a repeat visit here, so ignore the
             ;; outcome of CCAS and check explicitly.
             (unless (ccas ref old desc #'undecided-p)
               (let ((v (ref:basic-val ref)))
                 (cond
                  ((eq v desc))  ;; we were here before, we got it
                  
                  ((and (eq v old)
                        (undecided-p))
                   (iter))
                  
                  ((mcas-desc-p v)
                   ;; someone else is trying, help them out, then
                   ;; try again
                   (mcas-help v)
                   (iter))
                  
                  (t ;; not a descriptor, and not eq old with
                     ;; :undecided, so we must have missed our chance,
                     ;; or else we already resolved to :failed or
                     ;; :successful, and this decide will have no
                     ;; effect.
                     (decide :failed))
                  ))))))
      (declare (dynamic-extent #'successful-p
                               #'patch-fail  #'patch-succeed
                               #'decide      #'acquire))
      (when (undecided-p)
        (map nil #'acquire triples))
      (decide :successful)
      )))

(defun mcas (&rest triples)
  ;; triples - a sequence of (ref old new) as would be suitable for
  ;; CAS. But each ref must be a total-order MCAS-REF.
  (mcas-help (make-mcas-desc
              :triples (sort (apply 'um:triples triples)
                             '<
                             :key (lambda (tup)
                                    (mcas-ref-id (first tup)))
                             ))))

(defun mcas-read (ref)
  (um:nlet-tail iter ()
    (let ((v (ccas-read ref)))
      (cond ((mcas-desc-p v)
             (mcas-help v)
             (iter))
            
            (t  v)
            ))))

(defmethod ref:val ((m mcas-ref))
  (mcas-read m))

(defmethod ref:cas ((m mcas-ref) old new)
  (mcas m old new))

;; -------------------------------------------------------------------------------------

#|
(defun tst1 (&optional (n 1000000))
  (let ((a  (mcas-ref 1))
        (b  (mcas-ref 2))
        (ct 0))
    (loop repeat n do
          (loop until (mcas a 1 7
                            b 2 8))
          (incf ct)
          (mcas a 7 1
                b 8 2))
    ct))

(defun tstx (&optional (n 1000000))
  (let ((a  (mcas-ref 1))
        (b  (mcas-ref 2))
        (ct 0))
    (rch:spawn-process (lambda ()
                         (loop repeat n do
                               (loop until (mcas a 1 3
                                                 b 2 4))
                               (incf ct)
                               (mcas a 3 5
                                     b 4 6))))
    (loop repeat n do
          (loop until (mcas a 5 7
                            b 6 8))
          (incf ct)
          (mcas a 7 1
                b 8 2))
    ct))

(defun tstxx (&optional (n 1000000))
  (let ((a  (mcas-ref 1))
        (b  (mcas-ref 2))
        (ct 0))
    (rch:spawn-process (lambda ()
                         (loop repeat n do
                               (loop until (mcas a 1 3
                                                 b 2 4))
                               (incf ct)
                               (mcas a 3 5
                                     b 4 6))))
    (rch:spawn-process (lambda ()
                         (loop repeat n do
                               (loop until (mcas a 5 7
                                                 b 6 8))
                               (incf ct)
                               (mcas a 7 9
                                     b 8 10))))
    (loop repeat n do
          (loop until (mcas a 9 11
                            b 10 12))
          (incf ct)
          (mcas a 11 1
                b 12 2))
    ct))

(defun tst (&optional (n 1000000))
  (let* ((nmax (ash 1 32))
         (elts (loop for ix from 0 below n
                    collect (lw:mt-random nmax))))
    (print "Timing of Sets Add")
    (time
     (let ((s  (sets:empty)))
       (dolist (item elts)
         (sets:addf s item))
       (sets:elements s)))
    (print "Timing of Sort")
    (time
     (sort elts '<))
    (values)
    ))
(tst)
|#

  