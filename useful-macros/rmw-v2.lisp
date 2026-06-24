;; rmw.lisp -- Read/Modify/Write for general places
;;
;; Lock-free, ABA hazard-free, guaranteed mutation in SMP
;;
;; DM/RAL  11/20
;; -----------------------------------------------------------
(defpackage #:com.ral.useful-macros.rmw-v2
  (:use :common-lisp #:um))

(in-package #:com.ral.useful-macros.rmw-v2)

;; ----------------------------------------------------------
;; ABA Problems and Common Lisp
;;
;; I contend that ABA problems cannot happen in Common Lisp because...
;;
;; In order to perform CAS on a pointer, you have to grab a copy of
;; the pointer value for use as the old value in the CAS comparison
;; operation.
;;
;; ABA problems occur when, after grabbing a copy, the item becomes
;; deleted, a new and different object gets created using the same
;; memory as the old item (possible due to MRU allocation), and then
;; the pointer, with the same address, gets put back in place before
;; we execute the CAS comparison. The CAS completes successfully and
;; we go on thinking that our copy of the old item is the picture of
;; what is actually there.
;;
;; The issue is that what looks like ABA is really ABC where Addr(C) =
;; Addr(A). CAS sees ABA and produces ABX, where X is our locking
;; placeholder value while we work with A. That CAS operation has just
;; discarded C. Hence an ABA Problem.
;;
;; If we now modify A to become A′, and then use CAS to move from ABX
;; to ABA′ and then exit, we will have updated the system based on
;; stale information and discarded the true state of affairs.
;;
;; But this cannot happen in Common Lisp becaues the old item can
;; never have been deleted and had its memory re-used for a different
;; object. As long as there are extant references to data, that data
;; memory will never be discarded by the GC and then reused for
;; allocation.
;;
;; Object A can never become C before the CAS, but A could become A'
;; where Addr(A') = Addr(A), e.g., the third element of a list could
;; have been mutated without changing the address of the head of the
;; list.  But we still have a reference to the list head as our old
;; value, and any work we do on the list reflects its actual contents.
;;
;; So there can be no ABA problems in Lisp.
;;
;; ----------------------------------------------------------
;; The 2-phase CAS protocol -- used by lock-free algorithms to help
;; speed along forward progress.
;;
;; If we succeed in CAS but our thread gets delayed while other
;; threads get the CPU, the other threads will see a lock value in the
;; CAS cell and be put-off from working until our original thread
;; removes the lock.
;;
;; So in order to prevent this kind of stall, the lock value becomes
;; an object that contains the old value prior to CAS, and a pointer
;; to the update function. That update function should be idempotent
;; and purely functional because it may be executed more than once.
;;
;; If the original thread gets delayed and another thread comes along
;; to look at the pointer value, it will see the lock object and can
;; help speed progress along by performing the update on behalf of the
;; first thread, and then it can get the value it wants for itself.
;;
;; The twist here with RMW operations is that we often need more than
;; one data value from the Read to properly perform a Modify/Write.
;;
;; For example, popping an item from the head of a list pointed to by
;; a Ref cell: the RMW needs the CDR of the list for storage back into
;; the Ref cell, while the user needs the value of the CAR cell that was
;; popped off. So the update function used by RMW must supply 2
;; values.
;;
;; That means that we can't just have another thread finish the update
;; and stuff the new head address into the Ref cell for us. That
;; wouldn't produce the CAR value we need for the original caller.
;;
;; So we resort to a secondary mechanism where any answers produced
;; keeps a list of all values returned by the update function. The
;; first item should always be what the unlocking CAS needs to see for
;; the new value in the Ref Cell. And that is the value sought by any
;; other reader threads. The remaining values pertain only to the
;; original caller in the first thrad, and must be retained until the
;; first thread runs again.
;;
;; But in following the 2-phase CAS protocol, we can no longer simply
;; read nor write the value in a shared location.  It might be in a
;; state that is being updated by another thread. If so, we nudge it
;; along to final resolution before acquiring/setting its value.
;;
;; As long as all modifications to "place" are performed with WR or
;; RMW then we can be assured that the protocol is being followed.
;; Reading of place should be performed with RD, which helps push
;; along any update in progress before returning its stable value.
;;
;; Mutations performed with WR and RMW force all pending reads/writes
;; to be completed, and ensures cache coherency with other cores by
;; invalidating their cache lines for mutated place.
;;
;; ----------------------------------------------------------------
;; Assured lock-free, ABA hazard immune, mutation
;;
;; We need two primitives for every class of object:
;;  (BASIC-VAL obj) - returns the value contained in obj at this moment.
;;  (BASIC-CAS obj old new) - accomplishes a CAS on obj, returning T/F.
;;
;; We can't really know what value is held in obj for any length of
;; time after our mutation of it.  Another thread could come along and
;; mutate right after we did.  So what we return is the value that was
;; stored but which may not reflect reality by the time you look
;; again.  When you need to know what value is held in obj, perform a
;; RD on it to get the value it had at the time of the RD call.

;; ---------------------------------------------
;; This version uses a 2-phase approach, where competing threads help
;; complete an attempt in progress before launching their own.
;;
;; It has a theoretically bounded number of CAS attempts (= N+1) for N
;; competing threads. If a thread cannot complete, another thread will
;; do so for it.

(defgeneric rd-object   (obj))
(defgeneric wr-object   (obj new))
(defgeneric rmw-object  (obj new-fn))
(defgeneric cas-object  (obj old new))
(defgeneric atomic-exch-object (obj new))

;; --------------------------------------------------------------------------

(eval-always
  (defvar *rmw-functions* (make-hash-table))
  
  (defun gen-fn-name (kind accessor)
    (intern (format nil "~A-~A"
                    (string kind)
                    (string accessor))
            ))
  
  (defun gen-fn-names (accessor)
    (values (gen-fn-name :rd accessor)
            (gen-fn-name :rmw accessor))))
  
(defun add-rmw-functions (accessor rd-fn rmw-fn)
  ;; RMW functions should always be added in pairs for each accessor,
  ;; since RMW depends on RD
  (setf (gethash accessor *rmw-functions*) (cons rd-fn rmw-fn)))

#|
(setf *rmw-functions* (make-hash-table))                      
|#
;; -----------------------------------------------------------------------------------
;; Define generalized RMW functions with logic defined just once for all cases.
;; Extend the defns to allow for aux return values from a successful RMW op.
;; --------------------------------------------
;; The potential need for multiple return values from RMW mandates
;; that we use a 2-phase RMW protocol.
;;
;; Example: Consider a simple LIST used as a queue. You want to pop an
;; item off the queue and return both the value, and a flag indicating
;; if there were actually something on the queue when, perhaps, NIL
;; was the value. Otherwise, return a false flag if the queue were
;; empty.
;;
;; And you want this information from an atomic RMW to pop the LIST.
;; So the RMW mutator function has to return 3 items: the new List
;; head, the value that was at the head, and a true flag, or else just
;; return a NIL for all 3 items if the queue were empty.
;;
;; So once we have ownership of the queue inside an RMW, which we do
;; by atomically swapping the queue LIST with an RMW-DESC structure,
;; we need to supply 3 return items to the original caller: the CDR of
;; the queue LIST, the CAR (value of the queue entry at the head), and
;; a true flag. Or else, for an empty list a NIL for all 3 values.
;;
;; But if another thread comes along before we can finish the RMW, and
;; tries to RMW or RD the queue, will see the RMW-DESC and so tries to
;; help us finish our RMW in order to obtain a RD value.
;;
;; But that second thread also has to copy the extra needed return
;; values and stash them away for eventual return to the original RMW
;; thread. It does that by storing the multiple values into an RMW-ANS
;; struct and atomically replacing the RMW-DESC in the queue cell with
;; the RMW-ANS struct, before returning the RD value to the thread
;; caller.
;;
;; Any other threads that might want to RD the queue cell will now see
;; the RMW-ANS struct and can grab the new queue cell value from the
;; stored args in the struct.
;;
;; Any other threads that might want to RMW the queue cell, upon
;; seeing the RMW-ANS, will have to wait until the original RMW thread
;; finishes, as indicated by the fact that the RD value does not match
;; the RMW-ANS struct contained in the queue cell during an initial
;; CAS operation.
;;
;; Once the original RMW thread resumes and sees that his RMW-DESC has
;; been replaced by a RMW-ANS struct it can now return the multiple
;; values that it needs, after atomically swapping out the RMW-ANS
;; struct with the new queue cell value.
;;
;; ----------------------------------------------------

(defstruct rmw-desc
  old      ;; captured old value
  new-fn)  ;; mutator function
           ;; - Warning! can be called multiple times and from arbitrary threads

(defstruct rmw-ans
  ans)     ;; multiple-value-list of return vals from successful RMW

(defun rd-gen (rdr-fn cas-fn)
  #F
  (prog ()
    AGAIN
    (let ((v (funcall rdr-fn)))
      (cond ((rmw-desc-p v)
             ;; Try to finish up the RMW for the current owner.
             (let*  ((ans  (multiple-value-list 
                            (funcall (rmw-desc-new-fn v) (rmw-desc-old v))))
                     (rans (make-rmw-ans
                            :ans  ans)))
               (cond ((funcall cas-fn v rans)
                      ;; We provided the ans
                      (return (car ans)))
                     (t
                      ;; Someone else finished before we could.
                      ;; Retry the READ.
                      (go AGAIN))
                     )))
            ((rmw-ans-p v)
             ;; RMW was answered but not finished
             (return (car (rmw-ans-ans v))))
            (t
             ;; Normal READ
             (return v))
            ))))

(defun rmw-gen (rdr-fn cas-fn new-fn)
  ;; NEW-FN must be side effect free. May be called more than once.
  #F
  (let ((desc (make-rmw-desc
               :new-fn new-fn)))
    ;; NOTE: desc cannot be dynamic-extent
    (prog ()
      AGAIN
      (let ((old (rd-gen rdr-fn cas-fn)))
        (setf (rmw-desc-old desc) old)  ;; must save old first
        (cond ((funcall cas-fn old desc)
               ;; The place is ours now...
               (let* ((ans (multiple-value-list (funcall new-fn old)))
                      (new (car ans)))
                 (if (funcall cas-fn desc new)
                     (return (values-list ans))
                   ;; else - someone else answered for us.
                   (let* ((rans (funcall rdr-fn)) ;; has to be a RMW-ANS struct
                          (ans  (rmw-ans-ans rans))
                          (new  (car ans)))
                     (funcall cas-fn rans new)
                     (return (values-list ans))
                     ))))
              ((rmw-ans-p (funcall rdr-fn))
               ;; Someone else has a pending RMW
               (mpc:process-allow-scheduling)
               (go AGAIN))
              (t
               ;; else - couldn't own place, try again
               (go AGAIN))
              )))))

;; ----------------------------------------------------------
;; helpers

(defmacro rmw-template (place rmw-fn &rest args)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place)
    (declare (ignore store-vars writer-form))
    (with-unique-names (old new rdr-fn cas-fn)
      `(let* ,(mapcar #'list vars vals)
         (flet ((,rdr-fn ()
                  ,reader-form)
                (,cas-fn (,old ,new)
                  (cas ,reader-form ,old ,new)))
           (declare (dynamic-extent #',rdr-fn #',cas-fn))
           (,rmw-fn #',rdr-fn #',cas-fn ,@args)))
      )))

;; ----------------------------------------------------------
;; User level macros
;;
;; Warning! These all accept a place argument. Treat them like SETF.
;; That also means that a bare symbol argument will be accepted, but
;; does not denote the value, but rather the symbol-value place.
;;
;; And since LW does not accept lexical vars as valid targets of
;; COMPARE-AND-SWAP, these cannot either.

(defmacro rd (place)
  (if (symbolp place)
      `(rd-symbol-value ',place)
    (if-let (pair (gethash (car place) *rmw-functions*))
        `(,(car pair) ,@(cdr place))
      `(rmw-template ,place rd-gen)
      )))

(defmacro rmw (place new-fn)
  (if (symbolp place)
      `(rmw-symbol-value ',place ,new-fn)
    (if-let (pair (gethash (car place) *rmw-functions*))
        `(,(cdr pair) ,@(cdr place) ,new-fn)
      `(rmw-template ,place rmw-gen ,new-fn)
      )))


(defmacro wr (place new)
  ;; We need to abide by the RMW protocol. An RMW might be under way,
  ;; and the caller might be depending on the answer to their op.
  `(rmw ,place (constantly ,new)))

(defsetf rd wr)

#|
(defmacro ???
  `(setf (mpcompat:globally-accessible ,place) ,new))
|#

(defmacro cas (place old new)
  `(mpcompat:compare-and-swap ,place ,old ,new))

(defmacro atomic-exch (place new)
  `(mpcompat:atomic-exchange ,place ,new))

;; ----------------------------------------------------
;; Pre-built RD,RMW for common forms

(defmacro define-rmw-functions (accessor-form)
  (destructuring-bind (accessor . args) accessor-form
    (multiple-value-bind (rd-name rmw-name) (gen-fn-names accessor)
      (with-unique-names (new-fn)
        `(progn
           (defun ,rd-name ,args
             (rmw-template ,accessor-form rd-gen))
           (defun ,rmw-name (,@args ,new-fn)
             (rmw-template ,accessor-form rmw-gen ,new-fn))
           (add-rmw-functions ',accessor ',rd-name ',rmw-name))
        ))))

(define-rmw-functions (car cons))
(define-rmw-functions (cdr cons))
#-:ALLEGRO
(define-rmw-functions (symbol-value sym))
#+:ALLEGRO
(define-rmw-functions (sys:global-symbol-value sym))
(define-rmw-functions (svref svec ix))
(define-rmw-functions (slot-value struct slot-name))

;; -----------------------------------------------------------------------------------

#|
(define-struct-rmw-functions ref:ref-val (ref:ref ref:val))

(mpcompat:atomic-exchange (diddly doodad) new)
(let ((x 15))
  (mpcompat:compare-and-swap x 32 16))
(cas (ref:ref-val x) old new)
(let ((x 55))
  (cas x old new))
(cas *xrdf-expansions* old new)
(cas (diddly doodad) old new)
(rdf *rmw-tbl*)
(rdf (symbol-value '*x*))
(rdf (ref:ref-val r))

(rmwf (ref:ref-val r) new-fn)

(macroexpand '(mpcompat:compare-and-swap (ref-val r) old new))
(macroexpand '(mpcompat:compare-and-swap *print-base* old new))
(macroexpand '(mpcompat:compare-and-swap (svref x 15) old new))
(defun tst (accessor)
  (let* ((exp (macroexpand-1 `(mpcompat:compare-and-swap ,accessor old new)))
         (form (third exp)))
    (values (car form) (third form))))
(tst '*print-base*)
(tst '(car x))
(tst '(cdr x))
(tst '(svref x 15))
(tst '(ref-val r))
|#
