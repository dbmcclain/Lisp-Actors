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
;; To overcome the RMW ABA-Problem...
;;
;; We need to guard against ABA hazards during RMW. So we perform RMW
;; with a 2-phase protocol: First acquire its value, while marking
;; place as in-progress for update. The marking is done with a
;; descriptor that enables any thread to help toward completion if we
;; get interrupted. Then carry out the computation of the new value
;; and perform the update, if it hasn't already been done by another
;; thread on our behalf. This protocol requires two CAS operations.
;;
;; We can no longer simply read nor write the value in a shared
;; location.  It might be in a state that is being updated by another
;; thread. If so, we nudge it along to final resolution before
;; acquiring/setting its value.
;;
;; As long as all modifications to "place" are performed with WR or
;; RMW then we can be assured that there will be no ABA hazards.
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
        (if (funcall cas-fn old desc)
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
                  )))
          ;; else - couldn't own place, try again
          (go AGAIN))
        ))))

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
    (remhash accessor *rmw-functions*) ;; to permit redefs
    (multiple-value-bind (rd-name rmw-name) (gen-fn-names accessor)
      (with-unique-names (new-fn)
        `(progn
           (defun ,rd-name ,args
             (rd ,accessor-form))
           (defun ,rmw-name (,@args ,new-fn)
             (rmw ,accessor-form ,new-fn))
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
