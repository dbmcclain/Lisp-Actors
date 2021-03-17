;; rmw.lisp -- Read/Modify/Write for general places
;;
;; Lock-free, ABA hazard-free, guaranteed mutation in SMP
;;
;; DM/RAL  11/20
;; -----------------------------------------------------------
(in-package :um)
;; ----------------------------------------------------------
;; To overcome the RMW ABA-Problem...
;;
;; We need to guard against ABA hazards during RMW. So we perform RMW
;; with a 2-phase protocol: First acquire its value, while marking
;; place as in-progress for update. The marking is done with a
;; descriptor that enables any thread to help toward completion if we
;; get interrupted. Then carry out the computation of the new value
;; and perform the update, if it hasn't already been done by anther
;; thread on our behalf. This protocol requires two CAS operations.
;;
;; We can no longer simply read nor write the value in a shared
;; location.  It might be in a state that is being updated by another
;; thread. If so, we nudge it along to final resolution before
;; acquiring/setting its value.
;;
;; As long as all modofiations to place are performed with WR or RMW
;; then we can be assured that there will be no ABA hazards. Reading
;; of place should be performed with RD, which helps push along any
;; update in progress before returning its stable value.
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
;; mutate right after we did.  So we break precedent with SETF and
;; don't bother returning what we just set it to. When you need to
;; know what value is held in obj, perform a RD on it to get the value
;; it had at the time of the RD call.

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
  (setf (gethash accessor *rmw-functions*) (list rd-fn rmw-fn)))

;; -----------------------------------------------------------------------------------
;; Define generalized RMW functions with logic defined just once for all cases

(defstruct rmw-desc
  old     ;; captured old value
  new-fn) ;; mutator function
          ;; - Warning! can be called multiple times and from arbitrary threads

(defun rd-gen (rdr-fn cas-fn)
  (prog ()
    again
    (let ((v (funcall rdr-fn)))
      (cond ((rmw-desc-p v)
             (let ((new (funcall (rmw-desc-new-fn v) (rmw-desc-old v))))
               (if (funcall cas-fn v new)
                   (return new)
                 (go again))))
            (t
             (return v))
            ))))

(defun rmw-gen (rdr-fn cas-fn new-fn)
  (let ((desc (make-rmw-desc
               :new-fn new-fn)))
    (prog ()
      again
      (let ((old (rd-gen rdr-fn cas-fn)))
        (setf (rmw-desc-old desc) old)
        (if (funcall cas-fn old desc)
            (funcall cas-fn desc (funcall new-fn old))
          (go again))
        ))))

;; ----------------------------------------------------------
;; helpers

(defmacro rmw-template (place rmw-fn &rest args)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place)
    (declare (ignore store-vars writer-form))
    (lw:with-unique-names (old new rdr-fn cas-fn)
      `(let* ,(mapcar #'list vars vals)
         (flet ((,rdr-fn ()
                  ,reader-form)
                (,cas-fn (,old ,new)
                  (sys:compare-and-swap ,reader-form ,old ,new)))
           (,rmw-fn #',rdr-fn #',cas-fn ,@args)))
      )))

;; ----------------------------------------------------------
;; User level macros

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
        `(,(cadr pair) ,@(cdr place) ,new-fn)
      `(rmw-template ,place rmw-gen ,new-fn)
      )))

(defmacro wr (place new)
  `(setf (sys:globally-accessible ,place) ,new))

(defmacro cas (place old new)
  `(sys:compare-and-swap ,place ,old ,new))

(defmacro atomic-exch (place new)
  `(sys:atomic-exchange ,place ,new))

;; ----------------------------------------------------
;; Pre-built RD,RMW for common forms

(defmacro define-rmw-functions (accessor-form)
  (destructuring-bind (accessor . args) accessor-form
    (remhash accessor *rmw-functions*) ;; to permit redefs
    (multiple-value-bind (rd-name rmw-name) (gen-fn-names accessor)
      (lw:with-unique-names (new-fn)
        `(progn
           (defun ,rd-name ,args
             (rd ,accessor-form))
           (defun ,rmw-name (,@args ,new-fn)
             (rmw ,accessor-form ,new-fn))
           (add-rmw-functions ',accessor ',rd-name ',rmw-name))
        ))))

(define-rmw-functions (car cons))
(define-rmw-functions (cdr cons))
(define-rmw-functions (symbol-value sym))
(define-rmw-functions (svref svec ix))

;; -----------------------------------------------------------------------------------

#|
(define-struct-rmw-functions ref:ref-val (ref:ref ref:val))

(sys:atomic-exchange (diddly doodad) new)
(let ((x 15))
  (sys:compare-and-swap x 32 16))
(cas (ref:ref-val x) old new)
(let ((x 55))
  (cas x old new))
(cas *xrdf-expansions* old new)
(cas (diddly doodad) old new)
(rdf *rmw-tbl*)
(rdf (symbol-value '*x*))
(rdf (ref:ref-val r))

(rmwf (ref:ref-val r) new-fn)

(macroexpand '(sys:compare-and-swap (ref-val r) old new))
(macroexpand '(sys:compare-and-swap *print-base* old new))
(macroexpand '(sys:compare-and-swap (svref x 15) old new))
(defun tst (accessor)
  (let* ((exp (macroexpand-1 `(sys:compare-and-swap ,accessor old new)))
         (form (third exp)))
    (values (car form) (third form))))
(tst '*print-base*)
(tst '(car x))
(tst '(cdr x))
(tst '(svref x 15))
(tst '(ref-val r))
|#
