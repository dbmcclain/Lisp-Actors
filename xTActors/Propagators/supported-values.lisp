;; supported-values.lisp -- Section 6.1 of Radul & Sussman - Supported with Provenance
;;
;; DM/RAL 02/22
;; ------------------------------------------------
;; Load this file, then execute contents of secion-6.1.lisp 

(in-package :propagators)

(defstruct (supported
            (:constructor supported (val sup)))
  val sup)

(defmethod contradictory? ((x supported))
  (contradictory? (supported-val x)))

(defun lset= (a b)
  (and (subsetp a b)
       (subsetp b a)))

(defmethod default-equal? ((a supported) (b supported))
  (or (eql a b)
      (and (default-equal? (supported-val a) (supported-val b))
           (lset= (supported-sup a) (supported-sup b)))
      ))

(defun more-informative-support? (s1 s2)
  ;; true if s1 is a strict subset of s2
  (and (subsetp (supported-sup s1) (supported-sup s2))
       (not (subsetp (supported-sup s2) (supported-sup s1)))
       ))
       
(defun implies? (v1 v2)
  ;; true if same or tighter interval from v1
  (default-equal? v1 (merge-info v1 v2)))

(defun merge-supports (&rest v&ss)
  (reduce 'union (mapcar 'supported-sup v&ss)))

(defun v&s-merge (v&s1 v&s2)
  (let* ((v&s1-val  (supported-val v&s1))
         (v&s2-val  (supported-val v&s2))
         (val-merge (merge-info  v&s1-val v&s2-val)))
    (cond ((default-equal? val-merge v&s1-val)
           (if (implies? v&s2-val val-merge)
               ;; Confirmation of existing information
               (if (more-informative-support? v&s2 v&s1)
                   v&s2
                 v&s1)
             ;; else - New information is not interesting
             v&s1))
          ((default-equal? val-merge v&s2-val)
           ;; New information overrides old information
           v&s2)
          (t
           ;; Interesting merge, need both provenances
           (supported val-merge
                      (merge-supports v&s1 v&s2)))
          )))

(defmethod merge-info ((a supported) b)
  (merge-info-supported b a))

(defmethod merge-info-number ((b supported) a)
  (v&s-merge (supported a nil) b))

(defmethod merge-info-interval ((b supported) a)
  (v&s-merge (supported a nil) b))

(defmethod merge-info-ball ((b supported) a)
  (v&s-merge (supported a nil) b))

(defgeneric merge-info-supported (b a)
  ;; here it is known that a is supported
  (:method (b a)
   (v&s-merge a (supported b nil)))
  (:method ((b (eql nothing)) a)
   a)
  (:method ((b supported) a)
   (v&s-merge a b)))

;; ------------------------------------------------------------
;; Operator extensions for Supported values

(defgeneric arith-val (x)
  (:method (x)
   x)
  (:method ((x supported))
   (supported-val x)))

(defgeneric provenance (x)
  (:method (x)
   nil)
  (:method ((x supported))
   (supported-sup x)))
 
(defun supported-binop (op a b)
  (supported (funcall op (arith-val a) (arith-val b))
             (union (provenance a) (provenance b))))

(defun supported-unop (op x)
  (supported (funcall op (arith-val x)) (provenance x)))

;; ----------------------------

(defmacro add-supported-binop (name)
  (let ((op-number    (um:symb name "-number"))
        (op-interval  (um:symb name "-interval"))
        (op-ball      (um:symb name "-ball"))
        (op-supported (um:symb name "-supported")))
    `(progn
       (defmethod ,name ((a supported) b)
         (,op-supported b a))
       (defgeneric ,op-supported (b a)
         ;; here it is known that a is supported
         (:method (b a)
          (supported-binop ',name a (supported b nil)))
         (:method ((b supported) a)
          (supported-binop ',name a b)))
       (defmethod ,op-number ((b supported) a)
         (supported-binop ',name (supported a nil) b))
       (defmethod ,op-interval ((b supported) a)
         (supported-binop ',name (supported a nil) b))
       (defmethod ,op-ball ((b supported) a)
         (supported-binop ',name (supported a nil) b)))
    ))

(add-supported-binop generic-+)
(add-supported-binop generic--)
(add-supported-binop generic-*)
(add-supported-binop generic-/)

;; ----------------------------

(defmethod generic-sq ((x supported))
  (supported-unop 'generic-sq x))

(defmethod generic-sqrt ((x supported))
  (supported-unop 'generic-sqrt x))
