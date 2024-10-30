
(defpackage :com.ral.useful-macros.abbrev-bignums
  (:use #:common-lisp #:um)
  (:export
   #:with-abbrev
   #:without-abbrev
   #:abbrev-str
   #:abbrev-istr))

(in-package :com.ral.useful-macros.abbrev-bignums)

;; ------------------------------------------------------
;; Allow abbreviated display of all large integers, without having to
;; wrap them in special functions - this affects all displays,
;; printouts, inspectors, debuggers, etc. Very nice to have.

(defvar *print-bignum-abbrev*  nil)

(defmacro without-abbrev (&body body)
  `(let ((*print-bignum-abbrev* nil))
     ,@body))

(defmacro with-abbrev (&body body)
  `(let ((*print-bignum-abbrev* t))
     ,@body))

#+:LISPWORKS
(lw:defadvice
    (with-standard-io-syntax avoid-bignum-abbrev :around)
    (form env)
  `(let ((*print-bignum-abbrev* nil))
     ,(lw:call-next-advice form env)))

;; ---------------------------------------------------------
#| I don't need no steenking digits, man...
e.g.,
(setf *print-base* 16)
(setf *print-bignum-abbrev* nil)
*ed-r* -> 1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77965C4DFD307348944D45FD166C971
(setf *print-bignum-abbrev* t)
*ed-r* -> 1FFFFFF..166C971
|#

(defvar *abbrev-length*  7.)

(defun cut-and-splice (str)
  (concatenate 'string
               (subseq str 0 *abbrev-length*)
               ".."
               (subseq str (- (length str) *abbrev-length*))))
  
(defun max-unabbrev-length ()
  (* 2 (1+ *abbrev-length*)))

(defun abbrev-str (str)
  (if (<= (length str) (max-unabbrev-length))
      str
    (cut-and-splice str)))

(defun abbrev-istr (str)
  (if (< (length str) (max-unabbrev-length))
      (um:sepi str)
    (cut-and-splice str)))
         

(defun print-int-obj (obj &optional (stream *standard-output*))
  (let ((str (with-output-to-string (s)
               (without-abbrev
                 (prin1 obj s)))))
    (princ (abbrev-istr str) stream)
    obj
    ))

;; -------------------------------------------------------

#|
  ;; This version complies with the spec - PRIN1 should produce a machine readable display.
  ;; PRIN1 is for machines, PRINC is for humans.
  ;;
  ;; Unfortunately, the REPL appears to call PRIN1 after every eval.
  ;;
  ;; Both PRIN1 and PRINC appear to call PRINT-OBJECT for integers >= 2^64
  
#+:LISPWORKS
(lw:defadvice
    (prin1 avoid-bignum-abbrev :around)
    (obj &optional (stream *standard-output*))
  (without-abbrev
    (lw:call-next-advice obj stream)))
|#

#|
;; This version departs from the spec. Human friendly REPL displayed
;; results. If you need machine readable, use WITH-STANDARD-IO-SYNTAX
;; around the call to PRIN1.

#+:LISPWORKS
(lw:defadvice
    (prin1 bignum-around-princ :around)
    (obj &optional (stream *standard-output*))
  (if (or (not (integerp obj))
          *print-readably*
          (not *print-bignum-abbrev*))
      (lw:call-next-advice obj stream)
    (print-int-obj obj stream)))
|#

;; --------------------------------------------
(defun common-code (obj stream)
  (let ((str (without-abbrev
               (princ-to-string obj)
               )))
    (let ((*print-escape* nil))
      (print-object (abbrev-istr str) stream))
    obj))
;; --------------------------------------------
;; PRIN1
#|
#+:LISPWORKS
(lw:defadvice
    (prin1 bignum-around-princ :around)
    (obj &optional (stream *standard-output*))
  (cond ((and (integerp obj)
              (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))
|#
#|
(hcl:delete-advice prin1 bignum-around-princ)
|#

;; --------------------------------------------
;; PRINC
#|
#+:LISPWORKS
(lw:defadvice
    (princ bignum-around-princ :around)
    (obj &optional (stream *standard-output*))
  (cond ((and (integerp obj)
              (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))
|#
#|
(hcl:delete-advice princ bignum-around-princ)
|#

;; --------------------------------------------
;; Method PRINT-OBJECT (INTEGER T)
#|
#+:LISPWORKS
(lw:defadvice
    ((method print-object (integer t))
     bignum-around-print-object :around)
    (obj stream)
  (cond ((and (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))
|#
;; --------------------------------------------
;; Misc adjustments?

#+:LISPWORKS
(lw:defadvice
    (comm:open-tcp-stream no-abbrev :around)
    (&rest args)
  ;; COMM:OPEN-TCP-STREAM appears to call PRINC-TO-STRING with its
  ;; PORT argument for effect, and expects an unadulterated digit
  ;; string in return.
  (without-abbrev
    (apply #'lw:call-next-advice args)))
#|
(hcl:delete-advice comm:open-tcp-stream no-abbrev)
|#
;; --------------------------------------------

#+:LISPWORKS
(lw:defadvice
    (princ-to-string bignum-around :around)
    (obj)
  (without-abbrev
    (lw:call-next-advice obj)))

#+:LISPWORKS
(defmethod print-object ((obj integer) (stream lw-xp::xp-structure))
  (cond ((and (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (call-next-method)
        )))

#+:LISPWORKS
(lw:defadvice
    (princ bignum-around :around)
    (obj &optional (stream *standard-output*))
  (cond ((and (integerp obj)
              (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))

#+:LISPWORKS
(lw:defadvice
    (prin1 bignum-around :around)
    (obj &optional (stream *standard-output*))
  (cond ((and (integerp obj)
              (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))
#|
(hcl:delete-advice princ-to-string bignum-around)
(hcl:delete-advice prin1 bignum-around)
(hcl:delete-advice princ bignum-around)
|#
;; --------------------------------------------

#|
#+:LISPWORKS
(defmethod print-object ((obj integer) (stream editor::editor-output-stream))
  (cond ((and (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (call-next-method)
        )))
|#
#|
#+:LISPWORKS
(defmethod print-object ((obj integer) (stream editor::rubber-stream))
  (cond ((and (not *print-readably*)
              *print-bignum-abbrev*)
         (common-code obj stream))
        (t
         (call-next-method)
        )))
|#
#|
(trace (method print-object (integer lw-xp::xp-structure)))
(trace common-code)
(untrace)
(princ-to-string edec:*ed-r*)
(princ edec:*ed-r*)
(inspect *standard-output*)
|#

