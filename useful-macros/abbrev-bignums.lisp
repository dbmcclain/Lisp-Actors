
(defpackage :com.ral.useful-macros.abbrev-bignums
  (:use #:common-lisp #:um)
  (:export
   #:with-abbrev
   #:without-abbrev
   #:abbrev-str))

(in-package :com.ral.useful-macros.abbrev-bignums)

;; ------------------------------------------------------
;; Allow abbreviated display of all large integers, without having to
;; wrap them in special functions - this affects all displays,
;; printouts, inspectors, debuggers, etc. Very nice to have.

(defvar *print-bignum-abbrev*  t)

#| I don't need no steenking digits, man...
e.g.,
(setf *print-base* 16)
(setf *print-bignum-abbrev* nil)
*ed-r* -> 1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77965C4DFD307348944D45FD166C971
(setf *print-bignum-abbrev* t)
*ed-r* -> 1FFFFFF..166C971
|#

(defvar *max-unabbrev-length*  17.)
(defvar *abbrev-length*         7.)

(defun abbrev-str (str)
  (let ((len (length str)))
    (if (< len *max-unabbrev-length*)
	(um:sepi str 
		 :count (if (eql *print-base* 16.) 4. 5.))
      (concatenate 'string
              (subseq str 0 *abbrev-length*)
              ".."
              (subseq str (- len *abbrev-length*)))
      )))

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

#+:LISPWORKS
(lw:defadvice
    (prin1 avoid-bignum-abbrev :around)
    (obj &optional stream)
  (without-abbrev
    (lw:call-next-advice obj stream)))

#+:LISPWORKS
(lw:defadvice
    (princ bignum-around-princ :around)
    (obj &optional stream)
  (cond
   ((integerp obj)
    (if (or *print-readably*
            (not *print-bignum-abbrev*))
        (lw:call-next-advice obj stream)
      (print-object obj (or stream
                            *standard-output*))
      ))
   (t
    (lw:call-next-advice obj stream))
   ))

#+:LISPWORKS
(lw:defadvice
    ((method print-object (integer t))
     bignum-around-print-object :around)
    (obj out-stream)
  (if (or *print-readably*
          (not *print-bignum-abbrev*))
      (lw:call-next-advice obj out-stream)
    (let ((str (with-output-to-string (s)
                 (prin1 obj s))))
      (princ (abbrev-str str) out-stream)
      obj)))

