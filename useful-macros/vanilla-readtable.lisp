;; vanilla-readtable.lisp
;;
;; DM/RAL  2026/05/27 01:58:42 UTC
;; ----------------------------------

(defpackage #:vanilla-readtable
  (:use #:common-lisp #:com.ral.useful-macros))

(in-package #:vanilla-readtable)

;; ----------------------------------

(defvar *vanilla-readtable*
  (copy-readtable nil))

(defvar *ral-syntax-readtable*
  (copy-readtable))

(defun update-ral-syntax ()
  (setf *ral-syntax-readtable* (copy-readtable)))

(defmacro using-ral-syntax ()
  `(um:eval-always
     (setf *readtable* (copy-readtable *ral-syntax-readtable*))
     ))

(defmacro using-standard-syntax ()
  `(um:eval-always
     (setf *readtable* (copy-readtable nil))))

;; --------------------------------------------

#|  ;; just use the Unicode chars ′ = U+2032, ″ = U+2033
(defun install-primed-symbols ()
  (let ((readtable (copy-readtable nil)))
    (set-macro-character #\' (get-macro-character #\' readtable) t readtable)
    (setf *vanilla-readtable* readtable))
  #|
  (set-macro-character #\' (get-macro-character #\') t)
  (set-macro-character #\" (get-macro-character #\") t)
  ;; now for Unicode variants
  (set-macro-character #\’ (get-macro-character #\’) t)
  (set-macro-character #\” (get-macro-character #\”) t)
  |#
  )

#+:ALLOW-PRIMED-SYMBOLS
(install-primed-symbols)
|#

;; --------------------------------------------

(defmacro with-vanilla-readtable (&body body)
  `(let ((*readtable*  *vanilla-readtable*))
     ,@body))

(defun report-attempt-to-write-vanilla-readtable (rt)
  (when (eq rt *vanilla-readtable*)
    (error "Vanilla Readtable is Read-Only!")))

;; --------------------------------------------

#+:LISPWORKS
(progn
  (lw:defadvice (set-dispatch-macro-character not-in-vanilla-readtable :before)
      (disp-char sub-char function &optional (readtable *readtable*))
    (declare (ignore disp-char sub-char function))
    (report-attempt-to-write-vanilla-readtable readtable))
  
  (lw:defadvice (set-macro-character not-in-vanilla-readtable :before)
      (char function &optional non-terminating-p (readtable *readtable*))
    (declare (ignore char function non-terminating-p))
    (report-attempt-to-write-vanilla-readtable readtable))

  (lw:defadvice (set-syntax-from-char not-in-vanilla-readtable :before)
      (to-char from-char &optional (to-readtable *readtable*) from-readtable)
    (declare (ignore to-char from-char from-readtable))
    (report-attempt-to-write-vanilla-readtable to-readtable)))

;; --------------------------------------------

#+:SBCL
(progn
  (defun check-set-mac-char (char function &optional non-terminating-p (readtable *readtable*))
    (declare (ignore char function non-terminating-p))
    (report-attempt-to-write-vanilla-readtable readtable))
  
  (defun check-set-disp-mac-char (char subchar function &optional (readtable *readtable*))
    (declare (ignore char subchar function))
    (report-attempt-to-write-vanilla-readtable readtable))

  (defun check-set-syntax-from-char (to-char from-char &optional (to-readtable *readtable*) from-readtable)
    (declare (ignore to-char from-char from-readtable))
    (report-attempt-to-write-vanilla-readtable to-readtable))
  
  (sb-ext:with-unlocked-packages (:cl)
    (cl-advice:make-advisable 'cl:set-macro-character
                              :arguments '(char function &optional non-terminating-p (readtable *readtable*))
                              :force-use-arguments t)
    (cl-advice:make-advisable 'cl:set-dispatch-macro-character
                              :arguments '(char subchar function &optional (readtable *readtable*))
                              :force-use-arguments t)
    (cl-advice:make-advisable 'cl:set-syntax-from-char
                              :arguments '(to-char from-char &optional (to-readtable *readtable*) from-readtable)
                              :force-use-arguments t)
    (cl-advice:add-advice :before 'cl:set-macro-character
                          #'check-set-mac-char)
    (cl-advice:add-advice :before 'cl:set-dispatch-macro-character
                          #'check-set-disp-mac-char)
    (cl-advice:add-advice :before 'cl:set-syntax-from-char
                          #'check-set-syntax-from-char)))
                               
;; --------------------------------------------

(let (#+:LISPWORKS (lw:*handle-warn-on-redefinition* nil))
  ;; Prevent printouts from using xxx\-\1\2\3 for xxx-123, etc.  ; <- make that magic invisible ;-)
  (defmethod print-object :around ((object symbol) out-stream)
    (with-vanilla-readtable
      (call-next-method))))
