;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :sdle-store)

(defun setup-special-floats ()
  (flet ((short-float-values ()
           (list (cons 'excl::*infinity-single* +short-float-inf+)
                 (cons 'excl::*negative-infinity-single +short-float-neg-inf+)
                 (cons 'excl::*nan-single* +short-float-nan+)))
         (single-float-values ()
           (list (cons 'excl::*infinity-single* +single-float-inf+)
                 (cons 'excl::*negative-infinity-single +single-float-neg-inf+)
                 (cons 'excl::*nan-single* +single-float-nan+)))
         (double-float-values ()
           (list (cons 'excl::*infinity-double* +double-float-inf+)
                 (cons 'excl::*negative-infinity-double* +double-float-neg-inf+)
                 (cons 'excl::*nan-double* +double-float-nan+)))
         (long-float-values ()
           (list (cons 'excl::*infinity-double* +long-float-inf+)
                 (cons 'excl::*negative-infinity-double* +long-float-neg-inf+)
                 (cons 'excl::*nan-double* +long-float-nan+))))
    (setf *special-floats*
          (append (short-float-values)
                  (single-float-values)
                  (double-float-values)
                  (long-float-values)))))

#|
(defstore-sdle-store (obj structure-object stream)
 (output-type-code +structure-object-code+ stream)
 (store-type-object obj stream))

(defrestore-sdle-store (structure-object stream)
  (restore-type-object stream))
|#

;; DM/RAL 07/09 -- better accommodation of unkown structure classes on restore

(defun structure-slot-names (class)
  (mapcar #'mop:slot-definition-name
	  (mop:class-slots class)))

(defstore-sdle-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (let ((slot-names (structure-slot-names (class-of obj))))
    (store-object (type-of obj) stream)
    (store-count (length slot-names) stream)
    (dolist (slot-name slot-names)
      (store-object slot-name stream))
    (dolist (slot-name slot-names)
      (store-object (slot-value obj slot-name) stream))))

(defrestore-sdle-store (structure-object stream)
  (let* ((class-name    (restore-object stream))
         (count         (read-count stream))
         (slot-names    (loop repeat count
                              collect (restore-object stream)))
         (class         (find-or-create-class class-name
					      'standard-object slot-names)))
    (cond ((eq (type-of class) 'structure-class)
           ;; we apparently found the class and it was a struture
           (let* ((new-instance  (allocate-instance class))
                  (allowed-slots (structure-slot-names class)))
             (resolving-object (obj new-instance)
               (dolist (slot-name slot-names)
                 (let ((val (restore-object stream)))
                   (when (member slot-name allowed-slots)
                     ;; slot-names are always symbols so we don't
                     ;; have to worry about circularities
                     (setting (slot-value obj slot-name) val))) ))
             (after-retrieve new-instance)
             new-instance))

          (t ;; else -- no such struture known to mankind
             ;; dummy up as a new standard-object instead of a struct
             ;; (to avoid non-portability issues regarding internals of struct implementation)
             (let ((new-instance (allocate-instance class)))
               (resolving-object (obj new-instance)
                 (dolist (slot-name slot-names)
                   (let ((val (restore-object stream)))
                     (when (slot-exists-p new-instance slot-name)
                       (setting (slot-value obj slot-name) val)) )))
               (after-retrieve new-instance)
               new-instance))
          )))

;; EOF
