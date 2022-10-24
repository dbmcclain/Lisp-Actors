;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :sdle-store)

;; Special float handling
(defun create-float-values (value &rest codes)
  (let ((neg-inf (expt value 3)))
    (mapcar 'cons
            (list (expt (abs value) 2)
                  neg-inf
                  (/ neg-inf neg-inf))
            codes)))

#|
;; Custom structure storing from Alain Picard.
(defstore-sdle-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (let* ((slot-names (structure:structure-class-slot-names (class-of obj))))
    (store-object (type-of obj) stream)
    (store-count (length slot-names) stream)
    (dolist (slot-name slot-names)
      (store-object slot-name stream)
      (store-object (slot-value obj slot-name) stream))))

(defrestore-sdle-store (structure-object stream)
  (let* ((class-name (restore-object stream))
         (class (find-class class-name))
         (length (read-count stream))
         (new-instance (structure::allocate-instance class)))
    (loop repeat length do
          (let ((slot-name (restore-object stream)))
            ;; slot-names are always symbols so we don't
            ;; have to worry about circularities
            (resolving-object (obj new-instance)
              (setting (slot-value obj slot-name) (restore-object stream)))))
    new-instance))
|#
#| |#
;; DM/RAL 07/09 -- better accommodation of unkown structure classes on restore

(defstore-sdle-store (obj structure-object stream)
  (let ((obj  (before-store obj)))
    (cond ((typep obj 'structure-object)
           (output-type-code +structure-object-code+ stream)
           (let* ((obj-class  (class-of obj))
                  (class-name (class-name obj-class))
                  (slot-names (structure:structure-class-slot-names obj-class)))
             (store-object class-name stream)
             (store-count (length slot-names) stream)
             (dolist (slot-name slot-names)
               (store-object slot-name stream))
             (dolist (slot-name slot-names)
               (store-object (slot-value obj slot-name) stream))))
          (t
           (store-object obj stream))
          )))

(defrestore-sdle-store (structure-object stream)
  (let* ((class-name    (restore-object stream))
         (count         (read-count stream))
         (slot-names    (loop repeat count
                              collect (restore-object stream)))
         (class         (find-or-create-class class-name 'standard-object slot-names)))
    (cond ((eq (type-of class) 'structure-class)
           ;; we apparently found the class and it was a struture
           (let* ((new-instance  (structure::allocate-instance class))
                  (allowed-slots (structure:structure-class-slot-names class)))
             (resolving-object (obj new-instance)
               (dolist (slot-name slot-names)
                 (let ((val (restore-object stream))
                       the-slot)
                   (when (setf the-slot (car (member slot-name allowed-slots :test #'string-equal)))
                     ;; slot-names are always symbols so we don't
                     ;; have to worry about circularities
                     (setting (slot-value obj the-slot) val))) ))
             (after-retrieve new-instance)))

          (t ;; else -- no such struture known to mankind
             ;; dummy up as a new standard-object instead of a struct
             ;; (to avoid non-portability issues regarding internals of struct implementation)
             (let ((new-instance (allocate-instance class)))
               (resolving-object (obj new-instance)
                 (dolist (slot-name slot-names)
                   (let ((val (restore-object stream)))
                     (when (clos:slot-exists-p new-instance slot-name)
                       (setting (slot-value obj slot-name) val)) )))
               (after-retrieve new-instance)))
          )))
#| |#

;; EOF
