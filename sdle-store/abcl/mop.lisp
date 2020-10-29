;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :sdle-store)

(defmacro use-primitive (partial-name)
  (let* ((pname (symbol-name partial-name))
         (standard-name (symbolicate "SLOT-DEFINITION-" pname))
         (primitive (find-symbol
                     (format nil "%SLOT-DEFINITION-~a" pname)
                     :system)))
    `(defmethod ,standard-name (slotdef)
       (,primitive slotdef))))

(use-primitive name)
(use-primitive allocation)
(use-primitive initform)
(use-primitive initargs)
(use-primitive readers)
(use-primitive writers)

(defun class-slots (object)
  (system:%class-slots object))

;; This doesn't seem to be available in ABCL
(defmethod slot-definition-type (slotdef)
  t)

;; EOF