;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :sdle-store)

(defstore-sdle-store (obj structure-object stream)
 (output-type-code +structure-object-code+ stream)
 (store-type-object obj stream))

(defrestore-sdle-store (structure-object stream)
  (restore-type-object stream 'structure-object))

; EOF
