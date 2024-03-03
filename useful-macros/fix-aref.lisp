;; fix-aref.lisp
;;
;; DM/RAL  2024/03/03 22:05:35 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.fix-aref
  (:use #:common-lisp))

(in-package #:com.ral.useful-macros.fix-aref)

;; ----------------------------------

#+:LISPWORKS
(lw:defadvice (SETF::|"COMMON-LISP" "AREF"| fixup-floats :around)
    (val arr ix)
  (handler-case
      (lw:call-next-advice val arr ix)
    (error (e)
      (handler-case
          (lw:call-next-advice (coerce val (array-element-type arr)) arr ix)
        (error ()
          (error e)))
      )))

#+:LISPWORKS
(lw:defadvice (make-array fixup-floats :around)
    (dims &rest args &key element-type initial-element initial-contents &allow-other-keys)
  (handler-case
      (apply #'lw:call-next-advice dims args)
    (error (e)
      (handler-case
          (progn
            (when initial-element
              (setf args (list* :initial-element (coerce initial-element element-type) args)))
            (when initial-contents
              (setf args (list* :initial-contents (map 'vector (um:rcurry #'coerce element-type) initial-contents) args)))
            (apply #'lw:call-next-advice dims args))
        (error ()
          (error e))
        ))
    ))

