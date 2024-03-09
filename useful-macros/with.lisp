 ;; with.lisp - OCaml-style WITH for structs
;;
;; DM/RAL  2024/03/09 12:51:19 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.with
  (:use #:common-lisp #:com.ral.useful-macros))

(in-package #:com.ral.useful-macros.with)

;; ----------------------------------

(defmethod with ((obj structure-object) &rest props)
  (let* ((new  (copy-structure obj)))
    (nlet iter ((props props))
      (if (endp props)
          new
        ;; else
        (destructuring-bind (sym val . rest) props
          (let ((slot-name (find-symbol (symbol-name sym)
                                        (symbol-package (class-name (class-of obj))))))
            (setf (slot-value new slot-name) val)
            (go-iter rest)))
        ))))

#|
(defstruct thing
  a b c)

(let* ((x  (make-thing :a 1 :b 2 :c 3)))
  (with x
    :a 15))
|#