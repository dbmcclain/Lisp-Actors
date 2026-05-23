;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package #:com.ral.useful-macros)
;; -------------------------------------------------------------------

(defun map-array (fn arr)
  (let ((new  (make-array (array-dimensions arr)
                          :element-type (array-element-type arr)
                          :adjustable   (adjustable-array-p arr)
                          :fill-pointer (and (adjustable-array-p arr)
                                             (fill-pointer arr)))
              ))
    (if (cdr (array-dimensions arr))
        (let ((rma-src (make-array (array-total-size arr)
                                   :element-type (array-element-type arr)
                                   :displaced-to arr
                                   :displaced-index-offset 0))
              (rma-dst (make-array (array-total-size arr)
                                   :element-type (array-element-type arr)
                                   :displaced-to new
                                   :displaced-index-offset 0)))
          (map-into rma-dst fn rma-src))
      ;; else
      (map-into new fn arr))
    new))

(defun skip-underscore (ans)
  (cond ((null ans)
         nil)
        ((consp ans)
         (cons (skip-underscore (car ans))
               (skip-underscore (cdr ans))))
        ((arrayp ans)
         (map-array #'skip-underscore ans))
        ((symbolp ans)
         (or (um:read-extended-number-syntax (symbol-name ans))
             ans))
        (t
         ans)
        ))

(defmacro with-extended-number-syntax (&body body)
  `(progn
     ,@(mapcar #'skip-underscore body)))

