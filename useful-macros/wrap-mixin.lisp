;; wrap-mixin.lisp - change class of instances to include a mixin class
;;
;; DM/RAL  2025/11/10 00:20:27 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.wrap-mixin
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.wrap-mixin)

;; ----------------------------------

(defun wrap-instance-with-mixin (obj mixin-class &key class-name)
  (let* ((mixin-class  (if (symbolp mixin-class)
                           (find-class mixin-class)
                         mixin-class))
         (obj-class    (class-of obj))
         (class-name   (cond ((null class-name)
                              (intern (concatenate 'string
                                                   (string (class-name mixin-class))
                                                   "-"
                                                   (string (class-name obj-class)))
                                      (symbol-package (class-name mixin-class))))
                             
                             ((symbolp class-name)
                              class-name)

                             (t
                              (intern (string class-name)
                                      (symbol-package (class-name mixin-class))))
                             ))
         (bridge-class  (#+:LISPWORKS clos:ensure-class
                         #+:SBCL      sb-mop:ensure-class
                                      class-name
                                      :direct-superclasses (list mixin-class obj-class)
                                      :metaclass           (class-of obj-class))))
    (change-class obj bridge-class)))


