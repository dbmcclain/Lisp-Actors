;; wrap-mixin.lisp - change class of instances to include a mixin class
;;
;; DM/RAL  2025/11/10 00:20:27 UTC
;; ----------------------------------

(defpackage #:com.ral.useful-macros.wrap-mixin
  (:use #:common-lisp #:um))

(in-package #:com.ral.useful-macros.wrap-mixin)

;; ----------------------------------

(defun wrap-instance-with-mixin (obj mixin-class &key name-prefix class-name)
  (let* ((obj-class        (class-of obj))
         (mixin-class      (if (symbolp mixin-class)
                               (find-class mixin-class)
                             mixin-class))
         (mixin-class-name (class-name mixin-class))
         (class-name       (cond ((null class-name)
                                  (let ((obj-class-name (class-name obj-class)))
                                    (intern (concatenate 'string
                                                         (or (and name-prefix
                                                                  (string name-prefix))
                                                             (symbol-name mixin-class-name))
                                                         "-"
                                                         (package-name (symbol-package obj-class-name))
                                                         "-"
                                                         (symbol-name obj-class-name))
                                            (symbol-package mixin-class-name))
                                    ))
                                 
                                 ((symbolp class-name)
                                  class-name)
                                 
                                 (t
                                  (intern (string class-name)
                                          (symbol-package mixin-class-name)))
                                 ))
         (augm-class       (#+:LISPWORKS clos:ensure-class
                            #+:SBCL      sb-mop:ensure-class
                            class-name
                            :direct-superclasses (list mixin-class obj-class)
                            :metaclass           (class-of obj-class))
                           ))
    (change-class obj augm-class)
    ))
    
