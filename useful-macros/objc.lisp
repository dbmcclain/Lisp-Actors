
(defpackage :com.ral.useful-macros.objc
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.objc)

;; ------------------------------------------------------------
;; DM/RAL 09/22 - Try to make OBJC:INVOKE use Mac-like arguments.

(defun st-to-objc (&rest args)
  (let* ((grpd  (group args 2))
         (names (mapcar #'first grpd))
         (vals  (mapcar #'second grpd)))
    (cons (apply #'concatenate 'string names) vals)))

#|
(objc-invoke-st "initWithBitmapDataPlanes:" nil
            "pixelsWide:"      1
            "pixelsHigh:"      1
            "bitsPerSample:"   8
            "samplesPerPixel:" 4
            "hasAlpha:"        T
            "isPlanar:"        NIL
            "colorSpaceName:"  (ns-device-rgb-color-space)
            "bytesPerRow:"     4
            "bitsPerPixel:"    32)
 |#

(defmacro objc-invoke-st (&rest args)
  ;; i.e., using Smalltalk syntax
  `(objc:invoke ,@(apply #'st-to-objc args)))
