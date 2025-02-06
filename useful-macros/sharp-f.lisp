
(defpackage :com.ral.useful-macros.sharp-f
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.sharp-f)

(eval-always
  ;; --------------------------------------
  ;; Reader macro for #f
  ;; sets compiler optimization levels
  ;; #0f .. #3f   #f defaults to #3f for fastest code
  (defun |reader-for-#F| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (unless *read-suppress*
      (setq numarg (or numarg 3))
      (unless (<= numarg 3)
        (error "Bad value for #f: ~a" numarg))
      `(declare (optimize (speed ,numarg)
                          (safety ,(- 3 numarg))
                          #+LISPWORKS (float ,(- 3 numarg))
                          ))))
  
  (set-dispatch-macro-character #\# #\f '|reader-for-#F|))

(defmacro fast-progn (&rest body)
  `(locally #f ,@body))

(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))

;; --------------------------------------------
#|
Hi David,

Thanks for the update.  We will add something to the Release Notes about it.

BTW, you can tell the compiler to check the declarations by compiling with
debug 3 and safety 3, which would have given an error when trying to get value
of DF, because it doesn't match the type declaration.  Safety defaults to 3,
but debug doesn't, so the default compilation doesn't check it.

For example, after compiling the code below, calling:

   (a (make-instance 'aa))

gives a proper error that 1/2 is not a double-float.  Without the debug
declaration, or with lower safety or debug, it returns the wrong value.

----------  example code --------------------------
(defclass aa ()
 ((kk :initform 1/2 :accessor aa-kk)))

(defun a (x)
 (declare (optimize (debug 3) (safety 3)))
 (with-accessors ((aa aa-kk)) x
   (declare (type double-float aa))
   (* aa 7d0)))
---------------------------------------------------

Regards,

Martin Simmons
LispWorks Technical Support
http://www.lispworks.com/support/
 
 |#