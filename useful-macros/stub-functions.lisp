
(defpackage :com.ral.useful-macros.stub-functions
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.stub-functions)

;;;; Stubs for Unimplemented Functions

;;; DEFSTUB: define a stub function named FUNCTION-NAME and set property
;;; STUB-FUNCTION-P true on FUNCTION-NAME. The property setting is done at both
;;; macro-evaluation and macro-expansion times, allowing its value to be used
;;; both at compile and load times.

;;; There are a set of functions that, for purposes of code clarity, need to
;;; exist in regularly compiled code, but which shall, for the forseeable
;;; future, never actually be called and therefore need not be defined.  We are
;;; free, however, to define them as "stub" functions. A `stub function' should
;;; never be called. If it is called at runtime, its behavior is undefined in
;;; production. (It's OK for it to behave the same as in development, but that
;;; is not required and should not be relied upon.)  In development, it's highly
;;; desireable that calling a stub function should result in a runtime error
;;; being signaled.

;;; The main purpose and benefit of using defstub is to prevent the compiler
;;; from complaining about unimplemented functions every single compile, when
;;; you have no intention of ever fixing the situation in the present
;;; development period.

;; -----------------------------------------------------------------
;; NOTE -- I (DBM) did not create this code. I suspect it came from
;; (c.a.2019) one of: Shanon Spires, Mark David, Paul Tarvydas, or
;; Mark Evanston, during the Emotiq Project, porting my own code to
;; other (Non-Lispworks) Lisp systems.  I appreciate the cleverness
;; displayed in the details of this code!
;; -----------------------------------------------------------------

(eval-always
(defmacro stub-function-p (function-name)
  "Accessor on FUNCTION-NAME (getable, setf'able). Value either true, if
FUNCTION-NAME is a symbol that is the name of a stub function, or false (nil)
for any other symbol."
  `(get ,function-name 'stub-function-p))
)
  

(defmacro defstub (function-name)
  (unless (fboundp function-name)
    (setf (stub-function-p function-name) t) ; set both at compile and load time
    `(progn
       (setf (stub-function-p ',function-name) t)
       (defun ,function-name (&rest args)
         (declare (ignore args))
         (error "~s, a stub function, called at run time, but it should not be."
                ',function-name))
       ',function-name)))

;; -----------------------------------------------------------------------------
;; with-fast-impl (macro)

(eval-always
(defmacro error-running-fast-impl-function? (fast-name)
  "Accessor on a fast-impl-function name (getable, setf'able). Value can either
be nil (initially) the Lisp error condition object from a first error condition
from calling the function."
  `(get ',fast-name 'error-running-fast-impl-function))
)

(defun use-slow-code ()
  ;; Added (DBM) to allow fast code to fall back to the slow variant
  (throw 'use-slow-code nil))

(defmacro reset-error (fast-name)
  ;; Added (DBM) to allow repair after modifying errant code
  `(setf (error-running-fast-impl-function? ,fast-name) nil))

(defun #1=do-with-fast-impl (fast-name fast-fn slow-fn)
  ;; Changed (DBM) to allow fast to fall back to slow by calling USE-SLOW-CODE
  ;; also behaves properly when/if fast-fn returns NIL
  (unless (error-running-fast-impl-function? fast-name)
    (handler-case
        (catch 'use-slow-code
          (return-from #1#
            (funcall fast-fn)))
      (error (error-condition)
        ;; Throw a bone to a developer tracking down the error: log to
        ;; error output, and store error condition in a property on the
        ;; function name symbol.
        (format *error-output*
                "!!! *** Taking function ~S out. *** !!!~%" 
                fast-name)
        (format *error-output*
                "!!! ***   Error condition = ~A *** !!!~%" 
                error-condition)
        (format *error-output*
                "!!! ***   Error condition type = ~S *** !!!~%"
                (type-of error-condition))
        
        ;; Consider enabling this, maybe just in development mode:
        ;; (cerror "Continue" "Error on fast-impl call of ~S" fast-name)
        
        (setf (error-running-fast-impl-function? fast-name)
              error-condition))
      ))
  (funcall slow-fn))

#|
 ;; original version
(defun do-with-fast-impl (fast-name fast-fn slow-fn)
  (or (and (null (error-running-fast-impl-function? fast-name))
           (handler-case
               (funcall fast-fn)
             (error (error-condition)
               (progn
                 ;; Throw a bone to a developer tracking down the error: log to
                 ;; error output, and store error condition in a property on the
                 ;; function name symbol.
                 (format *error-output*
                         "!!! *** Taking function ~S out. *** !!!~%" 
                         fast-name)
                 (format *error-output*
                         "!!! ***   Error condition = ~A *** !!!~%" 
                         error-condition)
                 (format *error-output*
                         "!!! ***   Error condition type = ~S *** !!!~%"
                         (type-of error-condition)))
               
               ;; Consider enabling this, maybe just in development mode:
               ;; (cerror "Continue" "Error on fast-impl call of ~S" fast-name)
               
               (setf (error-running-fast-impl-function? fast-name)
                     error-condition)
               nil)))
      (funcall slow-fn)))
|#

(defmacro with-fast-impl ((fast-name) fast-form slow-form)
  ;; changed (DBM) to no longer require fast-form to begin with call
  ;; to fast-name - allows for condiitonal expressions that might
  ;; invoke USE-SLOW-CODE
  (if (stub-function-p fast-name)
      ;; If at expansion time we already know FAST-NAME names a stub function,
      ;; do not expand a call to it: simply emit SLOW-FORM straight inline.
      slow-form
    `(do-with-fast-impl ',fast-name
                        (lambda ()
                          ,fast-form)
                        (lambda ()
                          ,slow-form)) ))
