;; restricted-eval.lisp
;;
;; We need to extend the notion of *READ-EVAL* so that we can usefully
;; serialize and restore some atomic value extensions - like UUID. But
;; we want to restrict the reader eval to only those functions that we
;; want and nothing more.
;;
;; DM/RAL  2022/11/27 19:34:59
;; ----------------------------------

(defpackage #:com.ral.useful-macros.restricted-eval
  (:use #:common-lisp)
  (:shadow #:read #:read-preserving-whitespace #:read-from-string)
  (:export
   #:*symbols-to-make-available*
   #:add-symbol
   #:add-symbols
   #:with-added-symbols
   #:remove-symbol
   #:remove-symbols
   #:with-removed-symbols
   #:read
   #:read-preserving-whitespace
   #:read-from-string))

(in-package #:com.ral.useful-macros.restricted-eval)

;; ----------------------------------

(defparameter *symbols-to-make-available*
  '(com.ral.uuid:uuid))


(defun add-symbol (sym)
  ;; Imperative version
  (pushnew sym *symbols-to-make-available*))

(defun add-symbols (syms)
  ;; Imperative version
  (dolist (sym syms)
    (add-symbol sym)))

(defun remove-symbol (sym)
  ;; Imperative version
  (setf *symbols-to-make-available* (delete sym *symbols-to-make-available*)))

(defun remove-symbols (syms)
  ;; Imperative version
  (dolist (sym syms)
    (remove-symbol sym)))


(defmacro with-added-symbols (syms &body body)
  ;; FPL version
  `(let ((*symbols-to-make-available* (union ',syms *symbols-to-make-available*)))
     ,@body))

(defmacro with-removed-symbols (syms &body body)
  ;; FPL version
  `(let ((*symbols-to-make-available* (set-difference *symbols-to-make-available* ',syms)))
     ,@body))


(defun init ()
  (when (find-package :%%-RESTRICTED-%%)
    (delete-package :%%-RESTRICTED-%%))
  (prog1
      (make-package :%%-RESTRICTED-%% :use nil)
    (import *symbols-to-make-available* :%%-restricted-%%)
    (export *symbols-to-make-available* :%%-restricted-%%)
    ))

;; ---------------------------

(defparameter *restrict-reader*  nil)

(lw:defadvice (cl:find-package restricted-eval :around)
    (name/package)
  (declare (optimize speed))
  (or *restrict-reader*
      (lw:call-next-advice name/package)))

;; ---------------------------

(defun do-with-restrictions (fn)
  (let ((restricted (init)))
    (unwind-protect
        (let ((*package*         restricted)
              (*restrict-reader* restricted)
              (*read-eval*       t))
          (funcall fn))
      (delete-package restricted))
    ))

(defun read (&rest args)
  (do-with-restrictions (lambda ()
                          (apply #'cl:read args))))

(defun read-preserving-whitespace (&rest args)
  (do-with-restrictions (lambda ()
                          (apply #'cl:read-preserving-whitespace args))))

(defun read-from-string (str &rest args)
  (do-with-restrictions (lambda ()
                          (apply #'cl:read-from-string str args))))

;; ---------------------------
#|
(prin1 #/uuid/{e6942a3c-6eca-11ed-8b65-787b8acbe32e})

(cl:read-from-string "#.(COM.RAL.UUID:UUID \"{e6942a3c-6eca-11ed-8b65-787b8acbe32e}\")")
(read-from-string "#.(COM.RAL.UUID:UUID \"{e6942a3c-6eca-11ed-8b65-787b8acbe32e}\")") ;; should work
(read-from-string "#.(+ 1 2 3)") ;; should raise an error
|#
