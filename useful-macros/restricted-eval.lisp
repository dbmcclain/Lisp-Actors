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
  '(list
    vector
    string
    make-array
    make-string
    make-list
    make-condition
    unsigned-byte
    simple-array
    character
    base-char
    make-instance
    coerce
    com.ral.uuid:uuid))


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

#+:LISPWORKS
(lw:defadvice (cl:find-package restricted-eval :around)
    (name/package)
  (declare (optimize speed))
  (or *restrict-reader*
      (lw:call-next-advice name/package)))

#+:ALLEGRO
(progn
  (excl:def-fwrapper restricted-eval (name/package)
    (declare (optimize speed))
    (or *restrict-reader*
	(excl:call-next-fwrapper)))

  (excl:fwrap 'find-package 'rrwfp 'restricted-eval))

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
(read-from-string "#.(vector :a :b :c)")

(let* ((x (list 1 2 3))
       (y (append (list 'a 'b 'c) x)))
  (hcl:dump-forms-to-file "my-forms.data" (list y x)))

(let (ans)
  (hcl:load-data-file "my-forms.data"
                      :eval nil
                      :callback (lambda (item) (push item ans)))
  (setf ans (nreverse ans))
  (assert (eq (cadr ans) (nthcdr 3 (car ans))))
  ans)


(let* ((x (list 1 2 3))
       (y (append (list 'a 'b 'c) x)))
  (destructuring-bind (yy xx)
      (ms:unmarshal (ms:marshal (list y x)))
    (assert (eq xx (nthcdr 3 yy)))
    ))

(ac:with-maximum-io-syntax
 (let ((x #/uuid/{25123714-6f56-11ed-9a1c-787b8acbe32e})
       (y #/uuid/{27e2ebd2-6f56-11ed-9a1c-787b8acbe32e}))
   (prin1 (list x y (vector x x y)))))

(read-from-string "(#1=#/uuid/{25123714-6f56-11ed-9a1c-787b8acbe32e}
                    #2=#/uuid/{27e2ebd2-6f56-11ed-9a1c-787b8acbe32e}
                    #(#1# #1# #2#))")
|#


