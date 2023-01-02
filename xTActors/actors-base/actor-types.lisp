;; actor-types.lisp
;;
;; DM/RAL  2022/10/24 16:56:15
;; ----------------------------------

(defpackage #:com.ral.actors.types
  (:use #:common-lisp #:ac))

(in-package #:com.ral.actors.types)

;; ----------------------------------

(unless (fboundp 'do-nothing)
  ;; needed for SBCL
  (defun do-nothing (&rest _)
    (declare (ignore _))
    (values)))

;; ----------------------------------------------------------

(declaim (inline %actor-cas))

;; ---------------------------------------------------

(defstruct (actor
               (:constructor %create (beh)))
  (beh #'do-nothing :type function))

(defstruct (service
            (:include actor)
            (:constructor %create-service (beh))))

;; ----------------------------------------------------------

(defgeneric screened-beh (arg)
  (:method ((fn function))
   fn)
  (:method ((ac actor))
   (actor-beh ac))
  (:method (x)
   (error "Invalid behavior: ~S" x)))

;; ----------------------------------------------------------

(defun create-service (&optional (fn #'do-nothing))
  (%create-service (screened-beh fn)))

(defun %actor-cas (actor old-beh new-beh)
    (mpc:compare-and-swap (actor-beh (the actor actor)) old-beh new-beh))

;; -----------------------------------------------------

