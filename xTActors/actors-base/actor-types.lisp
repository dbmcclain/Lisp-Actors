;; actor-types.lisp
;;
;; DM/RAL  2022/10/24 16:56:15
;; ----------------------------------

(defpackage #:com.ral.actors.types
  (:use #:common-lisp #:ac))

(in-package #:com.ral.actors.types)

;; ----------------------------------

(defmacro send* (actor &rest msg)
  ;; for when it is known that final arg in msg is a list
  `(apply #'send ,actor ,@msg))

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

(defun set-beh (actor beh)
  (setf (actor-beh actor) beh))

(defstruct (service
            (:include actor)
            (:constructor %create-service (beh))))

(defun create-service (&optional (fn #'do-nothing))
  (%create-service (screened-beh fn)))

(defmacro def-actor (name &optional (beh '#'do-nothing))
  (lw:with-unique-names (behe)
    `(deflex ,name 
       (let ((,behe ,beh))
         (if (service-p ,behe)
             (create-service ,behe)
           (create ,behe))))
    ))

(defun %actor-cas (actor old-beh new-beh)
    (mpc:compare-and-swap (actor-beh (the actor actor)) old-beh new-beh))

;; -----------------------------------------------------

