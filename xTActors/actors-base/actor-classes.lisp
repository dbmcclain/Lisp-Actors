;; actor-classes.lisp
;;
;; DM/RAL  2022/10/24 16:50:40
;; ----------------------------------

(defpackage #:com.ral.actors.classes
  (:use #:common-lisp #:ac))

(in-package #:com.ral.actors.classes)

;; ----------------------------------

(declaim (inline %actor-cas))

;; ----------------------------------

(defmacro send* (actor &rest msg)
  ;; for when it is known that final arg in msg is a list
  `(apply #'send ,actor ,@msg))

(unless (fboundp 'do-nothing)
  ;; needed for SBCL
  (defun do-nothing (&rest _)
    (declare (ignore _))
    (values)))

;; -----------------------------------

(defclass actor ()
  ;; Unify syntax between Actor SEND and FUNCALL
  ((beh-cons  :initform (list #'do-nothing)))
  (:metaclass clos:funcallable-standard-class))

(defmethod actor-beh ((ac actor))
    (car (slot-value ac 'beh-cons)))

(defmethod set-actor-beh ((ac actor) (beh function))
    (setf (car (slot-value ac 'beh-cons)) beh))

(defmethod set-actor-beh ((ac actor) (beh actor))
    (set-actor-beh ac (actor-beh beh)))

(defsetf actor-beh set-actor-beh)
  
(defmethod initialize-instance :after ((ac actor) &key (beh #'do-nothing) &allow-other-keys)
    (setf (actor-beh ac) beh)
    (clos:set-funcallable-instance-function ac (lambda (&rest msg)
                                                 (send* ac msg))))

(defmethod actor-p (x)
    nil)
  
(defmethod actor-p ((x actor))
    t)

(defun %create (&optional (beh #'do-nothing))
    (make-instance 'actor
                   :beh beh)) ;; already screened in create

;; -----------------------------------------------------

(defclass service (actor)
    ()
    (:metaclass clos:funcallable-standard-class))
  
(defmethod service-p (x)
    nil)
  
(defmethod service-p ((ac service))
    t)

(defmethod create-service (beh)
    (change-class (create beh) 'service))

;; -----------------------------------------------------

(defmacro def-actor (name &optional (beh '#'do-nothing))
  (lw:with-unique-names (msg behe)
    `(progn
       (define-symbol-macro ,name (symbol-value ',name))
       (defun ,name (&rest ,msg)
         (send* ,name ,msg))
       (let ((,behe ,beh))
         (setf ,name (if (service-p ,behe)
                         (create-service ,behe)
                       (create ,behe)))))
    ))

(defun %actor-cas (actor old-beh new-beh)
  (mpc:compare-and-swap (car (slot-value (the actor actor) 'beh-cons))
                        old-beh new-beh))

;; ----------------------------------------
