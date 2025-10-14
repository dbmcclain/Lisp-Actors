
(defpackage #:micro-actors
  (:use #:common-lisp)
  (:export
   #:self
   #:run
   #:actor
   #:make-actor
   #:actor-beh
   #:send
   #:become
   ))

(in-package #:micro-actors)

(defvar *self* nil)

(define-symbol-macro self *self*)

(defvar *event-queue* (mp:make-mailbox))

(defclass actor ()
  ((beh :initarg :beh
        :accessor actor-beh)))

(defun become (beh-fn)
  (setf (actor-beh self) beh-fn))

(defmethod make-actor (&optional beh-fn)
  (make-instance 'actor
                 :beh (or beh-fn #'funcall)))

(defmethod send ((actor actor) &rest message)
  (mp:mailbox-send *event-queue* (cons actor message)))

;; ----------------------------------------------------
;; Runtime system

(defvar *run-lock*   (mp:make-lock))
(defvar *run-thread* nil)

(defun run ()
  (mp:process-run-function "MicroActor Run"
                           ()
                           (lambda ()
                             (mp:with-lock (*run-lock* nil 0)
                               (unwind-protect
                                   (progn
                                     (setf *run-thread* mp:*current-process*)
                                     (loop
                                      (destructuring-bind (*self* . message)
                                          (mp:mailbox-read *event-queue*)
                                        (with-simple-restart (abort "Process next event")
                                          (apply (actor-beh self) message)))))
                                 (setf *run-thread* nil))
                               ))
                           ))
(run)

;; ----------------------------------------------------
#|
;; test
 
(defun make-test-beh (ct prev start)
  (um:dlambda
    (:start ()
     (if (zerop ct)
         (send prev :end)
       (let ((actor (make-actor (make-test-beh (1- ct) self start))))
         (send actor :start))))
    (:end ()
     (if prev
         (send prev :end)
       (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) 2 ct))))
    ))

(let* ((actor (make-actor (make-test-beh 1000000 nil (usec:get-time-usec)))))
  (send actor :start))

(defun make-test-beh-stac (ct prev start)
  (um:dlambda
    (:start ()
     (if (zerop ct)
         (stac:send prev :end)
       (let ((actor (stac:make-actor (make-test-beh-ac (1- ct) stac:self start))))
         (stac:send actor :start))))
    (:end ()
     (if prev
         (stac:send prev :end)
       (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) 2 ct))))
    ))

(let* ((actor (stac:make-actor (make-test-beh-ac 1000000 nil (usec:get-time-usec)))))
  (stac:send actor :start))


(defun make-test-beh (ct prev)
  (let (start)
    (um:dlambda
      (:start (head)
       (if (zerop ct)
           (send head :tail self)
         (let ((actor (make-actor (make-test-beh (1- ct) self))))
           (send actor :start head))))
      (:tail (last)
       (setf start (usec:get-time-usec))
       (send last :meas))
      (:meas ()
       (if prev
           (send prev :meas)
         (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) ct))))
      )))

(let* ((actor (make-actor (make-test-beh 1000000 nil))))
  (send actor :start actor))

(defun make-test-beh-stac (ct prev)
  (let (start)
    (um:dlambda
      (:start (head)
       (if (zerop ct)
           (stac:send head :tail stac:self)
         (let ((actor (stac:make-actor (make-test-beh-stac (1- ct) stac:self))))
           (stac:send actor :start head))))
      (:tail (last)
       (setf start (usec:get-time-usec))
       (stac:send last :meas))
      (:meas ()
       (if prev
           (stac:send prev :meas)
         (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) ct))))
      )))

(let* ((actor (stac:make-actor (make-test-beh-stac 1000000 nil))))
  (stac:send actor :start actor))

|#

