
(defpackage #:micro-actors-smp
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

(in-package #:micro-actors-smp)

(defvar *self* nil)

(define-symbol-macro self *self*)

(defvar *event-lock*   (mp:make-lock))
(defvar *event-signal* (mp:make-condition-variable))
(defvar *event-queue*  (cons (hcl:make-unlocked-queue)
                             (hcl:make-unlocked-queue)))

(defclass actor ()
  ((beh  :initarg :beh
         :accessor actor-beh)
   (busy :initform (list nil)
         :accessor actor-busy)))

(defun become (beh-fn)
  (setf (actor-beh self) beh-fn))

(defmethod make-actor (&optional beh-fn)
  (make-instance 'actor
                 :beh (or beh-fn #'funcall)))

(defmethod send ((actor actor) &rest message)
  (mp:with-lock (*event-lock*)
    (hcl:unlocked-queue-send (car *event-queue*) (cons actor message))
    (mp:condition-variable-signal *event-signal*)))

(defmethod get-next-event ()
  (mp:with-lock (*event-lock*)
    (prog ((ct 0))
      again
      (let ((pair (or (hcl:unlocked-queue-read (car *event-queue*))
                      (let ((tmp (cdr *event-queue*)))
                        (setf (cdr *event-queue*) (car *event-queue*)
                              (car *event-queue*) tmp)
                        (incf ct)
                        (when (> ct 1)
                          (setf ct 0)
                          (mp:condition-variable-wait *event-signal* *event-lock*))
                        (go again))
                      )))
        (if (sys:compare-and-swap (car (actor-busy (car pair))) nil t)
            (return pair)
          (progn
            (hcl:unlocked-queue-send (cdr *event-queue*) pair)
            (go again)))
        ))))

(defun run-actor ()
  (destructuring-bind (*self* . message)
      (get-next-event)
    (with-simple-restart (abort "Process next event")
      (apply (actor-beh self) message))
    (setf (car (actor-busy self)) nil)
    (mp:condition-variable-signal *event-signal*)))

;; ----------------------------------------------------
;; Runtime system

(defun run (ix)
  (mp:process-run-function (format nil "MicroActor Run ~D" ix)
                           ()
                           (lambda ()
                             (loop
                              (run-actor)))
                           ))
(defun start ()
  (dotimes (ix 8)
    (run ix)))
#|
(start)
|#
;; ----------------------------------------------------
#|
;; test

(send (make-actor (lambda (x) (/ 0))) nil)

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

(let* ((actor (make-actor (make-test-beh 100000 nil (usec:get-time-usec)))))
  (send actor :start))

(time (dotimes (ix 1000000)
        (make-actor (make-test-beh 1000000 nil 0))))

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

(defun make-test-beh-ac (ct prev)
  (let (start)
    (um:dlambda
      (:start (head)
       (if (zerop ct)
           (ac:send head :tail ac:self)
         (let ((actor (ac:make-actor (make-test-beh-ac (1- ct) ac:self))))
           (ac:send actor :start head))))
      (:tail (last)
       (setf start (usec:get-time-usec))
       (ac:send last :meas))
      (:meas ()
       (if prev
           (ac:send prev :meas)
         (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) ct))))
      )))

(let* ((actor (ac:make-actor (make-test-beh-ac 1000000 nil))))
  (ac:send actor :start actor))

(time (dotimes (ix 1000000)
        (ac:make-actor (make-test-beh 1000000 nil 0))))

(time (dotimes (ix 1000000)
        ix))
|#

