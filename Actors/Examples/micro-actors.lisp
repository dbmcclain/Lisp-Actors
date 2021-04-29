
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

(defun make-test-beh-ac (ct prev start)
  (um:dlambda
    (:start ()
     (if (zerop ct)
         (ac:send prev :end)
       (let ((actor (ac:make-actor (make-test-beh-ac (1- ct) ac:self start))))
         (ac:send actor :start))))
    (:end ()
     (if prev
         (ac:send prev :end)
       (format t "~%Finshed: ~F" (/ (- (usec:get-time-usec) start) 2 ct))))
    ))

(let* ((actor (ac:make-actor (make-test-beh-ac 1000000 nil (usec:get-time-usec)))))
  (ac:send actor :start))


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

(defun tstx (n)
  (labels ((fact (x &optional (ans 1))
             (if (plusp x)
                 (fact (1- x) (* x ans))
               ans)))
    (let (ans)
      (loop for ix from 1 to n do
            (setf ans (* (fact 40) (sin (* pi (/ (ash ix 15) ix)))))))))
  
(time (tstx 1000000))

|#

