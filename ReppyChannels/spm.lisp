;; spm.lisp -- Generic Serviceable Protocol Server using Reppy's Channels
;;
;; DM/MCFA  01/00
;; ----------------------------------------------------

(defpackage #:spm
  (:use #:common-lisp #:reppy-channels)
  (:export
   #:SERVICE-REQUEST
   #:SET-HANDLER
   #:GET-HANDLER
   #:<SERVICEABLE-PROTOCOL-MIXIN>
   #:kill-server
   #:spm-error

   #:pr
   ))

(in-package :SPM)

;; ---------------------------------------------------------
(defclass <serviceable-protocol-mixin> ()
  ;; smp-handlers must be able to handle args = (inst &rest args)
  ((handlers :accessor spm-handlers
             :initarg  :handlers  ;; property list
             :initform nil)))

(define-condition spm-error (error)
  ((req  :accessor spm-error-req
         :initarg  :req)
   ))

(defmethod print-object ((err spm-error) stream)
  (format stream "SPM: no handler for ~A" (spm-error-req err)))

(defvar *spm-server* nil)

(defun make-server ()
  ;; using a generic server to perform handlers allows us to serialize
  ;; access to the handlers. Only the server actually executes handler
  ;; code.
  (setf *spm-server*
        (ac:make-actor
         (um:dlambda*
           (:get-handler (replyCh inst req)
            (poke replyCh (getf (spm-handlers inst) req)))
           (:set-handler (inst req fn)
            (setf (getf (spm-handlers inst) req) fn))
           (:handle (replyCh inst req &rest args)
            (poke replych
                  (um:capture-ans-or-exn 
                    (um:if-let (handler (getf (spm-handlers inst) req))
                        (apply handler inst args)
                      ;; else
                      (error 'spm-error :req req)))
                  ))
           ))))

(defun ensure-server-exists ()
  (unless *spm-server*
    (make-server)))

(defmethod initialize-instance :before ((inst <serviceable-protocol-mixin>)
                                        &rest args)
  (declare (ignore inst args))
  (ensure-server-exists))

(defmethod service-request ((inst <serviceable-protocol-mixin>) req &rest args)
  (let ((replyCh  (make-channel)))
    (apply #'ac:send *spm-server* :handle replyCh inst req args)
    (um:recover-ans-or-exn (recv replyCh))))

(defmethod set-handler ((inst <serviceable-protocol-mixin>) req fn)
  (ac:send *spm-server* :set-handler inst req fn))
  
(defmethod get-handler ((inst <serviceable-protocol-mixin>) req)
  (let ((replyCh (make-channel)))
    (ac:send *spm-server* :get-handler inst req)
    (recv replyCh)))

(defsetf get-handler set-handler)

;; ----------------------------------------------------------------
;; Example: Serialized output from PRINT
#|
(defvar *spm-serial-services*
  (let ((service (make-instance '<serviceable-protocol-mixin>)))
    (set-handler service :print (lambda (_ arg)
                                  (declare (ignore _))
                                  (print arg)))
    service))

(defun pr (arg)
  (service-request *spm-serial-services* :print arg))
|#

#|
(let ((x 15))
  (mp:funcall-async (lambda ()
                      (spm:pr x)))
  (spm:pr :This))
 |#
