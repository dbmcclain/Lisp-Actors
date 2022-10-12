;; safe-streams.lisp -- Attempt to safely track streams going out of scope
;;
;; DM/RAL 10/22
;; ------------------------------------------------------------

(defpackage #:ral-streams
  (:use :common-lisp)
  (:export
   #:add-stream-tracking
   #:remove-stream-tracking
   ))

(in-package #:ral-streams)

(hcl:defglobal-variable *tracked-streams* nil)

(um:defmonitor stream-tracker ()

  (defun make-test (stream fn)
    (lambda (pair)
      (and (eql (car pair) stream)
           (eql (cdr pair) fn))))
  
  (defun add-stream-tracking (stream fn)
    (let ((test (make-test stream fn)))
      (unless (find-if test *tracked-streams*)
        (um:critical-section
          (unless (find-if test *tracked-streams*)
            (setf *tracked-streams*
                  (acons stream fn *tracked-streams*))
            ))
        )))
  
  (defun remove-stream-tracking (stream fn)
    (let ((test (make-test stream fn)))
      (when (find-if test *tracked-streams*)
        (um:critical-section
          (when (find-if test *tracked-streams*)
            (setf *tracked-streams*
                  (delete-if test *tracked-streams*))
            ))
        )))
  
  (defun discard-stream (stream)
    (when (assoc stream *tracked-streams*)
      (let (actions)
        (um:critical-section
          (when (assoc stream *tracked-streams*)
            (setf *tracked-streams*
                  (delete-if (lambda (pair)
                               (when (eql (car pair) stream)
                                 (push (cdr pair) actions)))
                             *tracked-streams*)
                  )))
        (dolist (action actions)
          (funcall action stream)))
      )))

(lw:defadvice (close check-close :after)
    (stream &rest args)
  (declare (ignore args))
  (discard-stream stream))

;; ---------------------------------------
#|
(progn
  (print *tracked-streams*)
  (values))

(defun print-stream-closing (stream)
  (format t "~%Stream Closing: ~S" stream))

(with-input-from-string (s "xxx")
  (add-stream-tracking s 'print-stream-closing)
  (print *tracked-streams*)
  (print s)
  (values))

(with-open-stream (s (make-string-input-stream "xxx"))
  (add-stream-tracking s 'print-stream-closing) 
  (add-stream-tracking s 'print-stream-closing) ;; redundant adds do nothing
  (add-stream-tracking s 'print-stream-closing) 
  (print *tracked-streams*)
  (print s)
  (values))
|#
