;; capture.lisp -- Usefully capture any result or exception for relay
;; back to other callers
;;
;; DM 11/20
;; -------------------------------------------------------------

(in-package :um)

(defstruct capture-packet
  data)

(defun capture-ans-or-exn (fn &rest args)
  (make-capture-packet
   :data (multiple-value-list
          (ignore-errors
            (multiple-value-list
             (apply fn args))))
   ))

(defmacro with-captured-ans-or-exn (&body body)
  `(capture-ans-or-exn (lambda ()
                         ,@body)))

(defgeneric recover-ans-or-exn (val)
  (:method (val)
   val)
  (:method ((val capture-packet))
   (multiple-value-bind (ans exn)
       (values-list (capture-packet-data val))
     (if exn
         (error exn)
       (values-list ans)))))

