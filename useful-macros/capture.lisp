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

(defmacro mcapture-ans-or-exn (&body body)
  `(capture-ans-or-exn (lambda ()
                         ,@body)))

(defmethod recover-ans-or-exn ((capt capture-packet))
  (multiple-value-bind (ans exn)
      (values-list (capture-packet-data capt))
    (if exn
        (error exn)
      (values-list ans))))

(defmethod recover-ans-or-exn (ans)
  ans)

