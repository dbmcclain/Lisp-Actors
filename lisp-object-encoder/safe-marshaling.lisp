;; safe-marshaling.lisp -- Attempt to provide a benign transport for all objects
;;
;; DM/RAL 03/21
;; ----------------------------------------------------------------------------

(defpackage #:safe-marshaling
  (:nicknames #:rsmb)
  (:import-from #:sdle-store
   #:find-backend
   #:defbackend
   #:backend-store-object
   #:store-error)
  (:import-from #:lisp-object-encoder
   #:loe-back-end
   #:defstore
   #:defrestore
   #:encode
   #:decode)
  (:export
   #:safe-encode
   #:safe-decode
   #:unserializable
   #:view
   ))

(in-package #:safe-marshaling)

;; the magic word for "RAL Safe Marshaling Backend"
(defconstant +safe-backend-magic+ (um:magic-word "RSMB"))

(um:eval-always
  (unless (find-backend 'safe-marshaling-backend)
    (defbackend safe-marshaling-backend
                :magic-number +safe-backend-magic+
                :extends      (loe-back-end))))

(define-condition unserializable-object (error)
  ()
  (:report "Unserializable Object"))

(defun unserializable (&rest args)
  (declare (ignore args))
  (error 'unserializable-object))
(setf (symbol-value 'unserializable) 'unserializable)

(defmethod sdle-store:backend-store-object :around ((backend safe-marshaling-backend)
                                                    obj stream)
  (let ((pos (file-position stream)))
    (handler-case
        (call-next-method)
      
      (store-error (c)
        (warn (um:format-error c))
        (file-position stream pos)
        (call-next-method backend 'unserializable stream))
      )))

(defun safe-encode (msg &rest args)
  (apply #'encode msg
         :backend 'safe-marshaling-backend
         args))

(defun safe-decode (vec &rest args)
  (apply #'decode vec
         :backend 'safe-marshaling-backend
         args))

(defun view (v)
  (map 'string #'code-char v))
