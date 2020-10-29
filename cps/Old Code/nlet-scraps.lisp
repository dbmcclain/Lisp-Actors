;; -----------------------------------------------------
;; Special treatment for =nlet-tail to ensure constant stack depth

(define-condition values-signal ()
  ((args  :reader values-signal-args :initarg :args)))

(defun retk (&rest args)
  (error 'values-signal :args args))

(defun fwdk (c)
  (apply 'funcall (values-signal-args c)))

(defun fwd-loop (c)
  (nlet-tail iter ((c c))
    (handler-case
        (fwdk c)
      (values-signal (cnew)
        (iter cnew)))))

(defun enshroud (&rest body)
  (let ((g!args (gensym)))
    `(handler-case
	 (macrolet ((=values (&rest ,g!args)
		      `(retk %sk ,@,g!args)))
	   ,@body)
       (values-signal (c)
	 (fwd-loop c)))
    ))

