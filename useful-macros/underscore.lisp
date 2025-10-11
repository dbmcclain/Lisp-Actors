;; underscore.lisp - A LW advice to permit underscore separators in number entry
;;
;; DM/RAL  06/21
;; -------------------------------------------------------------------
(in-package :common-lisp-user)
;; -------------------------------------------------------------------
;; Handle default Reader ops

#+:LISPWORKS
(progn
  (defadvice (sys::read-token underscore-skipper :around)
      (&rest args)
    (let* ((ans  (multiple-value-list (apply #'call-next-advice args)))
           (val  (car ans))
           name)
      (cond ((and (symbolp val)
                  (find #\_ (setf name (symbol-name val))))
             (let ((namex (remove #\_ name)))
               (multiple-value-bind (ans2 end)
                   (read-from-string namex nil nil)
                 (if (and (numberp ans2)
                          (eql end (length namex)))
                     (progn
                       (unintern val (symbol-package val))
                       ans2) ;; just 1 retval - same as when a normal number is encountered
                   ;; else
		   (values-list ans))
                 )))
            (t
             (values-list ans))
            )))
  
  ;; ----------------------------------------------------
  ;; Handle #X, #B, #O, #R formats
  
  (defvar *in-sharp-r* nil)

  (defadvice (sys::sharp-r underscore-skipper :around)
      (&rest args)
    (let ((*in-sharp-r* t))
      (apply #'call-next-advice args)))
  
  (defadvice (sys::new-read-extended-token underscore-skipper :around)
      (&rest args)
    (let* ((ans (multiple-value-list (apply #'call-next-advice args)))
           (val (car ans)))
      (cond ((and *in-sharp-r*
                  (stringp val))
             (apply #'values (remove #\_ val) (cdr ans)))
            (t
             (values-list ans))
            ))))

;; --------------------------------------------

#+:SBCL
(sb-ext:with-unlocked-packages (:sb-impl)
  (cl-advice:make-advisable 'sb-impl::read-maybe-nothing)
  (cl-advice:add-advice :around 'sb-impl::read-maybe-nothing
			(lambda (next-fn &rest args)
			  (multiple-value-bind (flag result)
			      (apply next-fn args)
			    (case flag
			      (0
			       (values flag result))
			      (t
			       (let (name)
				 (cond ((and (symbolp result)
					     (find #\_ (setf name (symbol-name result))))
					(let ((namex (remove #\_ name)))
					  (multiple-value-bind (ans end)
					      (read-from-string namex nil nil)
					    (if (and (numberp ans)
						     (eql end (length namex)))
						(progn
						  (unintern result (symbol-package result))
						  (values flag ans))
						(values flag result))
					    )))
				       (t
					(values flag result))
				       )))
			      )))
			))

