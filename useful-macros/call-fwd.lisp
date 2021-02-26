
(in-package #:um)

(defmacro with-fwd (&body body)
  ;; wrap body such that CALL-FWD can be used like FUNCALL, but for
  ;; tail-pure forwarding, never to return.
  ;;
  ;; CALL-FWD also behaves like a normal return, in that, handlers and
  ;; unwind-protect are exited prior to making the terminal FUNCALL.
  ;;
  ;; Caution: Watch out for deferred CALL-FWD, when a lambda
  ;; containing CALL-FWD for an enclosing WITH-FWD wrapped function
  ;; body is handed off for later execution. (cross-thread, or upward
  ;; funarg) That won't (can't) work. It can only work for downward
  ;; funargs. - Same problems as exist for GO, CATCH/THROW,
  ;; RESTART-CASE/INVOKE-RESTART, HANDLER-CASE/SIGNAL/ERROR
  (lw:with-unique-names (gblk gfwd vdest vargs dest args)
    `(block ,gblk
       (let (,vdest ,vargs)
         (tagbody
          (return-from ,gblk
            (macrolet ((apply-fwd (,dest &rest ,args)
                         `(progn
                            (setf ,',vdest ,,dest
                                  ,',vargs (list* ,@,args))
                            (go ,',gfwd)))
                       (call-fwd (,dest &rest ,args)
                         `(apply-fwd ,,dest ,@,args nil)))
              ,@body))
          ,gfwd
          (apply ,vdest ,vargs))
         ))
    ))

#|
(with-fwd
  (unwind-protect
      (progn
        (doit)
        (call-fwd #'thing 15))
    (print :unwind)))  
|#
