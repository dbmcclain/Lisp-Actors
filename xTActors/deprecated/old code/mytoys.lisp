
(in-package :ac)

(defmacro µ ((name . args) &body body)
  `(defmacro ,name ,args ,@body))

(µ (∂ (name . args) &body body)
  `(defun ,name ,args ,@body))

(µ (α args &body body)
  `(actor ,args ,@body))

(µ (β args form &body body)
  `(let ((β  (α ,args ,@body)))
     ,form))

(export '(α β ∂ µ))

(editor:setup-indent "µ" 1)
(editor:setup-indent "∂" 1)
(editor:setup-indent "α" 1)
(editor:indent-like "β" 'destructuring-bind)

#|
(send (α _ (/ 0)))
|#

(∂ (long-running-beh action)
  (flet ((doit (cust args)
           (let ((tag  (tag self)))
             (become (busy-running-beh action tag cust +emptyq+))
             (send* action tag args))))
    (alambda
     ((cust :run . args)
      (doit cust args))

     ((cust :run-immediately . args)
      (doit cust args))
     )))

(∂ (busy-running-beh action tag cust queue)
  (alambda
   ((atag . ans) when (eql atag tag)
    (send* cust ans)
    (if (emptyq? queue)
        (become (long-running-beh action))
      (multiple-value-bind (next-up . new-queue) (popq queue)
        (destructuring-bind (next-cust . next-args) next-up
          (let ((new-tag  (tag self)))
            (become (busy-running-beh action new-tag next-cust new-queue))
            (send* action new-tag next-args))
          ))
      ))

   ((acust :run . args)
    (become (busy-running-beh action tag cust
                              (addq queue (cons acust args)))))

   ((acust :run-immediately . _)
    (send acust nil))
   ))

(∂ (long-running action)
  (make-actor (long-running-beh action)))

