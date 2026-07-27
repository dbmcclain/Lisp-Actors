
(in-package :ac)

(defun rsrc-gate-beh (rsrc)
  (ensure-par-safe-behavior
   (lambda (cust &rest msg)
     (let ((tag (tag self)))
       (become (rsrc-locked-beh rsrc cust tag nil))
       (send* rsrc tag msg)))))

(defun rsrc-locked-beh (rsrc cur-cust tag pend)
  (ensure-par-safe-behavior
   (lambda (cust &rest msg)
     (cond ((eq cust tag)
            (send* cur-cust msg)
            (if pend
                (multiple-value-bind (cmd new-pend)
                    (popq pend)
                  (become (rsrc-locked-beh rsrc (car cmd) tag new-pend))
                  (send* rsrc tag (cdr cmd)))
              (become (rsrc-gate-beh rsrc))
              ))
           
           (t
            (become (rsrc-locked-beh rsrc cur-cust tag (addq pend (cons cust msg))))
            )))))
    
