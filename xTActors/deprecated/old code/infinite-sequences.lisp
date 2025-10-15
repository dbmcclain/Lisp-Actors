
(in-package :ac)

(defun base-beh ()
  (lambda (cust x)
    (let ((next (make-actor self-beh)))
      (become (val-beh x next))
      (send cust x))))

(defun val-beh (x next)
  (lambda (cust x)
    (repeat-send next)))

(let ((ac (make-actor (base-beh))))
  (send (actor (n)
          (unless (zerop n)
            (send ac println n)
            (send self (1- n))))
        20))
