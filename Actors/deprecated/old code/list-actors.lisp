
(in-package :ac)

(defun cons-beh (hd tl)
  (alambda
   ((cust :hd)
    (send cust hd))
   ((cust :tl)
    (send cust tl))
   ))

(defun make-cons (hd tl)
  (make-actor (cons-beh hd tl)))
