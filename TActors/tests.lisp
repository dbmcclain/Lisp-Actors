
(in-package :ac)

(ac:make-remote-actor "echo@rincon.local"
                      :register :RECHO)

(ac:make-remote-actor "eval@rincon.local"
                      :register :REVAL)

(ac:make-remote-actor "eval@arroyo.local"
                      :register :REVAL)

(loop repeat 5 do
      (send :reval (ac:usti println) `(machine-instance)))

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(get-actor-names println)

(find-actor println :reval)


(send cust (+ a b))

(@bind (+)
    (send + :eval @bind)
  (@bind (a)
      (send a :eval @bind)
    (@bind (b)
        (send b :eval @bind)
      (send + :apply cust a b))))

(@bind ((+ a b))
    (send (par + a b) :eval @bind)
  (send + :apply cust a b))
          

(defun list-beh (lst)
  (um:dlambda
    (:eval (cust)
     (send (@bind vals
               (send (par lst) :eval