
(in-package :ac)

(make-remote-actor "echo@rincon.local"
                   :register :RECHO)

(make-remote-actor "eval@rincon.local"
                   :register :REVAL)

(make-remote-actor "eval@arroyo.local"
                   :register :REVAL)

(loop repeat 5 do
      (send :reval (ac:usti println) `(machine-instance)))

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(send println :hello)
(get-actor-names println)
(find-actor println :reval)

;; --------------------------------------
;; Arg pattern convention has customer first, followed by message args

;; to break (+ a b) down into elementary particle Actors...
(send cust (+ a b))

(beta (+)
    (send + beta :eval)
  (beta (a)
      (send a beta :eval)
    (beta (b)
        (send b beta :eval)
      (send + cust :apply a b))))

(beta ((+ a b))
    (send par beta '(+ a b) :eval)
  (send + cust :apply a b))
          
(let ((junk 15))
  (defun tst ()
    (+ junk 1))
  (defun tst2 ()
    (+ junk 2)))

(let ((junk 15))
  (defun tst ()
    (um:dlambda
      (:a (x)
       (send junk x 32))
      (:B (y)
       (send junk y 33))
      )))
(inspect #'tst)


(let* ((sleeper (α (cust) (send (io (α (cust) (sleep 1) (send cust :done))) cust)))
       (timeout (α (cust) (send (scheduled-message cust 1.005 :timeout))))
       (racer   (race sleeper timeout)))
  (send racer println))

: 2+ ( cust x -- )
  2 v+ send ;
