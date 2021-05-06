
(in-package :ac)

(ac:make-remote-actor "echo@rincon.local"
                      :register :RECHO)

(ac:make-remote-actor "eval@rincon.local"
                      :register :REVAL)

(ac:make-remote-actor "eval@arroyo.local"
                      :register :REVAL)

(loop repeat 10 do
      (send :reval (ac:usti (println)) `(machine-instance)))

#|
(spawn (lambda ()
         (let ((s1 (make-actor
                    (um:dlambda
                      (:get-val ()
                       (=bind (x)
                           (=async (=values 15))
                         (+ x 2)))))))
           (assert (eql 17 (ask s1 :get-val))))
         ))
|#

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(get-actor-names (println))
(find-actor (println) :reval)

