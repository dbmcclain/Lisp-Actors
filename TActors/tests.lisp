
(in-package :ac)

(ac:make-remote-actor "echo@rincon.local"
                      :register :RECHO)

(ac:make-remote-actor "eval@rincon.local"
                      :register :REVAL)

(ac:make-remote-actor "eval@arroyo.local"
                      :register :REVAL)

(loop repeat 5 do
      (send :reval (ac:usti (println)) `(machine-instance)))

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(get-actor-names (println))

(find-actor (println) :reval)


