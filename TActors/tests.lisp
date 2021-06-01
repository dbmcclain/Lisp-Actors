
(in-package :ac)

(make-remote-actor "echo@rincon.local"
                   :register :RECHO)

(make-remote-actor "eval@rincon.local"
                   :register :REVAL)

(make-remote-actor "eval@arroyo.local"
                   :register :REVAL)

(make-remote-actor "eval@RAMBO"
                   :register :REVAL)

(loop repeat 5 do
      ;; remote EVAL, print result on our local printer
      (send :reval (ac:usti println) `(machine-instance)))

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(send println :hello)
(get-actor-names println)
(find-actor println :reval)

