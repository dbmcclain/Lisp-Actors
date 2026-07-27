
(in-package :ac)

(spawn (lambda ()
         (let ((s1 (make-actor
                    (um:dlambda
                      (:get-val ()
                       (=bind (x)
                           (=async (=values 15))
                         (+ x 2)))))))
           (assert (eql 17 (ask s1 :get-val))))
         ))