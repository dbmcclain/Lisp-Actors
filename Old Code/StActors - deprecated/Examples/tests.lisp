
(in-package :ac)

(defun tst (&optional (n 10000))
    (labels ((constr ()
               (let ((x  (sets:empty))
                     (xv (vm:unoise 1000 10)))
                 (map nil (lambda (v)
                            (sets:addf x v))
                      xv)
                 (setf xv (sets:elements x))
                 (values x xv)))
             (enc/dec (x xv)
               (let ((enc (loenc:encode x)))
                 ;; (sleep 0)
                 (let ((dec (loenc:decode enc)))
                   (assert (equalp (sets:elements dec) xv))
                   ))))
      (loop repeat 5 do
            (spawn-worker
             (lambda ()
               (multiple-value-bind (x xv)
                   (constr)
                 (loop repeat n do
                       (enc/dec x xv)))))
            )))

