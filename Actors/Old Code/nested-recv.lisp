
(in-package :actors)

(defun tst-inner (&rest msg)
  (declare (ignore msg))
  (labels ((wait ()
             (recv
               ((list :finish-inner) t)
               ((list :retry-inner)
                (wait))
               )))
    (wait)))

(defun tst-outer (&rest msg)
  (declare (ignore msg))
  (tst-inner)
  (recv
    ((list :finish-outer) t)
    ))

(defvar *me* nil)

(inspect (setf *me* (spawn 'tst-outer)))

(send *me* :retry-inner)