
(in-package :ac)

(defun test-beh (a b c)
  (lambda (cust &rest msg)
    (become (sink-beh))
    (/ 0)
    (send cust 'test-beh a b c)))

(defvar *tst* (make-actor (test-beh 1 2 3)))

(send *tst* println)
