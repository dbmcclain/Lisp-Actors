
(in-package :ac)

"a + b/c"
(defactor plus
  (lambda (cust a b)
    (send cust (+ a b))))

(defactor div
  (lambda (cust a b)
    (send cust (/ a b))))

(defactor oper
  (lambda (cust)
    (β (ans)
        (send div β b c)
      (send plus cust a ans))))

(defun call/ret (ac . args)  ;; aka ASK
  (let ((mb (mp:make-mailbox)))
    (send* ac mb args)
    (mp:mailbox-read mb)))