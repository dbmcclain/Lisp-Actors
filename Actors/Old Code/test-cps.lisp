
(in-package :ac)


(defun tst (x)
  (=bind (sum)
      (ac:spawn (lambda ()
                  (summer x 15)))
    (pr sum)))

(=defun summer (n1 n2)
  (=values (+ n1 n2)))

(ac:spawn 'tst 100)


(defun tst (x)
  (print x))

(lw:defadvice (tst tst-before :before)
    (x)
  (print "about to print"))

(lw:defadvice (tst tst-after :after)
    (x)
  (print "after print"))

(tst 15)
(funcall 'tst 15)
(funcall #'tst 15)