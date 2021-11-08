
(in-package :ac)

(defun tst ()
  (actor (cust ct)
    (let ((start (usec:get-time-usec)))
      (beta (stop)
          (send (actor (cust ct)
                  (if (plusp ct)
                      (send self cust (1- ct))
                    (send cust (usec:get-time-usec))))
                beta ct)
        (send cust (float (/ (- stop start) (+ ct 2))))
        ))))

(defun med3 (tst)
  (actor (cust &rest msg)
    (beta (ans1)
        (send* tst beta msg)
      (beta (ans2)
          (send* tst beta msg)
        (beta (ans3)
            (send* tst beta msg)
          (send cust (cadr (sort (list ans1 ans2 ans3) #'<)))
          )))))

(defun med (n tst)
  (actor (cust &rest msg)
    (let ((arr  (make-array n)))
      (beta _
          (send (actor (cust ix)
                  (if (>= ix n)
                      (send cust)
                    (let ((me self))
                      (beta (ans)
                          (send* tst beta msg)
                        (setf (aref arr ix) ans)
                        (send me cust (1+ ix)))
                      )))
                beta 0)
        (let* ((med (vm:median arr))
               (mad (vm:mad arr med)))
          (send cust med mad)
          )))))

#|
(send (tst) println 1000000)
(send (med3 (tst)) println 1000000)
(send (med 3 (tst)) println 1000000)

(time
 (loop repeat 1000000 do (usec:get-time-usec)))
|#
