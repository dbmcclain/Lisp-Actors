
(in-package :com.ral.actors.secure-comm)

(defun rcvr-beh (&optional (ct 0))
  (alambda
   ((cust :reset)
    (send cust :ok)
    (become (rcvr-beh 0)))
   ((:inc)
    (let ((new-ct (1+ ct)))
      (send fmt-println "rcvr: ~D" new-ct)
      (become (rcvr-beh new-ct))))
   ((:show)
    (send fmt-println "rcvr count = ~D" ct))
   ))

(deflex rcvr (create (rcvr-beh)))

(defun tst (host n)
  (let ((recho (remote-service :echo host)))
    (loop repeat n do
            (send recho rcvr :inc))
    ))

#|
(tst "localhost")
(tst "arroyo.local" 20)
(tst "rincon.local")
(tst "rambo.local")
(tst "dachshund.local")
(tst "honeypot.local")
(tst "david-pc.local")
(tst "umbra.local")
(tst "zircon.local")
(tst "fornax.local" 5)

(send rcvr :show)
(send rcvr sink :reset)
|#
