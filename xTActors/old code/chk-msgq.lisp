
(in-package :com.ral.actors.base)

(defun tst ()
  (fsend (lambda ()
           (um:nlet iter ((msg *msg-freelist*)
                          (ctr 0))
             (if msg
                 (go-iter (msg-link msg) (1+ ctr))
               ctr)))
         println))
(tst)

(fsend (lambda ()
         (do ((msg *msg-freelist* (msg-link msg))
              (ctr 0              (1+ ctr)))
             ((null msg) ctr)
           ()))
       println)

(send (io (fn-actor (lambda ()
                      (do ((msg *msg-freelist* (msg-link msg))
                           (ctr 0              (1+ ctr)))
                          ((null msg) ctr)
                        ()))))
       println)