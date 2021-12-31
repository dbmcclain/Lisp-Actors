
(in-package :com.ral.actors.base)

(defun reactive-obj-beh (getter setter subscribers)
  (lambda* msg
    (with-sponsor base-sponsor
      (match msg
        ((cust :subscribe)
         (become (reactive-obj-beh obj (adjoin cust subscribers))))

        ((cust :unsubscribe)
         (become (reactive-obj-beh obj (remove cust subscribers))))
        
        ((cust :get)
         (send cust (funcall getter)))
        
        ((cust :set val)
         (let ((old (funcall getter)))
           (if (funcall setter val)
               (send-to-all (remove cust subscribers) self val)
             (send-to-all subscribers self old))
           ))
        ))))

(defun reactive-obj (getter setter)
  (make-actor (reactive-obj-beh getter setter nil)))

