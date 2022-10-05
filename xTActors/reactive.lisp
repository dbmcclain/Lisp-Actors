
(in-package :com.ral.actors.base)

(def-beh reactive-obj-beh (getter setter subscribers)
  ((cust :subscribe)
   (become (reactive-obj-beh getter setter (adjoin cust subscribers))))
   
  ((cust :unsubscribe)
   (become (reactive-obj-beh getter setter (remove cust subscribers))))
   
  ((cust :get)
   (send cust (funcall getter)))
   
  ((cust :set val)
   (let ((old (funcall getter)))
     (if (funcall setter val)
         (send-to-all (remove cust subscribers) self val)
       (send-to-all subscribers self old))
     )))

(defun reactive-obj (getter setter)
  (create (reactive-obj-beh getter setter nil)))

