
(in-package :com.ral.actors.language)

(actor-speak
 
 (def side-job-beh (cust side-action side-args)
   (beh (&rest msg)
     (apply side-action side-args)
     (apply cust msg)))
 
 (def guard-beh (action timeout must-do &rest must-args)
   (beh (cust fail &rest args)
     (val ((must-do-once (create (once-beh must-do)))
           (fail+side    (create (side-job-beh fail must-do-once must-args)))
           (cust+side    (create (side-job-beh cust must-do-once must-args))))
       (send-after 10 fail+side)
       (send* action cust+side fail+side args))
     ))
 )

