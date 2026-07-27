
(in-package :ac)

(defun time-ordered-master-beh (cur-ctr next-evt-ctr next)
  (alambda

   ((cust :get-reservation)
    (send cust next-evt-ctr)
    (become (time-ordered-master-beh cur-ctr (1+ next-evt-ctr) next)))

   ((cust :do evt-id . msg)
    (cond ((= evt-id cur-ctr)
           (send* cust msg)
           (become (time-ordered-master-beh (1+ cur-ctr) next-evt-ctr next))
           (send next self :cur-evt (1+ cur-ctr)))

          (t
           (let ((new-next (make-actor
                            (pend-evt-beh evt-id cust msg next))))
             (become (time-ordered-master-beh cur-ctr next-evt-ctr new-next))
             ))
          ))))

(defun empty-pending-evt-beh ()
  (alambda

   ((prev :prune)
    (send prev :pruned self-beh))
   ))

(defun pend-evt-beh (evt-id cust msg next)
  (alambda

   ((prev :prune)
    (send prev :pruned self-beh))

   ((parent :cur-evt ix) when (= ix evt-id)
    (send* parent cust :do evt-id msg)
    (become (pruned-beh next))
    (send next self :prune))

   ( msg
     (send* next msg))
   ))

(defun pruned-beh (next)
  (alambda

   ((:pruned beh)
    (become beh))

   ( msg
     (send* next msg))
   ))

(defun make-time-ordered-dispatcher ()
  (actors ((pend   (empty-pending-evt-beh))
           (master (time-ordered-master-beh 0 0 pend)))
    (actor (cust . msg)
      (beta (id)
          (send master beta :get-reservation)
        (send* master cust :do id msg))
      )))
