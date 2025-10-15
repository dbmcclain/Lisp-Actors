
(in-package :ac)

(defun tree-beh (tree)
  (alambda
   ((cust :add key val)
    (become (tree-beh (maps:add tree key val)))
    (send cust :ok))

   ((cust :remove key)
    (become (tree-beh (maps:remove tree key)))
    (send cust :ok))
   ))
