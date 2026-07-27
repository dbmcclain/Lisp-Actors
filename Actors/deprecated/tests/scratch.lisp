
(in-package :hash)

(let* ((x  (hash:hash/256 0))
       (xv (bev-vec x))
       (xe (loenc:encode xv))
       )
  (assert (equalp xv (vec-repr:vec x)))
  (write xe :base 16))