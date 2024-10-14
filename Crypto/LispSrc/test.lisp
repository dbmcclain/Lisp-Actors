
(in-package :ac)

(let+ ((x  (list 1 2 3))
       (y  (cdr x))
       ((xx yy) (loenc:decode (loenc:encode (list x y)))))
  (assert (equalp x xx))
  (assert (equalp y yy))
  (assert (eq yy (cdr xx))))
