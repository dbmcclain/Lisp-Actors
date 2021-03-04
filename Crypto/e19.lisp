
(in-package :edwards-ecc)

(let* ((g 5)
       (fld (loop for ix from 1 below 19 collect (expt-mod 19 g ix))))
  (inspect fld))

(pprint
 (loop for g from 2 below 19 collect
       (let* ((seq (loop for ix from 1 below 19 collect
                         (expt-mod 19 g ix)))
              (pos    (position 1 seq)))
         (list (first seq) (1+ pos) seq))))
 

(pprint
 (loop for m in '(4 8 12 16)
       collect (cons m (mapcar (lambda (p)
                                 (cons p (mod p m)))
                               primes::*primes*))))