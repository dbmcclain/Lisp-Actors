
(in-package :ecc)

A x^4 + B x^3 + C x^2 + D x + E

(with-gf2^8
  (mapcar (lambda (n)
            (gf^ 2 n))
          (um:range 0 8)))
