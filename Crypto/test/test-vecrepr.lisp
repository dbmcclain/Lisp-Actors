
(in-package :core-crypto-test)

(define-test conversions
  (let* ((n   (field-random *ed-r*))
         (n1  (bev n))
         (n2  (lev n))
         (n3  (hex n))
         (n4  (base58 n))
         (n5  (base64 n)))
    (assert-true (and (int= n n1)
                      (int= n n2)
                      (int= n n3)
                      (int= n n4)
                      (int= n n5)
                      (vec= (bev-vec n1) (reverse (lev-vec n2)))
                      ))))
#|
(lisp-unit:run-tests :all :crypto/test)
|#
