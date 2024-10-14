
(in-package :pbc)

(let* ((a  (mul-pt-zr (get-g1) 10))
      (b  (mul-pt-zr (get-g2) 20))
      (a*b (compute-pairing a b))
      (aa (mul-pt-zr (get-g1) 11))
      (aa*b (compute-pairing aa b))
      (r    (div-gts aa*b a*b))
      (rr   (mul-gts a*b r)))
  (inspect r)
  (assert (int= rr aa*b)))


(let* ((h1 (int (hash/256 :dog)))
       (h2 (int (hash/256 :cat)))
       (h3 (int (hash/256 :test))))
  (labels ((f (x)
             (let ((y (int (hash/256 x))))
               (* (- y h1)
                  (- y h2)
                  (- y h3)))))
    (f :dave)))
       