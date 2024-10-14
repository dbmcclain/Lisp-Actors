
(in-package :edec)


(with-ed-curve :curve-E521
  (ed-affine *ed-gen*))

(with-ed-curve :curve-E521f
  (ed-affine *ed-gen*))


(let* ((pt (with-ed-curve :curve-e521
             (ed-mul *ed-gen* 15)))
       (pt2 (with-ed-curve :curve-e521
              (ed-affine pt)))
       (pt3 (with-ed-curve :curve-e521f
              (ed-affine pt))))
  (list pt pt2 pt3))
  