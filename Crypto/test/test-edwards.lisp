
(in-package :core-crypto-test)

(defun check-edwards-curves-fn ()
  (and
   (hash-check edec::*curve1174*    edec::*chk-curve1174*)
   (hash-check edec::*curve-E382*   edec::*chk-curve-E382*)
   (hash-check edec::*curve41417*   edec::*chk-curve41417*)
   (hash-check edec::*curve-Ed448*  edec::*chk-curve-Ed448*)
   (hash-check edec::*curve-E521*   edec::*chk-curve-E521*)
   (hash-check edec::*curve-Ed3363* edec::*chk-curve-Ed3363*)
   ))

(define-test check-edwards-curves
  (assert-true (check-edwards-curves-fn)))

(defun get-pts ()
  (loop repeat 10000 collect
        (ed-nth-proj-pt (field-random *ed-q*))
        ))

(define-test tst-affine
  (loop for ct from 1
        for pt in (get-pts) do
        (let* ((apt (ed-affine pt)))
          (assert-true (ed-pt= apt pt)))))

(define-test tst-proj-affine-mul
  (loop for pt in (get-pts) do
        (let* ((r   (field-random *ed-r*))
               (pt1 (ed-mul pt r))
               (pt2 (ed-mul (ed-affine pt) r)))
          (assert-true (ed-pt= pt1 pt2)))))

(define-test tst-add-mul
  (loop for pt in (get-pts)
        for ct from 1
        for r = (1+ (random 100)) then (1+ (random 100))
        do
        (let ((a1  (ed-mul pt r))
              (a2  pt))
          (loop repeat (1- r) do
                (setf a2 (ed-add a2 pt)))
          (assert-true (ed-pt= a1 a2)))))

(define-test tst-cmpr-decmpr
  (loop repeat 1000 do
        (let* ((pt  (ed-random-generator))
               (x   (ed-compress-pt pt))
               (ptx (ed-decompress-pt x)))
          (assert-true (ed-pt= pt ptx)))))

(define-test tst-valid-curve
  (assert-true (and (ed-valid-point-p *ed-gen*)
                    (ed-neutral-point-p
                     (ed-add (ed-mul *ed-gen* (1- *ed-r*))
                             *ed-gen*)))))

