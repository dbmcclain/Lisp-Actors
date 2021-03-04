
(in-package :edec)

(print *ed-name*)

(setf x (ed-projective *ed-gen*))
(setf x (ed-projective-add x x))
(ed-affine x)

;; ---------------------------------------------------------------
;; Samples for tests...

(defvar *proj-pts*  nil)

(defun stuff-xs (fn &optional (n 10000))
  (setf *proj-pts* (loop repeat n collect (funcall fn))))

;; ---------------------------------------------------------------

(let ((x (ed-mul *ed-gen* (1- *ed-r*))))
  (ed-neutral-point-p (ed-add *ed-gen* x)))

(ed-pt= *ed-gen* (ed-decompress-pt (ed-compress-pt *ed-gen*)))

;; ---------------------------------------------------------------
;; Get some test points

;; get a list of points in projective coords
(defun stuff-pts ()
  (setf *proj-pts*
        (loop repeat 10000 collect
              (ed-nth-proj-pt (field-random *ed-q*))
              ;; (ed-basic-mul *ed-gen* (field-random *ed-q*))
              )))
(stuff-pts)

;; show us an image of the point on the curve
(let* ((pts (map 'vector 'ed-affine *proj-pts*))
       (fscal (lambda (x)
                (float (/ x *ed-q*) 1d0)))
       (xis (map 'vector (um:compose fscal 'ecc-pt-x) pts))
       (yis (map 'vector (um:compose fscal 'ecc-pt-y) pts)))
  (plt:plot 'plt xis yis
            :clear t
            :title "Field Distribution of Test Points" 
            :symbol :dot))

(defun tst-affine ()
  (loop for ct from 1
        for pt in *proj-pts* do
        (let* ((apt (ed-affine pt)))
          (assert (ed-pt= apt pt))))
  :okay)
(tst-affine)

(defun tst-proj-affine-mul ()
  (loop for pt in *proj-pts* do
        (let* ((r   (field-random *ed-r*))
               (pt1 (ed-mul pt r))
               (pt2 (ed-mul (ed-affine pt) r)))
          (assert (ed-pt= pt1 pt2))))
  :okay)
(tst-proj-affine-mul)

(loop repeat 10 do
      (stuff-pts)
      (tst-affine))

;; ---------------------------------------------------------------
;; Add Timings

(defun tst-add-mul ()
  (loop for pt in *proj-pts*
        for ct from 1
        for r = (1+ (random 100)) then (1+ (random 100))
        do
        (let ((a1  (ed-mul pt r))
              (a2  pt))
          (loop repeat (1- r) do
                (setf a2 (ed-add a2 pt)))
          (assert (ed-pt= a1 a2))))
  :okay)
(tst-add-mul)

(defun time-affine-add ()
  (let ((pts (mapcar 'ed-affine *proj-pts*)))
    ;; about 207 usec Ed3363
    ;; about 138 usec Curve1174
    (loop repeat 3 do
          (time (loop for pt in pts do (ed-affine-add pt pt))))))
(time-affine-add)

(defun time-projective-add ()
  ;; about 12.7 usec Ed3363
  ;; about 10.8 usec Curve1174
  (loop repeat 3 do
        (time (loop for pt in *proj-pts* do (ed-projective-add pt pt)))))
(time-projective-add)

(defun time-c-projective-add ()
  ;; about 8.6 usec Ed3363
  ;;       5.9 usec Curve1174
  (loop repeat 3 do
        (time (loop for pt in *proj-pts* do (ed-add pt pt)))))
(time-c-projective-add)

;; should be same as ed-projective-add, about 8.6 usec
(time (loop for pt in *proj-pts* do (ed-add pt pt)))

(defun time-overhead ()
  (loop repeat 3 do
        ;; about 1.4 usec interp, 0.0 usec compiled
        (time (loop for pt in *proj-pts* do 'identity))))
(time-overhead)

;; ---------------------------------------------------------------
;; Multiply Timings

(defun time-c-affine-mul ()
  (let ((pts (mapcar 'ed-affine *proj-pts*)))
    ;; about 79.5 usec Ed3363 - definitely faster to do in C lib
    ;;       48.8 usec Curve1174 (51 w/ Granger's method)
    (loop repeat 3 do
          (time (loop for pt in pts do (ed-mul pt (ecc-pt-x pt)))))))
(time-c-affine-mul)

(defun time-c-projective-mul ()
  ;; about 85.6 usec Ed3363 - a slight preference for affine pts
  ;;       52.9 us Curve1174
  (loop repeat 3 do
        (time (loop for pt in *proj-pts* do (ed-mul pt (ecc-pt-x pt))))))
(time-c-projective-mul)

(let ((x *ed-gen*)
      (y (1- *ed-r*)))
  ;; about 79 usec - definitely faster to do in C lib
  (time (loop repeat 10000 do (ed-mul x y))))

;; ---------------------------------------------------------------
;; Bulletproof Timins

;; about 0.098 s Ed3363
;;       0.064 s Curve1173
(loop repeat 3 do
      (time (range-proofs:make-range-proof 1234567890)))

;; about 0.117 s Ed3363
;;       0.047 s Curve1173
(let ((x (range-proofs:make-range-proof 1234567890)))
  (loop repeat 3 do
        (time (range-proofs:validate-range-proof x))))

;; ---------------------------------------------------------------
;; Modular Inverse Timings

(defun time-mod-inv ()
  (let ((xs (loop repeat 10000 collect (field-random *ed-q*))))
    (with-mod *ed-q*
      ;; about 65 usec
      (loop repeat 3 do
            (time (loop for x in xs do (minv x)))))))
(time-mod-inv)

(defun time-exp-mod-inv ()
  (let ((xs  (loop repeat 10000 collect (field-random *ed-q*)))
        (pow (- *ed-q* 2)))
    (with-mod *ed-q*
      ;; about 290 usec
      (loop repeat 3 do
            (time (loop for x in xs do (m^ x pow)))))))
(time-exp-mod-inv)

(defun time-c-mod-inv ()
  (let ((xs (loop repeat 10000 collect
                  (make-ecc-proj-pt
                   :x 1
                   :y 0
                   :z (field-random *ed-q*)))))
    ;; in the special case where modular base = *ed-q* for Ed3363
    ;; we could get an inverse by using the to-affine conversion...
    ;; How much faster if we did that?
    ;;
    ;; About 7.6 usec vs 96 usec. 12.6x speedup! (curve Ed3363)
    ;;       5.7 usec Curve1174
    (with-mod *ed-q*
      (loop repeat 3 do
            (time (loop for x in xs do (ed-affine x)))))))
(time-c-mod-inv)

;; ---------------------------------------------------------------
;; Affine / Projective Conversions Timings

;; about 9.0 usec Ed3363
;;       7 us Curve1174
(defun time-affine-conv ()
  (loop repeat 3 do
        (time (loop for x in *proj-pts* do (ed-affine x)))))
(time-affine-conv)

;; about 1 usec interp, 0 usec compiled
(defun time-proj-conv ()
  (loop repeat 3 do
        (time (loop for x in *proj-pts* do (ed-projective x)))))
(time-proj-conv)

;; about 68 usec
(time (loop for x in *proj-pts* do (ed-random-projective x)))

(loop for x in *proj-pts* do
      (assert (ed-pt= x (ed-affine x))))

;; ---------------------------------------------------------------
;; Point Compression/Decompression Timings

;; about 9.2 usec Ed3363
;;       7.1 us Curve1174
(defun time-compress-pt ()
  (loop repeat 3 do
        (time (loop for x in *proj-pts* do (ed-compress-pt x)))))
(time-compress-pt)

(let ((xs (loop for x in *proj-pts* collect (ed-compress-pt x))))
  ;; about 3100 usec Ed3363
  ;;        730 us Curve1174
  (time (loop for x in xs do (ed-decompress-pt x))))

(progn
  (loop repeat 1000 do
        (let* ((pt  (ed-random-generator))
               (x   (ed-compress-pt pt))
               (ptx (ed-decompress-pt x)))
          (assert (ed-pt= pt ptx))))
  :okay)

;; ================================================================
;; ----------------------------------------------------------------
;; Check sanctity of Elligator2 encodings/decodings

(with-ed-curve :curve1174
  ;; okay
  (let ((x (elli2-encode *ed-gen*)))
    (ed-pt= *ed-gen* (elli2-decode x))))

(with-ed-curve :curve1174
  ;; okay
  (loop for x in *proj-pts* do
        (let ((x (elli2-encode *ed-gen*)))
          (assert (ed-pt= *ed-gen* (elli2-decode x))))))
  
  
(with-ed-curve :curve-E382
  ;; okay
  (let ((x (elli2-encode *ed-gen*)))
    (ed-pt= *ed-gen* (elli2-decode x))))
  
(with-ed-curve :curve41417
  ;; does not work at all - gets a "not a square" error
  (let ((x (elli2-encode *ed-gen*)))
    (ed-pt= *ed-gen* (elli2-decode x))))
  
(with-ed-curve :curve-e521
  ;; okay
  (let ((x (elli2-encode *ed-gen*)))
    (ed-pt= *ed-gen* (elli2-decode x))))
  
(with-ed-curve :curve-ed3363
  ;; Runs without incident, but always returns NIL - does not work
  (let ((x (elli2-encode *ed-gen*)))
    (assert x)
    (ed-pt= *ed-gen* (elli2-decode x))))
  
(with-ed-curve :curve-ed3363
  ;; okay
  (funcall (compile nil
                    (lambda ()
                      (loop repeat 10
                            for ix from 1
                            do
                            (let* ((pt (ed-random-generator))
                                   (x  (elli2-encode pt)))
                              (if x
                                (unless (ed-pt= pt (elli2-decode x))
                                  (print (list ix pt x (elli2-decode x))))
                                (print (list ix :failed)))
                              )))
                    )))

(with-ed-curve :curve-ed3363
  ;; okay
  (funcall (compile nil
                    (lambda ()
                      (loop repeat 10
                            for ix from 1
                            do
                            (let* ((r  (field-random *ed-q*))
                                   (pt (elli2-decode r))
                                   (rr (elli2-encode pt)))
                              (unless (= r rr)
                                  (print (list ix r pt rr)))
                              )))
                    )))

(with-ed-curve :curve-ed448
  ;; Does not work - does not construct the x encoding
  (let ((x (elli2-encode *ed-gen*)))
    (assert x)
    (ed-pt= *ed-gen* (elli2-decode x))))

;; ------------------------------------------------------------------------
