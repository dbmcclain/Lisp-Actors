|#
;; ------------------------------------------------------------------

(defun maxabs (x y)
  (max (abs x) (abs y)))

;; Largest seen variate is around 5.6 sigma So, there is a finite
;; probability of a mis-decoding. Hence, need to use ECC encoding of
;; message.
(let ((v (vm:gnoise 1000000)))
  (reduce #'max (map 'vector #'abs v)))

;; ---------------------------------
;; Hamming(7,4) encoding
;; Detect and correct any 1-bit error in 4-bit groups using 7-bit encoding

(defun btst (n m)
  (if (zerop (logand n m)) 0 1))

(defun bin4 (n)
  (list (btst n 8)
        (btst n 4)
        (btst n 2)
        (btst n 1)))

(defun bin7 (n)
  (list
   (btst n 64)
   (btst n 32)
   (btst n 16)
   (btst n 8)
   (btst n 4)
   (btst n 2)
   (btst n 1)))

(defun dec3 (lst)
  (+ (elt lst 0)
     (* 2 (elt lst 1))
     (* 4 (elt lst 2))))

(defun dec4 (lst)
  (+ (* 8 (elt lst 0))
     (* 4 (elt lst 1))
     (* 2 (elt lst 2))
     (* 1 (elt lst 3))))

(defun dec7 (lst)
  (+ (* 64 (elt lst 0))
     (* 32 (elt lst 1))
     (* 16 (elt lst 2))
     (* 8  (elt lst 3))
     (* 4  (elt lst 4))
     (* 2 (elt lst 5))
     (* 1 (elt lst 6))))

(defun trnx (m)
  (when (car m)
    (cons (mapcar #'car m)
          (trnx (mapcar #'cdr m)))))

(let* ((gt '((1 1 0 1)
             (1 0 1 1)
             (1 0 0 0)
             (0 1 1 1)
             (0 1 0 0)
             (0 0 1 0)
             (0 0 0 1)))
       (h  '((1 0 1 0 1 0 1)
             (0 1 1 0 0 1 1)
             (0 0 0 1 1 1 1)))
       (r  '((0 0 1 0 0 0 0)
             (0 0 0 0 1 0 0)
             (0 0 0 0 0 1 0)
             (0 0 0 0 0 0 1))))
  (labels ((enc/dec (n m)
             (mapcar (lambda (v)
                       (reduce #'logxor (mapcar #'* n v)))
                     m))
           (enc (n)
             (enc/dec n gt))
           (dec (n)
             (enc/dec n h))
           (cor (n)
             (let ((d (1- (dec3 (dec n)))))
               (if (minusp d)
                   n
                 (let ((nn (copy-seq n)))
                   (setf (elt nn d) (logxor 1 (elt nn d)))
                   nn))
               )))
    (loop for ix from 0 below 16 collect
          (let* ((b (bin4 ix))
                 (e (enc b))
                 (d (dec e))
                 (r (enc/dec e r)))
            (list ix b e d r)))
    #||#
    ;; try single-bit errors
    (loop for enc in '((0 0 0 0 0 0 0)
                       (1 0 0 0 0 0 0)
                       (0 1 0 0 0 0 0)
                       (0 0 1 0 0 0 0)
                       (0 0 0 1 0 0 0)
                       (0 0 0 0 1 0 0)
                       (0 0 0 0 0 1 0)
                       (0 0 0 0 0 0 1))
            collect
            (list enc (dec enc) (enc/dec (cor enc) r)))
    #||#
    #|
    (loop for enc in '((1 1 0 1 0 0 1 0)
                       (0 1 0 1 0 0 1 0)
                       (1 0 0 1 0 0 1 0)
                       (1 1 1 1 0 0 1 0)
                       (1 1 0 0 0 0 1 0)
                       (1 1 0 1 1 0 1 0)
                       (1 1 0 1 0 1 1 0)
                       (1 1 0 1 0 0 0 0)
                       (1 1 0 1 0 0 1 1))
            collect
            (list enc (dec enc)))
          |#
    (loop for ix from 0 below 128 collect
          (list ix (dec4 (enc/dec (cor (bin7 ix)) r))))
    (with-standard-io-syntax
      (print
       (list
        (coerce
         (loop for ix from 0 below 16 collect
                 (dec7 (enc (bin4 ix))))
         'vector)
        (coerce
         (loop for ix from 0 below 128 collect
                 (dec4 (enc/dec (cor (bin7 ix)) r)))
         'vector)
        )))
    ))

(setf *print-base* 16.)
(setf *print-base* 10.)

|#
