
(in-package :ecc)

(defun enc-2/4 (a b)
  (with-gf2^8
    (labels ((shr (x)
               (gf- (gf* b x)
                    (gf* a (gf- x 1)))))
      (map 'vector #'shr '(0 1 2 3 4)))))

(defun poly2 (x1 x2 y1 y2)
  (lambda (x)
    (gf+
     (gf/ (gf* y1 (gf- x x2)) (gf- x1 x2))
     (gf/ (gf* y2 (gf- x x1)) (gf- x2 x1)))))

(defun dec-2/4 (vec4)
  (with-gf2^8
    (labels ((dec (x1 x2 y1 y2)
               (list x1 x2 (mapcar (poly2 x1 x2 y1 y2)
                                   '(0 1)))))
      (list (dec 0 1 (aref vec4 0) (aref vec4 1))
            (dec 0 2 (aref vec4 0) (aref vec4 2))
            (dec 0 3 (aref vec4 0) (aref vec4 3))
            (dec 0 4 (aref vec4 0) (aref vec4 4))
            (dec 1 2 (aref vec4 1) (aref vec4 2))
            (dec 1 3 (aref vec4 1) (aref vec4 3))
            (dec 1 4 (aref vec4 1) (aref vec4 4))
            (dec 2 3 (aref vec4 2) (aref vec4 3))
            (dec 2 4 (aref vec4 2) (aref vec4 4))
            (dec 3 4 (aref vec4 3) (aref vec4 4)))
      )))

(defun make-lagrange-poly (pairs)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (gf* prod (gf+ x2 x)))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum pair)
                 (destructuring-bind (x y) pair
                   (let ((xs (mapcar #'car (remove pair pairs))))
                     (gf+ sum
                          (gf* y (gf/ (lprod x0 xs)
                                      (lprod x  xs))))
                     ))))
        (reduce #'term pairs
                :initial-value 0)))
    ))

(defun poly3 (x1 x2 x3 y1 y2 y3)
  (make-lagrange-poly (list (list x1 y1)
                            (list x2 y2)
                            (list x3 y3))))

(defun enc-2/5 (a b)
  (with-gf2^8
    (map 'vector (poly3 0 1 2 (gf+ a b) a b) '(1 2 3 4 5))))

(defun dec-2/5 (vec5)
  (with-gf2^8
    (labels ((bref (ix)
               (aref vec5 (1- ix)))
             (dec (x1 x2 x3)
               (destructuring-bind (y1 y2 y3)
                   (mapcar #'bref (list x1 x2 x3))
                 (destructuring-bind (chk a b)
                     (mapcar (poly3 x1 x2 x3 y1 y2 y3) '(0 1 2))
               `((,x1 ,x2 ,x3) (,(if (zerop (gf+ chk a b))
                                     :ok
                                   :bad)
                                ,a ,b))
               ))))
      (list (dec 1 2 3)
            (dec 1 2 4)
            (dec 1 2 5)
            (dec 1 3 4)
            (dec 1 3 5)
            (dec 1 4 5)
            (dec 2 3 4)
            (dec 2 3 5)
            (dec 2 4 5)
            (dec 3 4 5))
      )))
#|
(enc-2/5 127 64)
=> #(127 64 0 26 90)

(dec-2/5 #(127 64 0 26 90))
=>
(((1 2 3) (:OK 127 64))
 ((1 2 4) (:OK 127 64))
 ((1 2 5) (:OK 127 64))
 ((1 3 4) (:OK 127 64))
 ((1 3 5) (:OK 127 64))
 ((1 4 5) (:OK 127 64))
 ((2 3 4) (:OK 127 64))
 ((2 3 5) (:OK 127 64))
 ((2 4 5) (:OK 127 64))
 ((3 4 5) (:OK 127 64)))

(dec-2/5 #(127 64 0 26 91)) ;; one byte destroyed
=>
(((1 2 3) (:OK 127 64))
 ((1 2 4) (:OK 127 64))
 ((1 2 5) (:BAD 127 64))
 ((1 3 4) (:OK 127 64))
 ((1 3 5) (:OK 127 237)) ;; !!
 ((1 4 5) (:BAD 127 204))
 ((2 3 4) (:OK 127 64))
 ((2 3 5) (:OK 197 64)) ;; !!
 ((2 4 5) (:BAD 199 64))
 ((3 4 5) (:OK 124 65))) ;; !!

(dec-2/5 #(126 64 0 26 91)) ;; two bytes destroyed
=>
(((1 2 3) (:OK 126 64))
 ((1 2 4) (:BAD 126 64))
 ((1 2 5) (:BAD 126 64))
 ((1 3 4) (:OK 126 180))
 ((1 3 5) (:OK 126 137))
 ((1 4 5) (:BAD 126 183))
 ((2 3 4) (:OK 127 64))
 ((2 3 5) (:OK 197 64))
 ((2 4 5) (:BAD 199 64))
 ((3 4 5) (:OK 124 65)))

 |#

(defun enc-xor-2/4 (a b)
  (with-gf2^8
    (list a b (gf+ a b) (gf+ a (gf* 2 b)))))

(defun dec-xor-2/4 (lst)
  (with-gf2^8
    (destructuring-bind (a b a+b a+2b) lst
      `(((1 2) ,a ,b)
        ((1 3) ,a ,(gf- a+b a))
        ((1 4) ,a ,(gf/ (gf- a+2b a) 2))
        ((2 3) ,(gf- a+b b) ,b)
        ((2 4) ,(gf- a+2b (gf* 2 b)) ,b)
        ,(let ((b (gf/ (gf+ a+b a+2b) 3)))
           `((3 4) ,(gf- a+b b) ,b))
        ))))

#|
(with-mod 929
  (labels ((poly (x)
             (m+ 1
                 (m* x
                     (m+ 2
                         (m* x 3))))))
    (mapcar #'poly '(0 1 2 3 4 5 6))))
|#
;; -------------------------------------------------------------
;; polynomials are represented by coefficent vectors (highest order first)
;;
;; poly-xxx fns are expected to be called within (WITH-MOD base ...)
;; gf-poly-xx fns are expected to be called within (WITH-GFxxx ...)

;; structural functions
(defun prepend-zeros (v nel)
  (concatenate 'vector (make-array nel :initial-element 0) v))

(defun make-same-length (a b)
  (let ((lena (length a))
        (lenb (length b)))
    (cond
     ((< lena lenb)
      (setf a (prepend-zeros a (- lenb lena))))
     ((< lenb lena)
      (setf b (prepend-zeros b (- lena lenb)))))
    (values a b)))

(defun trim-leading-zeros (v)
  (let ((vlen (1- (length v))))
    (um:nlet iter ((pos 0))
      (if (and (< pos vlen)
               (zerop (aref v pos)))
          (go-iter (1+ pos))
        (subseq v pos)))))

(defun poly-append-element (v x)
  (concatenate 'vector v (vector x)))

(defun poly-append-zero (v)
  (poly-append-element v 0))

;; ------------------------------------------------
;; arithmetic functions

(defun gen-poly-add (a b add-fn)
  (multiple-value-bind (a b) (make-same-length a b)
    (trim-leading-zeros (map 'vector add-fn a b))))

(defun poly-add (a b)
  (gen-poly-add a b #'m+))

(defun gf-poly-add (a b)
  (gen-poly-add a b #'gf+))

(defun z-poly-add (a b)
  (gen-poly-add a b #'+))

(defun poly-sub (a b)
  (gen-poly-add a b #'m-))

(defun gf-poly-sub (a b)
  (gen-poly-add a b #'gf-))

(defun z-poly-sub (a b)
  (gen-poly-add a b #'-))

(defun gen-poly-scale (v sf mul-fn)
  (map 'vector (um:curry mul-fn sf) v))

(defun poly-scale (v sf)
  (gen-poly-scale v sf #'m*))

(defun gf-poly-scale (v sf)
  (gen-poly-scale v sf #'gf*))

(defun z-poly-scale (v sf)
  (gen-poly-scale v sf #'*))

(defun gen-poly-mul (a b poly-add-fn poly-scale-fn)
  (let ((prod (vector)))
    (loop for mpx across b do
          (setf prod (funcall poly-add-fn
                              (poly-append-zero prod)
                              (funcall poly-scale-fn a mpx))))
    (trim-leading-zeros prod)))

(defun poly-mul (a b)
  (gen-poly-mul a b #'poly-add #'poly-scale))

(defun gf-poly-mul (a b)
  (gen-poly-mul a b #'gf-poly-add #'gf-poly-scale))

(defun z-poly-mul (a b)
  (gen-poly-mul a b #'z-poly-add #'z-poly-scale))

(defun gen-poly-divmod (p q inv-fn mul-fn sub-fn)
  (let* ((p    (trim-leading-zeros p))
         (plen (length p))
         (q    (trim-leading-zeros q))
         (qlen (length q)))
    (labels ((scale-fn (v sf)
               (map 'vector (um:curry mul-fn sf) v)))
      (let ((sf  (aref q 0)))
        (unless (eql sf 1)
          (let ((inv-sf (funcall inv-fn sf)))
            (setf q (scale-fn q inv-sf)
                  p (scale-fn p inv-sf))
            )))
      (um:nlet iter ((p    p)
                     (plen plen)
                     (quot (vector)))
        (if (< plen qlen)
            (values (trim-leading-zeros quot)
                    (trim-leading-zeros p)) ;; remainder mod q
          (let* ((pref (subseq p 0 qlen))
                 (suf  (subseq p qlen))
                 (ld   (aref pref 0)))
            (setf quot (poly-append-element quot ld)
                  pref (subseq
                        (if (zerop ld)
                            pref
                          (map 'vector sub-fn pref
                               (scale-fn q ld)))
                        1)
                  p    (concatenate 'vector pref suf))
            (go-iter p (1- plen) quot))
          )))))

(defun poly-divmod (p q)
  (gen-poly-divmod p q #'m/ #'m* #'m-))

(defun gf-poly-divmod (p q)
  (gen-poly-divmod p q #'gf/ #'gf* #'gf-))

(defun z-poly-divmod (p q)
  (gen-poly-divmod p q #'/ #'* #'-))

(defun gen-poly-eval (p x add-fn mul-fn)
  (reduce (lambda (sum c)
            (funcall add-fn c
                     (funcall mul-fn x sum)))
          p
          :initial-value 0))

(defun poly-eval (p x)
  (gen-poly-eval p x #'m+ #'m*))

(defun gf-poly-eval (p x)
  (gen-poly-eval p x #'gf+ #'gf*))

(defun z-poly-eval (p x)
  (gen-poly-eval p x #'+ #'*))

;; -----------------------------------------------------------------
;; Reed-Solomon Erasure Codes
#|
(with-gf2^8
  (let* ((gpoly (gf-poly-mul #(1 4) #(1 2)))
         (msg    #(127 63))
         (mpoly (gf-poly-mul msg #(1 0 0))))
    (multiple-value-bind (_ rem)
        (gf-poly-divmod mpoly gpoly)
      (let ((cpoly (gf-poly-sub mpoly rem)))
        (setf ;; (aref cpoly 1) (gf+ (aref cpoly 1) 3)
              ;; (aref cpoly 0) (gf+ (aref cpoly 0) 5)
              ;; (aref cpoly 0) 0
              (aref cpoly 1) 0
              ;; (aref cpoly 2) (gf+ (aref cpoly 2) 1)
              )
        (let ((s1    (gf-poly-eval cpoly 2))
              (s2    (gf-poly-eval cpoly 4))
              (s3    (gf-poly-eval cpoly 8))
              (s4    (gf-poly-eval cpoly 16)))
          (let ((e1  (gf* 36 (gf+ (gf* 4 s1) s2)))
                (e2  (gf* 72 (gf+ (gf* 8 s1) s2))))
          (list cpoly `(:syn ,s1 ,s2 ,s3 ,s4) `(:err ,e1 ,e2))
          ))))))


(with-gf2^8
  (let* ((ndrives   8)
         (prim-root 2)
         (p1        prim-root)
         (p2        (gf^ p1 2))
         (gpoly     (gf-poly-mul (vector 1 p1) (vector 1 p2)))
         (msg       #(127 63 32 47 65 66))
         ;; (msg       #(1   1  0  0  0  0))
         (mpoly     (gf-poly-mul msg #(1 0 0)))
         (bad-drive #(1 2))
         ;; bad drives A & B
         (ixA   (1- (aref bad-drive 0)))
         (exptA (- ndrives ixA 1))
         (ixB   (1- (aref bad-drive 1)))
         (exptB (- ndrives ixB 1)))
    (multiple-value-bind (_ rem)
        (gf-poly-divmod mpoly gpoly)
      (let ((cpoly (gf-poly-sub mpoly rem)))
        (setf ;; (aref cpoly 1) (gf+ (aref cpoly 1) 3)
              ;; (aref cpoly 0) (gf+ (aref cpoly 0) 5)
              ;; (aref cpoly 0) 0
              ;; (aref cpoly 3) 0
              ;; (aref cpoly 2) (gf+ (aref cpoly 2) 1)
              ;; (aref cpoly (1- (aref bad-drive 0))) 0
              (aref cpoly IXA) 0
              )
        (let* ((s1    (gf-poly-eval cpoly p1))
               (s2    (gf-poly-eval cpoly p2))
               (mat   (vector (gf^ p1 exptA) (gf^ p1 exptB)
                              (gf^ p2 exptA) (gf^ p2 exptB)))
               (det-inv (gf/ (gf+ (gf* (aref mat 0) (aref mat 3))
                                  (gf* (aref mat 1) (aref mat 2)))))
               (matinv (gf-poly-scale (vector (aref mat 3) (aref mat 1)
                                              (aref mat 2) (aref mat 0))
                                      det-inv)))
          (let ((e1  (gf+ (gf* (aref matinv 0) s1)
                          (gf* (aref matinv 1) s2)))
                (e2  (gf+ (gf* (aref matinv 2) s1)
                          (gf* (aref matinv 3) s2))))
          `(:bad-drive ,bad-drive
            :recv      ,cpoly
            :syn       (,s1 ,s2)
            :err       (,e1 ,e2)
            :corr ,(let ((v (copy-seq cpoly)))
                     (setf (aref v ixA) (gf+ e1 (aref v ixA))
                           (aref v ixB) (gf+ e2 (aref v ixB)))
                     v))        
          ))))))

(with-gf2^8
  (let* ((prim  2)
         (rt1   prim)
         (rt2   (gf^ rt1 2))
         (gpoly (gf-poly-mul (vector 1 rt1) (vector 1 rt2))))
    (loop for ix from 0 below 6
          for msg = #(1 0 0) then (poly-append-zero msg)
          collect
          (multiple-value-bind (_ rem)
              (gf-poly-divmod msg gpoly)
            (list ix rem)))))

Encoding Matrix for 2-code checksum for up to 6 drives.
For fewer drives use a lower left submatrix.
For 8-bit encoding using GF[2^8]
Using 2 checks allows for fixing 2 errors in erasure mode, with known drive errors,
or finding and fixing a single drive error, or detecting more than 2 errors.

Using rt1 = 2, rt2 = 4 = rt1^2

  [  1   0   0   0   0   0] msg              [A]
  [  0   1   0   0   0   0] [A] drive 1      [B]
  [  0   0   1   0   0   0] [B]              [C]
  [  0   0   0   1   0   0] [C]           => [D]
  [  0   0   0   0   1   0] [D]              [E]
  [  0   0   0   0   0   1] [E]              [F]
  [182 179 237 120  28   6] [F] drive 6      [chk1] using root 2
  [241  59 231 224  48   8]                  [chk2] using root 2^2

  Syndromes: (for rt1, rt2)
    syn = A*rt^7 + B*rt^6 + C*rt^5 + D*rt^4 + E*rt^3 + F*rt^2 + chk1*rt + chk2

                                      [A]
                                      [B]
    [128  64  32  16   8   4   2   1] [C]       => [syn(2)  ]
    [ 19 205 116  29  64  16   4   1] [D]          [syn(2^2)]
                                      [E]
                                      [F]
                                      [chk1]
                                      [chk2]

  If single error, then syn(2^2)/syn(2) = 2^N for error ord N (7 = A, 0 = chk2)
  Then error E = syn(2)/2^N = syn(2)^2/syn(2^2)

  If known double errors (erasure mode) then if E1 at ord N1, E2 and ord N2:

     1/Det * [4^N2  2^N2] * [syn(2)  ]  => [E1]
             [4^N1  2^N1]   [syn(2^2)]     [E2]

      with

         Det = 2^N1 * 4^N2 + 2^N2 * 4^N1  in GF(256)

  Make corrections with (Cn + En) - code + error and position ord N
  
|#
(defun gen-poly-dot (v1 v2 op-add op-mul)
  (assert (eql (length v1)
               (length v2)))
  (reduce op-add (map 'vector op-mul v1 v2)))

(defun poly-dot (v1 v2)
  (gen-poly-dot v1 v2 #'m+ #'m*))

(defun gf-poly-dot (v1 v2)
  (gen-poly-dot v1 v2 #'gf+ #'gf*))

(defun z-poly-dot (v1 v2)
  (gen-poly-dot v1 v2 #'+ #'*))

(defun enc-6/8 (vec)
  (assert (eql 6 (length vec)))
  (with-gf2^8
    (concatenate 'vector vec
                 (vector
                  (gf-poly-dot vec #(182 179 237 120 28 6))
                  (gf-poly-dot vec #(241  59 231 224 48 8)))
                 )))

(defun dec-6/8-syn (vec)
  (values
   (gf-poly-dot vec #(128  64  32  16   8   4  2  1))
   (gf-poly-dot vec #( 19 205 116  29  64  16  4  1))
   ))

(defun dec-6/8 (vec)
  (assert (eq 8 (length vec)))
  (let ((v (subseq vec 0 6))
        errpos)
    (with-gf2^8
      (multiple-value-bind (syn2 syn4) (dec-6/8-syn vec)
        (cond ((and (zerop syn2)
                    (zerop syn4))
               v)
              
              ((setf errpos (position (gf/ syn4 syn2) #(128 64 32 16 8 4 2 1)))
               (case errpos
                 ((6) (values v :error-chk1))
                 ((7) (values v :error-chk2))
                 (otherwise
                  (let ((c (aref v errpos)))
                    (setf (aref v errpos) (gf+ c (gf/ (gf* syn2 syn2) syn4)))
                    (values v
                            :correctable-error
                            `(:pos ,errpos))))
                 ))
              
              (t
               (values vec
                       :uncorrectable-error
                       `(:syn ,(vector syn2 syn4))))
              )))
    ))
  
#|  
;; -------------------------------------------------------------
;; for gf2^8 = gf(256)
;; factors of 255 are 3,5,17
;; subgroups of orders 1,3,5,15,17,51,85,255
(with-gf2^8
  (loop for x from 2 below 256
        for n = (um:nlet iter ((n 1)
                               (prod 1))
                  (if (eql 1 (setf prod (gf* prod x)))
                      n
                    (go-iter (1+ n) prod)))
        when (eql n 17)
        collect x))

;; 61 is prime
;; factors of 60 are 2^2,3,5
;; subgroups of orders 1,2,4,3,5,6,10,12,15,20,30,60
(with-mod 61
  (loop for x from 2 below 61
        for n = (um:nlet iter ((n    1)
                               (prod 1))
                  (if (eql 1 (setf prod (m* prod x)))
                      n
                    (go-iter (1+ n) prod)))
        when (eql n 30)
        collect x))

;; 31 is prime
;; 30 has factors 2,3,5
;; subgroups of order 1,2,3,5,6,10,15,30
(with-mod 31
  (loop for x from 2 below 31
        for n = (um:nlet iter ((n    1)
                               (prod 1))
                  (if (eql 1 (setf prod (m* prod x)))
                      n
                    (go-iter (1+ n) prod)))
        when (eql n 30)
        collect x))
|#

(defun share2 (x)
  (with-gf2^128
    (let ((a (random-between #.(ash 1 64) #.(ash 1 128))))
      (values (gf+ a x)
              (gf+ (gf* 2 a) x))
      )))

(defun decode-share2 (y1 y2)
  (with-gf2^128
    (solve-gf-lagrange 0
                       (make-crypto-share
                        :x 1
                        :y y1)
                       (make-crypto-share
                        :x 2
                        :y y2))))

#|
(multiple-value-call #'decode-share2 (share2 15))
(with-gf2^128
  (loop repeat 10000 do
        (let ((x (random-between 1 #.(ash 1 128))))
          (assert (= 1 (gf* x (gf/ x))))
          )))
(with-gf2^571
  (loop repeat 10000 do
        (let ((x (random-between 1 #.(ash 1 571))))
          (assert (= 1 (gf* x (gf/ x))))
          )))
|#

