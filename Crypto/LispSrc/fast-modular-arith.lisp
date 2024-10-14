;; fast-modular-arith.lisp
;;
;; DM/RAL  2023/02/21 07:13:57
;; ----------------------------------

(in-package #:user)

(defpackage #:fast-modular-arith
  (:use #:common-lisp #:edec #:vec-repr #:hash #:modmath)
  (:export
   #:xmod
   #:ffbase
   #:ffbase-nbits
   #:ffbase-base
   #:ffbase-wrap
   #:ffbase-montgy-ninv
   #:ffbase-montgy-rsq
   #:make-ffbase
   #:ffld
   #:ff-montgomery-mixin
   #:ffld-base
   #:ffld-class
   #:ffld-monty-class
   #:define-ffld
   #:ffadd
   #:ffsub
   #:ffneg
   #:ffmul
   #:ffsqr
   #:ffinv
   #:ffdiv
   #:ff+
   #:ff-
   #:ff*
   #:ff/
   #:ff^
   #:ff-to-montgy
   #:ff-from-montgy
   #:ff-montgy-mul
   #:ff-mongty-sqr
   #:ff=
   #:ff<
   #:ff>=
   ))

(in-package #:fast-modular-arith)

;; ----------------------------------
#|
;; Max SmallInt is 60 bits. So, allowing for carry to develop, we
;; should restrict ourselves to no more than 58 bits in normal form.

(defstruct modbase
  b nbits mod-nbits nvec nmsb)

(defstruct modnum
  base v26)

(defun mpy26 (a b)
  #F
  (declare (fixnum a b))
  (let* ((p  (* a b))
         (lo (ldb (byte 26 0) p))
         (hi (ash p -26)))
    (declare (fixnum p lo hi))
    (values lo hi)))

(defun vec26 (x nbits)
  (um:nlet iter ((x x)
                 (nbits nbits)
                 (v nil))
    (if (< nbits 26)
        (let ((xlo (ldb (byte nbits 0) x))
              (xhi (ash x (- nbits))))
          (values
           (coerce (cons xlo v) 'vector)
           xhi))
      (go-iter (ash x -26)
               (- nbits 26)
               (cons (ldb (byte 26 0) x) v))
      )))
    
(defun raw-enc26 (x base)
  (multiple-value-bind (v xhi)
      (vec26 x (modbase-nbits base))
    (values
     (make-modnum
      :base base
      :v26  v)
     xhi)))

(defun enc26 (x base)
  (raw-enc26 (mod x (modbase-b base)) base))

#|
  0  26  52  78 104
130 156 182 208 234
|#

(defun gadd (x y w)
  ;; w = x + y
  #F
  (declare (sys:simple-int64-vector x y w))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix))
             (x (ix)
               `(sys:int64-aref x ,ix))
             (y (ix)
               `(sys:int64-aref y ,ix)))
    (setf (w 0.) (sys:int64+ (x 0.) (y 0.))
          (w 1.) (sys:int64+ (x 1.) (y 1.))
          (w 2.) (sys:int64+ (x 2.) (y 2.))
          (w 3.) (sys:int64+ (x 3.) (y 3.))
          (w 4.) (sys:int64+ (x 4.) (y 4.))
          (w 5.) (sys:int64+ (x 5.) (y 5.))
          (w 6.) (sys:int64+ (x 6.) (y 6.))
          (w 7.) (sys:int64+ (x 7.) (y 7.))
          (w 8.) (sys:int64+ (x 8.) (y 8.))
          (w 9.) (sys:int64+ (x 9.) (y 9.))
          )))

(defun gsub (x y w)
  ;; w = x - y
  #F
  (declare (sys:simple-int64-vector x y w))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix))
             (x (ix)
               `(sys:int64-aref x ,ix))
             (y (ix)
               `(sys:int64-aref y ,ix)))
    (setf (w 0.) (sys:int64- (x 0.) (y 0.))
          (w 1.) (sys:int64- (x 1.) (y 1.))
          (w 2.) (sys:int64- (x 2.) (y 2.))
          (w 3.) (sys:int64- (x 3.) (y 3.))
          (w 4.) (sys:int64- (x 4.) (y 4.))
          (w 5.) (sys:int64- (x 5.) (y 5.))
          (w 6.) (sys:int64- (x 6.) (y 6.))
          (w 7.) (sys:int64- (x 7.) (y 7.))
          (w 8.) (sys:int64- (x 8.) (y 8.))
          (w 9.) (sys:int64- (x 9.) (y 9.))
          )))

(defun gdec (x w)
  ;; w -= x
  #F
  (declare (sys:simple-int64-vector x w))
  (gsub w x w))

(defun gcopy (x w)
  ;; w = x
  #F
  (declare (sys:simple-int64-vector x w))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix))
             (x (ix)
               `(sys:int64-aref x ,ix)))
    (setf (w 0.) (x 0.)
          (w 1.) (x 1.)
          (w 2.) (x 2.)
          (w 3.) (x 3.)
          (w 4.) (x 4.)
          (w 5.) (x 5.)
          (w 6.) (x 6.)
          (w 7.) (x 7.)
          (w 8.) (x 8.)
          (w 9.) (x 9.)
          )))

(defun gmul2 (w)
  ;; w *= 2
  #F
  (declare (sys:simple-int64-vector w))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix)))
    (setf (w 0.) (sys:int64<< (w 0.) 1)
          (w 1.) (sys:int64<< (w 1.) 1)
          (w 2.) (sys:int64<< (w 2.) 1)
          (w 3.) (sys:int64<< (w 3.) 1)
          (w 4.) (sys:int64<< (w 4.) 1)
          (w 5.) (sys:int64<< (w 5.) 1)
          (w 6.) (sys:int64<< (w 6.) 1)
          (w 7.) (sys:int64<< (w 7.) 1)
          (w 8.) (sys:int64<< (w 8.) 1)
          (w 9.) (sys:int64<< (w 9.) 1)
          )))

(defun gsb2 (x w)
  ;; w -= 2*x
  #F
  (declare (sys:simple-int64-vector x w))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix))
             (x (ix)
               `(sys:int64-aref x ,ix)))
    (setf (w 0.) (sys:int64- (w 0.) (sys:int64<< (x 0.) 1))
          (w 1.) (sys:int64- (w 1.) (sys:int64<< (x 1.) 1))
          (w 2.) (sys:int64- (w 2.) (sys:int64<< (x 2.) 1))
          (w 3.) (sys:int64- (w 3.) (sys:int64<< (x 3.) 1))
          (w 4.) (sys:int64- (w 4.) (sys:int64<< (x 4.) 1))
          (w 5.) (sys:int64- (w 5.) (sys:int64<< (x 5.) 1))
          (w 6.) (sys:int64- (w 6.) (sys:int64<< (x 6.) 1))
          (w 7.) (sys:int64- (w 7.) (sys:int64<< (x 7.) 1))
          (w 8.) (sys:int64- (w 8.) (sys:int64<< (x 8.) 1))
          (w 9.) (sys:int64- (w 9.) (sys:int64<< (x 9.) 1))
          )))

(defconstant +kBPW+        26.)
(defconstant +kBPW-Final+  17.)
;; (defconstant +bot-26-bits+ (sys:int64-1- (sys:int64<< sys:+int64-1+ 26.)))
;; (defconstant +bot-17-bits+ (sys:int64-1- (sys:int64<< sys:+int64-1+ 17.)))
;; (defconstant +kWrap+       (sys:integer-to-int64 9.))
(makunbound '+bot-26-bits+)
            
(defvar *masks* (sys:make-simple-int64-vector 2
                                              :initial-contents (list (1- (ash 1 +kBPW+))
                                                                      (1- (ash 1 +kBPW-Final+)))
                                              ))

(define-symbol-macro +bot-26-bits+ (sys:int64-aref *masks* 0))
(define-symbol-macro +bot-17-bits+ (sys:int64-aref *masks* 1))

(defun scr (w)
  ;; short coeff reduction
  #F
  (declare (sys:simple-int64-vector w))
  (let ((tt (sys:make-simple-int64-vector 3)))
    (declare (sys:simple-int64-vector tt))
    (macrolet ((w (ix)
                 `(sys:int64-aref w ,ix))
               (tt (ix)
                 `(sys:int64-aref tt ,ix)))
      (setf (tt 0) (sys:int64-logand (w 0.) +bot-26-bits+)
            
            (tt 1) (sys:int64+ (w 1.) (sys:int64>> (w 0.) +kBPW+))
            (w 1.) (sys:int64-logand (tt 1) +bot-26-bits+)
            (tt 2) (sys:int64+ (w 2.) (sys:int64>> (tt 1) +kBPW+))
            (w 2.) (sys:int64-logand (tt 2) +bot-26-bits+)

            (tt 1) (sys:int64+ (w 3.) (sys:int64>> (tt 2) +kBPW+))
            (w 3.) (sys:int64-logand (tt 1) +bot-26-bits+)
            (tt 2) (sys:int64+ (w 4.) (sys:int64>> (tt 1) +kBPW+))
            (w 4.) (sys:int64-logand (tt 2) +bot-26-bits+)
            
            (tt 1) (sys:int64+ (w 5.) (sys:int64>> (tt 2) +kBPW+))
            (w 5.) (sys:int64-logand (tt 1) +bot-26-bits+)
            (tt 2) (sys:int64+ (w 6.) (sys:int64>> (tt 1) +kBPW+))
            (w 6.) (sys:int64-logand (tt 2) +bot-26-bits+)
                           
            (tt 1) (sys:int64+ (w 7.) (sys:int64>> (tt 2) +kBPW+))
            (w 7.) (sys:int64-logand (tt 1) +bot-26-bits+)
            (tt 2) (sys:int64+ (w 8.) (sys:int64>> (tt 1) +kBPW+))
            (w 8.) (sys:int64-logand (tt 2) +bot-26-bits+)

            (tt 1) (sys:int64+ (w 9.) (sys:int64>> (tt 2) +kBPW+))
            (w 9.) (sys:int64-logand (tt 1) +bot-17-bits+)

            (tt 2) (sys:int64>> (tt 1) +kBPW-Final+)
            (w 0.) (sys:int64+ (tt 0)
                               (sys:int64+ (tt 2)
                                           (sys:int64<< (tt 2) 3.)))
            ))))



(defun scr (w)
  ;; short coeff reduction
  #F
  (declare (sys:simple-int64-vector w))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix)))
    (let ((t0  (sys:int64-logand (w 0.) +bot-26-bits+))
          (t1  (sys:int64+ (w 1.) (sys:int64>> (w 0.) +kBPW+))))
      (setf (w 1.) (sys:int64-logand t1 +bot-26-bits+))

      (let ((t2 (sys:int64+ (w 2.) (sys:int64>> t1 +kBPW+))))
        (setf (w 2.) (sys:int64-logand t2 +bot-26-bits+))
        
        (let ((t1 (sys:int64+ (w 3.) (sys:int64>> t2 +kBPW+))))
          (setf (w 3.) (sys:int64-logand t1 +bot-26-bits+))

          (let ((t2 (sys:int64+ (w 4.) (sys:int64>> t1 +kBPW+))))
            (setf (w 4.) (sys:int64-logand t2 +bot-26-bits+))
            
            (let ((t1 (sys:int64+ (w 5.) (sys:int64>> t2 +kBPW+))))
              (setf (w 5.) (sys:int64-logand t1 +bot-26-bits+))

              (let ((t2 (sys:int64+ (w 6.) (sys:int64>> t1 +kBPW+))))
                (setf (w 6.) (sys:int64-logand t2 +bot-26-bits+))
                           
                (let ((t1 (sys:int64+ (w 7.) (sys:int64>> t2 +kBPW+))))
                  (setf (w 7.) (sys:int64-logand t1 +bot-26-bits+))

                  (let ((t2 (sys:int64+ (w 8.) (sys:int64>> t1 +kBPW+))))
                    (setf (w 8.) (sys:int64-logand t2 +bot-26-bits+))

                    (let ((t1 (sys:int64+ (w 9.) (sys:int64>> t2 +kBPW+))))
                      (setf (w 9.) (sys:int64-logand t1 +bot-17-bits+))

                      (let ((t2 (sys:int64>> t1 +kBPW-Final+)))
                        (setf (w 0.) (sys:int64+ t0
                                                 (sys:int64+ t2
                                                             (sys:int64<< t2 3.)))
                              )))))))))))))

(defun gmuli (w i)
  ;; w *= immed const
  #F
  (declare (sys:simple-int64-vector w)
           (sys:int64 i))
  (macrolet ((w (ix)
               `(sys:int64-aref w ,ix)))
    (let ((tmp (sys:int64* (w 0) i)))
      (setf (w 0) (sys:int64-logand tmp +bot-26-bits+))
      (let ((tmp (sys:int64+
                  (sys:int64* (w 1) i)
                  (sys:int64>> tmp +kBPW+)) ))
        (setf (w 1) (sys:int64-logand tmp +bot-26-bits+))
        (let ((tmp (sys:int64+
                    (sys:int64* (w 2) i)
                    (sys:int64>> tmp +kBPW+)) ))
          (setf (w 2) (sys:int64-logand tmp +bot-26-bits+))
          (let ((tmp (sys:int64+
                      (sys:int64* (w 3) i)
                      (sys:int64>> tmp +kBPW+)) ))
            (setf (w 3) (sys:int64-logand tmp +bot-26-bits+))
            (let ((tmp (sys:int64+
                        (sys:int64* (w 4) i)
                        (sys:int64>> tmp +kBPW+)) ))
              (setf (w 4) (sys:int64-logand tmp +bot-26-bits+))
              (let ((tmp (sys:int64+
                          (sys:int64* (w 5) i)
                          (sys:int64>> tmp +kBPW+)) ))
                (setf (w 5) (sys:int64-logand tmp +bot-26-bits+))
                (let ((tmp (sys:int64+
                            (sys:int64* (w 6) i)
                            (sys:int64>> tmp +kBPW+)) ))
                  (setf (w 6) (sys:int64-logand tmp +bot-26-bits+))
                  (let ((tmp (sys:int64+
                              (sys:int64* (w 7) i)
                              (sys:int64>> tmp +kBPW+)) ))
                    (setf (w 7) (sys:int64-logand tmp +bot-26-bits+))
                    (let ((tmp (sys:int64+
                                (sys:int64* (w 8) i)
                                (sys:int64>> tmp +kBPW+)) ))
                      (setf (w 8) (sys:int64-logand tmp +bot-26-bits+))
                      (let ((tmp (sys:int64+
                                  (sys:int64* (w 9) i)
                                  (sys:int64>> tmp +kBPW+)) ))
                        (setf (w 9) (sys:int64-logand tmp +bot-17-bits+))
                        
                        (let ((tmp (sys:int64>> tmp +kBPW-Final+)))
                          (setf (w 0) (sys:int64+ (w 0)
                                                  (sys:int64+ tmp
                                                              (sys:int64<< tmp 3.))))
                          )))))))))))))


9: 0*9 + 1*8 + 2*7 + 3*6 + 4*5 + 5*4 + 6*3 + 7*2 + 8*1 + 9*0
8: 0*8 + 1*7 + 2*6 + 3*5 + 4*4 + 5*3 + 6*2 + 7*1 + 8*0 + wr*(9*9)  wr = 1200 = 2^(10*26) mod (2^251 - 9)
7: 0*7 + 1*6 + 2*5 + 3*4 + 4*3 + 5*2 + 6*1 + 7*0 + wr*(8*9 + 9*8)
6: 0*6 + 1*5 + 2*4 + 3*3 + 4*2 + 5*1 + 6*0 + wr*(7*9 + 8*8 + 9*7)
5: 0*5 + 1*4 + 2*3 + 3*2 + 4*1 + 5*0 + wr*(6*9 + 7*8 + 8*7 + 9*6)
4: 0*4 + 1*3 + 2*2 + 3*1 + 4*0 + wr*(5*9 + 6*8 + 7*7 + 8*6 + 9*5)
3: 0*3 + 1*2 + 2*1 + 3*0 + wr*(4*9 + 5*8 + 6*7 + 7*6 + 8*5 + 9*4)
2: 0*2 + 1*1 + 2*0 + wr*(3*9 + 4*8 + 5*7 + 6*6 * 7*5 + 8*4 + 9*2)
1: 0*1 + 1*0 + wr*(2*9 + 3*8 + 4*7 + 5*6 + 6*5 + 7*4 + 8*3 + 9*2)
0: 0*0 + wr*(1*9 + 2*8 + 3*7 + 4*6 + 5*5 + 6*4 + 7*3 + 8*2 + 9*1)

F: F0 + t*F1
G: G0 + t*G1
F*G: (F0 + t*F1)*(G0 + t*G1) = F0*G0 + t*(F0*G1 + F1*G0) + t^2*F1*G1
     = (1-t)*(F0*G0 - t*F1*G1) + t*(F0+F1)*(G0+G1)

  0  21  42
 63  84 105
126 147 168
189 210 231 (252)

F: F0 + 2^126 * F1 = (f0 + 2^21*f1 + 2^42*f2 + 2^63*f3 + 2^84*f4  + 2^105*f5)
             + 2^126*(f6 + 2^21*f7 + 2^42*f8 + 2^63*f9 + 2^84*f10 + 2^105*f11)
F0: F00 + 2^63 * F01 = (f0 + 2^21*f1 + 2^42*f2) + 2^63*(f3 + 2^21*f4 + 2^42*f5)
F1: F10 + 2^63 * F11 = (f6 + 2^21*f7 + 2^42*f8) + 2^63*(f9 + 2^21*f10 + 2^42*f11)

F00 * G00:
2^0  h0 = f0*g0
2^21 h1 = f0*g1 + f1*g0
2^42 h2 = f0*g2 + f1*g1 + f2*g0
2^63 h3 = f1*g2 + f2*g1
2^84 h4 = f2*g2
|#
;; ---------------------------------------------------------------------

(defun xmod (n m &optional wrap)
  ;; n mod m
  #F
  (declare (integer n m))
  (let ((mbits (integer-length m)))
    (declare (fixnum mbits))
    
    (um:nlet iter ((n  n))
      (let ((nbits (integer-length n)))
        (declare (fixnum nbits))
        
        (cond ((< nbits mbits)
               (if (minusp n)
                   (+ n m)
                 n))
              
              ((= nbits mbits)
               (if (minusp n)
                   (go-iter (+ n m))
                 (let ((ans (- n m)))
                   (declare (integer ans))
                   (if (minusp ans)
                       n
                     ans))
                 ))
              (t  ;; nbits > mbits
                  (let* ((hi  (ash n (- mbits)))
                         (lo  (ldb (byte mbits 0) n)) ;; (- n (ash hi mbits)))
                         (wr  (or wrap
                                  (get 'xmod m)
                                  (setf (get 'xmod m) (- (ash 1 mbits) m)))))
                    (declare (integer hi lo wr))
                    (go-iter (+ lo (* hi wr)))
                    ))
              )))
    ))

#|
(xmod (1- (ash 1 250)) *ed-r*)
(xmod (* (ash 1 251) (ash 1 251)) *ed-r*)
(let* ((x (+ (1- (ash 1 250))))
       (hi (ash x -249))
       (lo (- x (ash hi 249))))
  (eql (ldb (byte 249 0) x) lo))
|#
#|
(let ((xs (loop repeat 10000 collect (int (com.ral.crypto.prng:ctr-drbg 400)))))
  (time (dolist (v xs)
          (xmod v *ed-r*)))
  (time (dolist (v xs)
          (mod v *ed-r*)))))
|#
;; ---------------------------------------------------------------------

(defun splitv (v n)
  (declare (type (vector fixnum) v)
           (type fixnum n))
  (values (subseq v 0 n)
          (subseq v n)))

(defun generic-karatsuba-multv (nel nel0 x y bpw)
  ;; Based on Bernstein's "Refined Karatsuba Identity" mutliplication:
  ;;   (X0+w*X1)*(Y0+w*Y1) = (1-w)*(X0*Y0 - w*X1*Y1) + w*(X0+X1)*(Y0+Y1)
  #F
  (declare (type fixnum nel nel0 bpw)
           (type (vector fixnum) x y))
  (let* ((nel1   (- nel nel0))
         (nel<   (min nel0 nel1))
         (nel*2  (ash nel 1))
         (nelw   (- nel*2 nel<))
         (ans    (make-array nel*2
                             :element-type 'fixnum
                             :initial-element 0))
         (answ   (make-array nelw
                             :element-type 'fixnum
                             :displaced-to ans
                             :displaced-index-offset nel<)))
    (declare (type fixnum nel1 nel< nel*2 nelw)
             (type (vector fixnum) ans answ))
    (multiple-value-bind (x0 x1) (splitv x nel<)
      (declare (type (vector fixnum) x0 x1))
      (multiple-value-bind (y0 y1) (splitv y nel<)
        (declare (type (vector fixnum) y0 y1))
        (let*   ((x0y0   (multv x0 y0 bpw))
                 (x1y1   (multv x1 y1 bpw))
                 (x0+x1  (map-into x1 #'+ x0 x1))
                 (y0+y1  (map-into y1 #'+ y0 y1))
                 (xsys   (multv x0+x1 y0+y1 bpw)))
          (declare (type (vector fixnum)
                         x0y0 x1y1 x0+x1 y0+y1 xsys))
          ;; ans = x0*y0
          (replace ans x0y0)
          ;; ans -= w*X1*Y1
          (map-into answ #'- answ x1y1)
          ;; ans *= (1 - w)
          (let ((cans (subseq ans 0 nelw)))
            (map-into answ #'- answ cans))
          ;; ans += w*(X0+X1)*(Y0+Y1)
          (map-into answ #'+ answ xsys)
          ans
          )))))

(defun multv1 (x y bpw)
  #F
  (declare (type fixnum bpw)
           (type (vector fixnum 1) x y))
  (let ((ans  (make-array 2.
                          :element-type 'fixnum))
        (nrshift (- bpw))
        (bits    (byte bpw 0))
        (tmp     0)
        (cy      0))
    (declare (type fixnum nrshift tmp cy)
             (type (vector fixnum 2.) ans))
    (macrolet ((x (ix)
                 `(aref x ,ix))
               (y (ix)
                 `(aref y ,ix))
               (ans (ix)
                 `(aref ans ,ix))
               (mpx (n m)
                 `(* (x ,n) (y ,m)))
               (limb (n &rest pairs)
                 `(setf tmp (+ cy
                               ,@(mapcar #`(mpx ,@a1) pairs))
                        cy  (ash tmp nrshift)
                        (ans ,n) (ldb bits tmp)))
               (final (n)
                 `(progn
                    (setf (ans ,n) cy)
                    ans)))
      (limb 0 (0 0))
      (final 1)
      )))

(defun multv2 (x y bpw)
  #F
  (declare (type fixnum bpw)
           (type (vector fixnum 2.) x y))
  (let ((ans     (make-array 4.
                             :element-type 'fixnum))
        (nrshift (- bpw))
        (bits    (byte bpw 0))
        (tmp     0)
        (cy      0))
    (declare (type fixnum nrshift tmp cy)
             (type (vector fixnum 4.) ans)) 
    (macrolet ((x (ix)
                 `(aref x ,ix))
               (y (ix)
                 `(aref y ,ix))
               (ans (ix)
                 `(aref ans ,ix))
               (mpx (n m)
                 `(* (x ,n) (y ,m)))
               (limb (n &rest pairs)
                 `(setf tmp (+ cy
                               ,@(mapcar #`(mpx ,@a1) pairs))
                        cy  (ash tmp nrshift)
                        (ans ,n) (ldb bits tmp)))
               (final (n)
                 `(progn
                    (setf (ans ,n) cy)
                    ans)))
      (limb 0 (0 0))
      (limb 1 (0 1) (1 0))
      (limb 2 (1 1))
      (final 3)
      )))

(defun multv3 (x y bpw)
  #F
  (declare (type fixnum bpw)
           (type (vector fixnum 3.) x y))
  (let ((ans     (make-array 6.
                             :element-type 'fixnum))
        (nrshift (- bpw))
        (bits    (byte bpw 0))
        (tmp     0)
        (cy      0))
    (declare (type fixnum nrshift tmp cy)
             (type (vector fixnum 6.) ans)) 
    (macrolet ((x (ix)
                 `(aref x ,ix))
               (y (ix)
                 `(aref y ,ix))
               (ans (ix)
                 `(aref ans ,ix))
               (mpx (n m)
                 `(* (x ,n) (y ,m)))
               (limb (n &rest pairs)
                 `(setf tmp (+ cy
                               ,@(mapcar #`(mpx ,@a1) pairs))
                        cy  (ash tmp nrshift)
                        (ans ,n) (ldb bits tmp)))
               (final (n)
                 `(progn
                    (setf (ans ,n) cy)
                    ans)))
      (limb 0 (0 0))
      (limb 1 (0 1) (1 0))
      (limb 2 (0 2) (1 1) (2 0))
      (limb 3 (1 2) (2 1))
      (limb 4 (2 2))
      (final 5)
      )))

(defun multv5 (x y bpw)
  #F
  (declare (type fixnum bpw)
           (type (vector fixnum 5.) x y))
  (let ((ans     (make-array 10.
                             :element-type 'fixnum))
        (nrshift (- bpw))
        (bits    (byte bpw 0))
        (tmp     0)
        (cy      0))
    (declare (type fixnum nrshift tmp cy)
             (type (vector fixnum 10.) ans)) 
    (macrolet ((x (ix)
                 `(aref x ,ix))
               (y (ix)
                 `(aref y ,ix))
               (ans (ix)
                 `(aref ans ,ix))
               (mpx (n m)
                 `(* (x ,n) (y ,m)))
               (limb (n &rest pairs)
                 `(setf tmp (+ cy
                               ,@(mapcar #`(mpx ,@a1) pairs))
                        cy  (ash tmp nrshift)
                        (ans ,n) (ldb bits tmp)))
               (final (n)
                 `(progn
                    (setf (ans ,n) cy)
                    ans)))
      (limb 0 (0 0))
      (limb 1 (0 1) (1 0))
      (limb 2 (0 2) (1 1) (2 0))
      (limb 3 (0 3) (1 2) (2 1) (3 0))
      (limb 4 (0 4) (1 3) (2 2) (3 1) (4 0))
      (limb 5 (1 4) (2 3) (3 2) (4 1))
      (limb 6 (2 4) (3 3) (4 2))
      (limb 7 (3 4) (4 3))
      (limb 8 (4 4))
      (final 9)
      )))

(defun multv (x y &optional (bpw 21.))
  ;; Multiply two (same-sized) vectors of fixnums.
  ;; Vectors stored in low-endian order, LSB first, MSB last
  #F
  (declare (type (vector fixnum) x y)
           (type fixnum bpw))
  (let ((nel  (length x)))
    (declare (type fixnum nel))
    (assert (eql nel (length y)))
    (cond ((eql nel 2.)
           ;; faster schoolbook open coded method
           (multv2 x y bpw))
          ;; ------------------------------------------
          ((evenp nel)
           ;; Split in half, perform Karatsuba decomposition
           ;; From Bernstein's "Refined Karatsuba Identity":
           ;;   (X0 + w*X1)*(Y0 + w*Y1) = (1-w)*(X0*Y0 - w*X1*Y1) + w*(X0+X1)*(Y0+Y1)
           (generic-karatsuba-multv nel (ash nel -1) x y bpw))
          ;; --------------------------------------------
          ;; Open-coded Schoolbook multiply routines
          ;; ------------------------------
          ((eql nel 1)
           (multv1 x y bpw))
          ;; ------------------------------
          ((eql nel 3.)
           (multv3 x y bpw))
          ;; ------------------------------
          ((eql nel 5.)
           (multv5 x y bpw))
          ;; ------------------------------
          (t
           ;; By this point we have covered nel = 1, 2, 3, 4, 5, 6.
           ;; All further nel > 5, so use a nel=(5 + n) split.
           (generic-karatsuba-multv nel 5 x y bpw))
          )))

;; ----------------------------------------------------------------------------

(defun rsc (x &optional (bpw 21.))
  ;; reduce short coeffs
  #F
  (declare (type (vector fixnum) x))
  (let ((cy      0)
        (nrshift (- bpw))
        (bits    (byte bpw 0)))
    (declare (type fixnum cy nrshift))
    (map-into x (lambda (v)
                  (declare (fixnum v))
                  (let ((vv (+ v cy)))
                    (declare (fixnum vv))
                    (setf cy (ash vv nrshift))
                    (ldb bits vv) ;; (- vv (ash cy bpw))
                    ))
              x)
    (values x cy)))

;; ----------------------------------------------------------------------------
#|
(defun vec1221 (x)
  (let ((elts (loop repeat 12.
                    for offs from 0 by 21.
                    collect
                      (ldb (byte 21. offs) x))))
    (make-array 12.
                :element-type 'fixnum
                :initial-contents elts)
    ))

(defun vec21 (x)
  (let ((bits (byte 21. 0)))
    (um:nlet iter ((x x)
                   (nbits 251.)
                   (ans nil))
      (if (< nbits 21.)
          (coerce (nreverse (cons x ans)) 'vector)
        (go-iter (ash x -21.)
                 (- nbits 21.)
                 (cons (ldb bits x) ans)
                 ;; (cons (logand #.(1- (ash 1 21.)) x) ans)
                 ))
      )))

(let ((x (vec1221 *ed-q*)))
  (print x)
  (rsc (multv x x)))


(defun vec3nib (x)
  (coerce 
   (loop repeat 3.
         for offs from 0 by 4
         collect
           (ldb (byte 4 offs) x))
   'vector))

(let ((x (vec3nib #xfff)))
  (multv x x 4.))

(defun vec6nib (x)
  (coerce 
   (loop repeat 6.
         for offs from 0 by 4
         collect
           (ldb (byte 4 offs) x))
   'vector))

(vec6nib #x123456)

(Let ((x (vec6nib #xffffff)))
  (rsc
   (multv x x 4)
   4))

(let ((x (vec6nib -1)))
  (print x)
  (rsc x 4))
;; ------------------------------------------------------------------

(defclass qfld ()
  ((vec :accessor qfld-vec  :initarg :vec  :initform (make-array 12 :element-type 'fixnum :initial-element 0))))

(defmethod qfld ((x qfld))
  x)

(defmethod qfld ((x integer))
  (let ((xx (mod x *ed-q*)))
    (make-instance 'qfld
                   :vec (vec1221 xx))
    ))

(defconstant +qzero+ (vec1221 *ed-q*))

(defun qnorm (v)
  (multiple-value-bind (vv cy)
      (rsc v)
    (let* ((msb (aref vv 11.))
           (x   (ash msb -20.)))
      (setf (aref vv 11.) (ldb (byte 20. 0) msb))
      (incf (aref vv 0) (* (+ cy cy x) 9.))
      vv)))

(defun qmodv (v)
  (multiple-value-bind (lo hi) (splitv v 12.)
    (qnorm (map-into lo (lambda (l h)
                          (declare (fixnum l h))
                          (+ l (* h #x12)) )
                     lo hi))
    ))

(defmethod gnorm ((x qfld))
  (qnorm (qfld-vec x))
  x)

(defmethod gmul ((x qfld) (y integer))
  (let ((yy (mod y *ed-q*)))
    (if (< (integer-length y) 21.)
        (let ((prod (make-array 13
                                :element-type 'fixnum
                                :initial-element 0)))
          (make-instance 'qfld
                         :vec (qmodv (map-into prod (lambda (x)
                                                      (* x y))
                                               (qfld-vec x))) ))
      ;; else
      (gmul x (qfld y))
      )))

(defmethod gmul ((x integer) (y qfld))
  (gmul y x))

(defmethod gmul ((x qfld) (y qfld))
  (make-instance 'qfld
                 :vec (qmodv (multv (qfld-vec x) (qfld-vec y)))
                 ))

(defmethod gsqr ((x qfld))
  (gmul x x))

(defmethod gadd ((x qfld) (y qfld))
  (make-instance 'qfld
                 :vec (map 'vector #'+ (qfld-vec x) (qfld-vec y))
                 ))

(defmethod gsub ((x qfld) (y qfld))
  (make-instance 'qfld
                 :vec (map 'vector #'- (qfld-vec x) (qfld-vec y))
                 ))

(defmethod gneg ((x qfld))
  (make-instance 'qfld
                 :vec (map 'vector #'- (qfld-vec x))))

(defmethod int ((x qfld))
  (let ((xx  0)
        (xv  (qfld-vec (gnorm x))))
    (loop for ix from 11 downto 0 do
            (setf xx (+ (ash xx 21.)
                        (aref xv ix))))
    (mod xx *ed-q*)))

(inspect (qfld 0))
(inspect (qfld *ed-r*))

(let ((z (qfld 0))
      (x (qfld *ed-r*)))
  (inspect (gnorm (gsub z x))))
(inspect (qfld (- *ed-r*)))

(let ((x (qfld *ed-r*)))
  (inspect (gmul x x)))

(let ((x (qfld *ed-r*)))
  (int (gmul x x)))
(modq
  (int (modmath:m* *ed-r* *ed-r*)))

(inspect (qfld -1))
(int (qfld -1))
(let ((z (qfld 0))
      (x (qfld 1)))
  (int (gsub z x)))
|#
;; --------------------------------------------------

(defclass ffbase ()
  ((nbits       :reader ffbase-nbits       :initarg :nbits)
   (base        :reader ffbase-base        :initarg :base)
   (wrap        :reader ffbase-wrap        :initarg :wrap)
   (montgy-ninv :reader ffbase-montgy-ninv :initarg :ninv)
   (montgy-rsq  :reader ffbase-montgy-rsq  :initarg :rsq)
   ))

(defun make-ffbase (base)
  (declare (type integer base))
  (check-type base (integer 2. *))
  (let* ((nbits  (integer-length base))
         (r      (ash 1 nbits))
         (wrap   (- r base))
         ;; (rsq    (xmod (* wrap wrap) base wrap)))
         (rsq    (mod (* wrap wrap) base)))
    (multiple-value-bind (gcd ninv rinv)
        (bezout base r)
      (declare (ignore gcd rinv))
      (make-instance 'ffbase
                     :base  base
                     :nbits nbits
                     :wrap  wrap
                     :ninv  ninv
                     :rsq   rsq)
      )))

(defclass ffld ()
  ((val  :accessor ffld-val  :initarg :val  :initform 0)))

(defclass ff-montgomery-mixin ()
  ())

(define-condition subclass-responsibility (error)
  ((fn  :reader subclass-responsibility-fn  :initarg :fn)
   (cls :reader subclass-responsibility-cls :initarg :cls))
  (:report (lambda (cx stream)
             (format stream "METHOD ~A (~A) : Subclass responsiblity"
                     (subclass-responsibility-fn cx)
                     (subclass-responsibility-cls cx)))
   ))

(defun subclass-responsibility (fn-name obj)
  (error 'subclass-responsibility
         :fn  fn-name
         :cls (class-name (class-of obj))))

(define-condition ffield-mismatch (error)
  ())

(defun need-same-ffield (x y)
  (unless (eql (class-of x) (class-of y))
    (error 'ffield-mismatch)))

(defgeneric ffld-base (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-base x)))

(defgeneric ffld-class (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-class x)))

(defgeneric ffld-monty-class (x)
  (:method ((x ffld))
   (subclass-responsibility 'ffld-monty-class x)))

(defun %wrapped-basic-ffld (proto class x)
  (declare (type ffld proto)
           (type symbol class)
           (type integer x))
  (let ((ffbase (ffld-base proto)))
    (declare (type ffbase ffbase))
    (with-accessors ((base ffbase-base)
                     ;; (wrap ffbase-wrap)
                     ) ffbase
      (make-instance class
                     :val (mod x base)
                          ;; (xmod x base wrap)
                          )
      )))

(defun %basic-ffld (proto x)
  (declare (type ffld proto))
  (%wrapped-basic-ffld proto (ffld-class proto) x))

(defun %basic-fmfld (proto x)
  ;; For direct construction of Montomery elements from an integer
  ;; value. Contrast with FFLD, which scales the integer.
  (declare (type ffld proto))
  (%wrapped-basic-ffld proto (ffld-monty-class proto) x))
   
(defgeneric ffld (proto x)
  (:method ((proto ff-montgomery-mixin) (x integer))
   (ff-to-montgy (%basic-ffld proto x)))
  (:method ((proto ffld) (x integer))
   (%basic-ffld proto x))
  (:method ((proto ffld) x)
   (ffld proto (int x))))

(defmethod int ((x ff-montgomery-mixin))
  (int (ff-from-montgy x)))

(defmethod int ((x ffld))
  (ffld-val x))

(defmacro define-ffld (name base)
  (lw:with-unique-names (modbase monty-name)
    `(progn
       (defparameter ,modbase  (make-ffbase ,base))
       (defclass ,name (ffld)
         ((base  :reader     ffld-base
                 :allocation :class
                 :initform   ,modbase)))
       (defclass ,monty-name (ff-montgomery-mixin ,name)
         ())
       (defmethod ffld-class ((x ,name))
         ',name)
       (defmethod ffld-monty-class ((x ,name))
         ',monty-name)
       (defgeneric ,name (x)
         (:method ((x ,name))
          x)
         (:method ((x integer))
          (with-accessors ((base  ffbase-base)
                           ;; (wrap  ffbase-wrap)
                           ) ,modbase
            (make-instance ',name
                           :val (mod x base)
                           ;; (xmod x base wrap)
                           )
            ))
           (:method (x)
            (,name (int x)))))
    ))

(defmethod print-object ((x ffld) out-stream)
  (format out-stream "(~A ~A)" (ffld-class x) (int x)))

;; --------------------------------------------------

(defgeneric ffadd (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym  (ff-to-montgy y)))
     (need-same-ffield x ym)
     (%basic-fmfld x
                   (+ (ffld-val x) (ffld-val ym)))
     ))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (%basic-ffld x
                  (+ (ffld-val x) (ffld-val ynm)))))
  (:method ((x ffld) (y integer))
   (ffadd x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffadd (ffld y x) y)))
  
(defgeneric ffsub (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (%basic-fmfld x
                   (- (ffld-val x) (ffld-val ym)))))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (%basic-ffld x
                  (- (ffld-val x) (ffld-val ynm)))))
  (:method ((x ffld) (y integer))
   (ffsub x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffsub (ffld y x) y)))

(defmethod ffneg ((x ffld))
  (let ((ffbase (ffld-base x)))
    (with-accessors ((base ffbase-base)) ffbase
      ;; no change of Montgomery or not
      (make-instance (class-of x)
                     :val (- base (ffld-val x))))
    ))

(defgeneric ffmul (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (ff-montgy-mul x ym)))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (%basic-ffld x
                  (* (ffld-val x) (ffld-val ynm)))))
  (:method ((x ffld) (y integer))
   (ffmul x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffmul (ffld y x) y)))

(defmethod ffsqr ((x ffld))
  (ffmul x x))

(defgeneric ffinv (x)
  (:method ((x ff-montgomery-mixin))
   (ff-to-montgy (ffinv (ff-from-montgy x))))
  (:method ((x ffld))
   (%basic-ffld x
                (with-mod (ffbase-base (ffld-base x))
                  (minv (int x)))
                )))

(defgeneric ffdiv (x y)
  (:method ((x ffld) (y ffld))
   (ffmul x (ffinv y)))
  (:method ((x ffld) (y integer))
   (ffdiv x (ffld x y)))
  (:method ((x integer) (y ffld))
   (ffdiv (ffld y x) y)))

;; ---------------------------------------------

(defun ff+ (arg &rest args)
  (reduce #'ffadd args
          :initial-value arg))

(defun ff- (arg &rest args)
  (if args
      (reduce #'ffsub args
              :initial-value arg)
    (ffneg arg)))

(defun ff* (arg &rest args)
  (reduce #'ffmul args
          :initial-value arg))

(defun ff/ (arg &rest args)
  (if args
      (ffdiv arg (apply #'ff* args))
    (ffinv arg)))

;; --------------------------------------------------------------

(defun %redc (ffbase val)
  ;; val/r mod n : Mongomery reduction
  ;;
  ;; From Bezout's Identity: gcd(a, b) = a'*a + b'*b for some a',b'.
  ;; Function BEZOUT takes a, b and returns gcd(a,b), a', b'.
  ;;
  ;; By using this on prime base n, and its containing r,
  ;;
  ;;    r = 2^m > n > 2^(m-1)
  ;;
  ;; We have:
  ;;
  ;;     n'*n + r'*r = gcd(n,r) = 1,  with 0 < n',r' < n < r
  ;;
  ;; In mod n arithmetic this becomes
  ;;
  ;;    (n'*n + r'*r) mod n = r'*r mod n = 1, so r' = inv(r) mod n.
  ;;
  ;; And in mod r arithmetic,
  ;;
  ;;    (n'*n + r'*r) mod r = n'*n mod r = 1, so n' = inv(n) mod r.
  ;;
  ;; So if we need to compute x*inv(r) mod n:
  ;;
  ;;    x*inv(r) mod n = x*inv(r)*r/r mod n    ;; mult by r/r
  ;;                   = x*(inv(r)*r)/r mod n  ;; regroup
  ;;                   = x*(1 - n'*n)/r mod n  ;; subst inv(r)*r from Bezout
  ;;                   = (x - x*n'*n)/r mod n  ;; expand
  ;;                   = (x - x*n'*n + s*r*n)/r mod n, for arb s
  ;;                   = (x - n*(x*n' - s*r)/r mod n ;; factor n
  ;;                   = (x - n*q)/r mod n     ;; subst q = x*n' - s*r, arb s
  ;;
  ;; But since s is arbitrary, we can compute
  ;;
  ;;     q = (x*n' - s*r) => choose s so that q = x*n' mod r
  ;;                                            = (x mod r) * n' mod r,
  ;;
  ;; i.e., q is low m bits of product, using low m bits of x, n' already < r.
  ;;
  ;; Then, (x - n*q) = (x - n*(x*n' mod r))
  ;;                 = (x - (x*n*n') mod r - k*r)     ;; for some k
  ;;                 = (x - (x*n*inv(n)) mod r - k*r) ;; from Bezout relation, mod r
  ;;                 = (x - x mod r - k*r) => low m bits of difference become zero
  ;; So
  ;;    (x - n*q)/r is simply the right shift by m bits of the difference.
  ;; 
  ;;    x*inv(r) mod n = ((x - n*((x mod r)*n' mod r)) >> m) mod n
  ;;
  ;; Since x < n*n < r*n (even if x is the prod of a mult), and q*n <
  ;; r*n, we know that -n < (x - q*n)/r < n. Therefore, final mod n is
  ;; implemented using a single check and one addition.
  ;;
  (with-accessors ((n     ffbase-base)
                   (ninv  ffbase-montgy-ninv)
                   (nbits ffbase-nbits)) ffbase
    (let* ((bits   (byte nbits 0))
           (q      (ldb bits (* (ldb bits val) ninv)))
           (a      (ash (- val (* q n)) (- nbits))))
      (if (minusp a)
          (+ a n)
        a)
      )))

(defgeneric ff-to-montgy (x)
  (:method ((x ff-montgomery-mixin))
   x)
  (:method ((x ffld))
   ;; x -> r*x
   (let* ((ffbase (ffld-base x))
          (rsq    (ffbase-montgy-rsq ffbase)))
     (make-instance (ffld-monty-class x)
                    :val (%redc ffbase (* (ffld-val x) rsq)))
     )))

(defgeneric ff-from-montgy (x)
  (:method ((x ff-montgomery-mixin))
   ;; x -> x/r
   (let ((ffbase (ffld-base x)))
     (make-instance (ffld-class x)
                    :val (%redc ffbase (ffld-val x)))))
  (:method ((x ffld))
   x))

(defmethod ff-montgy-mul ((x ff-montgomery-mixin) (y ff-montgomery-mixin))
  (need-same-ffield x y)
  (let ((ffbase (ffld-base x)))
    (make-instance (class-of x)
                   :val (%redc ffbase (* (ffld-val x) (ffld-val y))))
    ))

(defmethod ff-montgy-sqr ((x ff-montgomery-mixin))
  (ff-montgy-mul x x))

;; -----------------------------------------------------

(defgeneric ff= (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (= (ffld-val x) (ffld-val ym))))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (= (ffld-val x) (ffld-val ynm))))
  (:method ((x ffld) (y integer))
   (= (int x) y)))

(defgeneric ff< (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (< (ffld-val x) (ffld-val ym))))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (< (ffld-val x) (ffld-val ynm))))
  (:method ((x ffld) (y integer))
   (< (int x) y)))

(defgeneric ff>= (x y)
  (:method ((x ff-montgomery-mixin) (y ffld))
   (let ((ym (ff-to-montgy y)))
     (need-same-ffield x ym)
     (>= (ffld-val x) (ffld-val ym))))
  (:method ((x ffld) (y ffld))
   (let ((ynm (ff-from-montgy y)))
     (need-same-ffield x ynm)
     (>= (ffld-val x) (ffld-val ynm))))
  (:method ((x ffld) (y integer))
   (>= (int x) y)))

;; ------------------------------------------------------

(defmethod ff^ ((x ffld) (n integer))
  (cond ((minusp n)
         (ff^ (ffinv x) (- n)))
        ((ff< x 2.) ;; x = 0,1
         x)
        (t
         ;; we know that x^(q-1) = 1, so x^(n*(q-1) + r) = x^r
         (let* ((ffbase (ffld-base x))
                (base   (ffbase-base ffbase))
                ;; (nred   (xmod n (1- base))))
                (nred   (mod n (1- base))))
           (declare (integer base nred))
           (case nred
             (0  (ffld x 1))
             (1  x)
             (2. (ffsqr x))
             (t  (modmath::generalized-windowed-exponentiation (ff-to-montgy x) nred
                                                               :window-nbits 4.
                                                               :op-mul       'ff-montgy-mul
                                                               :op-sqr       'ff-montgy-sqr))
             )))
        ))

(defmethod ffsqrt ((x ffld))
  (let ((ffbase (ffld-base x)))
    (ffld x (with-mod (ffbase-base ffbase)
              (msqrt (int x)))
          )))

#|
(let ((x (ecffq *ed-r*)))
  (ff= (ff/ x)
       (ff^ x (- *ed-q* 2))))
 |#
;; -----------------------------------------------------------
#|
(defun hash-to-grp-range (&rest args)
  (apply 'hash-to-range *ed-r* args))

(defun hash-to-pt-range (&rest args)
  (apply 'hash-to-range *ed-q* args))

;; -------------------------------------------------

(defun compute-deterministic-skey (seed &optional (index 0))
  (multiple-value-bind (_ hval)
      (hash-to-grp-range index seed)
    (declare (ignore _))
    hval))
|#

(defun ec-make-deterministic-keys (&rest seeds)
  (multiple-value-bind (_ skey)
      (apply #'hash-to-grp-range seeds)
    (declare (ignore _))
    (values skey (ec-nth skey))))

(defun ec-random-pair ()
  "Select a random private and public key from the curve"
  (ec-make-deterministic-keys :ecpt (com.ral.crypto.prng:ctr-drbg 256.)))

;; -----------------------------------------------------
;; Hashing onto curve

(defun ec-pt-from-hash (hintval)
  "Hash onto curve. Treat h as X coord, just like a compressed point.
Then if Y is a quadratic residue we are done.
Else re-probe with (X^2 + 1)."
  (let ((cof-fn  (ec-decmpr*h-fn)))
    (um:nlet iter ((x  (ecffq hintval)))
      (or
       (um:when-let (y (ignore-errors (ec-solve-y x)))
         (let ((pt (funcall cof-fn
                            (make-ecc-pt
                             :x x
                             :y y))))
           ;; Watch out! This multiply by cofactor is necessary
           ;; to prevent winding up in a small subgroup.
           ;;
           ;; we already know the point sits on the curve, but
           ;; it could now be the neutral point if initial
           ;; (x,y) coords were in a small subgroup.
           (and (not (ec-inf-p pt))
                pt)))
       ;; else - invalid point, so re-probe at x^2+1
       (go-iter (ff+ 1 (ffsqr x)))
       ))))

(defun ec-pt-from-seed (&rest seeds)
  (ec-pt-from-hash (apply #'hash/256 seeds)))

(defun ec-random-generator (&rest seeds)
  (apply 'ec-pt-from-seed
         (uuid:make-v1-uuid)
         (com.ral.crypto.prng:ctr-drbg (integer-length *ed-q*))
         seeds))

#|
(define-ffld qfld *ed-q*)
(define-ffld rfld *ed-r*)
|#
#|
(int (ffinv (qfld (ash 1 251))))
|#

#|
(qfld *ed-r*)
(ff-to-montgy (qfld *ed-r*))
(int (ffinv (qfld *ed-r*)))
(let ((x (qfld *ed-r*)))
  (int (ff* x (ffinv x))))
(int (ff+ (qfld 1) (qfld 2) (qfld 3)))
(int (ff- (qfld 1)))
(int (ff- (qfld 1) (qfld 2)))

(let* ((x (qfld *ed-r*))
       (xm (ff-to-montgy x))
       (ym (ff-montgy-mul xm xm))
       (y  (ff-from-montgy ym)))
  (list (int y)
        (int (ffmul x x))))

|#  
;; --------------------------------------------------
#||#
(define-ffld ff1174q *ed-q*)
(define-ffld ff1174r *ed-r*)

(defun ecffq (x)
  (ff-to-montgy (ff1174q x)))

(defun ecffr (x)
  (ff1174r x))

(defvar *ec-gen*
  (um:bind* ((:struct-accessors ecc-pt (x y) *ed-gen*))
    (make-ecc-proj-pt
     :x (ecffq x)
     :y (ecffq y)
     :z (ecffq 1))
    ))

;; -------------------------------------------------------

(defun ec-nbits ()
  (com.ral.cached-var:get-cached-symbol-data '*edcurve*
                                             :ec-nbits *edcurve*
                                             (lambda ()
                                               (integer-length *ed-q*))
                                             ))

(defun ec-nbytes ()
  (com.ral.cached-var:get-cached-symbol-data '*edcurve*
                                             :ec-nbytes *edcurve*
                                             (lambda ()
                                               (ceiling (ec-nbits) 8.))))

(defun ec-compressed-nbits ()
  (com.ral.cached-var:get-cached-symbol-data '*edcurve*
                                             :ec-compressed-nbits *edcurve*
                                             (lambda ()
                                               (1+ (ec-nbits)))))

(defun ec-compressed-nbytes ()
  (com.ral.cached-var:get-cached-symbol-data '*edcurve*
                                             :ec-compressed-nbytes *edcurve*
                                             (lambda ()
                                               (ceiling (ec-compressed-nbits) 8.))))

(defun ec-cmpr/h-sf ()
  (com.ral.cached-var:get-cached-symbol-data '*edcurve*
                                             :ec-cmpr/h-sf *edcurve*
                                             (lambda ()
                                               (ffinv (ecffr *ed-h*)))
                                             ))

(defun ec-decmpr*h-fn ()
  (com.ral.cached-var:get-cached-symbol-data '*edcurve*
                                             :ec-decmpr*h-fn *edcurve*
                                             (lambda ()
                                               (case *ed-h*
                                                 (1  'identity)
                                                 (2  'ec-double)
                                                 (4  (lambda (pt)
                                                       (ec-double
                                                        (ec-double pt))))
                                                 (8  (lambda (pt)
                                                       (ec-double
                                                        (ec-double
                                                         (ec-double pt)))))
                                                 (t (error "No decompression function"))
                                                 ))))

;; ----------------------------------------------------

(defun ec-inf ()
  (com.ral.cached-var::get-cached-symbol-data '*edcurve*
                                              :ec-inf *ed-c*
                                              (lambda ()
                                                (make-ecc-proj-pt
                                                 :x (ecffq 0)
                                                 :y (ecffq *ed-c*)
                                                 :z (ecffq 1))
                                                )))

(defun ec-pt= (pt1 pt2)
  (um:bind* ((p1  (ec-projective pt1))
             (p2  (ec-projective pt2))
             (:struct-accessors ecc-proj-pt ((x1 x)
                                             (y1 y)
                                             (z1 z)) p1)
             (:struct-accessors ecc-proj-pt ((x2 x)
                                             (y2 y)
                                             (z2 z)) p2))
    (and (ff= (ff* z1 x2) (ff* z2 x1))
         (ff= (ff* z1 y2) (ff* z2 y1)))
       ))

(defun ec-inf-p (pt)
  (ec-pt= pt (ec-inf)))

;; -------------------

(defgeneric ec-compress-pt (pt &key enc)
  ;; enc is one of :bev, :lev, :base58 or nil
  (:method ((pt ecc-cmpr-pt) &key enc)
   (if enc
       (ec-compress-pt (ec-projective pt) :enc enc)
     pt))
  (:method ((pt ecc-pt) &key enc)
   (ec-compress-pt (ec-projective pt) :enc enc))
  (:method ((pt ecc-proj-pt) &key enc)
   ;;
   ;; Standard encoding for EdDSA is X in little-endian notation, with
   ;; Odd(Y) encoded as MSB beyond X.
   ;;
   ;; If lev is true, then a little-endian UB8 vector is produced,
   ;; else an integer value.
   ;;
   (um:bind* ((cmpr/h  (ec-cmpr/h-sf))
              (p       (ec-affine (ec-mul pt cmpr/h)))
              (:struct-accessors ecc-pt (x y) p))
     (let ((val  (dpb (ldb (byte 1 0) (int y))
                      (byte 1 (ec-nbits)) (int x))))
       (ecase enc
         ((nil)    (make-ecc-cmpr-pt :cx val))
         (:bev     (bevn val (ec-compressed-nbytes)))
         (:lev     (levn val (ec-compressed-nbytes)))
         (:base58  (base58 (bevn val (ec-compressed-nbytes))))
         )))))
  
;; --------------------------

(defun ec-solve-y (x)
  (ffsqrt (ff/ (ff* (ff+ *ed-c* x)
                    (ff- *ed-c* x))
               (ff- 1 (ff* x x *ed-c* *ed-c* *ed-d*)))))

(defgeneric ec-decompress-pt (pt)
  (:method ((pt ecc-pt))
   pt)
  (:method ((pt ecc-proj-pt))
   pt)
  (:method ((pt ecc-cmpr-pt))
   (ec-decompress-pt (ecc-cmpr-pt-cx pt)))
  (:method ((v integer))
   (let* ((decmpr-fn (ec-decmpr*h-fn))
          (nbits     (ec-nbits))
          (sign      (ldb (byte 1 nbits) v))
          (x         (ecffq (ldb (byte nbits 0) v)))
          (y         (ec-solve-y x))
          (ys        (if (= sign (ldb (byte 1 0) (int y)))
                         y
                       (ff- y))))
     (funcall decmpr-fn
              (make-ecc-proj-pt
               :x  x
               :y  ys
               :z  (ffld x 1)))
     )))

;; --------------------------------------------------------------

(defgeneric ec-affine (pt)
  (:method ((pt ecc-pt))
   pt)
  (:method ((pt ecc-proj-pt))
   (um:bind* ((:struct-accessors ecc-proj-pt (x y z) pt))
     (make-ecc-pt
      :x (ff/ x z)
      :y (ff/ y z))
     ))
  (:method ((pt ecc-cmpr-pt))
   (ec-affine (ec-decompress-pt pt))))

(defgeneric ec-projective (pt)
  (:method ((pt ecc-pt))
   (um:bind* ((:struct-accessors ecc-pt (x y) pt))
     (make-ecc-proj-pt
      :x  x
      :y  y
      :z  (ffld x 1))))
  (:method ((pt ecc-proj-pt))
   pt)
  (:method ((pt ecc-cmpr-pt))
   (ec-projective (ec-decompress-pt pt))))

;; ----------------------------------------------

(defgeneric ec-satisfies-curve (pt)
  (:method ((pt ecc-proj-pt))
   (um:bind* ((:struct-accessors ecc-proj-pt (x y z) pt))
     (let ((xx (ff* x x))
           (yy (ff* y y))
           (zz (ff* z z)))
       (ff= (ff* zz (ff+ xx yy))
            (ff* (+ (ff* zz zz)
                    (ff* xx yy *ed-d*))
                 (* *ed-c* *ed-c*)))
       )))
  (:method ((pt ecc-pt))
   (ec-satisfies-curve (ec-projective pt)))
  (:method ((pt ecc-cmpr-pt))
   (ec-satisfies-curve (ec-projective pt))))

(defgeneric ec-valid-point-p (pt)
  (:method ((pt ecc-pt))
   (um:bind* ((:struct-accessors ecc-pt (x y) pt))
     (and (ec-satisfies-curve pt)
          (not (or (zerop (int x))
                   (zerop (int y))))
          pt)))
  (:method ((pt ecc-proj-pt))
   (ec-valid-point-p (ec-affine pt)))
  (:method ((pt ecc-cmpr-pt))
   (ec-valid-point-p (ec-affine pt)))
  (:method ((pt integer))
   ;; assume we have integer repr of compressed pt
   ;; -- some integer values cannot be decompressed into ECC pts.
   (ignore-errors
     (ec-valid-point-p (ec-decompress-pt pt))))
  (:method ((pt vector))
   ;; vector should be a BVEC for an integer representing a compressed
   ;; point
   (ignore-errors
     (ec-valid-point-p (int pt)))) )

(defun ec-validate-point (pt)
  (let ((vpt (ec-valid-point-p pt)))
    (assert vpt)
    vpt))

;; ----------------------------------------------

(defmethod hashable ((x ecc-proj-pt))
  (hashable (ec-compress-pt x :enc :lev)))

(defmethod hashable ((x ecc-pt))
  (hashable (ec-projective x)))

(defmethod hashable ((x ecc-cmpr-pt))
  (hashable (ec-projective x)))

;; ----------------------------------------------

(defgeneric ec-add (pt1 pt2)
  (:method ((pt1 ecc-proj-pt) (pt2 ecc-proj-pt))
   (um:bind* ((:struct-accessors ecc-proj-pt ((x1 x)
                                              (y1 y)
                                             (z1 z)) pt1)
              (:struct-accessors ecc-proj-pt ((x2 x)
                                              (y2 y)
                                              (z2 z)) pt2)
              (a  (ff* z1 z2))
              (b  (ff* a a))
              (c  (ff* x1 x2))
              (d  (ff* y1 y2))
              (e  (ff* c d *ed-d*))
              (f  (ff- b e))
              (g  (ff+ b e))
              (x3 (ff* a f (ff- (ff* (ff+ x1 y1)
                                     (ff+ x2 y2))
                               c d)))
              (y3 (ff* a g (ff- d c)))
              (z3 (ff* f g *ed-c*)))
     (make-ecc-proj-pt
      :x  x3
      :y  y3
      :z  z3)
     ))
  (:method (pt1 pt2)
   (ec-add (ec-projective pt1) (ec-projective pt2))))

(defgeneric ec-double (pt)
  (:method ((pt ecc-proj-pt))
   (ec-add pt pt))
  (:method (pt)
   (ec-double (ec-projective pt))))

(defgeneric ec-neg (pt)
  (:method ((pt ecc-proj-pt))
   (um:bind* ((:struct-accessors ecc-proj-pt (x y z) pt))
     (make-ecc-proj-pt
      :x  (ff- x)
      :y  y
      :z  z)))
  (:method (pt)
   (ec-neg (ec-projective pt))))

(defun ec-sub (pt1 pt2)
  (ec-add pt1 (ec-neg pt2)))

;; ------------------------------------------------------

(defclass bipolar-window-cache ()
  ((precv  :reader   bipolar-window-cache-precv
           :initarg  :precv)
   (offs   :reader   bipolar-window-cache-offs
           :initarg  :offs)
   (pt*1   :reader   bipolar-window-cache-pt*1
           :initarg  :pt*1)
   (pt*m1  :reader   bipolar-window-cache-pt*m1
           :initarg  :pt*m1)))

(defmethod make-bipolar-window-cache (&key nbits pt)
  (declare (fixnum nbits))
  (let* ((nel    (ash 1 nbits))
         (precv  (make-array nel :initial-element nil))
         (offs   (ash nel -1))
         (pt*1   (ec-projective pt))
         (pt*m1  (ec-neg pt*1)))
    (declare (fixnum nel offs))
    (setf (aref precv (1+ offs)) pt*1     
          (aref precv offs)      (ec-inf)
          (aref precv (1- offs)) pt*m1)
    (make-instance 'bipolar-window-cache
                   :precv  precv
                   :offs   offs
                   :pt*1   pt*1
                   :pt*m1  pt*m1)))

(defmethod get-prec ((wc bipolar-window-cache) (ix integer))
  ;; get cached pt*n, for n = -2^wn, -2^wn+1, ...,-1, 0, 1, ... 2^wn-2, 2^wn-1
  ;; each cached entry computed on demand if necessary
  (declare (fixnum ix))
  (with-accessors ((precv  bipolar-window-cache-precv)
                   (offs   bipolar-window-cache-offs)
                   (pt*1   bipolar-window-cache-pt*1)
                   (pt*m1  bipolar-window-cache-pt*m1)) wc
    (let ((jx (+ ix offs)))
      (declare (fixnum jx))
      (or (aref precv jx)
          (setf (aref precv jx)
                (if (oddp ix)
                    (if (minusp ix)
                        (ec-add pt*m1 (get-prec wc (1+ ix)))
                      (ec-add pt*1 (get-prec wc (1- ix))))
                  ;; else - ix even
                  (ec-double (get-prec wc (ash ix -1))))
                )))))

#|
(defun generalized-bipolar-windowed-mul (pt n &key window-nbits)
  ;; ECC point-scalar multiplication using fixed-width bipolar window
  ;; algorithm
  (declare (fixnum window-nbits)
           (integer n))
  (let* ((ws  (edec::windows n window-nbits))
         (wc  (make-bipolar-window-cache
               :nbits window-nbits
               :pt    pt))  ;; affine or projective in...
         (ans (ec-inf)))
    (loop for w fixnum in ws do
            (loop repeat window-nbits do
                    (setf ans (ec-double ans)))
            (let ((pw  (get-prec wc w)))
              (setf ans (ec-add pw ans))
              ))
    ans))
|#

(defun generalized-bipolar-windowed-mul (pt n &key window-nbits)
  ;; ECC point-scalar multiplication using fixed-width bipolar window
  ;; algorithm. Pt is projective.
  (declare (fixnum window-nbits)
           (integer n))
  (let ((wc  (make-bipolar-window-cache
              :nbits window-nbits
              :pt    pt)))
    (um:nlet outer ((ans  (ec-inf))
                    (ws   (edec::windows n window-nbits)))
      (if (endp ws)
          ans
        (let* ((w  (car ws))
               (pw (get-prec wc w)))
          (go-outer
           (ec-add pw
                   (um:nlet inner ((ans ans)
                                   (ix  window-nbits))
                     (if (zerop ix)
                         ans
                       (go-inner (ec-double ans) (1- ix)))
                     ))
           (cdr ws)))
        ))
    ))

(defgeneric ec-mul (pt n)
  (:method ((pt ecc-proj-pt) n)
   (generalized-bipolar-windowed-mul pt
                                     (int (ecffr n))
                                     :window-nbits 4))
  (:method (pt n)
   (ec-mul (ec-projective pt) n)))
  
(defun ec-div (pt n)
  (ec-mul pt
          (ffinv (ecffr n))
          ))

(defun ec-nth (n)
  (ec-mul *ec-gen* n))

#|
(let ((x (ecffq *ed-r*)))
  (ff= (ff/ x) (ff^ x (- *ed-q* 2))))
|#
;; ----------------------------------------------------------------

(defun 1bit-trunc (a b)
  #F
  (declare (integer a b))
  ;; a, b non-negative
  (let* ((na  (integer-length a))
         (nb  (integer-length b))
         (nsh (- na nb)))
    (declare (fixnum na nb nsh))
    (cond ((minusp nsh)
           (values -1 a))
          
          ((zerop nsh)
           (let ((r (- a b)))
             (declare (integer r))
             (if (minusp r)
                 (values -1 a)
               (values 0 r))))
          (t
           (let* ((bb (ash b nsh))
                  (r  (- a bb)))
             (declare (integer r bb))
             (if (minusp r)
                 (values (1- nsh) (+ r (ash bb -1)))
               (values nsh r))))
          )))

(defun tst (&optional (n 1000))
  (let* ((x      (ecffq *ed-r*))
         (ffbase (ffld-base x))
         (base   (ffbase-base ffbase))
         (base-2 (- base 2)))
    (assert (ff= (ff/ x) (ff^ x base-2)))
    (inspect x)
    (time
     (dotimes (ix n)
       (ff/ x)))
    (time
     (dotimes (ix n)
       (ff^ x base-2)))
    ))

(define-ffld ff21 (- (ash 1 21) 19))
(define-ffld ff30 (- (ash 1 30) 35))

#||#
;; --------------------------------------------------
#|
(defclass MODBASE ()
  ((nbits     :reader modbase-nbits     :initarg :nbits)
   (nlimb     :reader MODBASE-nlimb     :initarg :nlimb)
   (nbpw      :reader MODBASE-nbpw      :initarg :nbpw)
   (nmsb      :reader MODBASE-nmsb      :initarg :nmsb)
   (base      :reader MODBASE-base      :initarg :base)
   (msb-wrap  :reader MODBASE-msb-wrap  :initarg :msb-wrap)
   (base-enc  :reader MODBASE-base-enc  :initarg :base-enc)
   ))

(defun raw-vec-form (x nlimb nbpw)
  ;; assumes x fits within the limbs
  (declare (integer x)
           (fixnum nlimb nbpw))
  (let ((elts (loop repeat nlimb
                      for offs from 0 by nbpw
                      collect
                      (ldb (byte nbpw offs) x))))
    (make-array nlimb
                :element-type 'fixnum
                :initial-contents elts)
    ))

(defun vec-form (x base)
  (declare (integer x)
           (modbase base))
  (let ((xx  (mod x (modbase-base base))))
    (raw-vec-form xx
                  (modbase-nlimb base)
                  (modbase-nbpw base))
    ))

(defun make-modbase (base nbpw)
  (declare (integer base)
           (fixnum nbpw))
  (check-type nbpw (integer 1 #.(1- (ash (integer-length most-positive-fixnum) -1))))
  (let* ((nbits  (integer-length base))
         (nlimb  (ceiling nbits nbpw))
         (nmsb   (- nbits (* (1- nlimb) nbpw)))
         (wr     (- (ash 1 nbits) base)))
    (declare (fixnum nbits nlimb nmsb))
    (make-instance 'modbase
                   :nbits     nbits
                   :nlimb     nlimb
                   :nbpw      nbpw
                   :nmsb      nmsb
                   :base      base
                   :msb-wrap  wr
                   :base-enc  (raw-vec-form base nlimb nbpw))
    ))

(defclass ffld ()
  ;; abstract parent class for finite field vais
  ((enc  :accessor ffld-enc  :initarg :enc)))

(defgeneric ffld-base (x)
  (:method ((x ffld))
   (error "Subclass responsibility")))

(defmacro define-ffld (name &key base nbpw)
  (lw:with-unique-names (modbase)
    `(let ((,modbase  (make-modbase ,base ,nbpw)))
       (defclass ,name (ffld)
         ((base  :reader     ffld-base
                 :allocation :class
                 :initform   ,modbase)))
       (defgeneric ,name (x)
         (:method ((x ,name))
          x)
         (:method ((x integer))
          (make-instance ',name
                         :enc (vec-form x ,modbase)))
         (:method (x)
          (,name (int x)))))
    ))

(defmethod need-modbase ((x ffld) (modbase modbase))
  (assert (eql (ffld-base x) modbase)))

(defmethod ffld ((proto ffld) (x integer))
  (make-instance (class-of proto)
                 :enc (vec-form x (ffld-base proto))))

(defmethod int ((x ffld))
  (let ((modbase (ffld-base x)))
    (with-accessors ((nlimb  modbase-nlimb)
                     (nbpw   modbase-nbpw)
                     (base   modbase-base)) modbase
      (let ((val  0)
            (xenc (ffld-enc x)))
        (loop for ix from (1- nlimb) downto 0 do
              (setf val (+ (ash val nbpw)
                           (aref xenc ix))
                    ))
        (mod val base)))))
                    
;; ------------------------------------------------------------
;; Operations in 3-Address form

(defmethod ffadd ((x ffld) (y ffld) (z ffld))
  (let ((modbase  (ffld-base x)))
    (need-modbase y modbase)
    (need-modbase z modbase)
    (let ((xenc (ffld-enc x))
          (yenc (ffld-enc y))
          (zenc (ffld-enc z)))
      (dotimes (ix (modbase-nlimb modbase))
        (setf (aref zenc ix) (+ (aref xenc ix) (aref yenc ix))))
      z)))
      
(defmethod ffsub ((x ffld) (y ffld) (z ffld))
  (let ((modbase  (ffld-base x)))
    (need-modbase y modbase)
    (need-modbase z modbase)
    (let ((xenc (ffld-enc x))
          (yenc (ffld-enc y))
          (zenc (ffld-enc z)))
      (dotimes (ix (modbase-nlimb modbase))
        (setf (aref zenc ix) (- (aref xenc ix) (aref yenc ix))))
      z)))
      
(defmethod ffneg ((x ffld) (z ffld))
  (let ((modbase  (ffld-base x)))
    (need-modbase z modbase)
    (let ((xenc (ffld-enc x))
          (zenc (ffld-enc z)))
      (dotimes (ix (modbase-nlimb modbase))
        (setf (aref zenc ix) (- (aref xenc ix))))
      z)))

;; -----------------------------------

(defun %wrapping (v start cy modbase)
  (declare (type (vector fixnum) v)
           (type fixnum start)
           (type integer cy))
  (with-accessors ((nlimb     modbase-nlimb)
                   (nbpw      modbase-nbpw)
                   (nmsb      modbase-nmsb)
                   (msb-wrap  modbase-msb-wrap)) modbase
    (macrolet ((lsb (x)
                 `(aref ,x 0))
               (msb (x)
                 `(aref ,x (1- nlimb))
                 ))
      (let ((bits   (byte nbpw 0))
            (msbits (byte nmsb 0))
            (msshr  (- nmsb))
            (shr    (- nbpw))
            (shl    (- nbpw nmsb)))
        (um:nlet outer ((cy    cy)
                        (start start))
          (let* ((msb  (msb v))
                 (x    (+ (ash msb msshr)
                          (ash cy shl))))
            (declare (type fixnum msb x))
            (setf (msb v) (ldb msbits msb))
            (cond ((zerop x)
                   v)
                  
                  (t
                   (um:nlet inner ((xwr (* x msb-wrap))
                                   (ix  start))
                     (declare (type integer xwr)
                              (type fixnum ix))
                     (cond ((zerop xwr)
                            (go-outer 0 0))
                           
                           ((>= ix nlimb)
                            (go-outer xwr 0))
                         
                           (t
                            (let ((xx (+ xwr (aref v ix))))
                              (declare (type integer xx))
                              (setf (aref v ix) (ldb bits xx))
                              (go-inner (ash xx shr)
                                        (1+ ix))))
                           )))
                  )))
        ))))

(defun %ffprod-norm (prod modbase)
  (with-accessors ((nlimb  modbase-nlimb)) modbase
    (multiple-value-bind (lo hi) (splitv prod nlimb)
      (declare (type (vector fixnum) lo hi))
      (dotimes (ix nlimb)
        (%wrapping lo ix (aref hi ix) modbase))
      lo)))

(defmethod ffmul ((x ffld) (y ffld) (z ffld))
  (let ((modbase  (ffld-base x)))
    (need-modbase y modbase)
    (need-modbase z modbase)
    (let* ((xenc (ffld-enc x))
           (yenc (ffld-enc y))
           (nbpw (modbase-nbpw modbase))
           (prod (%ffprod-norm (multv xenc yenc nbpw) modbase)))
      (replace (ffld-enc z) prod)
      z)))

(defmethod ffsqr ((x ffld) (z ffld))
  (ffmul x x z))

(defmethod ffrsc ((x ffld))
  ;; in-place destructive normalization
  (let ((modbase (ffld-base x)))
    (declare (type modbase modbase))
    (multiple-value-bind (vv cy)
        (rsc (ffld-enc x) (modbase-nbpw modbase))
      (declare (type (vector fixnum) vv)
               (type integer cy))
      (%wrapping vv 0 cy modbase)
      x)))

(defmethod ffscale ((x ffld) (y integer) (z ffld))
  (let ((modbase  (ffld-base x)))
    (need-modbase z modbase)
    (with-accessors ((nbpw  modbase-nbpw)
                     (nlimb modbase-nlimb)) modbase
      (if (< (integer-length y) nbpw)
          (let ((xenc (ffld-enc x))
                (zenc (ffld-enc z))
                (bits (byte nbpw 0))
                (rsh  (- nbpw))
                (cy   0))
            (dotimes (ix nlimb)
              (let ((val (+ (* y (aref xenc ix)) cy)))
                (setf (aref zenc ix) (ldb bits val)
                      cy             (ash val rsh))
                ))
            (%wrapping zenc 0 cy modbase)
            z)
        ;; else
        (ffmul x (ffld x y) z))
      )))

;; ------------------------------------------------------------

(define-ffld qfld :base *ed-q* :nbpw 21.)
(define-ffld rfld :base *ed-r* :nbpw 21.)
|#

#|
(inspect (rfld *ed-r*))

(let ((z (qfld *ed-r*)))
  (ffsqr z z)
  (int z))

(modq
  (msqr *ed-r*))

(setf *print-base* 16.)
(setf *print-base* 10.)

(ash (* 25 25) -5)
(mod (* 25 25) 31)
(with-mod 32
  (minv 31))

(let* ((x  25)
       (p  (* x x))
       (q  (* 31 (logand 31 (* p 1)))))
  (ash (+ p q) -5))

(ash (- (* 25 25) (logand 31 (* 31 31))) -5)

(defun tst ()
  (with-mod 31
    (let* ((x  25)
           (w   (mmod 32))
           (1/w (minv w))
           (x/w (m* 1/w x))
           (x/w*x/w (* w x/w x/w))
           (x*x (m* x x)))
      (ldb (byte 5 0) x/w*x/w))))
(tst)

        
(modq
  (let* ((w   (mmod (ash 1 251)))
        (1/w  (minv w))
        (x    *ed-r*)
        (x/w  (m* 1/w *ed-r*))
        (x*x  (m* x x))
        (x/w*x/w (* x/w x/w))
|#

#|
(defun fast-qr (v1 v2)
  ;; crude fast quotient and remainder of v1/v2.
  ;; return quot in form nsh, where q = 2^nsh.
  ;; If v2 does not divide v1, return nsh < 0.
  (let* ((l1  (integer-length v1))
         (l2  (integer-length v2))
         (nsh (- l1 l2)))
    (cond ((minusp nsh)
           (values -1 v1))
          ((zerop nsh)
           (let ((r (- v1 v2)))
             (if (minusp r)
                 (values -1 v1)
               (values 0 r))))
          (t
           ;; conservative under-estimate, to avoid checking and
           ;; possible correction
           (decf nsh)
           (values nsh (- v1 (ash v2 nsh))))
          )))

(defun xminv (a m)
  ;; divide-free modular inverse by Extended Euclidean algorithm
  (declare (integer a m))
  (let* ((u  a)
         (v  m)
         (x1 1)
         (x2 0))
    (declare (integer u v x1 x2))
    (do ()
        ((= u 1) (mod x1 m))
      (multiple-value-bind (nsh r) (fast-qr v u)
        (declare (integer nsh r))
        (let ((x  (if (minusp nsh)
                      x2
                    (- x2 (ash x1 nsh)))))
          (declare (integer x))
          (shiftf v u r)
          (shiftf x2 x1 x))
        ))))

(fast-qr 60 125)
(xminv 16 61)
(mod (* 42 16) 61)
|#

#|
0....5...10...15
yyyy              0-4
  xxxxxxxxxx      2-12
  ssssssssssss    2-14
  zzzzzzzzzz      2-12

xxxxxxxxxx        0-10
     yyyy         5-9
     ssssssssss   5-15
     zzzzzzzzzz   5-15

xxxxxx         0-6
   yyyyyy      3-9
   sssssssss   3-12
   zzzzzz      3-9
|#    
#|
(defun add1026 (a b c)
  #F
  (setf (sys:typed-aref 'sys:int64 c  0.) (sys:int64+ (sys:typed-aref 'sys:int64 a  0.) (sys:typed-aref 'sys:int64 b  0.))
        (sys:typed-aref 'sys:int64 c  8.) (sys:int64+ (sys:typed-aref 'sys:int64 a  8.) (sys:typed-aref 'sys:int64 b  8.))
        (sys:typed-aref 'sys:int64 c 16.) (sys:int64+ (sys:typed-aref 'sys:int64 a 16.) (sys:typed-aref 'sys:int64 b 16.))
        (sys:typed-aref 'sys:int64 c 24.) (sys:int64+ (sys:typed-aref 'sys:int64 a 24.) (sys:typed-aref 'sys:int64 b 24.))
        (sys:typed-aref 'sys:int64 c 32.) (sys:int64+ (sys:typed-aref 'sys:int64 a 32.) (sys:typed-aref 'sys:int64 b 32.))
        (sys:typed-aref 'sys:int64 c 40.) (sys:int64+ (sys:typed-aref 'sys:int64 a 40.) (sys:typed-aref 'sys:int64 b 40.))
        (sys:typed-aref 'sys:int64 c 48.) (sys:int64+ (sys:typed-aref 'sys:int64 a 48.) (sys:typed-aref 'sys:int64 b 48.))
        (sys:typed-aref 'sys:int64 c 56.) (sys:int64+ (sys:typed-aref 'sys:int64 a 56.) (sys:typed-aref 'sys:int64 b 56.))
        (sys:typed-aref 'sys:int64 c 64.) (sys:int64+ (sys:typed-aref 'sys:int64 a 64.) (sys:typed-aref 'sys:int64 b 64.))
        (sys:typed-aref 'sys:int64 c 72.) (sys:int64+ (sys:typed-aref 'sys:int64 a 72.) (sys:typed-aref 'sys:int64 b 72.))
        ))

(defun sub1026 (a b c)
  #F
  (setf (sys:typed-aref 'sys:int64 c  0.) (sys:int64- (sys:typed-aref 'sys:int64 a  0.) (sys:typed-aref 'sys:int64 b  0.))
        (sys:typed-aref 'sys:int64 c  8.) (sys:int64- (sys:typed-aref 'sys:int64 a  8.) (sys:typed-aref 'sys:int64 b  8.))
        (sys:typed-aref 'sys:int64 c 16.) (sys:int64- (sys:typed-aref 'sys:int64 a 16.) (sys:typed-aref 'sys:int64 b 16.))
        (sys:typed-aref 'sys:int64 c 24.) (sys:int64- (sys:typed-aref 'sys:int64 a 24.) (sys:typed-aref 'sys:int64 b 24.))
        (sys:typed-aref 'sys:int64 c 32.) (sys:int64- (sys:typed-aref 'sys:int64 a 32.) (sys:typed-aref 'sys:int64 b 32.))
        (sys:typed-aref 'sys:int64 c 40.) (sys:int64- (sys:typed-aref 'sys:int64 a 40.) (sys:typed-aref 'sys:int64 b 40.))
        (sys:typed-aref 'sys:int64 c 48.) (sys:int64- (sys:typed-aref 'sys:int64 a 48.) (sys:typed-aref 'sys:int64 b 48.))
        (sys:typed-aref 'sys:int64 c 56.) (sys:int64- (sys:typed-aref 'sys:int64 a 56.) (sys:typed-aref 'sys:int64 b 56.))
        (sys:typed-aref 'sys:int64 c 64.) (sys:int64- (sys:typed-aref 'sys:int64 a 64.) (sys:typed-aref 'sys:int64 b 64.))
        (sys:typed-aref 'sys:int64 c 72.) (sys:int64- (sys:typed-aref 'sys:int64 a 72.) (sys:typed-aref 'sys:int64 b 72.))
        ))
|#
#|
(defun add1026 (a b c)
  #F
  (setf (sys:typed-aref 'sys:int32 c  0.) (sys:int32+ (sys:typed-aref 'sys:int32 a  0.) (sys:typed-aref 'sys:int32 b  0.))
        (sys:typed-aref 'sys:int32 c  4.) (sys:int32+ (sys:typed-aref 'sys:int32 a  4.) (sys:typed-aref 'sys:int32 b  4.))
        (sys:typed-aref 'sys:int32 c  8.) (sys:int32+ (sys:typed-aref 'sys:int32 a  8.) (sys:typed-aref 'sys:int32 b  8.))
        (sys:typed-aref 'sys:int32 c 12.) (sys:int32+ (sys:typed-aref 'sys:int32 a 12.) (sys:typed-aref 'sys:int32 b 12.))
        (sys:typed-aref 'sys:int32 c 16.) (sys:int32+ (sys:typed-aref 'sys:int32 a 16.) (sys:typed-aref 'sys:int32 b 16.))
        (sys:typed-aref 'sys:int32 c 20.) (sys:int32+ (sys:typed-aref 'sys:int32 a 20.) (sys:typed-aref 'sys:int32 b 20.))
        (sys:typed-aref 'sys:int32 c 24.) (sys:int32+ (sys:typed-aref 'sys:int32 a 24.) (sys:typed-aref 'sys:int32 b 24.))
        (sys:typed-aref 'sys:int32 c 28.) (sys:int32+ (sys:typed-aref 'sys:int32 a 28.) (sys:typed-aref 'sys:int32 b 28.))
        (sys:typed-aref 'sys:int32 c 32.) (sys:int32+ (sys:typed-aref 'sys:int32 a 32.) (sys:typed-aref 'sys:int32 b 32.))
        (sys:typed-aref 'sys:int32 c 36.) (sys:int32+ (sys:typed-aref 'sys:int32 a 36.) (sys:typed-aref 'sys:int32 b 36.))
        ))

(defun sub1026 (a b c)
  #F
  (setf (sys:typed-aref 'sys:int32 c  0.) (sys:int32- (sys:typed-aref 'sys:int32 a  0.) (sys:typed-aref 'sys:int32 b  0.))
        (sys:typed-aref 'sys:int32 c  4.) (sys:int32- (sys:typed-aref 'sys:int32 a  4.) (sys:typed-aref 'sys:int32 b  4.))
        (sys:typed-aref 'sys:int32 c  8.) (sys:int32- (sys:typed-aref 'sys:int32 a  8.) (sys:typed-aref 'sys:int32 b  8.))
        (sys:typed-aref 'sys:int32 c 12.) (sys:int32- (sys:typed-aref 'sys:int32 a 12.) (sys:typed-aref 'sys:int32 b 12.))
        (sys:typed-aref 'sys:int32 c 16.) (sys:int32- (sys:typed-aref 'sys:int32 a 16.) (sys:typed-aref 'sys:int32 b 16.))
        (sys:typed-aref 'sys:int32 c 20.) (sys:int32- (sys:typed-aref 'sys:int32 a 20.) (sys:typed-aref 'sys:int32 b 20.))
        (sys:typed-aref 'sys:int32 c 24.) (sys:int32- (sys:typed-aref 'sys:int32 a 24.) (sys:typed-aref 'sys:int32 b 24.))
        (sys:typed-aref 'sys:int32 c 28.) (sys:int32- (sys:typed-aref 'sys:int32 a 28.) (sys:typed-aref 'sys:int32 b 28.))
        (sys:typed-aref 'sys:int32 c 32.) (sys:int32- (sys:typed-aref 'sys:int32 a 32.) (sys:typed-aref 'sys:int32 b 32.))
        (sys:typed-aref 'sys:int32 c 36.) (sys:int32- (sys:typed-aref 'sys:int32 a 36.) (sys:typed-aref 'sys:int32 b 36.))
        ))
|#
#|
(defun add-mod (a b)
  (let* ((base (modnum-base a))
         (va   (modnum-v26 a))
         (vb   (modnum-v26 b)))
    (make-modnum
     :base base
     :v26 (map 'vector #'+ va vb))
    ))

(defun sub-mod (a b)
  (let* ((base (modnum-base a))
         (va   (modnum-v26 a))
         (vb   (modnum-v26 b)))
    (make-modnum
     :base base
     :v26 (map 'vector #'- va vb))
    ))

(let ((a  (enc26 1 *rbase*))
      (b  (enc26 3 *rbase*)))
  (sub-mod a b))




(defun norm (x)
  (let ((cy 0)
        (base (modnum-base x))
        (v  (modnum-v26 x)))
  (loop for ix from (1- (modbase-nvec base)) downto 0 do
          (let ((b26 (+ cy (aref v ix))))
            (setf cy  (ash b26 -26)
                  (aref v ix) (ldb (byte 26 0) b26))))
  (let ((wr (ash (aref v 0) (- (modbase-nmsb base)))))
    (cond ((zerop wr)
           (wrap-mod x))
          (t
           (setf (aref v 0) (ldb (byte (modbase-nmsb base) 0) (aref v 0)))
           (norm (add-mod x
                          (mpy-mod (enc26 wr base)
                                   (enc26 (modbase-mod-nbits base) base)))))
          ))))
  
                
(defvar *rbase*
  (let* ((b  *ed-r*)
         (nb (integer-length b))
         (wr (- (ash 1 nb) b))
         (nv (ceiling nb 26))
         (nm (- nb (* 26 (1- nv)))))
    (make-modbase
     :b b
     :nbits nb
     :mod-nbits wr
     :nvec nv
     :nmsb nm)
    ))

(raw-enc26 *ed-r* *rbase*)

;; ----------------------------------------------------------------------

(defconstant +max29+ (sys:int64-1- (sys:int64<< sys:+int64-1+ 29)))
(defconstant +max58+ (sys:int64-1- (sys:int64<< sys:+int64-1+ 58)))

(defun mpy58 (a b)
  #F
  (declare (fixnum a b))
  (let* ((a64   (sys:integer-to-int64 a))
         (b64   (sys:integer-to-int64 b))
         (alo   (sys:int64-logand a64 +max29+))
         (ahi   (sys:int64>> a64 29))
         (blo   (sys:int64-logand b64 +max29+))
         (bhi   (sys:int64>> b64 29))
         (lo    (sys:int64* alo blo))
         (hi    (sys:int64* ahi bhi))
         (mid   (sys:int64+
                 (sys:int64* alo bhi)
                 (sys:int64* ahi blo)))
         (midlo (sys:int64-logand mid +max29+))
         (midhi (sys:int64>> mid 29))
         (lox   (sys:int64+ lo (sys:int64<< midlo 29)))
         (cy    (sys:int64>> lox 58))
         (loxx  (sys:int64-logand lox +max58+))
         (hix   (sys:int64+ hi
                            (sys:int64+ midhi cy))))
    (declare (sys:int64 a64 b64
                        msk29 msk58
                        alo ahi
                        blo bhi
                        lo hi mid
                        midlo midhi
                        lox cy loxx hix))
    (values (sys:int64-to-integer loxx)
            (sys:int64-to-integer hix))
    ))
         
#|                
(defun mpy58 (a b)
  (declare (fixnum a b))
  (let* ((alo   (ldb (byte 29  0) a))
         (ahi   (ash a -29))
         (blo   (ldb (byte 29  0) b))
         (bhi   (ash b -29))
         (lo    (* alo blo))
         (hi    (* ahi bhi))
         (mid   (+ (* alo bhi) (* ahi blo)))
         (midlo (ldb (byte 29 0) mid))
         (midhi (ash mid -29))
         (lox   (+ lo (ash midlo 29)))
         (cy    (ash lox -58))
         (loxx  (ldb (byte 58 0) lox))
         (hix   (+ hi midhi cy)))
    (declare (fixnum alo ahi
                     blo bhi
                     lo hi mid
                     midlo midhi
                     lox loxx hix
                     cy))
    (values loxx hix)))
|#

(let ((x (1- (ash 1 58))))
  (multiple-value-bind (lo hi)
      (mpy58 x x)
    (list lo (type-of lo) hi (type-of hi))))

(let ((prod (* +max58+ +max58+)))
  (values (ldb (byte 58 0) prod) (ash prod -58)))

(+ 0 +max58+)

(mpy64 #xFFFF_FFFF_FFFF_FFFF #xFFFF_FFFF_FFFF_FFFF)

(with-standard-io-syntax
  (let ((*print-base* 16))
    (print (* #xFFFF_FFFF_FFFF_FFFF #xFFFF_FFFF_FFFF_FFFF))))

(defun wrap (x b)
  (cond ((minusp x)
         (- b (wrap (- x) b)))
        (t
         (let* ((nb  (integer-length b))
                (wf  (- (ash 1 nb) b)))
           (um:nlet iter ((x  x))
             (let ((xlo (ldb (byte nb 0) x))
                   (xhi (ash x (- nb))))
               (cond ((zerop xhi)
                      (if (>= xlo b)
                          (- xlo b)
                        xlo))
                     (t
                      (go-iter (+ xlo (* wf xhi))))
                     ))
             )))
        ))


(wrap (int (hash/256 *ed-r*)) *ed-r*)
(mod (int (hash/256 *ed-r*)) *ed-r*)
(wrap (* *ed-r* *ed-r*) *ed-r*)


(setf x (multiple-value-list (edec:ed-random-pair)))

(defun gen-mult (n)
  (let ((ws (edec::windows n 4)))
    `(defun smult (pt)
       (cond ((ed-valid-point-p pt)
              (let ((wc   (make-bipolar-window-cache
                           :nbits 4
                           :pt    pt))
                    (ans  (ed-projective (ed-neutral-point))))
                (setf (aref (bipolar-window-cache-precv wc) 8) ans)
                (flet ((acc (ix)
                         (setf ans (ed-projective-add (get-prec wc ix)
                                                      (ed-projective-double
                                                       (ed-projective-double
                                                        (ed-projective-double
                                                         (ed-projective-double ans))))
                                                      ))
                         ))
                  ,@(loop for w in ws collect `(acc ,w))
                  ans)))
             (t
              (ed-projective (ed-neutral-point)))
             ))
    ))

(let ((x (com.ral.actors.secure-comm::actors-skey))
      (*print-level* nil))
  (print (gen-mult x))
  (values))

(ed-pt= (com.ral.actors.secure-comm::actors-pkey)
        (edec::smult *ed-gen*))


(let ((*print-level* nil))
  (print (gen-mult (car x)))
  (values))

(edec::windows (car x) 4)
(0
 2
 -7
 6
 -2
 -3
 5
 -3
 -5
 7
 5
 0
 -3
 4
 -8
 3
 -1
 -2
 7
 2
 4
 2
 4
 -2
 6
 -8
 -7
 -3
 7
 7
 5
 1
 -3
 -3
 3
 3
 6
 -6
 2
 -2
 -7
 6
 -5
 3
 7
 -2
 -5
 -5
 5
 5
 -7
 -7
 -1
 5
 -4
 -6
 6
 -4
 -2
 5
 0
 -7
 7
 -1)
|#
