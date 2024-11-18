
(defpackage #:com.ral.nrglue
  (:export
   #:laguer
   #:zroots
   #:memcof
   #:evlmem
   #:fixrts
   #:predic
   ))


(in-package #:com.ral.nrglue)

(defun dfloat (x) 
 (declare (type number x))
 (the double-float (coerce x 'double-float)))

(defmacro fproclaim (x) `(eval-when (compile load eval) (proclaim ,x)))

(defmacro fref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices)))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defmacro sngl (x)
  `(float ,x 1.0f0))

(defmacro 2- (x)
  `(- ,x 2))

;-------------------------------------------------------------------------

(defun laguer (a m x &key (eps 1.0d-6) (polish t) (epss 6.0d-8) (maxit 100))
 (declare (type (simple-array (complex double-float) (*)) a))
 (declare (type (complex double-float) x))
 (declare (type symbol polish))
 (declare (type double-float epss))
 (declare (type fixnum maxit m))

 (prog ((dx #c(0d0 0d0))
        (x1 #c(0d0 0d0))
        (b #c(0d0 0d0)) 
        (d #c(0d0 0d0))
        (f #c(0d0 0d0))
        (g #c(0d0 0d0))
        (h #c(0d0 0d0))
        (sq #c(0d0 0d0))
        (gp #c(0d0 0d0)) 
        (gm #c(0d0 0d0))
        (g2 #c(0d0 0d0))
        (zero #c(0d0 0d0)) 
        (err 0d0)
        (abx 0d0)
        (cdx 0d0));(dxold 0d0) 
   (declare (type (complex double-float) dx x1 b d f g h sq gp gm g2 zero))
   (declare (type double-float abx cdx err))

;  (setq mp1 (1+ m))
 
   (setq zero #c(0d0 0d0)) 
;  (setf dxold (abs x)) 
   (do ((iter 1 (+ iter 1)))
       ((> iter maxit) t)
     (declare (type fixnum iter))
     (setf b (aref a m))
     (setf err (abs b))
     (setf d zero)
     (setf f zero)
     (setf abx (abs x))

     (do ((j (1- m) (1- j)))
         ((< j 0) t)
       (declare (type fixnum j))
       (setf f (+ (* x f) d))
       (setf d (+ (* x d) b))
       (setf b (+ (* x b) (aref a j)))
       (setf err (+ (abs b) (* abx err))))
     
     (setf err (* epss err))
     (cond
      ((<= (abs b) err) 
       (go end))
      (t 
       (setf g (/ d b)) 
       (setf g2 (* g g))
       (setf h (+ g2 (/ (* -2d0 f) b)))
       (setf sq (sqrt (* (1- m) (- (* m h) g2))))
       (setf gp (+ g sq)) (setf gm (- g sq))
       (if (< (abs gp) (abs gm)) (setf gp gm)) 
       (setf dx (/ m gp))))
     
     (setf x1 (- x dx))
     
     (if (= x x1) (go end))
     (setf x x1)
     (setf cdx (abs dx))
;    (setf dxold cdx)
     (if (and (not polish) 
              (<= cdx (* eps (abs x)))) (go end))) 
   
   (error " too many iterations in laguer ")
   end
   (return (the (complex double-float) x))))

;-------------------------------------------------------------------------

(defun zroots (a &key
         (polish nil) (eps 1.0d-6) (epss 1.0d-6) (maxit 100))
  ;; a is vector of polynomial coefficients in ascending orders
  ;; return a vector of polynomial roots in the complex plane
  (declare (type (array number *) a))
  (declare (type fixnum maxit))
  (declare (type double-float eps epss))
  (declare (type symbol polish))
  ;; (check-type a (array number (*)))
  ;; (check-type eps double-float)
  ;; (check-type epss double-float)
  ;; (check-type maxit (fixnum 1 *))

  (prog* (
          (m (1- (array-dimension a 0))) ;; polynomial order
          (roots 
           (make-array m :element-type '(complex double-float) 
                       :initial-element #c(0d0 0d0)))
          (ad 
           (make-array (1+ m) :element-type '(complex double-float) 
                       :initial-element #c(0d0 0d0)))
          (x #c(0d0 0d0))
          (b #c(0d0 0d0))
          (c #c(0d0 0d0))
          (isav 0))
    
    (declare (type (simple-array (complex double-float) (*)) roots ad))
    (declare (type (complex double-float) x b c))
    (declare (type fixnum m isav))
    
 

    (do ((j 0 (+ j 1)))
        ((> j m) t)
      (declare (type fixnum j))
      (setf (aref ad j) (coerce (aref a j) '(complex double-float))))
    
    (do ((j  m (1- j)))
        ((< j 1) t)
      (declare (type fixnum j))
      (setf x #c(0d0 0d0))
      (setq x (laguer ad j x :eps eps :polish nil)) 
      (if (<= (abs (imagpart x)) (* (* 2d0 (expt eps 2)) (abs (realpart x))))
          (setf x (complex (realpart x) 0d0)))
      (setf (aref roots (1- j)) x)
      (setf b (aref ad j))
      (do ((jj j (1- jj)))
          ((< jj 1) t)
        (declare (type fixnum jj))
        (setf c (aref ad (1- jj)))
        (setf (aref ad (1- jj)) b)
        (setf b (+ (* x b) c)))) 
    
    (when polish
      (do ((j 0 (+ j 1)))
          ((> j (1- m)) t)
        (declare (type fixnum j))
        (setf (aref roots j) (laguer a m (aref roots j) 
                                     :eps eps :epss epss :polish t :maxit maxit))))
    
    (do ((j 2 (+ j 1)))
        ((> j m) t)
      (declare (type fixnum j))
      (setf x (aref roots (1- j)))
      (do ((i (1- j) (1- i)))
          ((< i 1) t)
        (declare (type fixnum i))
        (when (<= (realpart (aref roots (1- i))) (realpart x)) 
          (setq isav i) (go label10))
        (setf (aref roots i) (aref roots (1- i))))
      
      (setf isav 0)
      label10
      (setf (aref roots isav) x)) 
    
    (return roots)))

;-------------------------------------------------------------------------------

(defun memcof (data m)
 (declare (type (simple-array double-float (*)) data))
 (declare (type fixnum m))

 (prog* ((pm 0d0)  (p 0d0)
  (n (array-dimension data 0)) 
  (cof (make-array m :element-type 'double-float :initial-element 0d0))
  (wk1 (make-array n :element-type 'double-float :initial-element 0d0))
  (wk2 (make-array n
 :element-type 'double-float :initial-element 0d0))
  (wkm (make-array m :element-type 'double-float :initial-element 0d0))
  (pneum 0d0) (denom 0d0)) 

  (declare (type (simple-array double-float (*)) cof wk1 wk2 wkm))
  (declare (type fixnum n))
  (declare (type double-float p pm pneum denom))

  (setf p 0d0) 

  (do ((j 1 (+ j 1)))
      ((> j n) t)
      (declare (type fixnum j))
    (setf p (+ p (expt (fref data j) 2)))) 

  (setf pm (/ p (dfloat n))) 
  (fset (fref wk1 1) (fref data 1)) 
  (fset (fref wk2 (1- n)) (fref data n)) 

  (do ((j 2 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (fset (fref wk1 j) (fref data j))
    (fset (fref wk2 (1- j)) (fref data j))) 

  (do ((k 1 (+ k 1)))
      ((> k m) t)
      (declare (type fixnum k))
    (setf pneum 0d0)
    (setf denom 0d0)
    (do ((j 1 (+ j 1)))
        ((> j (- n k)) t)
        (declare (type fixnum j))
      (setf pneum (+ pneum (* (fref wk1 j) (fref wk2 j))))
      (setf denom (+ (+ denom (expt (fref wk1 j) 2)) (expt (fref wk2 j) 2))))

    (fset (fref cof k) (/ (* 2 pneum) denom))
    (setf pm (* pm (- 1d0 (expt (fref cof k) 2))))
    (do ((i 1 (+ i 1)))
        ((> i (1- k)) t)
        (declare (type fixnum i))
      (fset (fref cof i) (- (fref wkm i)
                            (* (fref cof k) (fref wkm (- k i))))))
    
    (if (= k m) (go end))

    (do ((i 1 (+ i 1)))
        ((> i k) t)
        (declare (type fixnum k))
      (fset (fref wkm i) (fref cof i)))

    (do ((j 1 (+ j 1)))
        ((> j (1- (- n k))) t)
        (declare (type fixnum j))
      (fset (fref wk1 j) (- (fref wk1 j) (* (fref wkm k) (fref wk2 j))))
      (fset (fref wk2 j) (- (fref wk2 (+ j 1))
                            (* (fref wkm k) (fref wk1 (+ j 1))))))) 
   
  (error " should never reach here in memcof")
end
(return (values pm cof))
))

;-------------------------------------------------------------------------------

(defun evlmem (fdt cof pm)
 (declare (type (simple-array double-float (*)) cof)) 
 (declare (type double-float fdt pm))
 
 (prog ((evlmem 0d0) (m 0) (theta 0d0) (wpr 0d0) (wpi 0d0) 
        (wr 0d0) (wi 0d0) (sumr 0d0) (sumi 0d0) (wtemp 0d0))
  (declare (type double-float evlmem wr wi wpr wtemp theta sumr wpi)) 
  (declare (type fixnum m))

  (setq m (array-dimension cof 0)) 
  (setf theta (* 6.28318530717959d0 fdt)) 
  (setf wpr (cos theta)) 
  (setf wpi (sin theta)) 
  (setf wr 1.0d0) 
  (setf wi 0.0d0) 
  (setf sumr 1d0) 
  (setf sumi 0d0) 
  (do ((i 0 (+ i 1)))
      ((> i (1- m)) t)
      (declare (type fixnum i))
    (setf wtemp wr)
    (setf wr (+ (* wr wpr) (* (- wi) wpi)))
    (setf wi (+ (* wi wpr) (* wtemp wpi)))
    (setf sumr (+ sumr (* (* -1d0 (aref cof i)) (sngl wr))))
    (setf sumi (+ sumi (* (* -1d0 (aref cof i)) (sngl wi))))) 
  (setf evlmem (/ pm (+ (expt sumr 2) (expt sumi 2)))) 
  
  (return (the double-float evlmem))))

;------------------------------------------------------------------------------

(defun fixrts (d &key (npmax 100))
 (declare (type (simple-array double-float (*)) d)) 
 (declare (type fixnum npmax))
 (declare (ignore npmax))

 (prog* (
  (npoles (array-dimension d 0))
  (a (make-array (1+ npoles) :element-type '(complex double-float) 
                            :initial-element #c(0d0 0d0)))
  (roots (make-array (1+ npoles) :element-type '(complex double-float) 
                            :initial-element #c(0d0 0d0)))
  (n 0))

  (declare (type fixnum n npoles))
  (declare (type (simple-array (complex double-float) (*)) a))
  (declare (type (simple-array  (complex double-float) (*)) roots))
  (declare (ignore n))


  (setf (aref a npoles) #c(1d0 0d0)) 

  (do ((j npoles (1- j)))
      ((< j 1) t)
      (declare (type fixnum j))
    (setf (aref a (1- j)) 
          (complex (- (aref d (- npoles j))) 0d0))) 


  (setq roots (zroots a :polish t)) 


  (do ((j 0 (+ j 1)))
      ((> j (1- npoles)) t)
      (declare (type fixnum j))
    (if (> (abs (aref roots j)) 1d0)
     (setf (aref roots j) (/ 1d0 (conjugate (aref roots j)))))) 

  (setf (aref a 0) (- (aref roots 0))) 
  (setf (aref a 1) #c(1d0 0d0)) 
  (do ((j 2 (+ j 1)))
      ((> j npoles) t)
      (declare (type fixnum j))
    (setf (aref a j) #c(1d0 0d0))
    (do ((i j (1- i)))
        ((< i 2) t)
        (declare (type fixnum i))
      (setf (aref a (1- i)) (+ (aref a (2- i))
                          (* (- (aref roots (1- j))) (aref a (1- i))))))
    (setf (aref a 0) (* (- (aref roots (1- j))) (aref a 0)))) 

  (do ((j 1 (+ j 1)))
      ((> j npoles) t)
      (declare (type fixnum j))
    (setf (aref d (- npoles j)) (- (realpart (aref a (1- j)))))) 
  
  (return d)))

;------------------------------------------------------------------------------

(defun predic (data d nfut &key (npmax 100))
 (declare (type (simple-array double-float (*)) data)) 
 (declare (type (simple-array double-float (*)) d)) 
 (declare (type fixnum npmax))
 (declare (ignore npmax))

 (prog* ((ndata 0) 
  (npoles (array-dimension d 0))
  (future 
     (make-array nfut :element-type 'double-float :initial-element 0d0))
  (reg 
     (make-array npoles :element-type 'double-float :initial-element 0d0))
  (discrp 0d0) (sum 0d0))

  (declare (type (simple-array double-float (*)) future)) 
  (declare (type (simple-array double-float (*)) reg)) 
  (declare (type fixnum ndataa npoles))
  (declare (type double-float discrp sum))

  (setq ndata (array-dimension data 0))

  (do ((j 1 (+ j 1)))
      ((> j npoles) t)
    (setf (aref reg (1- j)) (aref data (- ndata j)))) 

  (do ((j 1 (+ j 1)))
      ((> j nfut) t)
    (setf discrp 0d0)
    (setf sum discrp)
    (do ((k 0 (+ k 1)))
        ((> k (1- npoles)) t)
      (setf sum (+ sum (* (aref d k) (aref reg k)))))
    (do ((k (1- npoles) (1- k)))
        ((< k 1) t)
      (setf (aref reg k) (aref reg (1- k))))

    (setf (aref reg 0) sum)
    (setf (aref future (1- j)) sum)) 
  
  (return future)))

;------------------------------------------------------------------------------

