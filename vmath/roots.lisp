
(in-package #:com.ral.roots)

;; ----------------------------------------------------------
;; Bracket a root given two starting values
;;
(defun zbrac (func x1 x2 &key (factor 1.6d0) (ntry 50))
  (check-type ntry fixnum)
  (check-type func function)
  (check-type x1   real)
  (check-type x2   real)
  (check-type factor real)
  (locally
    (declare (fixnum   ntry)
             (function func)
             (real     x1 x2 factor))
    (multiple-value-bind (x1 x2)
        (values (um:dfloat (min x1 x2))
                (um:dfloat (max x1 x2)))
      (declare (type double-float x1 x2))
        
      (if (>= x1 x2)
          (error " initial range in zbrac must be non-empty. ")) 
        
      (let ((factor (um:dfloat factor))
            (succes nil)
            (f1 (um:dfloat (funcall func x1)))
            (f2 (um:dfloat (funcall func x2))))
        (declare (type symbol succes))
        (declare (type double-float factor f1 f2))
          
        (do ((j ntry (1- j)))
            ((or (setf succes (minusp (* f1 f2)))
                 (not (plusp j))))
          (declare (type fixnum j))
            
          (cond
           ((< (abs f1) (abs f2))
            (setf f2 f1
                  x1 (+ x1 (* factor (- x1 (shiftf x2 x1))))
                  f1 (um:dfloat (funcall func x1))) )
             
           (t
            (setf f1 f2
                  x2 (+ x2 (* factor (- x2 (shiftf x1 x2))))
                  f2 (um:dfloat (funcall func x2))) )
             
           ))
          
        (values x1 x2 succes)
        ))))

;; -------------------------------------------------------------------------
;; Root by Bisection -- root needs to be bracketed
;;
(defun rtbis (func x1 x2 &key (xacc 0) (jmax 60))
  ;; double precision floats have 56 bit mantissas
  (check-type jmax fixnum)
  (check-type func function)
  (check-type x1 real)
  (check-type x2 real)
  (check-type xacc real)
  (locally
    (declare (fixnum   jmax)
             (real     x1 x2 xacc)
             (function fn))
    (multiple-value-bind (x1 x2 xacc)
        (values (um:dfloat (min x1 x2))
                (um:dfloat (max x1 x2))
                (um:dfloat xacc))
      (declare (type double-float x1 x2 xacc))
      
      (let ((fmid (um:dfloat (funcall func x2))))
        (declare (type double-float fmid))
        
        (if (zerop fmid)
            (the double-float x2)
          
          (let ((f  (um:dfloat (funcall func x1))))
            (declare (type double-float f))
            
            (if (zerop f)
                (the double-float x1)
              
              (progn
                (unless (minusp (* f fmid))
                  (error " root must be bracketed for bisection in rtbis "))
                
                (multiple-value-bind (rtbis dx)
                    (if (minusp f)
                        (values x1 (- x2 x1))
                      (values x2 (- x1 x2)))
                  (declare (type double-float rtbis dx))
                  
                  (do ((j jmax (1- j)))
                      ((or (= rtbis (+ rtbis dx))
                           (< (abs dx) xacc)
                           (zerop fmid)
                           (and (not (plusp j))
                                (error "Too many iterations in rtbis"))))
                    (declare (type fixnum j))
                    
                    (setf dx (* dx 0.5d0))
                    (let ((xmid (+ rtbis dx)))
                      (declare (type double-float xmid))
                      
                      (setf fmid (um:dfloat (funcall func xmid)))
                      
                      (unless (plusp fmid)
                        (setf rtbis xmid))
                      ))
                  
                  (the double-float rtbis)
                  
                  )))
            ))
        )) ))

;-------------------------------------------------------------------------
;; Root by Secant Method -- root does not need to be bracketed, but it helps...
;;
(defun rtsec (func x1 x2 &key (xacc 0) (maxit 30))
  (check-type func  function)
  (check-type x1    real)
  (check-type x2    real)
  (check-type xacc  real)
  (check-type maxit fixnum)
  (locally
    (declare (fixnum   maxit)
             (function func)
             (real     x1 x2 xacc))
    
    (multiple-value-bind (x1 x2 xacc)
        (values (um:dfloat x1)
                (um:dfloat x2)
                (um:dfloat xacc))
      (declare (type double-float x1 x2 xacc))

      (let* ((fl    (um:dfloat (funcall func x1)))
             (f     (um:dfloat (funcall func x2)))
             (rtsec 0d0)
             (dx    (- x1 x2))
             (xl    0d0))
        (declare (type double-float fl f rtsec dx xl))
          
        (if (< (abs fl) (abs f))
            (let ((swap fl))
              (declare (type double-float swap))
              (setf rtsec x1
                    xl    x2
                    fl    f
                    f     swap))
          (setf xl x1
                rtsec x2))
          
        (do ((j 1 (1+ j)))
            ((or (and (> j maxit)
                      (error "rtsec exceeded maximum iterations"))
                 (< (abs dx) xacc)
                 (zerop f)
                 (= rtsec (+ rtsec dx))))
          (declare (type fixnum j))

          (setf dx  (/ (* (- xl rtsec) f) (- f fl))
                xl  rtsec
                fl  f
                rtsec (+ rtsec dx)
                f  (um:dfloat (funcall func rtsec))
                ))
          
        (the double-float rtsec)

        ))))

;-------------------------------------------------------------------------
;; Root by Brent's Method -- root must be bracketed
;;
(defun zbrent (func x1 x2 &key (tol 0) (itmax 100) (eps 1.0d-16))
  (check-type func  function)
  (check-type x1    real)
  (check-type x2    real)
  (check-type tol   real)
  (check-type itmax fixnum)
  (check-type eps   real)
  (locally
    (declare (fixnum   itmax)
             (function func)
             (real     x1 x2 tol eps))

    (multiple-value-bind (a b tol eps)
        (values (um:dfloat x1)
                (um:dfloat x2)
                (um:dfloat tol)
                (um:dfloat eps))
      (declare (type double-float a b tol eps))

      (let* ((fa (um:dfloat (funcall func a)))
             (fb (um:dfloat (funcall func b))))
        (declare (type double-float fa fb))

        (cond ((zerop fa)  a)
              ((zerop fb)  b)
              (t
               (when (plusp (* fa fb))
                 (error "Root must be bracketed for zbrent"))

               (let* ((c    b)
                      (fc   fb)
                      (d    0d0)
                      (e    0d0)
                      (s    0d0)
                      (p    0d0)
                      (q    0d0)
                      (r    0d0)
                      (xm   (- a b))
                      (tol1 tol))
                 (declare (type double-float c fc d e s xm tol1 p q r))
            
                 (do ((iter 1 (1+ iter)))
                     ((or (and (> iter itmax)
                               (error "Zbrent exceeded maximum iterations"))
                          (zerop fb)
                          (<= (abs xm) tol1))
                      b)
                   (declare (type fixnum iter))
                     
                   (when (plusp (* fb fc))
                     (setf c a
                           fc fa
                           d (- b a)
                           e d))
              
                   (when (< (abs fc) (abs fb))
                     (setf a b
                           b c
                           c a
                           fa fb
                           fb fc
                           fc fa))
              
                   (setf tol1 (+ (* 2d0 eps (abs b)) (* 0.5d0 tol))
                         xm   (* 0.5d0 (- c b)))

                   (if (and (>= (abs e) tol1)
                            (> (abs fa) (abs fb)))
                       (progn
                         ;; attempt inverse quadratic interpolation
                         (setf s (/ fb fa))
                           
                         (if (= a c)
                             (setf p (* 2d0 xm s)
                                   q (- 1d0 s))
                           (setf q (/ fa fc)
                                 r (/ fb fc)
                                 p (* s (+ (* 2d0 xm q (- q r))
                                           (* (- a b) (- r 1d0))))
                                 q  (* (- q 1d0) (- r 1d0) (- s 1d0))
                                 ))
                         ;; check whether in bounds
                         (if (plusp p)
                             (setf q (- q))
                           (setf p (- p)))
                           
                         (if (< (* 2d0 p)
                                (min (- (* 3d0 xm q)
                                        (abs (* tol1 q)))
                                     (abs (* e q))))
                             ;; accept interpolation
                             (setf e d
                                   d (/ p q))
                           ;; interpolation failied, use bisection
                           (setf d xm
                                 e d)))

                     ;; else - bounds decreasing too slowly, use bisection
                     (setf d xm
                           e d))

                   ;; move last guess to a
                   (setf a b
                         fa fb)

                   ;; evaluate new trial root
                   (if (> (abs d) tol1)
                       (setf b (+ b d))
                     (setf b (if (minusp xm)
                                 (- b tol1)
                               (+ b tol1))
                           ))
                     
                   (setf fb (um:dfloat (funcall func b)))
                   (print (list iter b (um:nfmt fb)))
                   )))
              )))
    ))

;-------------------------------------------------------------------------
#|
;; not cleaned up yet...
(defun rtsafe (func funcd x1 x2 xacc &key (maxit 100))
  (declare (type fixnum maxit))
  
  (prog ((xd1 (um:dfloat x1))
         (xd2 (um:dfloat x2))
         (xdacc (um:dfloat xacc))
         (fl 0d0) (fh 0d0) (rtsafe 0d0) (dx 0d0) (dxold 0d0) 
         (xh 0d0) (xl 0d0) (f 0d0) (df 0d0) (temp 0d0))
    (declare (type double-float xd1 xd2 xdacc fl fh rtsafe dx dxold xh f xl df temp))
    
    (setq fl (um:dfloat (funcall func xd1)))
    (if (zerop fl)
        (return (the double-float xd1)))
    
    (setq fh (um:dfloat (funcall func xd2)))
    (if (zerop fh)
        (return (the double-float xd2)))
    
    (if (> (* fl fh) 0d0) (error " root must be bracketed for rtsafe ")) 

    (cond 
     ((< fl 0d0)
      (setf xl xd1) 
      (setf xh xd2))
     (t 
      (setf xh xd1) 
      (setf xl xd2))) 
    
    (setf rtsafe (* 0.5d0 (+ xd1 xd2))) 
    (setf dxold (abs (- xd2 xd1))) 
    (setf dx dxold) 
    (setq f  (um:dfloat (funcall func  rtsafe))
          df (um:dfloat (funcall funcd rtsafe)))
    (do ((j 1 (+ j 1)))
        ((> j maxit) t)
      (cond 
       ((or (>= (* (+ (* (+ rtsafe (- xh)) df) (- f))
                   (+ (* (+ rtsafe (- xl)) df) (- f)))
                0d0)
            (> (abs (* 2d0 f)) (abs (* dxold df))))
        (setf dxold dx)
        (setf dx (* 0.5d0 (+ xh (- xl)))) 
        (setf rtsafe (+ xl dx)) 
        (if (= xl rtsafe) (go end)))
       (t 
        (setf dxold dx)
        (setf dx (/ f df)) 
        (setf temp rtsafe) 
        (setf rtsafe (+ rtsafe (- dx)))
        (if (= temp rtsafe) (go end))))
      
      (if (< (abs dx) xdacc) (go end))
      (setq f  (um:dfloat (funcall func rtsafe))
            df (um:dfloat (funcall funcd rtsafe)))
      (if 
          (< f 0d0) 
          (setf xl rtsafe)
        (setf xh rtsafe))) 
    
    (error " rtsafe exceeding maximum iterations ")
end
    (return (the double-float rtsafe))))
|#

(defun deriv (fn dx)
  (let ((half-dx (* 0.5d0 dx)))
    (lambda (x)
      (if (zerop x)
          (/ (- (funcall fn half-dx)
                (funcall fn (- half-dx)))
             dx)
        (/ (- (funcall fn (* x (+ 1.0d0 half-dx)))
              (funcall fn (* x (- 1.0d0 half-dx))))
           (* x dx))
        ))))

(defun newton (fn start eps der)
  (check-type fn    function)
  (check-type start real)
  (check-type eps   real)
  (check-type der   (or real
                        function))
  ;; der must be either function or increment for numerical derivative
  (locally
    (declare (function  fn)
             (real      start eps))
    (let ((fder (if (functionp der)
                    der
                  (deriv fn der))))
      (labels ((iterate (x)
                 (if (is-ok x)
                     x
                   (iterate (do-better x))))
               (is-ok (x)
                 (declare (real x))
                 (< (abs (funcall fn x)) eps))
               (do-better (x)
                 (declare (real x))
                 (- x (/ (funcall fn x) (funcall fder x)))))
        (iterate start)))
    ))

(defun find-root (fn start &optional start2)
  #| |#
  (multiple-value-bind (start1 start2)
      (if start2
          (values start start2)
        (zbrac fn start (if (zerop start)
                            1.6
                          (* start 1.6))))
    (zbrent fn start1 start2))
  #| |#
  ;; (newton fn start 0.01 1.0d-8)
  )

