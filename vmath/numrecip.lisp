; From: Bill Schottstaedt <bil@ccrma.Stanford.EDU>
; Message-Id: <9408261415.AA03200@ ccrma.Stanford.EDU >
; Date: Fri, 26 Aug 94 07:15:28 -0700
; To: mkant@cs.cmu.edu

;;; -*- Lisp -*-
;;;
;;; translated from "Numerical Recipes in Pascal" by Press, Flannery, Teukolsky, Vetterling
;;;    Cambridge Univ Press, 1989.  All functions and variables are named the same.
;;;
;;;    bessj0(x) -- J0(x) 

;;;    bessj1(x) -- J1(x)
;;;    bessj(x) -- Jn(x) for n>= 2
;;;        Bessel-Jn(n x) -- my combination of these three
;;;    bessy0(x) -- Y0(x)
;;;    bessy1(x) -- Y1(x)
;;;    bessy(n,x) -- Yn(x) for n>=2
;;;        Bessel-Yn(x) -- my combination of these
;;;    bessi0(x) -- I0(x)
;;;    bessi1(x) -- I1(x)
;;;    bessi(x) -- In(x) for n>=2
;;;        Bessel-In(n,x)
;;;    bessk0(x) -- K0(x)
;;;    bessk1(x) -- K1(x)
;;;    bessk(n,x) -- Kn(x) for n>=2
;;;        Bessel-Kn(n,x)
;;;
;;;    gammln(x) -- Ln(gamma(x))
;;;    gamma(x) -- gamma(x)
;;;    beta(z,w) -- B(z,w) (beta function)
;;;    factrl(n) -- n!
;;;    factln(n) -- Ln(n!)
;;;    bico(n,k) -- binomial coefficient (n k) (n! / (k! (n - k)! ))
;;;    gammp(a,x) -- incomplete gamma function P(a,x)
;;;    gammq(a,x) -- ditto, Q(a,x) = 1.0 - P(a,x)
;;;    erf(x) -- error function
;;;    erfc(x) -- complementary error function
;;;    betai(a,b,x) -- incomplete beta function 

;;;        student-distribution and F-distribution
;;;
;;;    plgndr(l,m,x) -- Legendre polynomial
;;;
;;;    ran3(seed) -- white noise
;;;    expdev(seed) -- exponentially distributed noise
;;;    gasdev(seed) --  gaussian noise
;;;
;;;    zroots and laguer
;;;
;;;    four1(data,nn,isign) -- in place FFT or IFFT of complex or real data. NN must be power of  
2.

;;;    twofft(data1,data2,fft1,fft2,n) -- two FFT's at once from data into fft arrays
;;;    realft(data,n,isign) -- FFT of 2n real data points returning positive frequency half
;;;    convlv(data,n,respns,m,isign,ans) -- convolve DATA with RESPNS (or deconvolve if  
ISIGN=-1)
;;;    correl(data1,data2,n,ans) -- compute correlation of data1 and data2
;;;        autocorrel(data,n,ans) -- autocorrelation of data
;;;        DFT(data) -- discrete Fourier transform of any data, done by hand
;;;        spectrum(data window) -- window data, return normalized power spectrum
;;;    memcof(data,n,m,cof) -- fill COF with Maximum Entropy Method coefficients, return scaler  
("pm")
;;;    evlmem(fdt,cof,m,pm) --  return power spectrum estimate
;;;
;;;    locate(xx,x) -- find position of x in array xx [0 based]
;;;    indxx(arrin,indx) -- make arrin[indx[i]] be ascending order [0 based]
;;;    rank(indx,irank) -- return table of ranks, given index [0 based]
;;;    polint(xa,ya,x) -- polynomial interpolation using (xa ya) points and find y given x
;;;    ratint(xa,ya,x) -- ditto but using rational function
;;;    poldiv(u,n,v,nv,r,q) -- polynomial division
;;;    qtrap and qsimp -- integrate a function between two points (qtrap x0 x1 func)
;;;
;;;    moment(data) -- returns mean, average deviation, standard deviation, variance, skewness,  
kurtosis of data [0 based]
;;;    Mdian2(data) -- returns median, sorts data
;;;
;;;
;;;
;;;
;;;; ------------------------------------------------------------------------------------------
;;;;                    Linear Algebra


(defun ludcmp (a n)
  (let* ((tiny 1.0e-20)
	 (d 1.0)
	 (sum 0.0)
	 (indx (make-array (1+ n) :element-type 'integer :initial-element 0))
	 (vv (make-array (1+ n) :element-type 'float :initial-element 0.0)))
    (loop for i from 1 to n do
      (let ((big (loop for j from 1 to n maximize (abs (aref a i j)))))
	(if (zerop big) (error "singular matrix"))
	(setf (aref vv i) (/ 1.0 big))))
    (loop for j from 1 to n do
      (loop for i from 1 to (1- j) do
	(setf sum (aref a i j))
	(loop for k from 1 to (1- i) do
	  (decf sum (* (aref a i k) (aref a k j))))
	(setf (aref a i j) sum))
      (let ((big 0.0)
	    (dum 0.0)
	    (imax 0))
	(loop for i from j to n do
	  (setf sum (aref a i j))
	  (loop for k from 1 to (1- j) do
	    (decf sum (* (aref a i k) (aref a k j))))
	  (setf (aref a i j) sum)
	  (setf dum (* (aref vv i) (abs sum)))
	  (when (>= dum big)
	    (setf big dum)
	    (setf imax i)))
	(when (/= j imax)
	  (loop for k from 1 to n do
	    (setf dum (aref a imax k))
	    (setf (aref a imax k) (aref a j k))
	    (setf (aref a j k) dum))
	  (setf d (- d))
	  (setf (aref vv imax) (aref vv j)))
	(setf (aref indx j) imax)
	(if (zerop (aref a j j)) (setf (aref a j j) tiny))
	(when (/= j n)
	  (setf dum (/ 1.0 (aref a j j)))
	  (loop for i from (1+ j) to n do
	    (setf (aref a i j) (* dum (aref a i j)))))))
    (values indx d)))

(defun lubksb (a n indx b)
  (let ((ii 0))
    (loop for i from 1 to n do
      (let* ((ip (aref indx i))
	     (sum (aref b ip)))
	(setf (aref b ip) (aref b i))
	(if (/= ii 0)
	    (loop for j from ii to (1- i) do
	      (decf sum (* (aref a i j) (aref b j))))
	  (if (/= sum 0.0)
	      (setf ii i)))
	(setf (aref b i) sum)))
    (loop for i from n downto 1 do
      (let ((sum (aref b i)))
	(loop for j from (1+ i) to n do
	  (decf sum (* (aref a i j) (aref b j))))
	(setf (aref b i) (/ sum (aref a i i)))))))

;;;; ------------------------------------------------------------------------------------------
;;;;                    Interpolation
;;;;
;;;;

(defun polint (xa ya x)			;given arrays xa and ya, and x, return y and error  
estimate dy
  (let* ((n (array-dimension xa 0))
	 (ns 1)
	 (y 0.0) (dy 0.0)
	 (dif (abs (- x (aref xa 1))))
	 (c (make-array n :element-type 'float :initial-element 0.0))
	 (d (make-array n :element-type 'float :initial-element 0.0))
	 (w 0.0) (hp 0.0) (ho 0.0) (dift 0.0) (den 0.0))
    (do ((i 1 (+ i 1)))
	((> i n))
      (setf dift (abs (- x (aref xa i))))
      (cond ((< dift dif)
	     (setf ns i)
	     (setf dif dift)))
      (setf (aref c i) (aref ya i))
      (setf (aref d i) (aref ya i)))
    (setf y (aref ya ns))		;initial approximation of y
    (decf ns)
    (do ((m 1 (+ m 1)))
	((= m n) (values y dy))
      (do ((i 1 (+ i 1)))
	  ((> i (- n m)))
	(setf ho (- (aref xa i) x))
	(setf hp (- (aref xa (+ i m)) x))
	(setf w (- (aref c (+ i 1)) (aref d i)))
	(setf den (- ho hp))
	(if (zerop den) (error "input xa's are identical to within roundoff!"))
	(setf den (/ w den))
	(setf (aref d i) (* hp den))
	(setf (aref c i) (* ho den)))
      (cond ((< (* 2 ns) (- n m))
	     (setf dy (aref c (+ ns 1))))
	    (t (setf dy (aref d ns))
	       (decf ns)))
      (incf y dy))))
	 


(defun ratint (xa ya x)			;same as polint, but uses rational function
					;assume arrays are [1..n]
  (let* ((n (- (array-dimension xa 0) 1))
	 (tiny 1.0e-25)
	 (c (make-array (+ n 1) :element-type 'float :initial-element 0.0))
	 (d (make-array (+ n 1) :element-type 'float :initial-element 0.0))
	 (ns 1)
	 (y 0.0) (dy 0.0)
	 (hh (abs (- x (aref xa 1))))
	 (w 0.0) (tt 0.0) (h 0.0) (dd 0.0))
    (loop for i from 1 to n do
      (setf h (abs (- x (aref xa i))))
      (if (zerop h) (return (values (aref ya i) 0.0)))
      (cond ((< h hh)
	     (setf ns i)
	     (setf hh h)))
      (setf (aref c i) (aref ya i))
      (setf (aref d i) (+ (aref ya i) tiny)))
    (setf y (aref ya ns))
    (decf ns)
    (loop for m from 1 to (- n 1) do
      (loop for i from 1 to (- n m) do
	(setf w (- (aref c (+ i 1)) (aref d i)))
	(setf h (- (aref xa (+ i m)) x))
	(setf tt (/ (* (- (aref xa i) x) (aref d i)) h))
	(setf dd (- tt (aref c (+ i 1))))
	(if (zerop dd) (error "RatInt hit a pole at ~F!" x))
	(setf dd (/ w dd))
	(setf (aref d i) (* (aref c (+ i 1)) dd))
	(setf (aref c i) (* tt dd)))
      (cond ((< (* 2 ns) (- n m))
	     (setf dy (aref c (+ ns 1))))
	    (t (setf dy (aref d ns))
	       (decf ns)))
      (incf y dy))
    (values y dy)))



;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;                    Integration
;;;;


(defvar TrapzdIt 0)

(defun trapzd (a b s n f)
  (cond ((= n 1)
	 (setf trapzdIt 1)
	 (* 0.5 (- b a) (+ (funcall f a) (funcall f b))))
	(t (let* ((tnm trapzdIt)
		  (del (/ (- b a) tnm))
		  (x (+ a (* 0.5 del)))
		  (sum 0.0))
	     (loop for j from 1 to trapzdIt do
	       (incf sum (funcall f x))
	       (incf x del))
	     (setf trapzdIt (* 2 trapzdIt))
	     (* 0.5 (+ s (* (- b a) (/ sum tnm))))))))

(defun qtrap (a b f &optional (eps 1.0e-6) (jmax 20))
  (let ((olds -1.0e30)
	(curs 0.0))
    (loop for j from 1 to jmax do
      (setf curs (trapzd a b curs j f))
      (if (< (abs (- curs olds)) (* eps (abs olds))) 

	  (return curs))
      (setf olds curs))
    curs))

(defun qsimp (a b f &optional (eps 1.0e-6) (jmax 20))
  (let ((st 0.0)
	(ost -1.0e30)
	(os -1.0e30)
	(curs 0.0))
    (loop for j from 1 to jmax do
      (setf st (trapzd a b st j f))
      (setf curs (/ (- (* 4.0 st) ost) 3.0))
      (if (< (abs (- curs os)) (* eps (abs os)))
	  (return curs))
      (setf os curs)
      (setf ost st))
    curs))

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;                    Evaluation
;;;;


(defun poldiv (u n v nv q r)
  (progn
    (loop for j from 1 to n do
      (setf (aref r j) (aref u j))
      (setf (aref q j) 0.0))
    (loop for k from (- n nv) downto 0 do
      (setf (aref q (+ k 1)) (/ (aref r (+ nv k)) (aref v nv)))
      (loop for j from (+ nv k -1) downto (+ k 1) do
	(decf (aref r j) (* (aref q (+ k 1)) (aref v (- j k))))))
    (loop for j from nv to n do
      (setf (aref r j) 0.0))))
      

;; polmul is something like this:
(defun polmul (a1 a2 pol)
  (let ((a1-max (array-total-size a1))
	(a2-max (array-total-size a2))
	(order (max a1-max a2-max)))
    (loop for i from 1 to max-a1 do
      (loop for j from 1 to max-a2 do
	(incf (aref pol (+ i j -1))
	      (* (aref a1 i) (aref a2 j)))))))



;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;                    Special Functions
;;;;



;;;;                    Bessel functions
;;;;
;;;;

(defun bessj0 (x)			;returns J0(x) for any real x
  (if (< (abs x) 8.0)			;direct rational function fit
      (let* ((y (expt x 2))
	     (ans1 (+ 57568490574.0
		      (* y (+ -13362590354.0 

			      (* y  (+ 651619640.7
				       (* y (+ -11214424.18 

					       (* y (+ 77392.33017
						       (* y -184.9052456)))))))))))
	     (ans2 (+ 57568490411.0 

		      (* y (+ 1029532985.0 

			      (* y (+ 9494680.718
				      (* y (+ 59272.64853
					      (* y (+ 267.8532712 y)))))))))))
	(/ ans1 ans2))
    (let* ((ax (abs x))
	   (z (/ 8.0 ax))
	   (y (expt z 2))
	   (xx (- ax 0.785398164))
	   (ans1 (+ 1.0 

		    (* y (+ -0.1098628627e-2 

			    (* y (+ 0.2734510407e-4
				    (* y (+ -0.2073370639e-5
					    (* y 0.2093887211e-6)))))))))
	   (ans2 (+ -0.1562499995e-1
		    (* y (+ 0.1430488765e-3
			    (* y (+ -0.6911147651e-5
				    (* y (+ 0.7621095161e-6
					    (* y -0.934945152e-7))))))))))
      (* (sqrt (/ 0.636619772 ax))
	 (- (* (cos xx) ans1)
	    (* z (sin xx) ans2))))))


(defun bessj1 (x)			;returns J1(x) for any real x
  (if (< (abs x) 8.0)
      (let* ((y (expt x 2))
	     (ans1 (* x 

		      (+ 72362614232.0
			 (* y (+ -7895059235.0
				 (* y (+ 242396853.1
					 (* y (+ -2972611.439
						 (* y (+ 15704.48260
							 (* y -30.16036606))))))))))))
	     (ans2 (+ 144725228442.0 

		      (* y (+ 2300535178.0 

			      (* y (+ 18583304.74
				      (* y (+ 99447.43394
					      (* y (+ 376.9991397 y)))))))))))
	(/ ans1 ans2))
    (let* ((ax (abs x))
	   (z (/ 8.0 ax))
	   (y (expt z 2))
	   (xx (- ax 2.356194491))
	   (ans1 (+ 1.0
		    (* y (+ 0.183105e-2
			    (* y (+ -0.3516396496e-4
				    (* y (+ 0.2457520174e-5
					    (* y -0.240337019e-6)))))))))
	   (ans2 (+ 0.04687499995
		    (* y (+ -0.2002690873e-3
			    (* y (+ 0.8449199096e-5
				    (* y (+ -0.88228987e-6
					    (* y 0.105787412e-6))))))))))
      (* (signum x)
	 (sqrt (/ 0.636619772 ax))
	 (- (* (cos xx) ans1)
	    (* z (sin xx) ans2))))))


(defun bessj (n x)			;returns Jn(x) for any x and n>=2
  (if (zerop x) 0.0
    (let ((iacc 40)			;make iacc larger to increase accuracy
	  (ans 0.0)
          (bigno 1.0e10)
          (bigni 1.0e-10))
      (if (> (abs x) n)			;can use upward recurrence from J0 and J1
	  (do ((tox (/ 2.0 (abs x)))
	       (bjm (bessj0 (abs x)))
	       (bj (bessj1 (abs x)))
	       (j 1 (+ j 1))
	       (bjp 0.0))
	      ((= j n) (setf ans bj))
	    (setf bjp (- (* j tox bj) bjm))
	    (setf bjm bj)
	    (setf bj bjp))
	(let ((tox (/ 2.0 (abs x)))	;else use downward recurrence from even value (m)
	      (m (* 2 (floor		;DIV in Pascal, we hope...
		       (+ n (sqrt (* iacc n))) 2)))
	      (jsum 0.0)		;alternate 0 and 1 -- when 1, accumulate even terms in  
sum
	      (bjm 0.0)
	      (sum 0.0)
	      (bjp 0.0)
	      (bj 1.0))
	  (do ((j m (- j 1)))		;the downward recurrence
	      ((= j 0))
	    (setf bjm (- (* j tox bj) bjp))
	    (setf bjp bj)
	    (setf bj bjm)
	    (cond ((> (abs bj) bigno)	;renormalize (may not be necessary in common lisp)
		   (setf bj (* bj bigni))
		   (setf bjp (* bjp bigni))
		   (setf ans (* ans bigni))
		   (setf sum (* sum bigni))))
	    (if (/= 0 jsum) (incf sum bj))
	    (setf jsum (- 1 jsum))
	    (if (= j n) (setf ans bjp)))
	  (setf sum (- (* 2.0 sum) bj))
	  (setf ans (/ ans sum))))
      (if (and (minusp x) (oddp n)) (- ans) ans))))
	
 

(defun Bessel-Jn (nn x)				;return Jn(x) for any integer n, real x
  (let* ((n (floor (abs nn)))
	 (besn (if (zerop n) (bessj0 x)
		 (if (= n 1) (bessj1 x)
		   (bessj n x)))))
    (if (and (minusp nn)
	     (oddp nn))
	(- besn)
      besn)))
;; Numerical Recipes version gets infinite loop on negative orders
;; so here we use J-n(B)=(-1)^n Jn(B)

(defun bessy0 (x)			;Bessel function Y0(x)
  (if (< x 8.0)
      (let* ((y (expt x 2))
	     (ans1 (+ -2957821389.0 

		      (* y (+ 7062834065.0
			      (* y (+ -512359803.6
				      (* y (+ 10879881.29
					      (* y (+ -86327.92757
						      (* y 228.4622733)))))))))))
	     (ans2 (+ 40076544269.0
		      (* y (+ 745249964.8
			      (* y (+ 7189466.438
				      (* y (+ 47447.26470
					      (* y (+ 226.1030244 y)))))))))))
	(+ (/ ans1 ans2) (* 0.636619772 (bessj0 x) (log x))))
    (let* ((z (/ 8.0 x))
	   (y (expt z 2))
	   (xx (- x 0.785398164))
	   (ans1 (+ 1.0
		    (* y (+ -0.1098628627e-2
			    (* y (+ 0.2734510407e-4
				    (* y (+ -0.2073370639e-5
					    (* y 0.2093887211e-6)))))))))
	   (ans2 (+ -0.1562499995e-1
		    (* y (+ 0.1430488765e-3
			    (* y (+ -0.6911147651e-5
				    (* y (+ 0.7621095161e-6
					    (* y -0.934945152e-7)))))))))
	   (ans (+ (* (sin xx) ans1) (* z (cos xx) ans2))))
      (* (sqrt (/ 0.636619772 x)) ans))))
	     


(defun bessy1 (x)			;Bessel function Y1(x)
  (if (< x 8.0)
      (let* ((y (expt x 2))
	     (ans1 (* x (+ -0.4900604943e13
			   (* y (+ 0.1275274390e13
				   (* y (+ -0.5153438139e11
					   (* y (+ 0.7349264551e9
						   (* y (+ -0.4237922726e7
							   (* y 0.8511937935e4))))))))))))
	     (ans2 (+ 0.2499580570e14
		      (* y (+ 0.4244419664e12
			      (* y (+ 0.3733650367e10
				      (* y (+ 0.2245904002e8
					      (* y (+ 0.1020426050e6
						      (* y (+ 0.3549632885e3 y)))))))))))))
	(+ (/ ans1 ans2) (* 0.636619772 (- (* (bessj1 x) (log x)) (/ 1.0 x)))))
    (let* ((z (/ 8.0 x))
	   (y (expt z 2))
	   (xx (- x 2.356194491))
	   (ans1 (+ 1.0 

		    (* y (+ 0.183105e-2
			    (* y (+ -0.3516396496e-4
				    (* y (+ 0.2457520174e-5
					    (* y -0.240337019e-6)))))))))
	   (ans2 (+ 0.04687499995
		    (* y (+ -0.200269087e-3
			    (* y (+ 0.8449199096e-5
				    (* y (+ -0.88228987e-6
					    (* y 0.105787412e-6))))))))))
      (* (sqrt (/ 0.636619772 x)) (+ (* (sin xx) ans1) (* z (cos xx) ans2))))))
	     

						

(defun bessy (n x)			;Yn(x) for n>=2
  (do* ((tox (/ 2.0 x))
	(byp 0.0)
	(by (bessy1 x))
	(bym (bessy0 x))
	(j 1 (+ j 1)))
      ((= j n) by)
    (setf byp (- (* j tox by) bym))
    (setf bym by)
    (setf by byp)))



(defun Bessel-Yn (n x)			;return Yn(x) for any integer n, real x
  (progn
    (cond ((not (integerp n)) 

	   (cerror "Trucate ~F."
		   "Yn handles only integer order, n=~F."
		   n n)
	   (setf n (truncate n))))
    (if (zerop n) (bessy0 x)
      (if (= n 1) (bessy1 x)
	(bessy n x)))))
		

(defun bessi0 (x)			;I0(x)
  (if (< (abs x) 3.75)
      (let* ((y (expt (/ x 3.75) 2)))
	(+ 1.0
	   (* y (+ 3.5156229
		   (* y (+ 3.0899424
			   (* y (+ 1.2067492
				   (* y (+ 0.2659732
					   (* y (+ 0.360768e-1
						   (* y 0.45813e-2)))))))))))))
    (let* ((ax (abs x))
	   (y (/ 3.75 ax)))
      (* (/ (exp ax) (sqrt ax)) 

	 (+ 0.39894228
	    (* y (+ 0.1328592e-1
		    (* y (+ 0.225319e-2
			    (* y (+ -0.157565e-2
				    (* y (+ 0.916281e-2
					    (* y (+ -0.2057706e-1
						    (* y (+ 0.2635537e-1
							    (* y (+ -0.1647633e-1
								    (* y  
0.392377e-2))))))))))))))))))))

(defun bessi1 (x)			;I1(x)
  (if (< (abs x) 3.75)
      (let* ((y (expt (/ x 3.75) 2)))
	(* x (+ 0.5
		(* y (+ 0.87890594
			(* y (+ 0.51498869
				(* y (+ 0.15084934
					(* y (+ 0.2658733e-1
						(* y (+ 0.301532e-2
							(* y 0.32411e-3))))))))))))))
    (let* ((ax (abs x))
	   (y (/ 3.75 ax))
	   (ans1 (+ 0.2282967e-1
		    (* y (+ -0.2895312e-1
			    (* y (+ 0.1787654e-1 

				    (* y -0.420059e-2)))))))
	   (ans2 (+ 0.39894228
		    (* y (+ -0.3988024e-1
			    (* y (+ -0.362018e-2
				    (* y (+ 0.163801e-2
					    (* y (+ -0.1031555e-1 (* y ans1)))))))))))
	   (sign (if (minusp x) -1.0 1.0)))
      (* (/ (exp ax) (sqrt ax)) ans2 sign))))


(defun bessi (n x)			;In(x) for n>=2
  (if (zerop x) 0.0
    (let* ((iacc 40)
	   (bigno 1.0e10)
	   (bigni 1.0e-10)
	   (ans 0.0)
	   (tox (/ 2.0 (abs x)))
	   (bip 0.0)
	   (bi 1.0)
	   (m (* 2 (+ n (truncate (sqrt (* iacc n))))))
	   (bim 0.0))
      (loop for j from m downto 1 do
	(setf bim (+ bip (* j tox bi)))
	(setf bip bi)
	(setf bi bim)
	(cond ((> (abs bi) bigno)
	       (setf ans (* ans bigni))
	       (setf bi (* bi bigni))
	       (setf bip (* bip bigni))))
	(if (= j n) (setf ans bip)))
      (if (and (minusp x) (oddp n)) (setf ans (- ans)))
      (* ans (/ (bessi0 x) bi)))))


(defun Bessel-In (n x)			;return In(x) for any integer n, real x
  (progn
    (cond ((not (integerp n)) 

	   (cerror "Trucate ~F."
		   "Bessel-In handles only integer order, n=~F."
		   n n)
	   (setf n (truncate n))))
    (if (zerop n) (bessi0 x)
      (if (= n 1) (bessi1 x)
	(bessi n x)))))
		

(defun bessk0 (x)			;K0(x)
  (if (<= x 2.0)
      (let* ((y (* x (/ x 4.0))))
	(+ (* (- (log (/ x 2.0))) (bessi0 x)) -0.57721566
	   (* y (+ 0.42278420
		   (* y (+ 0.23069756
			   (* y (+ 0.3488590e-1
				   (* y (+ 0.262698e-2
					   (* y (+ 0.10750e-3
						   (* y 0.74e-5)))))))))))))
    (let* ((y (/ 2.0 x)))
      (* (/ (exp (- x)) (sqrt x)) 

	 (+ 1.25331414
	    (* y (+ -0.7832358e-1
		    (* y (+ 0.2189568e-1
			    (* y (+ -0.1062446e-1
				    (* y (+ 0.587872e-2
					    (* y (+ -0.251540e-2
						    (* y -0.53208e-3))))))))))))))))


(defun bessk1 (x)			;K1(x)
  (if (<= x 2.0)
      (let* ((y (* x (/ x 4.0))))
	(+ (* (log (/ x 2)) (bessi1 x)) 

	   (* (/ 1.0 x)
	      (+ 1.0
		 (* y (+ 0.15443144
			 (* y (+ -0.67278579
				 (* y (+ -0.18156897
					 (* y (+ -0.1919402e-1
						 (* y (+ -0.110404e-2
							 (* y -0.4686e-4)))))))))))))))
    (let* ((y (/ 2.0 x)))
      (* (/ (exp (- x)) (sqrt x)) 

	 (+ 1.25331414 

	    (* y (+ 0.23498619
		    (* y (+ -0.3655620e-1
			    (* y (+ 0.1504268e-1
				    (* y (+ -0.780353e-2
					    (* y (+ 0.325614e-2
						    (* y -0.68245e-3))))))))))))))))


(defun bessk (n x)			;Kn(x) for n>=2
  (do ((tox (/ 2.0 x))
       (bkm (bessk0 x))
       (bk (bessk1 x))
       (bkp 0.0)
       (j 1 (+ j 1)))
      ((= j n) bk)
    (setf bkp (+ bkm (* j tox bk)))
    (setf bkm bk)
    (setf bk bkp)))


(defun Bessel-Kn (n x)			;return Kn(x) for any integer n, real x
  (progn
    (cond ((not (integerp n)) 

	   (cerror "Trucate ~F."
		   "Bessel-Kn handles only integer order, n=~F."
		   n n)
	   (setf n (truncate n))))
    (if (zerop n) (bessk0 x)
      (if (= n 1) (bessk1 x)
	(bessk n x)))))
		
				    

;;;;
;;;;                            Gamma function, Beta function
;;;;
;;;;

(defun gammln (xx)			;Ln(gamma(xx)), xx>0 

					;for full accuracy between 0 and 1, use reflection  
formula first
  (let* ((stp 2.50662827465)
	 (x (- xx 1.0))
	 (tmp (+ x 5.5))
	 (tmp1 (- (* (+ x 0.5) (log tmp)) tmp))
	 (ser (+ 1.0 

		 (/ 76.18009173 (+ x 1.0))
		 (/ -86.50532033 (+ x 2.0))
		 (/ 24.01409822 (+ x 3.0))
		 (/ -1.231739516 (+ x 4))
		 (/ 0.120858003e-2 (+ x 5.0))
		 (/ -0.536382e-5 (+ x 6.0)))))
    (+ tmp1 (log (* stp ser)))))

(defun gamma (x)			;gamma(x)
  (if (plusp x)
      (if (>= x 1.0)
	  (exp (gammln x))
	(/ (exp (gammln (+ x 1.0))) x))
    (/ (* pi (exp (- (gammln (- 1.0 x))))) (sin (* pi (- 1.0 x))))))



(defun beta (z w)			;B(z,w)
  (exp (- (+ (gammln z) (gammln w)) (gammln (+ z w)))))


(defvar FactrlNtop 0)
(defvar FactrlA (make-array 34 :element-type 'float :initial-element 1.0))

(defun factrl(n)			;n! as real
  (if (or (not (integerp n)) (minusp n))
      (error "Invalid argument to factorial: ~S" n)
    (if (<= n FactrlNtop)		;n! is already in our table
	(aref FactrlA (+ n 1))
      (cond ((<= n 32)
	     (do ((j (+ FactrlNtop 1) (+ j 1)))
		 ((> j n))
	       (setf (aref FactrlA (+ j 1)) (* j (aref FactrlA j))))
	     (setf FactrlNtop n)
	     (aref FactrlA (+ n 1)))
	    (t (exp (gammln (+ n 1.0))))))))


(defvar FactlnA (make-array 101 :element-type 'float :initial-element -1.0))

(defun factln (n)			; return Ln(n!)
  (if (minusp n)
      (error "Invalid argument to factLn: ~S" n)
    (cond ((<= n 99)
	   (if (minusp (aref FactlnA (+ n 1)))
	       (setf (aref FactlnA (+ n 1)) (gammln (+ n 1.0))))
	   (aref FactlnA (+ n 1)))
	  (t (gammln (+ n 1.0))))))
	     


(defun bico (n k)			;return binomial coefficient (n k)
  (round (exp (- (factln n) (factln k) (factln (- n k))))))


(defun gser (a x)			;P(a,x) evaluated as series, also Ln(gamma)
  (if (minusp x) (error "~F is less than 0" x)
    (if (zerop x) 0.0
      (do* ((gln (gammln a))
	    (itmax 100)
	    (eps 3.0e-7)
	    (ap a)
	    (sum (/ 1.0 a))
	    (del sum)
	    (n 1 (+ n 1)))
	  ((or (> n itmax)
	       (< (abs del) (* (abs sum) eps)))
	   (values (* sum (exp (+ (- x) (* a (log x)) (- gln)))) gln))
	(incf ap)
	(setf del (* del (/ x ap)))
	(incf sum del)))))


(defun gcf (a x)			;Q(a,x) evaluated as continued fraction
  (do* ((itmax 100)
	(eps 3.0e-7)
	(gln (gammln a))
	(gold 0.0)			;previous value of g, tested for convergence
	(a0 1.0)
	(a1 x)
	(b0 0.0)
	(b1 1.0)			;setting up continued fraction
	(fac 1.0)
	(n 1 (+ n 1))
	(ana 0.0) (g 0.0) (anf 0.0))
      ((> n itmax) 

       (values (* g (exp (+ (- x) (* a (log x)) (- gln)))) gln))
    (setf ana (- n a))
    (setf a0 (* fac (+ a1 (* a0 ana))))
    (setf b0 (* fac (+ b1 (* b0 ana))))
    (setf anf (* fac n))
    (setf a1 (+ (* x a0) (* anf a1)))
    (setf b1 (+ (* x b0) (* anf b1)))
    (cond ((/= 0.0 a1)			;renormalize?
	   (setf fac (/ 1.0 a1))
	   (setf g (* b1 fac))
	   (if (< (abs (/ (- g gold) g)) eps)
	       (return (values (* g (exp (+ (- x) (* a (log x)) (- gln)))) gln)))))))


(defun gammp (a x)			;returns incomplete gamma function P(a,x)
  (if (or (<= a 0.0) (minusp x))
      (error "Invalid argument to gammp: ~F" (if (<= 0.0 a) a x))
    (if (< x (+ a 1.0))
	(gser a x)			;use series
      (- 1.0 (gcf a x)))))		;use continued fraction


(defun gammq (a x)			;incomplete gamma function Q(a,x) = 1 - P(a,x)
  (- 1.0 (gammp a x)))			;Numerical Recipes repeats the GAMMP procedure code --  
kinda odd.


(defun erf (x)				;returns error function erf(x)
  (if (minusp x)
      (- (gammp 0.5 (expt x 2)))
    (gammp 0.5 (expt x 2))))


(defun erfc (x)				;complementary error function erfc(x)
  (if (minusp x)
      (+ 1.0 (gammp 0.5 (expt x 2)))
    (gammq 0.5 (expt x 2))))


(defun betacf (a b x)			;continued fraction for incomplete beta function
  (do* ((itmax 100)
	(eps 3.0e-7)
	(am 1.0) (bm 1.0) (az 1.0)
	(tem 0.0) (em 0.0) (d 0.0)
	(bpp 0.0) (bp 0.0) (app 0.0)
	(aold 0.0) (ap 0.0)
	(m 1 (+ m 1))
	(qab (+ a b))
	(qap (+ a 1.0))
	(qam (- a 1.0))
	(bz (- 1.0 (/ (* qab x) qap))))
      ((> m itmax) az)
    (setf em m)
    (setf tem (+ em em))
    (setf d (* em (- b m) (/ x (* (+ qam tem) (+ a tem)))))
    (setf ap (+ az (* d am)))
    (setf bp (+ bz (* d bm)))
    (setf d (* (- (+ a em)) (+ qab em) (/ x (* (+ a tem) (+ qap tem)))))
    (setf app (+ ap (* d az)))
    (setf bpp (+ bp (* d bz)))
    (setf aold az)
    (setf am (/ ap bpp))
    (setf bm (/ bp bpp))
    (setf az (/ app bpp))
    (setf bz 1.0)
    (if (< (abs (- az aold)) (* eps (abs az)))
	(return az))))


(defun betai (a b x)			;incomplete beta function Ix(a,b)
  (if (or (minusp x) (> x 1.0))
      (error "x is out of bounds: ~F" x)
    (let ((bt (if (or (zerop x) (= x 1.0)) 0.0
		(exp (+ (gammln (+ a b))
			(- (gammln a))
			(- (gammln b))
			(* a (log x))
			(* b (log (- 1.0 x))))))))
      (if (< x (/ (+ a 1.0) (+ a b 2.0)))
	  (* bt (/ (betacf a b x) a))
	(- 1.0 (* bt (/ (betacf b a (- 1.0 x)) b)))))))


(defun Student-distribution (t1 n)
  (- 1.0 (betai (/ n 2) 0.5 (/ n (+ n (* t1 t1))))))

(defun F-distribution (F n1 n2)
  (betai (/ n2 2) (/ n1 2) (/ n2 (+ n2 (* n1 F)))))

;;;;
;;;;                            Spherical Harmonics
;;;;
;;;;

(defun plgndr (l m x)			;Legendre polynomial P m/l (x), m and l integer
					;0 <= m <= l and -1<= x <= 1 (x real)
  (if (or (minusp m) (> m l) (> (abs x) 1.0))
      (error "Invalid arguments to plgndr")
    (let ((pmm 1.0)
	  (fact 0.0) 

	  (somx2 0.0))
      (cond ((plusp m)
	     (setf somx2 (sqrt (* (- 1.0 x) (+ 1.0 x))))
	     (setf fact 1.0)
	     (loop for i from 1 to m do
	       (setf pmm (* (- pmm) fact somx2))
	       (incf fact 2.0))))
      (if (= l m) pmm
	(let ((pmmp1 (* x pmm (+ (* 2 m) 1))))
	  (if (= l (+ m 1)) pmmp1
	    (let ((pll 0.0))
	      (loop for ll from (+ m 2) to l do
		(setf pll (/ (- (* x (- (* 2 ll) 1) pmmp1) (* (+ ll m -1) pmm)) (- ll m)))
		(setf pmm pmmp1)
		(setf pmmp1 pll))
	      pll)))))))
	       


;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;                    Random Numbers
;;;;

(defvar ran3Inext 0)
(defvar ran3Inextp 0)
(defvar ran3Ma (make-array 56 :element-type 'float :initial-element 0.0))
(defvar rseed 0)

(defun ran3 (idum)			;uniform random deviate between 0.0 amd 1.0
					;set idum to -1 to re-initialize the sequence
					;taken from Knuth, Seminumerical Alogrithms
  (progn				;original uses a VAR (reference) parameter, hence rseed  
here
    (if (minusp idum) (setf rseed idum))
    (let ((mbig 4.0e6)
	  (mseed 1618033.0)
	  (fac 2.5e-7)
	  (mj 0.0) (mk 0.0) (ii 0))
      (cond ((minusp rseed)		;signals initialization
	     (setf mj (+ mseed rseed))	;initialize Ma[55] -- the 55 is magic!
	     (if (>= mj 0.0)
		 (decf mj (* mbig (truncate (/ mj mbig))))
	       (setf mj (+ (- mbig (abs mj)) (* mbig (floor (abs mj) mbig)))))
	       ;; that is, mj MOD mbig for real variables
	     (setf (aref ran3Ma 55) mj)
	     (setf mk 1.0)
	     (do ((i 1 (+ i 1)))
		 ((= i 55))
	       (setf ii (mod (* 21 i) 55))
	       (setf (aref ran3Ma ii) mk)
	       (setf mk (- mj mk))
	       (if (minusp mk) (incf mk mbig))
	       (setf mj (aref ran3Ma ii)))
	     (do ((k 1 (+ k 1)))	;initialize table with not-very-random numbers
		 ((= k 5))
	       (do ((i 1 (+ i 1)))
		   ((= i 56))
		 (decf (aref ran3Ma i) (aref ran3Ma (+ 1 (mod (+ i 30) 55))))
		 (if (minusp (aref ran3Ma i))
		     (incf (aref ran3Ma i) mbig))))
	     (setf ran3Inext 0)		;prepare indices for first generated number
	     (setf ran3Inextp 31)	;31 is magic -- see Knuth
	     (setf rseed 1)))		;end of intialization
      (incf ran3Inext)
      (if (= ran3Inext 56) (setf ran3Inext 1))
      (incf ran3Inextp)
      (if (= ran3Inextp 56) (setf ran3Inextp 1))
      (setf mj (- (aref ran3Ma ran3Inext) (aref ran3Ma ran3Inextp)))
      (if (minusp mj) (incf mj mbig))
      (setf (aref ran3Ma ran3Inext) mj)
      (* mj fac))))


(defun expdev (idum)			;exponentially distributed positive random deviate
					;of unit mean, using ran3 for uniform deviates
  (let ((dum 0.0))
    (loop while (zerop dum) do (setf dum (ran3 idum)))
    (- (log dum))))


(defvar GasdevIset 0)
(defvar GasdevGset 0.0)

(defun gasdev (idum)			;gaussian noise, zero mean, unit variance, uses ran3
  (cond ((zerop GasdevIset)		;no extra number handy -- go calulate two and save one  
for next time
	 (let ((v1 0.0) (v2 0.0) (r 0.0) (fac 0.0))
	   (do ()
	       ((< 0.0 r 1.0))
	     (setf v1 (- (* 2.0 (ran3 idum)) 1.0))
	     (setf v2 (- (* 2.0 (ran3 0)) 1.0))
	     ;; ran3(0) because we don't have reference args 

	     (setf r (+ (expt v1 2) (expt v2 2))))
	   (setf fac (sqrt (/ (* -2.0 (log r)) r)))
	   (setf GasdevGset (* v1 fac))
	   (setf GasdevIset 1)
	   (* v2 fac)))
	(t (setf GasdevIset 0)
	   GasdevGset)))
		
;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;                    Sorting
;;;;


;;;;                           Heapsort and Quicksort
;;;;

;;; the following procedures assume that arrays are 0-based, not 1-based as in the Recipes book

(defun locate (xx x)			;given array xx, searches for location of x
  (do* ((jm 0)
	(n (array-dimension xx 0))
	(jl -1)				;lower limit
	(ju n)				;upper limit
	(ascnd (> (aref xx (- n 1)) (aref xx 0))))
      ((<= (- ju jl) 1) jl)
    (setf jm (floor (+ ju jl) 2))	;midpoint
    (if (eql ascnd (> x (aref xx jm)))
	(setf jl jm)
      (setf ju jm))))


(defun indexx (arrin indx)		;make an index into ARRIN by ascending order
  (let ((n (array-dimension arrin 0)))
    (loop for i below n do (setf (aref indx i) i))
    (if (not (zerop n))
	(do* ((l (floor n 2))
	      (indxt 0) (q 0.0)
	      (ir (- n 1)))
	    (nil)			;WHILE TRUE DO...
	  (cond ((plusp l)
		 (decf l)
		 (setf indxt (aref indx l))
		 (setf q (aref arrin indxt)))
		(t (setf indxt (aref indx ir))
		   (setf q (aref arrin indxt))
		   (setf (aref indx ir) (aref indx 0))
		   (decf ir)
		   (if (zerop ir) (return (setf (aref indx 0) indxt)))))
	  (do ((i l)
	       (j (+ l 1)))
	      ((> j ir) (setf (aref indx i) indxt))
	    (if (and (< j ir)
		     (< (aref arrin (aref indx j)) (aref arrin (aref indx (+ j 1)))))
		(incf j))
	    (cond ((< q (aref arrin (aref indx j)))
		   (setf (aref indx i) (aref indx j))
		   (setf i j)
		   (incf j j))
		  (t (setf j (+ ir 1)))))))))


(defun rank (indx irank)
  (do ((n (array-dimension indx 0))
       (j 0 (+ j 1)))
      ((= j n))
    (setf (aref irank (aref indx j)) j)))
	  

;;;; ------------------------------------------------------------------------------------------
;;;;                    Root Finding

(defun sqr (x) (* x x))

(defun laguer (a m x eps polish)
  (let ((epss 6.0e-8)
	(mixt 100)
	(dxold (abs x)))
    (loop for iter from 1 to mixt do
      (let* ((b (aref a (1+ m)))
	     (err (abs b))
	     (d #C(0 0))
	     (f #C(0 0))
	     (g #C(0 0))
	     (g2 #C(0 0))
	     (gp #C(0 0))
	     (gm #C(0 0))
	     (h #C(0 0))
	     (cdum #C(0 0))
	     (sq #C(0 0))
	     (dx #C(0 0))
	     (cdx #C(0 0))
	     (x1 #C(0 0))
	     (abx (abs x)))
	(loop for j from m downto 1 do
	  (let ((dum (realpart f)))
	    (setf f (complex (+ (* (realpart x) (realpart f))
				(realpart d)
				(- (* (imagpart x) (imagpart f))))
			     (+ (* (realpart x) (imagpart f))
				(imagpart d)
				(* (imagpart x) dum))))
	    (setf dum (realpart d))
	    (setf d (complex (+ (* (realpart x) (realpart d))
				(realpart b)
				(- (* (imagpart x) (imagpart d))))
			     (+ (* (realpart x) (imagpart d))
				(imagpart b)
				(* (imagpart x) dum))))
	    (setf dum (realpart b))
	    (setf b (complex (+ (* (realpart x) (realpart b))
				(realpart (aref a j))
				(- (* (imagpart x) (imagpart b))))
			     (+ (* (realpart x) (imagpart b))
				(imagpart (aref a j))
				(* (imagpart x) dum))))
	    (setf err (+ (abs b) (* abx err)))))
	(setf err (* epss err))
	(if (<= (abs b) err) (return-from laguer x))
	(setf g (/ d b))
	(setf g2 (complex (- (sqr (realpart g)) (sqr (imagpart g)))
			  (* 2.0 (realpart g) (imagpart g))))
	(setf cdum (/ f b))
	(setf h (complex (- (realpart g2) (* 2.0 (realpart cdum)))
			 (- (imagpart g2) (* 2.0 (imagpart cdum)))))
	(setf cdum (complex (* (1- m) (- (* m (realpart h)) (realpart g2)))
			    (* (1- m) (- (* m (imagpart h)) (imagpart g2)))))
	(setf sq (sqrt cdum))
	(setf gp (+ g sq))
	(setf gm (- g sq))
	(if (< (abs gp) (abs gm)) (setf gp gm))
	(setf cdum (complex m 0))
	(setf dx (/ cdum gp))
	(setf x1 (- x dx))
	(if (= x x1) (return-from laguer x))
	(setf x x1)
	(setf cdx (abs dx))
	(setf dxold cdx)
	(if (and (not polish)
		 (<= cdx (* eps (abs x))))
	    (return-from laguer x))))))
	
(defun zroots (a m roots polish)
  (let ((eps 2.0e-6)
	(ad (make-array (+ m 2) :element-type 'complex))
	(b #C(0 0))
	(c #C(0 0))
	(dum 0.0))
    (loop for j from 1 to (1+ m) do
      (setf (aref ad j) (aref a j)))
    (loop for j from m downto 1 do
      (let ((x (laguer ad j #C(0 0) eps nil)))
	(if (<= (abs (imagpart x))
		(* 2.0 (sqr eps) (abs (realpart x))))
	    (setf x (complex (realpart x) 0.0)))
	(setf (aref roots j) x)
	(setf b (aref ad (1+ j)))
	(loop for jj from j downto 1 do
	  (setf c (aref ad jj))
	  (setf (aref ad jj) b)
	  (setf dum (realpart b))
	  (setf b (complex (+ (* (realpart b) (realpart x))
			      (realpart c)
			      (- (* (imagpart b) (imagpart x))))
			   (+ (* dum (imagpart x))
			      (imagpart c)
			      (* (imagpart b) (realpart x))))))))
    (if polish
	(loop for j from 1 to m do
	  (setf (aref roots j) (laguer a m (aref roots j) eps t))))
    (loop for j from 2 to m do
      (let ((x (aref roots j))
	    (i (1- j)))
	(loop while (and (>= i 1) 

			 (> (realpart (aref roots i)) (realpart x))) do
	  (setf (aref roots (1+ i)) (aref roots i))
	  (decf i))
	(setf (aref roots (1+ i)) x)))))





;;;; ------------------------------------------------------------------------------------------
;;;;                    Fourier Transform
;;;;

(defun four1 (data nn isign)		;replaces data by its discrete Fourier transform,
					;if ISIGN=1, or replaces data by NN times it inverse
					;discrete Fourier transform if ISIGN=-1.  DATA is a  
complex
					;array of NN elements (assumed here to be a real array  
of 2*(nn+1))
					;NN must be an integer power of 2, but we don't check it  
here.
  (let ((m 0) (istep 0) (i 0) (mmax 0)
	(n (* 2 nn))
	(j 1)
	(wtemp 0.0) (wr 0.0) (wpr 0.0) (wpi 0.0) (wi 0.0) (theta 0.0)
	(tempr 0.0) (tempi 0.0) (wrs 0.0) (wis 0.0))
    (do ((ii 1 (+ ii 1)))		;bit reversal section starts here
	((> ii nn))
      (setf i (- (* 2 ii) 1))
      (cond ((> j i)
	     (setf tempr (aref data j))	;swap (as complex) data[j] and data[i]
	     (setf tempi (aref data (+ j 1)))
	     (setf (aref data j) (aref data i))
	     (setf (aref data (+ j 1)) (aref data (+ i 1)))
	     (setf (aref data i) tempr)
	     (setf (aref data (+ i 1)) tempi)))
      (setf m (floor n 2))
      (do () 

	  ((or (< m 2) (<= j m)))	;WHILE (m >= 2) AND (j > m) DO BEGIN...
	(decf j m)
	(setf m (floor m 2)))
      (incf j m))
    (setf mmax  2)			;now the Danielson-Lanczos section
    (do ()
	((<= n mmax))			;WHILE (n > mmax) DO BEGIN...
      (setf istep (* 2 mmax))
      (setf theta (/ 6.28318530717959 (* isign mmax)))
      (setf wpr (* -2.0 (expt (sin (* 0.5 theta)) 2)))
      (setf wpi (sin theta))
      (setf wr 1.0)
      (setf wi 0.0)
      (do ((ii 1 (+ ii 1))		;FOR ii:=1 TO mmax DIV 2 DO...
	   (iilim (floor mmax 2)))
	  ((> ii iilim))
	(setf m (- (* 2 ii) 1))
	(setf wrs wr)
	(setf wis wi)
	(do ((jj 0 (+ jj 1))		;FOR jj:=0 TO (n-m) DIV istep DO...
	     (j 0) (i 0)
	     (jjlim (floor (- n m) istep)))
	    ((> jj jjlim))
	  (setf i (+ m (* jj istep)))
	  (setf j (+ i mmax))
	  (setf tempr (- (* wrs (aref data j)) (* wis (aref data (+ j 1)))))
	  (setf tempi (+ (* wrs (aref data (+ j 1))) (* wis (aref data j))))
	  (setf (aref data j) (- (aref data i) tempr))
	  (setf (aref data (+ j 1)) (- (aref data (+ i 1)) tempi))
	  (incf (aref data i) tempr)
	  (incf (aref data (+ i 1)) tempi))
	(setf wtemp wr)
	(setf wr (+ wr (- (* wr wpr) (* wi wpi))))
	(setf wi (+ wi (* wi wpr) (* wtemp wpi))))
      (setf mmax istep))))

;;; here's a version of the same function, but using two 0-based arrays, and a simpler  
recursion.

(defun four4 (xdata ydata n isign)
  (let ((mmax 0)
	(j 0)
	(ipow (floor (/ (log n) (log 2)))) 

	(pow 0)
	(wtemp 0.0) 

	(wr 0.0) 

	(wpr 0.0) 

	(prev 0)
	(wpi 0.0) 

	(wi 0.0) 

	(theta 0.0)
	(tempr 0.0) 

	(tempi 0.0) 

	(wrs 0.0) 

	(wis 0.0))
    (dotimes (i n)			;bit reversal section starts here
      (when (> j i)
	(setf tempr (aref xdata j))	;swap (as complex) data[j] and data[i]
	(setf tempi (aref ydata j))
	(setf (aref xdata j) (aref xdata i))
	(setf (aref ydata j) (aref ydata i))
	(setf (aref xdata i) tempr)
	(setf (aref ydata i) tempi))
      (let ((m (floor n 2)))
	(do () 

	    ((or (< m 2) (< j m)))
	  (decf j m)
	  (setf m (floor m 2)))
	(incf j m)))
    (setf prev 1)
    (setf mmax 2)			;now the Danielson-Lanczos section
    (setf pow (floor n 2))
    (setf theta (* 6.28318530717959 isign .5))
    (dotimes (lg ipow)
      (setf wpr (cos theta))
      (setf wpi (sin theta))
      (setf wr 1.0)
      (setf wi 0.0)
      (dotimes (ii prev)
	(setf wrs wr)
	(setf wis wi)
	(do* ((jj 0 (+ jj 1))
	      (i ii (+ i mmax))
	      (j (+ i prev) (+ j mmax)))
	    ((>= jj pow))
	  (setf tempr (- (* wrs (aref xdata j)) (* wis (aref ydata j))))
	  (setf tempi (+ (* wrs (aref ydata j)) (* wis (aref xdata j))))
	  (setf (aref xdata j) (- (aref xdata i) tempr))
	  (setf (aref ydata j) (- (aref ydata i) tempi))
	  (incf (aref xdata i) tempr)
	  (incf (aref ydata i) tempi))
	(setf wtemp wr)
	(setf wr (- (* wr wpr) (* wi wpi)))
	(setf wi (+ (* wi wpr) (* wtemp wpi))))
      (setf pow (* pow .5))
      (setf prev mmax)
      (setf theta (* theta .5))
      (setf mmax (* mmax 2)))))

(defun twofft (data1 data2 fft1 fft2 n)	;given real data DATA1 and DATA2, call FOUR1 (above)
					;returning two "complex" arrays FFT1 and FFT2.
  (let* ((nn (+ n n))
	 (nn2 (+ nn 2))
	 (nn3 (+ nn 3))
	 (jj 0) (rep 0.0) (rem 0.0) (aip 0.0) (aim 0.0))
    (do ((j 1 (+ j 1)))			;pack DATA1 and DATA2 into FFT1
	((> j n))
      (setf jj (+ j j))
      (setf (aref fft1 (- jj 1)) (aref data1 j))
      (setf (aref fft1 jj) (aref data2 j)))
    (four1 fft1 n 1)			;FFT of both input arrays
    (setf (aref fft2 1) (aref fft1 2))
    (setf (aref fft1 2) 0.0)
    (setf (aref fft2 2) 0.0)
    (do ((jj 1 (+ jj 1))		;FOR jj:=1 TO (n DIV 2) DO...
	 (j 0)
	 (jjlim (floor n 2)))
	((> jj jjlim))			;separate the two transforms
      (setf j (+ (* 2 jj) 1))
      (setf rep (* 0.5 (+ (aref fft1 j) (aref fft1 (- nn2 j)))))
      (setf rem (* 0.5 (- (aref fft1 j) (aref fft1 (- nn2 j)))))
      (setf aip (* 0.5 (+ (aref fft1 (+ j 1)) (aref fft1 (- nn3 j)))))
      (setf aim (* 0.5 (- (aref fft1 (+ j 1)) (aref fft1 (- nn3 j)))))
      (setf (aref fft1 j) rep)
      (setf (aref fft1 (+ j 1)) aim)
      (setf (aref fft1 (- nn2 j)) rep)
      (setf (aref fft1 (- nn3 j)) (- aim))
      (setf (aref fft2 j) aip)
      (setf (aref fft2 (+ j 1)) (- rem))
      (setf (aref fft2 (- nn2 j)) aip)
      (setf (aref fft2 (- nn3 j)) rem))))

(defun realft (data n isign)		;FFT of 2n real data points.  Replaces DATA
					;by positive frequency half of FFT result.
					;if ISIGN is -1, does inverse FFT from complex data
					;to real (must divide data by N in that case)
  (let ((theta (/ 6.28318530717959 (* n 2.0)))
	(c1 0.5)
	(c2 0.0)
	(h1r 0.0))
    (cond ((plusp isign)
	   (setf c2 -0.5)
	   (four1 data n 1))
	  (t (setf c2 0.5)
	     (setf theta (- theta))))
    (let* ((wpr (* -2.0 (expt (sin (* 0.5 theta)) 2)))
	   (wpi (sin theta))
	   (wr (+ 1.0 wpr))
	   (wi wpi)
	   (i1 0) (i2 0) (i3 0) (i4 0) 

	   (h1i 0.0) (h2r 0.0) (h2i 0.0) (wrs 0.0) (wis 0.0) (wtemp 0.0))
	   (do ((i 2 (+ i 1))		;FOR i:=2 TO n DIV 2 DO BEGIN...
		(ilim (floor n 2)))
	       ((> i ilim))
	     (setf i1 (+ i i -1))
	     (setf i2 (+ i1 1))
	     (setf i3 (- (+ n n 3) i2))
	     (setf i4 (+ i3 1))
	     (setf wrs wr)
	     (setf wis wi)
	     (setf h1r (* c1 (+ (aref data i1) (aref data i3))))
	     (setf h1i (* c1 (- (aref data i2) (aref data i4))))
	     (setf h2r (* (- c2) (+ (aref data i2) (aref data i4))))
	     (setf h2i (* c2 (- (aref data i1) (aref data i3))))
	     (setf (aref data i1) (- (+ h1r (* wrs h2r)) (* wis h2i)))
	     (setf (aref data i2) (+ h1i (* wrs h2i) (* wis h2r)))
	     (setf (aref data i3) (+ (- h1r (* wrs h2r)) (* wis h2i)))
	     (setf (aref data i4) (- (+ (* wrs h2i) (* wis h2r)) h1i))
	     (setf wtemp wr)
	     (setf wr (+ (- (* wr wpr) (* wi wpi)) wr))
	     (setf wi (+ (* wi wpr) (* wtemp wpi) wi)))
	   (setf h1r (aref data 1))
	   (cond ((plusp isign)
		  (setf (aref data 1) (+ h1r (aref data 2)))
		  (setf (aref data 2) (- h1r (aref data 2))))
		 (t (setf (aref data 1) (* c1 (+ h1r (aref data 2))))
		    (setf (aref data 2) (* c1 (- h1r (aref data 2))))
		    (four1 data n -1))))))


;;;;                           Convolution, Correlation
;;;;
;;;;


(defun convlv (data n respns m isign ans)	
					;convolves or deconvolves data with respns (assumed
					;to be smaller in size than data).  Impulse response
					;must be stored in respns in "wrap-around order" -- that
					;is positive time in low half, negative time in high.
					;isign=1 for convolution, -1 for deconvolution, answer
					;appears in ans.  data must be power of 2 in size, and
					;ans must be twice that big (for FFT).
  (let ((fft (make-array (+ (* n 2) 1) :element-type 'float :initial-element 0.0)))
    (loop for i from 1 to (floor (- m 1) 2) do
      (setf (aref respns (+ n 1 (- i))) (aref respns (+ m 1 (- i)))))
    (loop for i from (floor (+ m 3) 2) to (- n (floor (- m 1) 2)) do
      (setf (aref respns i) 0.0))
    (twofft data respns fft ans n)
    (let ((no2 (floor n 2))
	  (ii 0) (dum 0.0) (mag2 0.0))
      (loop for i from 1 to (+ no2 1) do
	(setf ii (* i 2))
	(cond ((plusp isign)
	       (setf dum (aref ans (- ii 1)))
	       (setf (aref ans (- ii 1))
		 (/ (- (* (aref fft (- ii 1)) (aref ans (- ii 1)))
		       (* (aref fft ii) (aref ans ii)))
		    no2))
	       (setf (aref ans ii)
		 (/ (+ (* (aref fft ii) dum)
		       (* (aref fft (- ii 1)) (aref ans ii)))
		    no2)))
	      (t (if (zerop (setf mag2 (+ (expt (aref ans (- ii 1)) 2)
					  (expt (aref ans ii) 2))))
		     (error "deconvolving at response 0"))
		 (setf dum (aref ans (- ii 1)))
		 (setf (aref ans (- ii 1))
		   (/ (+ (* (aref fft (- ii 1)) (aref ans (- ii 1)))
			 (* (aref fft ii) (aref ans ii)))
		      (* mag2 no2)))
		 (setf (aref ans ii)
		   (/ (- (* (aref fft ii) dum)
			 (* (aref fft (- ii 1)) (aref ans ii)))
		      (* mag2 no2))))))
      (setf (aref ans 2) (aref ans (+ n 1)))
      (realft ans no2 -1))))


(defun correl (data1 data2 n ans)	;compute correlation of data1 and data2.  Return result  
in ANS.
  (let ((fft (make-array (+ (* n 2) 1) :element-type 'float :initial-element 0.0))
	(no2 0) (ii 0) (dum 0.0))
    (twofft data1 data2 fft ans n)
    (setf no2 (floor n 2))
    (loop for i from 1 to (+ no2 1) do
      (setf ii (* i 2))
      (setf dum (aref ans (- ii 1)))
      (setf (aref ans (- ii 1))
	(/ (+ (* (aref fft (- ii 1)) (aref ans (- ii 1)))
	      (* (aref fft ii) (aref ans ii)))
	   no2))
      (setf (aref ans ii)
	(/ (- (* (aref fft ii) dum)
	      (* (aref fft (- ii 1)) (aref ans ii)))
	   no2)))
    (setf (aref ans 2) (aref ans (+ n 1)))
    (realft ans no2 -1)))

(defun autocorrel (data n ans)		;auto-correlation
  (correl data data n ans))


;;;;                           Power spectrum (windowing etc)
;;;;
;;;;

;;; this SPECTRUM procedure is an amalgam of JOS, JAM, and Recipes code.
;;;

(defconstant rectangular-window 0)
(defconstant hanning-window 1)
(defconstant welch-window 2)
(defconstant parzen-window 3)
(defconstant bartlett-window 4)
(defconstant hamming-window 5)
(defconstant order-0-window 6)
(defconstant order-1-window 7)
(defconstant order-2-window 8)
(defconstant order-3-window 9)
(defconstant order-4-window 10)
(defconstant exponential-window 11)
(defconstant kaiser-window 12)

(defun sqr (x) (* x x))

(defun apply-window (data n i val)
  (progn
    (setf (aref data i) (* (aref data i) val))
    (setf (aref data (- n i 1)) (* (aref data (- n i 1)) val))))

(defun spectrum (data &optional (window parzen-window) (beta 2.5))
					;data is real array, windows, FFTs, returns magnitude  
spectrum (in place)
  (let* ((n (array-dimension data 0))
	 (n2 (ceiling (/ (log n) (log 2))))
	 (midn (floor n 2))
	 (fftn (expt 2 n2))
	 (freq (/ two-pi n))
	 (rate (/ 1.0 midn))
	 (angle 0.0)
	 (expn (+ 1.0 (/ (log 2) midn)))
	 (expsum 1.0)
	 (ffti (make-array (+ fftn 1) :element-type 'float :initial-element 0.0)))
    (if (and (/= window rectangular-window)
	     (/= window order-0-window))
	(loop for i from 0 to midn do	;all these windows are symmetric around the midpoint
	  (if (or (= window hamming-window)
		  (= window order-1-window))
	      (progn
		(apply-window data n i (- 0.54 (* 0.46 (cos angle))))
		(incf angle freq))
	    (if (= window bartlett-window) ;triangular window
		(progn
		  (apply-window data n i angle)
		  (incf angle rate))
	      (if (= window hanning-window) ;cosine that starts and ends at 0
		  (progn
		    (apply-window data n i (- 0.5 (* 0.5 (cos angle))))
		    (incf angle freq))
		(if (= window parzen-window) ;triangle with small offset 

		    (apply-window data n i (- 1.0 (abs (/ (- i (* 0.5 (- n 1))) (* 0.5 (+ n  
1))))))
		  (if (= window welch-window) ;parzen squared
		      (apply-window data n i (- 1.0 (sqr (/ (- i (* 0.5 (- n 1))) (* 0.5 (+ n  
1))))))
		    (if (= window order-2-window)
			(progn
			  (apply-window data n i (+ 0.42323 

						    (* -0.49755 (cos angle)) 

						    (* 0.07922 (cos (* angle 2)))))
			  (incf angle freq))
		      (if (= window order-3-window)
			  (progn
			    (apply-window data n i (+ 0.35875 

						      (* -0.48829 (cos angle)) 

						      (* 0.14128 (cos (* angle 2)))
						      (* -0.01168 (cos (* angle 3)))))
			    (incf angle freq))
			(if (= window order-4-window)
			    (progn
			      (apply-window data n i (+ 0.287333
							(* -0.4471689 (cos angle))
							(* 0.2084454 (cos (* angle 2)))
							(* -0.0519053 (cos (* angle 3)))
							(* 0.00514933 (cos (* angle 4)))))
			      (incf angle freq))
			  (if (= window kaiser-window)
			      (apply-window data n i (/ (bessi0 (* beta (sqrt (- 1.0 (sqr (/ (-  
n (* 2 i)) n)))))) (bessi0 beta)))
			    (if (= window exponential-window)
				(progn
				  (apply-window data n i (- expsum 1.0))
				  (setf expsum (* expsum expn)))))))))))))))
    ;;now DATA is windowed and ready to be transformed
    (fft data ffti fftn)
    ;;now turn this into a polar spectrum
    (setf expn 0.0)
    (loop for i from 0 below n do
      (setf expsum (sqrt (+ (sqr (aref data i)) (sqr (aref ffti i)))))
      (setf expn (max expn expsum))
      (setf (aref data i) expsum))
    ;;now normalize the power spectrum (this is a dubious kludge...)
    (loop for i below n do (setf (aref data i) (/ (aref data i) expn)))))

(defun log-magnitude (data)		;assumes data normalized
  (let ((20log10 (/ 20 (log 10)))
	(lowest 1.0e-6))
    (loop for i from 0 below (array-total-size data) do
      (setf (aref data i) (* 20log10 (log (max (aref data i) lowest)))))))

;;;------------------------------------------------------------------------



(defun memcof (data n m pm cof)
  (let* ((wk1 (make-array n :element-type 'float :initial-element 0.0))
	 (wk2 (make-array n :element-type 'float :initial-element 0.0))
	 (wkm (make-array m :element-type 'float :initial-element 0.0))
	 (num 0.0) (p 0.0) (denom 0.0))
    (setf pm 0.0)
    (loop for j from 1 to n do
      (incf p (expt (aref data j) 2)))
    (setf pm (/ p n))
    (setf (aref wk1 1) (aref data 1))
    (setf (aref wk2 (- n 1)) (aref data n))
    (loop for j from 2 to (- n 1) do
      (setf (aref wk1 j) (aref data j))
      (setf (aref wk2 (- j 1)) (aref data j)))
    (loop for k from 1 to m do
      (setf num 0.0)
      (setf denom 0.0)
      (loop for j from 1 to (- n k) do
	(incf num (* (aref wk1 j) (aref wk2 j)))
	(incf denom (+ (expt (aref wk1 j) 2) (expt (aref wk2 j) 2))))
      (setf (aref cof k) (* 2.0 (/ num denom)))
      (setf pm (* pm (- 1.0 (expt (aref cof k) 2))))
      (loop for i from 1 to (- k 1) do
	(setf (aref cof i) (- (aref wkm i) (* (aref cof k) (aref wkm (- k i))))))
      (cond ((/= k m)
	     (loop for i from 1 to k do 

	       (setf (aref wkm i) (aref cof i)))
	     (loop for j from 1 to (- n k 1) do
	       (decf (aref wk1 j) (* (aref wkm k) (aref wk2 j)))
	       (setf (aref wk2 j) (- (aref wk2 (+ j 1)) (* (aref wkm k) (aref wk1 (+ j  
1)))))))))
    pm))


(defun evlmem (fdt cof m pm)
  (let* ((theta (* pi 2.0 fdt))
	 (wpr (cos theta))
	 (wpi (sin theta))
	 (wr 1.0)
	 (wi 0.0)
	 (sumr 1.0)
	 (sumi 0.0)
	 (wtemp 0.0) (wrs 0.0) (wis 0.0))
    (loop for i from 1 to m do
      (setf wtemp wr)
      (setf wr (- (* wr wpr) (* wi wpi)))
      (setf wi (+ (* wi wpr) (* wtemp wpi)))
      (setf wrs wr)
      (setf wis wi)
      (decf sumr (* (aref cof i) wrs))
      (decf sumi (* (aref cof i) wis)))
    (/ pm (+ (expt sumr 2) (expt sumi 2)))))
      




;;;;                           Linear Prediction
;;;;
;;;;


(defun fixrts (d npoles)
  (let ((a (make-array (+ npoles 2) :element-type 'complex))
	(roots (make-array (+ npoles 2) :element-type 'complex))
	(dum 0.0))
    (setf (aref a (1+ npoles)) #C(1.0 0.0))
    (loop for j from npoles downto 1 do
      (setf (aref a j) (complex (- (aref d (+ npoles 1 (- j))))
				0.0)))
    (zroots a npoles roots t)
    (loop for j from 1 to npoles do
      (let ((size (+ (sqr (realpart (aref roots j))) 

		     (sqr (imagpart (aref roots j))))))
	(if (> size 1.0)
	    (setf (aref roots j) (complex (/ (realpart (aref roots j)) size)
					  (/ (imagpart (aref roots j)) size))))))
    (setf (aref a 1) (- (aref roots 1)))
    (setf (aref a 2) #C(1.0 0.0))
    (loop for j from 2 to npoles do
      (setf (aref a (1+ j)) #C(1.0 0.0))
      (loop for i from j downto 2 do
	(setf dum (realpart (aref a i)))
	(setf (aref a i) (complex (+ (realpart (aref a (1- i)))
				     (- (* (realpart (aref a i)) (realpart (aref roots j))))
				     (* (imagpart (aref a i)) (imagpart (aref roots j))))
				  (- (imagpart (aref a (1- i)))
				     (* dum (imagpart (aref roots j)))
				     (* (imagpart (aref a i)) (realpart (aref roots j)))))))
      (setf dum (realpart (aref a 1)))
      (setf (aref a 1) (complex (- (* (imagpart (aref a 1)) (imagpart (aref roots j)))
				   (* (realpart (aref a 1)) (realpart (aref roots j))))
				(- (+ (* dum (imagpart (aref roots j)))
				      (* (imagpart (aref a 1)) (realpart (aref roots j))))))))
    (loop for j from 1 to npoles do
      (setf (aref d (+ npoles 1 (- j))) (- (realpart (aref a j)))))))


(defun predic (data ndata d npoles future nfut)
  (let ((reg (make-array (1+ npoles) :element-type 'float)))
    (loop for j from 1 to npoles do
      (setf (aref reg j) (aref data (+ ndata 1 (- j)))))
    (loop for j from 1 to nfut do
      (let ((discrp 0.0)
	    (sum 0.0))
	(loop for k from 1 to npoles do
	  (incf sum (* (aref d k) (aref reg k))))
	(loop for k from npoles downto 2 do
	  (setf (aref reg k) (aref reg (1- k))))
	(setf (aref reg 1) sum)
	(setf (aref future j) sum)))))



	 


;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;                    Statistical Description
;;;;


(defun moment (data)			;returns mean, average deviation, standard deviation,  
variance, skewness, kurtosis
  (let ((n (array-dimension data 0))
	(s 0.0) (p 0.0)
	(ave 0.0) (adev 0.0) (svar 0.0) (skew 0.0) (curt 0.0) (sdev 0.0))
    (if (< n 1) (error "data must have more than ~D elements" n))
    (loop for j below n do (incf s (aref data j)))
    (setf ave (/ s n))
    (loop for j below n do
      (setf s (- (aref data j) ave))
      (incf adev (abs s))
      (setf p (* s s))
      (incf svar p)
      (setf p (* p s))
      (incf skew p)
      (setf p (* p s))
      (incf curt p))
    (setf adev (/ adev n))
    (setf svar (/ svar (- n 1)))
    (setf sdev (sqrt svar))
    (cond ((/= 0.0 svar)
	   (setf skew (/ skew (* n sdev sdev sdev)))
	   (setf curt (- (/ curt (* n svar svar)) 3.0)))
	  (t (error "no skew/kurtosis when variance = 0")))
    (values ave adev sdev svar skew curt)))

(defun mdian1 (data)
  (let* ((n (array-dimension data 0))
	 (n2 (floor n 2)))
    (sort n data)
    (if (oddp n) 

	(aref data (+ n2 1))
      (* 0.5 (+ (aref data n2) (aref data (+ n2 1)))))))

(defun avevar (data)
  (let* ((n (array-dimension data 0))
	 (s 0.0)
	 (ave 0.0)
	 (svar 0.0))
    (loop for j below n do
      (incf ave (aref data j)))
    (setf ave (/ ave n))
    (loop for j below n do
      (setf s (- (aref data j) ave))
      (incf svar (* s s)))
    (values ave (/ svar (- n 1)))))


;;;------------------------------------------------------------------------
;;; C version of fft
#|

/* fft and convolution of real data in zero-based arrays */

shuffle (float* rl, float* im, int n)
{
  /* bit reversal */

  int i,m,j;
  float tempr,tempi;
  j=0;
  for (i=0;i<n;i++)
    {
      if (j>i)
	{
	  tempr = rl[j];
	  tempi = im[j];
	  rl[j] = rl[i];
	  im[j] = im[i];
	  rl[i] = tempr;
	  im[i] = tempi;
	}
      m = n>>1;
      while ((m>=2) && (j>=m))
	{
	  j -= m;
	  m = m>>1;
	}
      j += m;
    }
}

c_fft (float* rl, float* im, int n, int isign, int ipow)
{
  /* standard fft: real part in rl, imaginary in im,        */
  /* ipow = ceiling (log n / log 2), isign=1 fft, =-1 ifft. */
  /* rl and im are zero-based.                              */
  /*                                                        */
  /* oddly enough, the integer version (using integer ops   */
  /* and "block floating point" scaling) was much slower,   */
  /* and splitting out the no-ops (twiddle factors==0 etc)  */
  /* made no difference at all.                             */

  int mmax,j,pow,prev,lg,i,ii,jj;
  float wrs,wis,tempr,tempi;
  double wr,wi,theta,wtemp,wpr,wpi;

  shuffle(rl,im,n);
  mmax = 2;
  prev = 1;
  pow = n*0.5;
  theta = (one_pi*isign);
  for (lg=0;lg<ipow;lg++)
    {
      wpr = cos(theta);
      wpi = sin(theta);
      wr = 1.0;
      wi = 0.0;
      for (ii=0;ii<prev;ii++)
	{
	  wrs = (float) wr;
	  wis = (float) wi;
	  i = ii;
	  j = ii + prev;
	  for (jj=0;jj<pow;jj++)
	    {
	      tempr = wrs*rl[j] - wis*im[j];
	      tempi = wrs*im[j] + wis*rl[j];
	      rl[j] = rl[i] - tempr;
	      im[j] = im[i] - tempi;
	      rl[i] += tempr;
	      im[i] += tempi;
	      i += mmax;
	      j += mmax;
	    }
	  wtemp = wr;
	  wr = (wr*wpr) - (wi*wpi);
	  wi = (wi*wpr) + (wtemp*wpi);
	}
      pow = pow*0.5;
      prev = mmax;
      theta = theta*0.5;
      mmax = mmax*2;
    }
}
 

convolve (float* rl1, float* rl2, int n, int ipow)
{
  /* convolves two real arrays.                                           */
  /* rl1 and rl2 are assumed to be set up correctly for the convolution   */
  /* (that is, rl1 (the "signal") is zero-padded by length of             */
  /* (non-zero part of) rl2 and rl2 is stored in wrap-around order)       */
  /* We treat rl2 as the imaginary part of the first fft, then do         */
  /* the split, scaling, and (complex) spectral multiply in one step.     */
  /* result in rl1                                                        */

  int j,n2,nn2;
  float rem,rep,aim,aip,invn;

  c_fft(rl1,rl2,n,1,ipow);
  

  n2=n*0.5;
  invn = 0.25/n;
  rl1[0] = ((rl1[0]*rl2[0])/n);
  rl2[0] = 0.0;

  for (j=1;j<=n2;j++)
    {
      nn2 = n-j;
      rep = (rl1[j]+rl1[nn2]);
      rem = (rl1[j]-rl1[nn2]);
      aip = (rl2[j]+rl2[nn2]);
      aim = (rl2[j]-rl2[nn2]);

      rl1[j] = invn*(rep*aip + aim*rem);
      rl1[nn2] = rl1[j];
      rl2[j] = invn*(aim*aip - rep*rem);
      rl2[nn2] = -rl2[j];
    }
  

  c_fft(rl1,rl2,n,-1,ipow);
}

|#
;;; end of file
