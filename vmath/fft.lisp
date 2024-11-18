; From bil@ccrma.stanford.edu Tue Aug 23 23:46:02 EDT 1994
; Article: 14079 of comp.lang.lisp
; From: bil@ccrma.Stanford.EDU (Bill Schottstaedt)
; Newsgroups: comp.lang.lisp
; Subject: Re: Fast Fourier Transform in Common Lisp?
; Date: 16 Aug 1994 14:07:57 GMT
; Organization: Stanford University

; In article <32oi2kINN1ov@life.ai.mit.edu> robyn@cocoa-chex.ai.mit.edu  
; (Robyn Kozierok) writes:
; > Does anyone know of a Fast Fourier Transform routine available in Common
; > Lisp? 
; 
; Here's the version from "Numerical Recipes" changed to use 0-based
; arrays and a slightly simpler recursion:

(defun fft (xdata ydata n &optional (isign 1))
  (declare (optimize (speed 3) (safety 1)))
  (let ((mmax 0) (j 0)
	(ipow (floor (log n 2)))
	(pow 0)	(wtemp 0.0) (wr 0.0) (wpr 0.0) 
	(prev 0) (wpi 0.0) (wi 0.0) (theta 0.0)
	(tempr 0.0) (tempi 0.0) (wrs 0.0) (wis 0.0))
    (dotimes (i n)			;bit reversal section starts here
      (when (> j i)
	(setf tempr (aref xdata j))	;swap data[j] and data[i]
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


; I also have lisp versions of about 2 or 3 dozen of the other
; numerical routines from that book which I'd be happy to share.



