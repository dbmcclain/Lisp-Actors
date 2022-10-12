#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package "ENGFMT")

(declaim (optimize (speed 3)
                   #|(safety 0)|#
                   ))

;; -----------------------------------------------------------------

(defun* generate-output-to-stream (stream sign (w &rest ds) nsig expon exponent-printer)
  (declare (type float sign)
           (type fixnum nsig expon w))
  (format stream
          (if (or (>= nsig 3)
                  (and (= nsig 2) ;; sufficient to show decimal point
                       (< w 100))
                  (and (= nsig 1) ;; ditto
                       (< w 10)))
              "~D.~{~D~}"
            "~D")
          (if (minusp sign)
              (- w)
            w)
          ds)
  (funcall exponent-printer expon stream))

(defun generate-output (stream &rest args)
  (cond ((eq stream t) (apply 'generate-output-to-stream *standard-output* args))
        ((null stream) (with-output-to-string (s)
                         (apply 'generate-output-to-stream s args)))
        (t             (apply 'generate-output-to-stream stream args))
        ))
        
;; -----------------------------------------------------------------

(defun finitep (v)
  (or (zerop v)
      (not (zerop (/ v)))))

(defvar *rnders*
  #(1  ;; 1d0 - 1d15
    10
    100
    1000
    10000
    100000
    1000000
    10000000
    100000000
    1000000000
    10000000000
    100000000000
    1000000000000
    10000000000000
    100000000000000
    1000000000000000))

(defun rnd-nsig (v nsig)
  ;; round a value according to nsig digits
  ;; return rounded scaled integer, scale factor, and number of dp
  (declare (float  v)
           (fixnum nsig))
  (bind*
      ((ns (the fixnum (- nsig 1 (floor (log v 10))))))
    
    (case ns
      (-2 (values (* 100 (round v 100)) 1 -2))
      (-1 (values (* 10 (round v 10)) 1 -1))
      (otherwise
       (bind*
           ((rfact (aref *rnders* ns)))
         
         (values (round (* rfact v)) rfact ns)))
      )))

(defun decode-number (v nsig base lbase)
  ;; return: sign, list-of-digits, exponent
  (declare (type float  v)
           (type fixnum nsig base lbase))
  
  (if (zerop v)
      ;; sign, digits, exponent
      (values 0.0 (loop for ix fixnum from 0 below nsig collect 0) 0)

    (bind*
        ((:values (frac e2 sign) (decode-float v))
         (:declare
          (type float frac sign)
          (type fixnum e2))
         
         ;; get the whole part of the exponent in specified base
         ;; and the fractional part for scaling the decoded base-2 fraction
         (:values (e10w e10f) (floor (* e2 (log 2d0 base))))
         (:declare
          (fixnum e10w)
          (float  e10f))

         ;; get the fraction in the indicated base, the rounded value according to nsig,
         ;; and the new exponent in the indicated base.
         (m10  (the float (* frac (expt base e10f))))
         (e10  (the fixnum (* e10w lbase)))
         (:declare
          (float  m10)
          (fixnum e10))
         
         (:values (rm10 rfact ns) (rnd-nsig m10 nsig)))

      ;; if rounded value is less than 1, then mult by base and decrease exponent
      (while (< rm10 rfact)
        (setf m10 (the float (* m10 base)))
        (decf e10 lbase)
        (bind*
            ((:values (rm rf n) (rnd-nsig m10 nsig)))
          
          (setf rm10  rm
                rfact rf
                ns    n)))
      
      ;; if rounded value is > base, then div by base and increase exponent
      (while (>= rm10 (* rfact base))
        (setf m10 (the float (/ m10 base)))
        (incf e10 lbase)
        (bind*
            ((:values (rm rf n) (rnd-nsig m10 nsig)))
          
          (setf rm10  rm
                rfact rf
                ns    n)))

      (values sign
              (if (plusp ns)
                  (bind*
                      ((digits nil))
                    
                    (loop for ix from 0 below ns
                          for (w f) = (multiple-value-list (truncate rm10 10))
                          then (multiple-value-list (truncate w 10))
                          do (push f digits)
                          finally (push w digits))
                    digits)
                
                (list rm10))
              e10)
      )))

(defun check-nsig (nsig)
  (assert (typep nsig '(integer 1 15))
      (nsig)
    "Number of significant digits (~S) must be an integer in [1,15]" nsig))

(defun format-with-computed-predigits (stream v nsig base lbase exponent-printer)
  (declare (type float v)
           (type fixnum nsig))
  (bind*
      ((:values (sign digits expon) (decode-number v nsig base lbase))
       (:declare
        (type float sign)
        (type list digits)
        (type fixnum expon)))
    
    (generate-output stream sign digits nsig
                     expon exponent-printer)
    ))

(defun complex-formatter (stream v formatter-fn args)
  (declare (type complex v)
	   (ftype (function (t real &rest t) string) formatter-fn))
  (apply 'format stream "#C(~A ~A)"
         (mapcar (lambda (part)
                   (declare (type real part))
                   (apply formatter-fn nil part args))
                 (list (realpart v)
                       (imagpart v))
                 )))

;; -----------------------------------------------------------------

(defmethod engineering-format (stream (v float)
                                      &key
                                      (nsig 3)
                                      (exponent-printer 'default-exponent-printer))
  (cond ((finitep v)
         (check-nsig nsig)
         (format-with-computed-predigits stream v nsig 1000 3
                                         exponent-printer))
        (t (if (plusp v)
               "+Infinity"
             "-Infinity"))
        ))

(defmethod engineering-format (stream (v rational) &rest args &key &allow-other-keys)
  (apply 'engineering-format stream (float v 1d0) args))

(defmethod engineering-format (stream (v complex) &rest args &key (nsig 3) &allow-other-keys)
  (check-nsig nsig)
  (complex-formatter stream v 'engineering-format args))
  
(defmethod engineering-format (stream v &key &allow-other-keys)
  (declare (ignore v))
  (format stream "NaN"))

;; -----------------------------------------------------------------

(defmethod scientific-format (stream (v float)
                                     &key
                                     (nsig 3)
                                     (exponent-printer 'default-exponent-printer))
  (cond ((finitep v)
         (check-nsig nsig)
         (format-with-computed-predigits stream v nsig 10 1
                                         exponent-printer))

        (t (if (plusp v)
               "+Infinity"
             "-Infinity"))
        ))

(defmethod scientific-format (stream (v rational) &rest args &key &allow-other-keys)
  (apply 'scientific-format stream (float v 1d0) args))

(defmethod scientific-format (stream (v complex) &rest args &key (nsig 3) &allow-other-keys)
  (check-nsig nsig)
  (complex-formatter stream v 'scientific-format args))

(defmethod scientific-format (stream v &key &allow-other-keys)
  (declare (ignore v))
  (format stream "NaN"))

;; -----------------------------------------------------------------
;; -----------------------------------------------------------------

(defun default-exponent-printer (expon &optional (stream *standard-output*))
  (declare (type fixnum expon))
  (unless (zerop expon)
    (princ #\E stream)
    (princ expon stream)
    ))

(defun lower-case-e-exponent-printer (expon &optional (stream *standard-output*))
  (declare (type fixnum expon))
  (unless (zerop expon)
    (princ #\e stream)
    (princ expon stream)))

(defun always-signed-exponent-printer (expon &optional (stream *standard-output*))
  (declare (type fixnum expon))
  (unless (zerop expon)
    (princ #\E stream)
    (when (plusp expon)
      (princ #\+ stream))
    (princ expon stream)))

(defun paren-style-exponent-printer (expon &optional (stream *standard-output*))
  (declare (type fixnum expon))
  (unless (zerop expon)
    (princ #\( stream)
    (princ expon stream)
    (princ #\) stream)))

(defun mathematica-style-exponent-printer (expon &optional (stream *standard-output*))
  (declare (type fixnum expon))
  (unless (zerop expon)
    (princ "*^" stream)
    (princ expon stream)))

;; -----------------------------------------------------------------

#|
(bind*
    ((x -1.23d-4)
     (ndigits 8))
  (engineering-format nil x :nsig ndigits))
|#

;; -------------------------------------------------------------------
;; design self-printable objects
;; now we can use ~A on them in format strings

(defclass engfmt ()
  ((val   :accessor engfmt-val  :initarg :val)
   (nsig  :accessor engfmt-nsig :initarg :nsig :initform 3)
   (eprt  :accessor engfmt-eprt :initarg :eprt :initform 'default-exponent-printer)))

(defun engfmt (val &key (nsig 3) (eprt 'default-exponent-printer))
  (make-instance 'engfmt
                 :val  val
                 :nsig nsig
                 :eprt eprt))

(defmethod print-object ((x engfmt) stream)
  (engineering-format stream (engfmt-val x)
                      :nsig (engfmt-nsig x)
                      :exponent-printer (engfmt-eprt x)))


(defclass scifmt ()
  ((val   :accessor scifmt-val  :initarg :val)
   (nsig  :accessor scifmt-nsig :initarg :nsig :initform 3)
   (eprt  :accessor scifmt-eprt :initarg :eprt :initform 'default-exponent-printer)))

(defun scifmt (val &key (nsig 3) (eprt 'default-exponent-printer))
  (make-instance 'scifmt
                 :val  val
                 :nsig nsig
                 :eprt eprt))

(defmethod print-object ((x scifmt) stream)
  (scientific-format stream (scifmt-val x)
                      :nsig (scifmt-nsig x)
                      :exponent-printer (scifmt-eprt x)))
