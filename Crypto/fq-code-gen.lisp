;; fq-code-gen.lisp -- machine assisted generation of Fq short-word
;; field arithmetic primitives.
;;
;; DM/Stegos  03/19
;; ------------------------------------------------------------
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

;; ------------------------------------------------------------
(in-package :edec)
;; ------------------------------------------------------------
;; Code gen for chunked field elements

(defvar *emit-port* t)
(defvar *modulus*   nil)  ;; |Fq|
(defvar *nb*        nil)  ;; bits per chunk

;; derived values during runtime code generation
(defvar *nchunks*   nil) ;; nbr of chunks
(defvar *nb-last*   nil) ;; nbr bits in last chunk
(defvar *fold-mul*  nil) ;; factor for modular folding of high-order chunks
(defvar *wrap-mul*  nil) ;; factor for modular wrapping excess of last chunk

(defun gen-code (*modulus* *nb*)
  (with-output-to-string (*emit-port*)
    (let ((*print-base* 10))
      (dolist (fn '(gen-gtype
                    gen-gcopy
                    gen-gadd
                    gen-gsub
                    gen-gdec
                    gen-gneg
                    gen-gmul2
                    gen-gsb2
                    gen-gdec2
                    gen-scr
                    gen-gnorm
                    gen-geq
                    gen-gmuli
                    gen-gmul
                    gen-gsqr
                    gen-ginv
                    gen-gsqrt
                    gen-from-byte-vec
                    gen-to-byte-vec
                    ))
        (funcall fn)))))

#|
(with-ed-curve :curve1174
  (gen-code *ed-q* 51))
|#

;; ----------------------------------------------------------------

(defmacro qq (str)
  (um:nlet iter ((s str)
                 (out  "")
                 (code nil))
    (if (zerop (length s))
        `(format nil ,out ,@(nreverse code))
      (progn
        (let* ((start (position #\$ s)))
          (if start
              (let* ((pre    (subseq s 0 start))
                     (end    (position #\$ s :start (1+ start)))
                     (post   (subseq s (1+ end)))
                     (arg    (read-from-string s t nil :start (1+ start) :end end)))
                (iter post (concatenate 'string out pre) (cons arg code)))
            ;; else
            (go-iter "" (concatenate 'string out s) code)))))))

(defun emic (fmt &rest args)
  ;; no forced newline
  (apply 'format *emit-port* fmt args))

(defun emit (fmt &rest args)
  (terpri *emit-port*)
  (apply 'emic fmt args))

(defun do-with-chunk-parameters (fn)
  (multiple-value-bind (*nchunks* rem) (ceiling (integer-length *modulus*) *nb*)
    (let* ((*nb-last*   (+ *nb* rem))
           (*wrap-mul*  (mod (ash 1 (integer-length *modulus*)) *modulus*))
           (*fold-mul*  (with-mod *modulus*
                          (m^ (ash 1 *nb*) *nchunks*))))
      (funcall fn))))

(defmacro with-chunk-parameters (&body body)
  `(do-with-chunk-parameters (lambda () ,@body)))

(defun get-vtype ()
  (format nil "G~A" *nb*))

(defun do-gen-fn (name args ret bodyfn)
  (with-chunk-parameters
    (let ((vtype (get-vtype)))
      (emit "fn ~A(~{~A~^, ~})~@[ -> ~A~] {"
            name
            (mapcar (lambda (arg)
                      (format nil "~A ~A" vtype arg))
                    args)
            ret)
      (funcall bodyfn)
      (emit "}~&"))))

(defmacro def-gen (name args ret &body body)
  (let ((def-name (intern (concatenate 'string
                                       (symbol-name 'gen-)
                                       (symbol-name name)))))
    `(defun ,def-name ()
       (do-gen-fn ,(string-downcase (symbol-name name))
                  ',(mapcar (lambda (arg)
                              (string-downcase (symbol-name arg)))
                            args)
                  ,(when ret
                     (string-downcase (symbol-name ret)))
                  (lambda ()
                    ,@body)))))

;; ---------------------------------------------------------------

(defun gen-gtype ()
  (with-chunk-parameters
    (let ((vtype (get-vtype)))
      (emit (qq #>.end
// For ~A$*ed-name*$ Fq: ~A$vtype$ is a ~A$*nchunks*$-element little-endian 64-bit vector
// where each element contains ~A$*nb*$-bits of the field integer
// and where the final element contains ~A$*nb-last*$-bits

type i64 ~A$vtype$[~A$*nchunks*$]~&
.end))
      )))

(def-gen gcopy (x y) nil 
  (emit "  // copy x to y")
  (dotimes (kx *nchunks*)
    (emit (qq "  y[~A$kx$] = x[~A$kx$]"))
    ))

;; =========================================
#|
(with-output-to-string (*emit-port*)
  (with-ed-curve :curve1174
    (let ((*modulus* *ed-q*)
          (*nb*      51))
      (gen-gmul))))
|#
;; =========================================

(def-gen gadd (x y z) nil 
  (emit "  // x + y -> z")
  (dotimes (kx *nchunks*)
    (emit (qq "  z[~A$kx$] = x[~A$kx$] + y{~A$kx$]"))))
  
(def-gen gsub (x y z) nil 
  (emit "  // x - y -> z")
  (dotimes (kx *nchunks*)
    (emit (qq "  z[~A$kx$] = x[~A$kx$] - y{~A$kx$]"))))

;; ------------------------------------------------------------
;; gdec and gdec2 no longer used?

(def-gen gdec (x y) nil
  (emit "  // y -= x")
  (dotimes (kx *nchunks*)
    (emit (qq "  y[~A$kx$] -= x[~A$kx$]"))))
  
(def-gen gdec2 (x) nil
  (emit #>.end
  // x-2 -> x
  x[0] -= 2.end))
  
;; ------------------------------------------------------------

(def-gen gneg (x) nil 
  (emit "  // -x -> y")
  (dotimes (kx *nchunks*)
    (emit (qq "  y[~A$kx$] = -x[~A$kx$]"))))
  
(def-gen gmul2 (x) nil
  (emit "  // 2*x -> x")
  (dotimes (kx *nchunks*)
    (emit (qq "  x[~A$kx$] *= 2"))))
  
(def-gen gsb2 (x y) nil
  (emit "  // y - 2*x -> y")
  (dotimes (kx *nchunks*)
    (emit (qq "  y[~A$kx$] -= 2 * x[~A$kx$]"))))

(def-gen scr (x) nil
  ;; possible trouble here?
  ;; what about 0x1FF_FFFF_FFFF where max hi should be 0x0FF?
  ;; first pass produces another ovfl in the hi word
  ;;
  ;; Might need another pass for true normalization.
  ;; But there is still a corner case in the modular field,
  ;; where a scr() representation might exceed the modulus by
  ;; a small amount.
  ;;
  (emit #>.end
  // Small coefficient reduction (SCR) on x
  // split64(nb,x) -> (hi, lo) returns low nb bits of x in Int64,
  // and hi (64-nb) bits in Int64
.end)
  (dotimes (kx *nchunks*)
    (emit "  let (hi, lo) = split64(~A, x[~A]~@[ + hi~])"
          (if (< kx (1- *nchunks*))
              *nb*
            *nb-last*)
          kx
          (plusp kx))
    (emit (qq "  x[~A$kx$] = lo")))
  (emit (qq "  x[0] += ~A$*wrap-mul*$*hi")))

(def-gen gnorm (x y) nil
  ;; the idempotent sequence (gneg, scr, gneg, scr)
  ;; removes modular excess from redundant encodings
  ;; and final scr produces a field normalized value
  (emit (qq #>.end
  ~A$(get-vtype)$ tmp
  gneg(x, tmp)
  scr(tmp)
  gneg(tmp, y)
  scr(y)
  scr(y)
  }.end)))
        
(def-gen geq (x y) bool
  ;; sequence (scr, gneg, scr) removes modular excess
  (emit (qq #>.end
  // compare x, y for equality
  ~A$(get-vtype)$ z1, z2
  gsub(x, y, z1)
  gnorm(z1, z2).end))
  (emit "  return (~{~A~^~&    && ~})"
        (lc:lc (qq "z2[~A$kx$] == 0")
               (kx <.. 0 *nchunks*))))

#|
;; test corner case for SCR
;; form two value that should be the same in G51 encoding,
;; but which differ by the modulus. Only a few values like
;; this could exist since |Fq| = 2^N-small

(defun encode-fq51 (x)
  (vector (ldb (byte 51 (* 0 51)) x)
          (ldb (byte 51 (* 1 51)) x)
          (ldb (byte 51 (* 2 51)) x)
          (ldb (byte 51 (* 3 51)) x)
          (ldb (byte 51 (* 4 51)) x)))

(defun scr (x)
  (multiple-value-bind (hi0 lo0) (split-bits 51 (aref x 0))
    (multiple-value-bind (hi1 lo1) (split-bits 51 (aref x 1))
      (multiple-value-bind (hi2 lo2) (split-bits 51 (aref x 2))
        (multiple-value-bind (hi3 lo3) (split-bits 51 (aref x 3))
          (multiple-value-bind (hi4 lo4) (split-bits 47 (aref x 4))
            (let ((wrap (* 9 hi4)))
              (vector (+ wrap lo0)
                      (+ hi0 lo1)
                      (+ hi1 lo2)
                      (+ hi2 lo3)
                      (+ hi3 lo4))
              )))))))

(defun split-bits (n val)
  (values (ash val (- n))
          (ldb (byte n 0) val)))

(defun gneg (x)
  (map 'vector '- x))

(defun gsub (a b)
  (map 'vector '- a b))

(defun gnorm (x)
  (um:nlet iter ((y (gneg (scr (gneg x)))))
    (if (some 'minusp y)
        (go-iter (scr y))
      y)))

(let ((limit (ash 1 (integer-length *ed-q*))))
  (loop for kx from 0 below 1000 do
        (let* ((a  (random-between 0 limit))
               (b  (random-between 0 limit))
               (fq (gsub (encode-fq51 a) (encode-fq51 b)))
               (y  (gnorm fq)))
          (check y))))

(let ((lim51 (ash 1 51))
      (lim47 (ash 1 47)))
  (defun check (y)
    (loop for v across y
          for ix from 0 do
          (assert (not (minusp v)))
          (assert (< v (if (< ix 4)
                           lim51
                         lim47))))
    (assert (< (+ (aref y 0)
                  (ash (aref y 1) 51)
                  (ash (aref y 2) (* 2 51))
                  (ash (aref y 3) (* 3 51))
                  (ash (aref y 4) (* 4 51)))
               *ed-q*))
    ))

(let* ((excess 5)
       (fq1 (gneg (encode-fq51 (+ excess (* 1 *ed-q*))))))
  (check (gnorm fq1)))

(let* ((excess 5)
       (fq1 (encode-fq51 excess))
       (fq2 (encode-fq51 (+ *ed-q*  1)))
       (fq3 (gsub fq1 fq2)))
  (gnorm fq3))

;; this case shows futility of SCR in corner case
(let* ((excess  5) ;; for curve1174 0 <= excess <= 9
       (fq1 (encode-fq51 excess))
       (fq2 (encode-fq51 (+ *ed-q* excess)))
       (fq3 (map 'vector '- fq1 fq2))
       (fq4 (map 'vector '- fq2 fq1)))
  (multiple-value-bind (fq5 z5) (scr fq3)
    (multiple-value-bind (fq6 z6) (scr fq4)
      (multiple-value-bind (fq7 z7) (scr fq5)
        (multiple-value-bind (fq8 z8) (scr fq6)
      (inspect (list :fq1 fq1
                     :fq2 fq2
                     :fq1-fq2 fq3
                     :fq2-fq1 fq4
                     :scr1-2  fq5
                     :z1-2    z5
                     :scr2-1  fq6
                     :z2-1    z6
                     :scrx1-2 fq7
                     :zx1-2   z7
                     :scrx2-1 fq8
                     :zx2-1   z8
                     )))))))

;; the negation ahead of scr(0) works...
(let* ((excess  5) ;; for curve1174 0 <= excess <= 9
       (fq1 (encode-fq51 excess))
       (fq2 (encode-fq51 (+ *ed-q* excess)))
       (fq3 (map 'vector '- fq1 fq2))
       (fq4 (map 'vector '- fq2 fq1))
       (fq4 (map 'vector '- fq4))) ;; insert negation
  (multiple-value-bind (fq5 z5) (scr fq3)
    (multiple-value-bind (fq6 z6) (scr fq4)
      (multiple-value-bind (fq7 z7) (scr fq5)
        (multiple-value-bind (fq8 z8) (scr fq6)
      (inspect (list :fq1 fq1
                     :fq2 fq2
                     :fq1-fq2 fq3
                     :fq2-fq1 fq4
                     :scr1-2  fq5
                     :z1-2    z5
                     :scr2-1  fq6
                     :z2-1    z6
                     :scrx1-2 fq7
                     :zx1-2   z7
                     :scrx2-1 fq8
                     :zx2-1   z8
                     )))))))

(defun zscr (x)
  (let ((x (gneg x)))
    (let ((x (scr x)))
      (let ((x (gneg x)))
        (scr x)))))

;; the negation ahead of scr(0) works...
(let* ((excess  5) ;; for curve1174 0 <= excess <= 9
       (fq1 (encode-fq51 excess))
       (fq2 (encode-fq51 (+ *ed-q* excess)))
       (fq3 (map 'vector '- fq1 fq2))
       (fq4 (map 'vector '- fq2 fq1)))
  (multiple-value-bind (fq5 z5) (zscr fq3)
    (multiple-value-bind (fq6 z6) (zscr fq4)
      (inspect (list :fq1 fq1
                     :fq2 fq2
                     :fq1-fq2 fq3
                     :fq2-fq1 fq4
                     :scr1-2  fq5
                     :z1-2    z5
                     :scr2-1  fq6
                     :z2-1    z6
                     )))))

;; the negation ahead of scr(0) works...
(let* ((excess  5) ;; for curve1174 0 <= excess <= 9
       (fq1 (encode-fq51 excess))
       (fq2 (encode-fq51 (+ *ed-q* excess)))
       (fq3 (gnorm fq1))
       (fq4 (gnorm fq2))
       (fq5 (scr fq2)))
  (inspect (list
            :fq1 fq1
            :fq2 fq2
            :fq3 fq3
            :fq4 fq4
            :fq5 fq5)))

|#

(defun gen-gmuli()
  (with-chunk-parameters
    (emit (qq #>.end
fn gmuli(i64 c, ~A$(get-vtype)$ x) {
  // c * x -> x
  // x is a 64-bit ~A$*nchunks*$-element little-endian vector
  // each element containing a ~A$*nb*$-bit fragment
  // products are 64x64->128-bit results
  // split128(nb,x) -> (hi, lo) returns low nb bits of x in Int64,
  // and hi (128-nb) bits in Int128

  let cc = i128(c).end))
    (dotimes (kx *nchunks*)
      (emit "  let (hi, lo) = split128(~A, i128(x[~A]) * cc~@[ + hi~])"
            (if (< kx (1- *nchunks*))
                *nb*
              *nb-last*)
            kx
            (plusp kx))
      (emit (qq "  x[~A$kx$] = lo")))
    (emit (qq #>.end
  x[0] += ~A$*wrap-mul*$*hi
}
.end))))

(def-gen gmul (x y z) nil
  (emit (qq #>.end
  // x * y -> z
  // x, y, z are 64-bit ~A$*nchunks*$-element little-endian vectors
  // each element containing a ~A$*nb*$-bit fragment
  // products are 64x64->128-bit results
  // split128(nb,x) -> (hi, lo) returns low nb bits of x in Int64,
  // and hi (128-nb) bits in Int128
.end))
  (dotimes (kx *nchunks*)
    (emit (qq "  let x~A$kx$ = Int128(x[~A$kx$])")))
  (dotimes (kx *nchunks*)
    (emit (qq "  let y~A$kx$ = Int128(y[~A$kx$])")))
  (dotimes (kx *nchunks*)
    (emit "  let (hi, lo) = split128(~A, ~{~A~^ + ~}"
          (if (< kx (1- *nchunks*))
              *nb*
            *nb-last*)
          (lc:lc (qq "x~A$ix$*y~A$(- kx ix)$")
                 (ix <.. 0 (1+ kx))))
    (when (< kx (1- *nchunks*))
      (let ((lst (lc:lc (format nil "x~A*y~A"
                                ix (+ *nchunks* (- kx ix)))
                        (ix <.. (1+ kx) *nchunks*))))
        (emic " + ~A*~A"
              *fold-mul*
              (if (cdr lst)
                  (format nil "(~{~A~^ + ~})" lst)
                (car lst)))))
    (emic "~@[ + hi~])"
          (plusp kx))
    (emit (qq "  z[~A$kx$] = lo")))
  (emit (qq "  z[0] += Int64(~A$*wrap-mul*$*hi)")))

;; =========================================
#|
(with-output-to-string (*emit-port*)
  (with-ed-curve :curve1174
    (let ((*modulus* *ed-q*)
          (*nb*      51))
      (gen-gsqr))))
|#
;; =========================================

(def-gen gsqr (x y) nil
  (emit (qq #>.end
  // x * x -> y
  // x, y are 64-bit ~A$*nchunks*$-element little-endian vectors
  // each element containing a ~A$*nb*$-bit fragment
  // products are 64x64->128-bit results
  // split128(nb,x) -> (hi, lo) returns low nb bits of x in Int64,
  // and hi (128-nb) bits in Int128
.end))
  (dotimes (kx *nchunks*)
    (emit (qq "  let x~A$kx$ = Int128(x[~A$kx$])")))
  (labels ((format-term (ix iy)
             (format nil "x~A*x~A" ix iy))
           (format-terms (lst)
             (if (cdr lst)
                 (format nil "(~{~A~^ + ~})" lst)
               (car lst))))
    (dotimes (kx *nchunks*)
      (emit "  let (hi, lo) = split128(~A, "
            (if (< kx (1- *nchunks*))
                *nb*
              *nb-last*))
      (let ((lst1 nil)
            (lst2 nil)
            (lst3 nil)
            (lst4 nil))
        (do ((ix 0 (1+ ix)))
            ((> ix kx))
          (let ((iy (- kx ix)))
            (cond ((= ix iy)
                   (setf lst1 (format-term ix ix)))
                  ((< ix iy)
                   (push (format-term ix iy) lst2))
                  (t (return))
                  )))
        (do ((ix (1+ kx) (1+ ix)))
            ((>= ix *nchunks*))
          (let ((iy (+ *nchunks* (- kx ix))))
            (cond ((= ix iy)
                   (setf lst3 (format-term ix ix)))
                  ((< ix iy)
                   (push (format-term ix iy) lst4))
                  (t (return))
                  )))
        (let ((lst nil))
          (when lst4
            (push (format nil "~A*~A"
                          (* 2 *fold-mul*)
                          (format-terms (nreverse lst4)))
                  lst))
          (when lst3
            (push (format nil "~A*~A"
                          *fold-mul*
                          lst3)
                  lst))
          (when lst2
            (push (format nil "2*~A"
                          (format-terms (nreverse lst2)))
                  lst))
          (when lst1
            (push lst1 lst))
          (emic "~{~A~^ + ~}~@[ + hi~])"
                lst
                (plusp kx))))
      (emit (qq "  y[~A$kx$] = lo"))
      ))
  (emit (qq "  y[0] += Int64(~A$*wrap-mul*$*hi)")))

;; ---------------------------------------------------------------
;; Handle generation of exponentiation primitives: ginv, gsqrt

(defun longest-run (x)
  ;; assumes exponent x is of the form ((2^pow)*(2^N-1)+scrap)
  ;; e.g., 0x1fffff5, then N = 21, pow = 4, scrap = 5
  ;; return bit-length of longest run, pow of 2 for longest run,
  ;; and final exponent scrap (runlen, pow, scrap)
  (um:nlet iter ((xx  x)
                 (tl  0)
                 (pos 0)
                 (ans nil))
    (if (= (integer-length xx)
           (logcount xx))
        `(,(integer-length xx) ,pos ,tl)
      ;; else
      (go-iter (ash xx -1)
               (dpb (ldb (byte 1 0) xx)
                    (byte 1 pos) tl)
               (1+ pos)
               ans))))

#|
(with-ed-curve :curve1174
  ;; (longest-run (- *ed-q* 2))
  (longest-run (ash (1+ *ed-q*) -2)))

(with-ed-curve :curve1174
  ;; (longest-run (- *ed-q* 2))
  (longest-run (- *ed-q* 2)))
|#

;; =========================================
#|
(with-output-to-string (*emit-port*)
  (with-ed-curve :curve1174
    (let ((*modulus* *ed-q*)
          (*nb*      51))
      (gen-gsqrt))))
|#
;; =========================================

(defun inner-gen-exp (exp)
  ;; WARNING: this code assumes that exp = ((2^N - 1)^(1+log2 x) * x)
  ;; for some large N and small x.
  ;; In other words, an exp of the form ((FFFFFFF...FFFF << n) | x)
  (let* ((vtype (get-vtype)))
    (destructuring-bind (runlen pow scrap) (longest-run exp)
      ;; setup the power-of-two vars
      (emit (qq #>.end
  ~A$vtype$ tmp1, tmp2, ans

  // generate large run components
  // bits_1 = x^(2^1-1) = x
  ~A$vtype$ bits_2 // = x^(2^2-1)
  gsqr(x, tmp1)
  gmul(x, tmp1, bits_2)
.end))
      (let* ((log2-runlen (integer-length runlen))
             (var         "bits_2")
             (ans         nil))
        (do ((kx 2 (1+ kx)))
            ((>= kx log2-runlen))
          (let* ((wid      (ash 1 kx))
                 (new-var  (format nil "bits_~A" wid)))
            (emit (qq "  ~A$vtype$ ~A$new-var$  // = x^(2^~A$wid$-1)"))
            (emit (qq "  gcopy(~A$var$, tmp1)"))
            (cond ((<= wid 4)
                   (emit "  gsqr(tmp1, tmp2)")
                   (emit "  gsqr(tmp2, tmp1)"))
                  
                  (t
                   (emit "  dotimes ~A {" (ash wid -2))
                   (emit "    gsqr(tmp1, tmp2)")
                   (emit "    gsqr(tmp2, tmp1) }")))
            (emit (qq "  gmul(~A$var$, tmp1, ~A$new-var$)~&"))
            (setf var new-var)))
        
        ;; generate the partial answer
        (emit "  // generate partial answer")
        (emit (qq "  // we need x^(2^~A$runlen$-1)"))
        (setf ans var)
        (um:nlet iter ((pos  (- log2-runlen 2)))
          (unless (zerop pos)
            (let* ((wid     (ash 1 pos))
                   (new-var (qq "bits_~A$wid$")))
              (setf var new-var)
              (when (logbitp pos runlen)
                (emit (qq "  gcopy(~A$ans$, tmp1)"))
                (cond ((< wid 4)
                       (emit "  gsqr(tmp1, tmp2)")
                       (emit "  gsqr(tmp2, tmp1)"))
                      
                      (t 
                       (emit "  dotimes ~A {" (ash wid -1))
                       (emit "    gsqr(tmp1, tmp2)")
                       (emit "    gsqr(tmp2, tmp1) }")))
                (emit (qq "  gmul(~A$new-var$, tmp1, ans)~&"))
                (setf ans "ans"))
              (go-iter (1- pos)))))
        (when (oddp runlen)
          (emit "  gsqr(ans, tmp1)")
          (emit "  gmul(x, tmp1, ans)~&"))
        
        ;; finish off the answer
        (emit "  // generate small residue of exponent")
        (um:nlet iter ((pos pow))
          (when (plusp pos)
            (emit "  gsqr(ans, tmp1)")
            (cond ((logbitp (1- pos) scrap)
                   (emit "  gmul(x, tmp1, ans)"))
                  (t
                   (emit "  gcopy(tmp1, ans)")))
            (go-iter (1- pos))))
        ))))

(def-gen ginv (x) nil
  (let* ((exp  (- *modulus* 2)))
    (emit (qq #>.end
  // 1/x -> x
  //
  // By Fermat's theorem, for prime |Fq|,
  // we have 1/X = X^(|Fq|-2) mod |Fq|
  // where (|Fq|-2) = 0x~A$(hex-str exp)$
.end))
    (inner-gen-exp exp)
    (emit #>.end
  // copy ans to output x
  gcopy(ans, x).end)
    ))

(def-gen gsqrt (x y) bool 
  (assert (eq 3 (logand *modulus* 3))) ;; ensure that |Fq| mod 4 = 3
  (let* ((exp  (ash (1+ *modulus*) -2)))
    (emit (qq #>.end
  // Sqrt(x) -> y
  //
  // By Fermat's theorem, for prime |Fq|, |Fq| mod 4 = 3
  // when X is a quadratic residue in the field,
  // we have Sqrt(X) = X^((|Fq|+1)/4) mod |Fq|
  // where (|Fq|+1)/4 = 0x~A$(hex-str exp)$
  // Return true if X was a quadratic-residue of Fq.
.end))
    (inner-gen-exp exp)
    (emit #>.end
  // copy ans to output y
  // and return true if y*y = x
  gcopy(ans, y)
  gsqr(y, tmp1)
  return geq(tmp1, x).end)
    ))

;; ----------------------------------------------------------------------
;; Byte vector conversions (little-endian)

(defun gen-to-byte-vec ()
  (with-chunk-parameters
    (let* ((nwds  (ceiling (integer-length *modulus*) 64))
           (vtype (get-vtype)))
      (emit (qq #>.end
fn to_byte_vec(~A$vtype$ x, u64 v[~A$nwds$]) {
  // convert Fq format into little-endian UInt64 vector form
  ~A$vtype$ tmp
  gnorm(x, tmp).end))
      (dotimes (kx (1- *nchunks*))
        (emit (qq "  dpb(tmp[~A$kx$], v, ~A$*nb*$, ~A$(* kx *nb*)$)")))
      (emit (qq #>.end
  dpb(tmp[~A$(1- *nchunks*)$], v, ~A$*nb-last*$, ~A$(* *nb* (1- *nchunks*))$)
}
.end)))))

(defun gen-from-byte-vec ()
  (with-chunk-parameters
    (let* ((nwds  (ceiling (integer-length *modulus*) 64))
           (vtype (get-vtype)))
      (emit (qq #>.end
fn from_byte_vec(u64 v[~A$nwds$], ~A$vtype$ x) {
  // convert little-endian UInt64 vector to Fq format.end))
      (dotimes (kx (1- *nchunks*))
        (emit (qq "  x[~A$kx$] = ldb(v, ~A$*nb*$, ~A$(* kx *nb*)$)")))
      (emit (qq #>.end
  x[~A$(1- *nchunks*)$] = ldb(v, ~A$*nb-last*$, ~A$(* *nb* (1- *nchunks*))$)
}
.end)))))

#|
(with-ed-curve :curve1174
  (gen-inv *ed-q* 51))

(with-ed-curve :curve1174
  (gen-sqr *ed-q* 42))

(with-ed-curve :curve-ed3363
  (gen-mpy *ed-q* 56))

(with-ed-curve :curve-e382
  (gen-mpy *ed-q* 48))

(with-ed-curve :curve41417
  (gen-mpy *ed-q* 52))

(with-ed-curve :curve-e521
  (gen-mpy *ed-q* 58))

;; ---------------------------------------------------------------------------
;; looking for optimal short-word encoding for use in Karatsuba multiplication
(with-ed-curve
    :curve1174
    ;; :curve-ed3363
    ;; :curve-e382
    ;; :curve41417
    ;; :curve-ed448 ;; this one is horrible for this kind of algorithm...
    ;; :curve-e521
  (let* ((base *ed-q*)
         (ntot (integer-length base)))
    (format t "~%NBits = ~A" ntot)
    (format t "~%  nchunks   wrmul")
    (format t "~%nb     nlast      wrfinal")
    (loop for nb from 24 to 64 do
          (multiple-value-bind (nchunks rem) (ceiling ntot nb)
            (let* ((nlast (+ nb rem))
                   (m     (ash 1 nb))
                   (wrmul (with-mod base
                            (m^ m nchunks)))
                   (wrfinal (with-mod base
                              (mmod (ash 1 ntot)))))
              (format t "~%~2D  ~2,' D  ~2,' D  ~A  ~A" nb nchunks nlast wrmul wrfinal))))))

;; ---------------------------------------------------------------------------------------

(defun longest-run (x)
  (um:nlet iter ((xx x)
                 (ans nil))
    (if (= (integer-length xx)
           (logcount xx))
        (let* ((new-ans (cons (integer-length xx) ans)))
          (list (nreverse new-ans) (hex-str x)))
      ;; else
      (let* ((ct  0)
             (xx  (um:nlet iter0 ((xx xx))
                    (if (oddp xx)
                        (progn
                          (incf ct)
                          (go-iter0 (ash xx -1)))
                      xx))))
        (if (zerop ct)
            (go-iter (ash xx -1) ans)
          (gp-iter (ash xx -1) (cons ct ans))))
      )))

(with-ed-curve :curve1174
  (longest-run (- *ed-q* 2))) ;; for inv

(with-ed-curve :curve1174
  (longest-run (ash (1+ *ed-q*) -2))) ;; for sqrt

(with-ed-curve :curve-e382
  (longest-run (- *ed-q* 2))) ;; for inv

(with-ed-curve :curve-e382
  (longest-run (ash (1+ *ed-q*) -2))) ;; for sqrt

(with-ed-curve :curve41417
  (longest-run (- *ed-q* 2))) ;; for inv

(with-ed-curve :curve41417
  (longest-run (ash (1+ *ed-q*) -2))) ;; for sqrt

;; ---------------------------------------------------
;; halving decompositions for runs of 1's in exponent

(defun gen-sqr-seq (nrun)
  (um:nlet iter ((ans  (list nrun))
                 (nrun nrun))
    (if (< nrun 2)
        ans
      (let ((hrun (ash nrun -1)))
        (if (oddp nrun)
            (go-iter (cons hrun (cons 1 ans)) hrun)
          (go-iter (cons hrun ans) hrun)))
      )))

(gen-sqr-seq 248)
(gen-sqr-seq 247)

;; ------------------------------------------------------
;; getting to a run of 248
;; -- binary decomposition
;; = 247 squarings, 11 mults 
(let* ((bits2 (* x (sqr x)))
       (bits4 (* bits2 (dotimes 2 (sqr bits2))))
       (bits8 (* bits4 (dotimes 4 (sqr bits4))))
       (bits16 (* bits8 (dotimes 8 (sqr bits8))))
       (bits32 (* bits16 (dotimes 16 (sqr bits16))))
       (bits64 (* bits32 (dotimes 32 (sqr bits32))))
       (bits128 (* bits64 (dotimes 64 (sqr bits64))))
       (bits192 (* bits64 (dotimes 64 (sqr bits128))))
       (bits224 (* bits32 (dotimes 32 (sqr bits192))))
       (bits240 (* bits16 (dotimes 16 (sqr bits224))))
       (bits248 (* bits8 (dotimes 8 (sqr bits240))))
       ))

;; -- halving decomposition
;; = 247 squarings, 11 mults
(let* ((bits2 (* x (sqr x)))
       (bits3 (* x (sqr bits2)))
       (bits6 (* bits3 (dotimes 3 (sqr bits3))))
       (bits7 (* x (sqr bits6)))
       (bits14 (* bits7 (dotimes 7 (sqr bits7))))
       (bits15 (* x (sqr bits14)))
       (bits30 (* bits15 (dotimes 15 (sqr bits15))))
       (bits31 (* x (sqr bits30)))
       (bits62 (* bits31 (dotimes 31 (sqr bits31))))
       (bits124 (* bits62 (dotimes 62 (sqr bits62))))
       (bits248  (* bits124 (dotimes 124 (sqr bits124))))
       ))

;; end result - identical ops count, regardless
;; seems simpler to just use binary decomp. All squarings are even.
;; ------------------------------------------------------
|#

#|
(qq "this is ~A\$(list 1 2 3)$ a test")
                     
(list #>.end
I'm a string
... and "so" am I
.end)

(defun gen-to-byte-vec ()
  (with-chunk-parameters
    (let* ((nwds  (ceiling (integer-length *modulus*) 64))
           (vtype (get-vtype)))
      (emit (qq #>.end
fn to_byte_vec(~A$vtype$ x, u64 v[~A$nwds$]) {
  // convert Fq format into little-endian u64 vector form
  ~A$vtype$ tmp
  gnorm(x, tmp).end))
      (dotimes (kx (1- *nchunks*))
        (emit (qq #>.end
  dpb(tmp[~A$kx$], v, ~A$*nb*$, ~A$(* kx *nb*)$).end)))
      (emit (qq #>.end
  dpb(tmp[~A$(1- *nchunks*)$], v, ~A$*nb-last*$, ~A$(* *nb* (1- *nchunks*))$)
}
.end))
      )))

(defun doit ()
  (with-ed-curve :curve1174
    (let ((*modulus* *ed-q*)
          (*nb*      51))
      (with-output-to-string (*emit-port*)
        (gen-gtype)))))

(doit)


|#
