;; utilities.lisp
;; DM/Acudora  11/11
;; -------------------------------------------------
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

(in-package :ecc-crypto-b571)



;;;; Stubs for Unimplemented Functions

;;; DEFSTUB: define a stub function named FUNCTION-NAME and set property
;;; STUB-FUNCTION-P true on FUNCTION-NAME. The property setting is done at both
;;; macro-evaluation and macro-expansion times, allowing its value to be used
;;; both at compile and load times.

;;; There are a set of functions that, for purposes of code clarity, need to
;;; exist in regularly compiled code, but which shall, for the forseeable
;;; future, never actually be called and therefore need not be defined.  We are
;;; free, however, to define them as "stub" functions. A `stub function' should
;;; never be called. If it is called at runtime, its behavior is undefined in
;;; production. (It's OK for it to behave the same as in development, but that
;;; is not required and should not be relied upon.)  In development, it's highly
;;; desireable that calling a stub function should result in a runtime error
;;; being signaled.

;;; The main purpose and benefit of using defstub is to prevent the compiler
;;; from complaining about unimplemented functions every single compile, when
;;; you have no intention of ever fixing the situation in the present
;;; development period.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro stub-function-p (function-name)
  "Accessor on FUNCTION-NAME (getable, setf'able). Value either true, if
FUNCTION-NAME is a symbol that is the name of a stub function, or false (nil)
for any other symbol."
  `(get ,function-name 'stub-function-p))
)
  

(defmacro defstub (function-name)
  (unless (fboundp function-name)
    (setf (stub-function-p function-name) t) ; set both at compile and load time
    `(progn
       (setf (stub-function-p ',function-name) t)
       (defun ,function-name (&rest args)
         (declare (ignore args))
         (error "~s, a stub function, called at run time, but it should not be."
                ',function-name))
       ',function-name)))

;; -----------------------------------------------------------------------------
;; with-fast-impl (macro)

(defmacro error-running-fast-impl-function? (fast-name)
  "Accessor on a fast-impl-function name (getable, setf'able). Value can either
be nil (initially) the Lisp error condition object from a first error condition
from calling the function."
  `(get ',fast-name 'error-running-fast-impl-function))

(defun do-with-fast-impl (fast-name fast-fn slow-fn)
  (or (and (null (error-running-fast-impl-function? fast-name))
           (handler-case
               (funcall fast-fn)
             (error (error-condition)
               (progn
                 ;; Throw a bone to a developer tracking down the error: log to
                 ;; error output, and store error condition in a property on the
                 ;; function name symbol.
                 (format *error-output*
                         "!!! *** Taking function ~S out. *** !!!~%" 
                         fast-name)
                 (format *error-output*
                         "!!! ***   Error condition = ~A *** !!!~%" 
                         error-condition)
                 (format *error-output*
                         "!!! ***   Error condition type = ~S *** !!!~%"
                         (type-of error-condition)))

               ;; Consider enabling this, maybe just in development mode:
               ;; (cerror "Continue" "Error on fast-impl call of ~S" fast-name)

               (setf (error-running-fast-impl-function? fast-name)
                     error-condition)
               nil)))
      (funcall slow-fn)))

(defmacro with-fast-impl (fast-form slow-form)
  (let ((fast-name (car fast-form)))
    (if (stub-function-p fast-name)
        ;; If at expansion time we already know FAST-NAME names a stub function,
        ;; do not expand a call to it: simply emit SLOW-FORM straight inline.
        slow-form
        `(do-with-fast-impl ',fast-name
           (lambda ()
             ,fast-form)
           (lambda ()
             ,slow-form)) )))
#|
(defmacro with-fast-impl (fast-form slow-form)
  slow-form)
  
 |#

#+:LISPWORKS
(defun fast-sha2-file (fname)
  (fli:with-dynamic-foreign-objects ()
    (let ((carr (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 32))
          (ans  (make-ub-array 32)))
      (unless (zerop (sha2_file (namestring fname) carr))
        (error "File error"))
      (loop for ix from 0 below 32 do
            (setf (aref ans ix)
                  (fli:dereference carr :index ix)))
      ans)))

#+:LISPWORKS
(defun fast-shad2-file (fname)
  (fli:with-dynamic-foreign-objects ()
    (let ((carr (fli:allocate-dynamic-foreign-object
                 :type :uint8 :nelems 32))
          (ans  (make-ub-array 32)))
      (unless (zerop (shad2_file (namestring fname) carr))
        (error "File error"))
      (loop for ix from 0 below 32 do
            (setf (aref ans ix)
                  (fli:dereference carr :index ix)))
      ans)))

(defun shad2-file (fname)
  (with-fast-impl
   (fast-shad2-file fname)
   (let ((dig (ironclad:make-digest :sha256))
         (pre (make-ub-array 64
                             :initial-element 0)))
     (safe-update-digest dig pre)
     (ironclad:digest-file dig fname)
     (let ((h  (ironclad:produce-digest dig)))
       (reinitialize-instance dig)
       (safe-update-digest dig h)
       (ironclad:produce-digest dig)))))

;; --------------------------------------------
;; ECC point representations -- affine and projective

(defclass ecc-infinity ()
  ())

(defvar +ecc-inf+
  (make-instance 'ecc-infinity))

;; -----------------------------

(defstruct ecc-pt
  x y)

(defclass ecc-projective-pt ()
  ((x  :accessor ecc-projective-pt-x  :initarg :x)
   (y  :accessor ecc-projective-pt-y  :initarg :y)
   (z  :accessor ecc-projective-pt-z  :initarg :z)))

(defmethod ecc-projective-pt-p ((pt ecc-projective-pt))
  t)

(defmethod ecc-projective-pt-p (pt)
  nil)
  

#-:COM.RAL
(defstub gf-random-k*)

(defun make-ecc-projective-pt (&key x y (z 1) alpha)
  (let* ((alpha   (or alpha
                      (gf-random-k*)))
         (alpha^2 (gf^2 alpha)))
    (make-instance 'ecc-projective-pt
                   :x (gf* alpha^2 x)
                   :y (gf* alpha alpha^2 y)
                   :z (gf* alpha z))))

;; ---------------------------------------------------------------------------------
;; ECC Curve Definition

(defvar *curve*  nil)

(define-symbol-macro *ecc-a*   (ecc-curve-a     *curve*))
(define-symbol-macro *ecc-b*   (ecc-curve-b     *curve*))
(define-symbol-macro *ecc-gen* (ecc-curve-gen   *curve*))
(define-symbol-macro *ecc-h*   (ecc-curve-h     *curve*))
(define-symbol-macro *ecc-r*   (ecc-curve-r     *curve*))
(define-symbol-macro *nbits*   (ecc-curve-nbits *curve*))
(define-symbol-macro $prim     (ecc-curve-gf    *curve*))
(define-symbol-macro *ecc-d*   (ecc-curve-d     *curve*))
(define-symbol-macro *ecc-s*   (ecc-curve-s     *curve*))
(define-symbol-macro *ecc-e*   (ecc-curve-e     *curve*))

;; for curves of the form:  y^2 + x*y = x^3 + a*x^2 + b
(defstruct ecc-curve
  (name    :anon)
  (parent  *curve*)
  (a       *ecc-a*)    ;; coeff of x^2
  (b       *ecc-b*)    ;; coeff of unity
  (gen     *ecc-gen*)  ;; generator point on curve
  (h       *ecc-h*)    ;; cofactor
  (r       *ecc-r*)    ;; prime order
  (nbits   *nbits*)    ;; GF nbits
  (gf      $prim)      ;; GF polynomial
  (d       *ecc-d*)    ;; random curve gen multiple
  (s       *ecc-s*)    ;; random curve translation
  (e       *ecc-e*))   ;; random curve Frobenius rotation expon

(defmacro with-ecc-curve (curve &body body)
  ;; perform body over iosmorphic curve
  `(let ((*curve* ,(if (and (consp curve)
                            (keywordp (car curve)))
                       `(make-ecc-curve ,@curve)
                     curve)))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-ecc-curve" 1)

;; -----------------------------------------------------------

(defvar *nist-b571*
  (setf *curve*
        (make-ecc-curve
         :name "NIST-B571"
         :parent nil
         :nbits 571
         :gf    (logior (ash 1 571) #x425)
         :a     1
         :b     (big32 #x02F40E7E #x2221F295 #xDE297117 #xB7F3D62F
                       #x5C6A97FF #xCB8CEFF1 #xCD6BA8CE #x4A9A18AD
                       #x84FFABBD #x8EFA5933 #x2BE7AD67 #x56A66E29
                       #x4AFD185A #x78FF12AA #x520E4DE7 #x39BACA0C
                       #x7FFEFF7F #x2955727A )
         :gen   (make-ecc-pt
                 :x 
                 (big32 #x0303001D #x34B85629 #x6C16C0D4 #x0D3CD775
                        #x0A93D1D2 #x955FA80A #xA5F40FC8 #xDB7B2ABD
                        #xBDE53950 #xF4C0D293 #xCDD711A3 #x5B67FB14
                        #x99AE6003 #x8614F139 #x4ABFA3B4 #xC850D927
                        #xE1E7769C #x8EEC2D19 )
                 :y
                 (big32 #x037BF273 #x42DA639B #x6DCCFFFE #xB73D69D7
                        #x8C6C27A6 #x009CBBCA #x1980F853 #x3921E8A6
                        #x84423E43 #xBAB08A57 #x6291AF8F #x461BB2A8
                        #xB3531D2F #x0485C19B #x16E2F151 #x6E23DD3C
                        #x1A4827AF #x1B8AC15B ))
         :h     2
         ;; prime order of the base point
         ;; (r * gen = infinity )
         :r     (big32 #x03FFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
                       #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF
                       #xFFFFFFFF #xE661CE18 #xFF559873 #x08059B18
                       #x6823851E #xC7DD9CA1 #x161DE93D #x5174D66E
                       #x8382E9BB #x2FE84E47 )
         :d     1
         :s     0
         :e     0)))
  
(defmacro with-b571 (&body body)
  `(with-ecc-curve *nist-b571*
     ,@body))

;; -----------------------------------------------------------

(defvar *nist-b163*
  (make-ecc-curve
   :name "NIST-B163"
   :parent nil
   :nbits 163
   :gf    #x0800000000000000000000000000000000000000C9
   :h     2
   :a     1
   :b     #x020A601907B8C953CA1481EB10512F78744A3205FD
   :r     #x040000000000000000000292FE77E70C12A4234C33
   :gen   (make-ecc-pt
           :x #x03F0EBA16286A2D57EA0991168D4994637E8343E36
           :y #x00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1)))

(defmacro with-b163 (&body body)
  `(with-ecc-curve *nist-b163*
     ,@body))

;; factors of (2^163-1)
;; 150287 * 704161 * 110211473 * 27669118297 * 36230454570129675721  (5 distinct prime factors)

;; -----------------------------------------------------------

#-:diddly
(let ((prev nil))
  (defun make-nonce ()
    (let ((next (do ((next (usec:get-universal-time-usec)
                           (usec:get-universal-time-usec)))
                    ((not (eql next (shiftf prev next))) next)
                  (sleep 0))))
      (convert-int-to-nbytesv (ash next 64) 16))))

#+:diddly
(let ((prev nil))
  (defun make-nonce ()
    (let ((next (do ((next (uuid:make-v1-uuid)
                           (uuid:make-v1-uuid)))
                    ((not (eql next (shiftf prev next))) next)
                  (sleep 0))))
      (convert-int-to-nbytesv (uuid:uuid-to-integer next) 16))))

;; --------------------------------------------------------
;; Obfuscated private key multiplication
#|
(defun ecc-stitch (r lst)
  (let ((a (ecc-mul r (car lst))))
    (if (endp (cdr lst))
        a
      (ecc-add a (ecc-stitch a (cdr lst))))))

(defun my-eval (e)
  (funcall (compile nil `(lambda () ,e))))

(defun list-stitch-factors (key levels)
  (um:nlet iter ((e   (primes:decompose key))
                 (lvl levels))
    ;; e is an expression '(* factor1 (1+ (* factor2 ....)))
    ;; where each factor is a number
    (if (zerop lvl)
        (list (eval e))
      (destructuring-bind (star factor (oneplus subexpr)) e
        (assert (eq '* star))
        (assert (eq '1+ oneplus))
        (cons (+ *ecc-r* factor)
              (iter subexpr (1- lvl)))) )))

(defmacro ecc-mul-kpriv (arg key &key (levels 7))
  ;; key must be a number, not an expression
  `(ecc-stitch ,arg ',(list-stitch-factors key levels)))
|#
;; -------------------------------------------------------

(defun ctr-drbg-int (nbits)
  (convert-bytes-to-int (ctr-drbg nbits)))

(defun random-between (lo hi)
  ;; random number in interval [lo,hi)
  (let ((rng  (abs (- hi lo)))
        (lmin (min hi lo)))
    (+ lmin (mod (ctr-drbg-int (integer-length rng))
                 rng))))


(defun read-point (stream)
  (let* ((x (read-int 72 stream))
         (y (read-int 72 stream)))
    (make-ecc-pt
     :x x :y y)))

(defun write-point (pt stream)
  (write-int (ecc-pt-x pt) 72 stream)
  (write-int (ecc-pt-y pt) 72 stream))


