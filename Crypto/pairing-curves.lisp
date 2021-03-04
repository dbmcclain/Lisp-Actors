;; pairing-curves.lisp -- PBC (Pairing Based Crypto) in Lisp
;;
;; DM/Emotiq 03/18
;; ---------------------------------------------------------
#|
Copyright (c) 2018 Emotiq AG

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

(defpackage :pairing-curves
  (:use :common-lisp :core-crypto)
  (:import-from :ecc-crypto-b571
   :encode-bytes-to-base58
   :decode-bytes-from-base58
   :convert-int-to-lev
   :convert-lev-to-int
   :ecc-pt
   :ecc-pt-x
   :ecc-pt-y
   :make-ecc-pt
   :ecc-infinity
   :ecc-infinite-p)
  (:export
   ))

(in-package :pairing-curves)

#|
 ;; pbc calculator built-in type A curve
 type a
q 8780710799663312522437781984754049815806883199414208211028653399266475630880222957078625179422662221423155858769582317459277713367317481324925129998224791
h 12016012264891146079388821366740534204802954401251311822919615131047207289359704531102844802183906537786776
r 730750818665451621361119245571504901405976559617
exp2 159
exp1 107
sign1 1
sign0 1

;; Type A Curves:  y^2 = x^3 + x
;; #E = q+1 = r*h
;; r,q prime
 
bash-3.2$ ./genaparam 255
type a
q 5036299105055989263705760388976217680146814178386359717202330155652680389956869099528841486537591502720081292954358942237870697577809642385501659410819551
h 173977311860539309503775690778056252529516468861492431232178812843604016982496
r 28948022309329048855892746252171976963317496168945442210320854804971688820737
exp2 254
exp1 101
sign1 1
sign0 1
|#

;; ------------------------------------------------

;; for general curves:  y^2 = x^3 + a*x + b
;; this always describes the smaller curve in the pairing
(defclass pairing-curve ()
  ((name  :reader curve-name
          :initarg :name)
   (q     :reader curve-q   ;; prime nbr field of curve embeddings
          :initarg :q)
   (h     :reader curve-h   ;; cofactor of field order #E(q) = h*r
          :initarg :h)
   (r     :reader curve-r   ;; prime nbr order of curve
          :initarg :r)
   (gen   :reader curve-gen ;; a generator for the curve field
          :initarg :gen)
   (a     :reader curve-a   ;; the "a" coefficient
          :initarg :a)
   (b     :reader curve-b   ;; the "b" coefficient
          :initarg :b)
   ))

;; ------------------------------------------------

;; Type A curves are symmetric super-singular curves of specified order
;; in a specified group order q prime.
;; Curve order r prime, #E = r*h = q+1, embedding order 2
;;
;; NOTE: Type A curves *do not* admit Elligator encodings
;; Find another way to hash onto the curves.
;;
(defclass type-a-curve (pairing-curve)
  ((exp2  :reader a-curve-exp2
          :initarg :exp2)
   (exp1  :reader a-curve-exp1
          :initarg :exp1)
   (sign1 :reader a-curve-sign1
          :initarg :sign1)
   (sign0 :reader a-curve-sign0
          :initarg :sign0)
   ))

(defun make-type-a-curve (&key name q h r exp2 exp1 sign1 sign0 gen)
  (make-instance 'type-a-curve
                 :name  name
                 :q     q
                 :h     h
                 :r     r
                 :exp2  exp2
                 :exp1  exp1
                 :sign1 sign1
                 :sign0 sign0
                 :a     1
                 :b     0
                 :gen   gen))

;; -----------------------------------------------------------

(defvar *curve-abi* ;; type-A built-in for Lynn pbc calculator
  (make-type-a-curve
   :name :curve-abi
   :q 8780710799663312522437781984754049815806883199414208211028653399266475630880222957078625179422662221423155858769582317459277713367317481324925129998224791
   :h 12016012264891146079388821366740534204802954401251311822919615131047207289359704531102844802183906537786776
   :r 730750818665451621361119245571504901405976559617
   ;; r = 2^159 + 2^107 + 1
   :exp2 159
   :exp1 107
   :sign1 1
   :sign0 1
   :gen   (make-ecc-pt
           :x 1085347653691114018972006336187618998613923775991612770130958843233832061513998578755891198152730208317951419265486990973218324306254948172336665411679012
           :y 5599899688322962665611418843882987388208473595264048707667621381251045700647787821253014386586626556027211013107464727177624911057601367551143961577352973)
   ))
  
(defvar *curve-ar255*  ;; from PBC gen/genaparam 255 512
  (make-type-a-curve
   :name :curve-ar255
   :q 5036299105055989263705760388976217680146814178386359717202330155652680389956869099528841486537591502720081292954358942237870697577809642385501659410819551
   :h 173977311860539309503775690778056252529516468861492431232178812843604016982496
   :r 28948022309329048855892746252171976963317496168945442210320854804971688820737
   ;; r = 2^254 + 2^101 + 1
   :exp2 254
   :exp1 101
   :sign1 1
   :sign0 1
   :gen   (make-ecc-pt
           :x  1670350915294576336135542448772382191352705932979614385914184406042794984249509825487657536280578633692689552681105232425651809296431578628586051292694500
           :y  4095933906315758149660369888122401239832052680723825266798745051638712789300999337263039036349056991025424664947241913315610460154093779718285233982939645)
   ))

;; ---------------------------------------------------------------
#|
  y^2 = x^3 + b
  bash-3.2$ ./genfparam 256
type f
q 16283262548997601220198008118239886027035269286659395419233331082106632227801
r 16283262548997601220198008118239886026907663399064043451383740756301306087801
b 10476541659213232777352255224319706265440471807344192411073251777589416636392
beta 2588849289436542488537732220497504302700946308066126767616133606209888506551
alpha0 15760619495780482509052463852330180194970833782280957391784969704642983647946
alpha1 3001017353864017826546717979647202832842709824816594729108687826591920660735
|#

;; Type F curves have embedding degree 12, are asymmetric pairings
;; NOTE: Type F curves *do not* admit Elligator encodings. Find another
;; way to hash onto the curves.
(defclass type-f-curve (pairing-curve)
  ((beta   :reader f-curve-beta
           :initarg :beta)
   (alpha0 :reader f-curve-alpha0
           :initarg :alpha0)
   (alpha1 :reader f-curve-alpha1
           :initarg :alpha1)
   ))

(defun make-type-f-curve (&key name q r b gen beta alpha0 alpha1)
  (make-instance 'type-f-curve
                 :name   name
                 :q      q
                 :h      (floor q r)
                 :r      r
                 :gen    gen
                 :a      0
                 :b      b
                 :beta   beta
                 :alpha0 alpha0
                 :alpha1 alpha1))

(defvar *curve-fr256*
  (make-type-f-curve
   :name   :curve-fr256
   :q      16283262548997601220198008118239886027035269286659395419233331082106632227801
   :r      16283262548997601220198008118239886026907663399064043451383740756301306087801
   :b      10476541659213232777352255224319706265440471807344192411073251777589416636392
   :beta   2588849289436542488537732220497504302700946308066126767616133606209888506551
   :alpha0 15760619495780482509052463852330180194970833782280957391784969704642983647946
   :alpha1 3001017353864017826546717979647202832842709824816594729108687826591920660735
   :gen    (make-ecc-pt
            :X 2325573746486681575455282670974405244147310517580295217847300176234938026233
            :Y 4732557668433558897603528221664551698715643461094236320837335461646552377369)
   ))

;; ---------------------------------------------------------------------

(defvar *curve*  *curve-fr256*)

(define-symbol-macro *a*       (curve-a     *curve*))
(define-symbol-macro *b*       (curve-b     *curve*))
(define-symbol-macro *q*       (curve-q     *curve*))
(define-symbol-macro *r*       (curve-r     *curve*))
(define-symbol-macro *h*       (curve-h     *curve*))
(define-symbol-macro *gen*     (curve-gen   *curve*))
(define-symbol-macro *name*    (curve-name  *curve*))

;; ------------------------------------------------------

(defvar *known-pairing-curves*
  (list *curve-abi* *curve-ar255* *curve-fr256*))

(defmethod select-curve ((curve pairing-curve))
  curve)

(defmethod select-curve ((curve symbol))
  (find curve *known-pairing-curves*
        :key 'curve-name))

(defmacro with-curve (curve &body body)
  `(let ((*curve* (select-curve ,curve)))
     ,@body))

(defun pairing-curves ()
  ;; present caller with a list of symbols that can be used to select
  ;; a curve using WITH-CURVE
  (mapcar 'curve-name *known-pairing-curves*))

;; ----------------------------------------------------------------

(defmethod pt= ((p1 ecc-infinity) (p2 ecc-infinity))
  t)

(defmethod pt= ((p1 ecc-pt) (p2 ecc-pt))
  (with-mod *q*
    (and (zerop (m- (ecc-pt-x p1)
                    (ecc-pt-x p2)))
         (zerop (m- (ecc-pt-y p1)
                    (ecc-pt-y p2)))
         )))

(defmethod pt= (p1 p2)
  nil)

;; -------------------------------------------

(defmethod pt-negate ((pt ecc-infinity))
  pt)

(defmethod pt-negate ((pt ecc-pt))
  (with-mod *q*
    (make-ecc-pt
     :x  (ecc-pt-x pt)
     :y  (m- (ecc-pt-y pt)))
    ))

;; ---------------------------------------
;; Faster doubling for pt-mul using Jacobi projective coords
#|
(defstruct proj-pt
  x y z)

(defun to-proj-pt (pt)
  (make-proj-pt
   :x (ecc-pt-x pt)
   :y (ecc-pt-y pt)
   :z 1))

(defmethod to-affine-pt ((pt proj-pt))
  (with-mod *q*
    (let* ((1/z   (m/ (proj-pt-z pt)))
           (1/z^2 (m* 1/z 1/z)))
      (make-ecc-pt
       :x  (m* (proj-pt-x pt) 1/z^2)
       :y  (m* (proj-pt-y pt) 1/z^2 1/z))
      )))

(defmethod to-affine-pt ((pt ecc-infinity))
  pt)


(defmethod proj-pt-double ((pt proj-pt))
  (um:bind* ((:struct-accessors proj-pt (x y z) pt)
             (tmpa (m* y y))
             (tmpb (m* 4 x tmpa))
             (tmpc (m* 8 tmpa tmpa))
             (tmpd (m+ (m* 3 x x) (m* *a* z z z z)))
             (x3   (m- (m* tmpd tmpd) (m* 2 tmpb)))
             (y3   (m- (m* tmpd (m- tmpb x3)) tmpc))
             (z3   (m* 2 y z)))
    (if (zerop z3)
        (ecc-infinity)
      (make-proj-pt
       :x  x3
       :y  y3
       :z  z3))
    ))

(defmethod proj-pt-double ((pt ecc-infinity))
  pt)

(defmethod proj-pt-add ((pt1 ecc-infinity) pt2)
  pt2)

(defmethod proj-pt-add (pt1 (pt2 ecc-infinity))
  pt1)

(defmethod proj-pt-add ((pt1 proj-pt) (pt2 proj-pt))
  (um:bind* ((:struct-accessors proj-pt ((x1 x)
                                         (y1 y)
                                         (z1 z)) pt1)
             (:struct-accessors proj-pt ((x2 x)
                                         (y2 y)
                                         (z2 z)) pt2)
             )))
|#              
;; ----------------------------------------------

(defmethod pt-double ((pt ecc-infinity))
  pt)

(defmethod pt-double ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt (x y) pt))
    (with-mod *q*
      (if (zerop (setf y (mmod y)))
          (ecc-infinity)
        (let* ((s  (m/ (m+ *a* (m* 3 x x)) 2 y))
               (x2 (m- (m* s s) x x))
               (y2 (m- (m* s (m- x x2)) y)))
          (make-ecc-pt
           :x x2
           :y y2)))
      )))

(defmethod pt-add ((pt1 ecc-infinity) pt2)
  pt2)

(defmethod pt-add (pt1 (pt2 ecc-infinity))
  pt1)

(defmethod pt-add ((pt1 ecc-pt) (pt2 ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x1 x)
                                        (y1 y)) pt1)
             (:struct-accessors ecc-pt ((x2 x)
                                        (y2 y)) pt2))
    (with-mod *q*
      (let ((den (m- x2 x1))
            (num (m- y2 y1)))
        (cond ((zerop den)
               (if (zerop num)
                   (pt-double pt1)
                 (if (zerop (m+ y1 y2))
                     (ecc-infinity)
                   (error "Shouldn't happen"))))
              (t  (let* ((s  (m/ num den))
                         (x3 (m- (m* s s) x1 x2))
                         (y3 (m- (m* s (m- x1 x3)) y1)))
                    (make-ecc-pt
                     :x  x3
                     :y  y3)
                    ))
              )))))

(defun pt-sub (pt1 pt2)
  (pt-add pt1 (pt-negate pt2)))

#|
(defun pt-mul (pt x)
  (cond ((ecc-infinite-p pt) pt)
        ((zerop x)  (ecc-infinity))
        (t (let ((nbits (integer-length x))
                 (ptsum (ecc-infinity)))
             (do ((p  (to-proj-pt pt) (proj-pt-double p))
                  (ix 0 (1+ ix)))
                 ((>= ix nbits) (to-affine-pt ptsum))
               (when (logbitp ix x)
                 (setf ptsum (proj-pt-add ptsum p)))
               )))
        ))
|#

(defun pt-mul (pt x)
  (cond ((ecc-infinite-p pt) pt)
        ((zerop x)  (ecc-infinity))
        (t (let ((nbits (integer-length x))
                 (ptsum (ecc-infinity)))
             (do ((p  pt (pt-double p))
                  (ix 0  (1+ ix)))
                 ((>= ix nbits) ptsum)
               (when (logbitp ix x)
                 (setf ptsum (pt-add ptsum p)))
               )))
        ))

(defun pt-div (pt x)
  (with-mod *q*
    (pt-mul pt (m/ x))))

;; ---------------------------------------------------------------
;; Curve arithmetic in bent field notation (a* = a-add, a^ = a-mul)
;; Bent field notation seeks to unify the presentation of crypto algorithms
;; between those used for RSA style and ECC.

(defun pt* (pt1 &rest pts)
  (let ((ptans  pt1))
    (dolist (pt pts ptans)
      (setf ptans (pt-add ptans pt)))))

(defun pt^ (pt &rest exps)
  (let ((ptans pt))
    (dolist (x exps ptans)
      (setf ptans (pt-mul ptans x)))))

;; ---------------------------------------------------------------
;; conversion between integers and little-endian UB8 vectors

(defun pt-nbits ()
  (get-cached-symbol-data '*curve*
                          :pt-nbits *curve*
                          (lambda ()
                            (integer-length *q*))))

(defun pt-nbytes ()
  (get-cached-symbol-data '*curve*
                          :pt-nbytes *curve*
                          (lambda ()
                            (ceiling (pt-nbits) 8))))

(defun chk-convert-int-to-lev (v &optional (nel (pt-nbytes)))
  (unless (<= (integer-length v) (* 8 nel))
    (error "Operation would truncate value"))
  (convert-int-to-lev v nel))

;; ----------------------------------------

(defun pt-compressed-nbits ()
  (get-cached-symbol-data '*curve*
                          :pt-compressed-nbits *curve*
                          (lambda ()
                            (1+ (pt-nbits)))))

(defun pt-compressed-nbytes ()
  (get-cached-symbol-data '*curve*
                          :pt-compressed-nbytes *curve*
                          (lambda ()
                            (ceiling (pt-compressed-nbits) 8))))

(defun compress-pt (pt &key lev) ;; lev = little-endian vector
  ;;
  ;; Standard encoding for EdDSA is X in little-endian notation, with
  ;; Odd(y) encoded as MSB beyond X.
  ;;
  ;; If lev is true, then a little-endian UB8 vector is produced,
  ;; else an integer value.
  ;;
  (um:bind* ((:struct-accessors ecc-pt (x y) pt))
    (let ((enc
           (if (oddp y)
               (dpb 1 (byte 1 (pt-nbits)) x)
             x)))
      (if lev
          (let ((enc (chk-convert-int-to-lev enc (pt-compressed-nbytes))))
            (if (eq lev :base58)
                (encode-bytes-to-base58 enc)
              enc))
        enc))))

(defmethod decompress-pt ((s string)) ;; assumed Base64
  (decompress-pt (decode-bytes-from-base58 s)))

(defmethod decompress-pt ((v vector)) ;; assumed LE UB8V
  (decompress-pt (convert-lev-to-int v)))

(defmethod decompress-pt ((v integer))
  (with-mod *q*
    (let* ((nbits (pt-nbits))
           (sgn   (ldb (byte 1 nbits) v))
           (x     (dpb 0 (byte 1 nbits) v))
           (yy    (m+ (m* x x x) (m* *a* x) *b*))
           (y     (msqrt yy))
           (y     (if (eql sgn (ldb (byte 1 0) y))
                      y
                    (m- y))))
      (assert (= yy (m* y y))) ;; check that yy was a square
      ;; if yy was not a square then y^2 will equal -yy, not yy.
      (validate-point
       (make-ecc-pt
        :x  x
        :y  y))
      )))

;; -----------------------------------------------------------------

(defun pt-solution-p (pt)
  ;; return true if pt satisfies the curve equation
  (unless (ecc-infinite-p pt)
    (with-mod *q*
      (um:bind* ((:struct-accessors ecc-pt ((x x)
                                            (y y)) pt))
        (assert (= (m* y y)
                   (m+ (m* x x x) (m* *a* x) *b*)))
        t))))

(defun validate-point (pt)
  (assert (not (ecc-infinite-p pt)))           ;; can't be pt at infinity
  (assert (pt-solution-p pt))                  ;; must satisfy curve equation
  (assert (ecc-infinite-p (pt^ pt *r*)))       ;; should be in our *r* group
  (assert (not (ecc-infinite-p (pt^ pt *h*)))) ;; must not be in *h* subgroup
  pt) ;; return point


(defun find-generator (a b q h)
  ;; search for a generator pt, given coeffs a, b
  ;; and group parameters q and cofactor h
  (with-mod q
    (um:nlet iter ()
      (let* ((x   (random-between 2 q))
             (yy  (m+ (m* x x x) (m* a x) b)))
        (if (quadratic-residue-p yy)
            (let ((gen (pt^ (make-ecc-pt
                             :x x
                             :y (msqrt yy))
                            h)))
              (multiple-value-bind (pt err)
                  (ignore-errors
                    (validate-point gen))
                (if err
                    (go-iter)
                  pt)))
          (iter)))
      )))

(defun find-embedding-degree (q r)
  ;; find N such that r divides (q^N - 1)
  (um:nlet iter ((ix 1))
    (when (< ix 1000)
      (if (zerop (mod (1- (expt q ix)) r))
          (format t "I got it! N = ~D" ix)
        (go-iter (1+ ix))))
    ))

(defun find-pt (x)
  (with-mod *q*
    (um:nlet iter ((x x))
      (let ((yy (m+ (m* x x x) (m* *a* x) *b*)))
        (if (quadratic-residue-p yy)
            (let ((pt  (pt^ (make-ecc-pt
                             :x x
                             :y (msqrt yy))
                            *h*)))
              (multiple-value-bind (pt err)
                  (ignore-errors
                   (validate-point pt))
                (if err
                    (iter (1+ x))
                  pt)))
          (go-iter (1+ x)))))))

