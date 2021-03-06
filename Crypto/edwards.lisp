;; edwards.lisp -- Edwards Curves
;; DM/RAL 07/15
;; -----------------------------------------------------------------------
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

(in-package :edwards-ecc)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------------------
;; Debug Instrumentation
#|
(defvar *watcher*
  (ac:make-actor
   (let ((counts (make-hash-table)))
     (lambda (&rest msg)
       (um:dcase msg
         (:reset ()
          (clrhash counts))
         (:read ()
          (um:accum acc
            (maphash (lambda (k v)
                       (acc (list k v)))
                     counts)))
         (:tally (kwsym)
          (let ((ct (gethash kwsym counts 0)))
            (setf (gethash kwsym counts) (1+ ct))))
         )))))

(defun clear-counters ()
  (ac:send *watcher* :reset))

(defun read-counters ()
  (ac:ask *watcher* :read))

(defun tally (kwsym)
  (ac:send *watcher* :tally kwsym))
|#
;; ------------------------------------------------------------------
;; Curve1174:  x^2 + y^2 = 1 + d*x^2*y^2
;; curve has order 4 * *ed-r* for field arithmetic over prime field *ed-q*
;;    (ed-mul *ed-gen* (* 4 *ed-r*)) -> (0, *ed-c*)
;;
;; isomorphs for d' = d / c'^4 then with (x,y) -> (x',y') = (c'*x, c'*y)
;;  x^2 + y^2 = 1 + d*x^2*y^2 -> x'^2 + y'^2 = c'^2*(1 + d'*x'^2*y'^2)
;;
;; See paper: "Elligator: Elliptic-curve points indistinguishable from uniform random strings"
;; by Bernstein, Hamburg, Krasnova, and Lange
;;
;; See also, https://safecurves.cr.yp.to

(defstruct ed-curve
  name c d q h r gen)

(defstruct (fast-ed-curve (:include ed-curve))
  affine-mul
  proj-mul
  proj-add
  to-affine)

;; -----------------------------------------------------------

(defstruct ecc-pt
  x y)

(defstruct ecc-proj-pt
  x y z)

(defstruct ecc-cmpr-pt
  cx)

#|
(defmethod make-load-form ((point ecc-pt) &optional env)
  (make-load-form-saving-slots point :environment env))

(defmethod make-load-form ((point ecc-proj-pt) &optional env)
  (make-load-form-saving-slots point :environment env))
|#

(defmethod vec-repr:int ((pt ecc-pt))
  (vec-repr:int (ed-compress-pt pt)))

(defmethod vec-repr:int ((pt ecc-proj-pt))
  (vec-repr:int (ed-compress-pt pt)))

(defmethod vec-repr:int ((pt ecc-cmpr-pt))
  (vec-repr:int (ecc-cmpr-pt-cx pt)))


(defmethod vec-repr:bev ((pt ecc-cmpr-pt))
  (vec-repr:bev (ecc-cmpr-pt-cx pt)))

(defmethod vec-repr:bev ((pt ecc-pt))
  (vec-repr:bev (ed-compress-pt pt)))

(defmethod vec-repr:bev ((pt ecc-proj-pt))
  (vec-repr:bev (ed-compress-pt pt)))

;; ------------------------------------------------------------------------------
;; Curve parameters from SafeCurves
;; https://safecurves.cr.yp.to
;;
;; c = curve parameter : Theorem 1, Definition 2, c = 2/s^2, d = -(c + 1)^2/(c - 1)^2
;; d = curve parameter
;; q = F_q prime number field
;; r = prime cofactor for curve order
;; h = cofactor for curve order #E(K) = h*r
;; gen = generator point

(defvar *curve1174*
  ;; rho-security (security by Pollard's rho attack) = 2^124.3
  ;; rho-security = (* 0.886 (sqrt *ed-r*))  (0.886 = (sqrt (/ pi 4)))
  ;; x^2 + y^2 = 1 - 1174 x^2 y^2
  (make-fast-ed-curve
   :name :Curve1174
   :c    1
   :d    -1174
   :q    (- (ash 1 251) 9)
   :r    904625697166532776746648320380374280092339035279495474023489261773642975601
   ;; = 2^249 - 11332719920821432534773113288178349711
   :h    4  ;; cofactor -- #E(K) = h*r
   :gen  (make-ecc-pt
          :x  1582619097725911541954547006453739763381091388846394833492296309729998839514
          :y  3037538013604154504764115728651437646519513534305223422754827055689195992590)

   :affine-mul '_Curve1174-affine-mul
   :proj-mul   '_Curve1174-projective-mul
   :proj-add   '_Curve1174-projective-add
   :to-affine  '_Curve1174-to-affine
   ))

;; ---------------------------

(defvar *curve-E382*
  ;; rho-security = 2^188.8
  (make-ed-curve
   :name :Curve-E382
   :c    1
   :d    -67254
   :q    (- (ash 1 382) 105)
   :r    2462625387274654950767440006258975862817483704404090416745738034557663054564649171262659326683244604346084081047321
            ;; = 2^380 - 1030303207694556153926491950732314247062623204330168346855
   :h    4
   :gen  (make-ecc-pt
          :x  3914921414754292646847594472454013487047137431784830634731377862923477302047857640522480241298429278603678181725699
          :y  17)
   ))

;; ---------------------------

(defvar *curve41417*
  ;; rho-security = 2^205.3
  (make-ed-curve
   :name :Curve41417
   :c    1
   :d    3617
   :q    (- (ash 1 414) 17)
   :r    5288447750321988791615322464262168318627237463714249754277190328831105466135348245791335989419337099796002495788978276839289
            ;; = 2^411 - 33364140863755142520810177694098385178984727200411208589594759
   :h    8
   :gen  (make-ecc-pt
          :x  17319886477121189177719202498822615443556957307604340815256226171904769976866975908866528699294134494857887698432266169206165
          :y  34)
   ))

(defvar *ed-sk1* '(95246105359107334368835702946360885221712654429149134602107470274728322648
                   . 871438026695917300513750426093020668134374754750828650996077213558451945727))

;; ---------------------------

(defvar *curve-Ed448* ;; the Goldilocks curve
  ;; rho-security = 2^222.8
  (make-ed-curve
   :name :Curve-Ed448
   :c    1
   :d    -39081
   :q    (- (ash 1 448) (ash 1 224) 1)
   :r    (- (ash 1 446) 13818066809895115352007386748515426880336692474882178609894547503885)
   :h    4
   :gen  (make-ecc-pt
          :x  117812161263436946737282484343310064665180535357016373416879082147939404277809514858788439644911793978499419995990477371552926308078495
          :y  19)
   ))

(defvar *ed-sk2* '(421974047105585348268953778604183148636297453017931755410200849399520674162
                   . 599385664174222180908461435453721901391720075307525572554742060814390186035))

;; ---------------------------

(defvar *curve-E521*
  ;; rho-security = 2^259.3
  (make-ed-curve
   :name :curve-E521
   :c    1
   :d    -376014
   :q    (- (ash 1 521) 1)
   :r    1716199415032652428745475199770348304317358825035826352348615864796385795849413675475876651663657849636693659065234142604319282948702542317993421293670108523
            ;; = 2^519 - 337554763258501705789107630418782636071904961214051226618635150085779108655765
   :h    4
   :gen  (make-ecc-pt
          :x  1571054894184995387535939749894317568645297350402905821437625181152304994381188529632591196067604100772673927915114267193389905003276673749012051148356041324
          :y  12)
   ))

(defvar *ed-sk3* '(92467223671543291667538537127794393729406965156397932625577105047698643617
                   . 80308136573684915858899498388575132495612607448470528849337536761764056915))

;; ---------------------------

(defvar *curve-Ed3363*  ;; the High-Five curve from MIRACL Labs
  ;; y^2 + x^2 = 1 + 11111 x^2 y^2
  ;; rho-security = 2^168 to 2^192 ?? just a guess
  (make-fast-ed-curve
   :name :curve-Ed3363
   :c    1
   :d    11111  ;; the High-Five! 
   :q    (- (ash 1 336) 3)
   :r    #x200000000000000000000000000000000000000000071415FA9850C0BD6B87F93BAA7B2F95973E9FA805
            ;; = 2^333 + 10345181422283708075595279275609870082194840463365
   :h    8
   :gen  (make-ecc-pt
          :x  #x0c
          :y  #xC0DC616B56502E18E1C161D007853D1B14B46C3811C7EF435B6DB5D5650CA0365DB12BEC68505FE8632)

   :affine-mul '_Ed3363-affine-mul
   :proj-mul   '_Ed3363-projective-mul
   :proj-add   '_Ed3363-projective-add
   :to-affine  '_Ed3363-to-affine
   ))

;; ------------------------------------------------------

(defvar *edcurve* *Curve1174*)

(define-symbol-macro *ed-c*     (ed-curve-c     *edcurve*)) ;; c constant in curve equation
(define-symbol-macro *ed-d*     (ed-curve-d     *edcurve*)) ;; d constant in curve equation
(define-symbol-macro *ed-q*     (ed-curve-q     *edcurve*)) ;; prime base of the coordinate field
(define-symbol-macro *ed-r*     (ed-curve-r     *edcurve*)) ;; primt bsae on the curve field
(define-symbol-macro *ed-h*     (ed-curve-h     *edcurve*)) ;; cofactor for the containing field
(define-symbol-macro *ed-gen*   (ed-curve-gen   *edcurve*)) ;; generator point for the curve
(define-symbol-macro *ed-name*  (ed-curve-name  *edcurve*)) ;; keyword symbol name of the curve

;; ------------------------------------------------------

(defvar *known-curves*
  (list *curve1174* *curve-e382* *curve41417* *curve-e521* *curve-Ed3363* *Curve-Ed448*))

(defmethod select-curve ((curve ed-curve))
  curve)

(defmethod select-curve ((curve symbol))
  (or (find curve *known-curves*
            :key 'ed-curve-name)
      (error "Not a known curve: ~A" curve)))

(defmacro with-ed-curve (curve &body body)
  `(let ((*edcurve* (select-curve ,curve)))
     ,@body))

(defun ed-curves ()
  ;; present caller with a list of symbols that can be used to select
  ;; a curve using WITH-ED-CURVE
  (mapcar 'ed-curve-name *known-curves*))

(defun set-ed-curve (curve)
  (setf *edcurve* (select-curve curve)))

;; -----------------------------------------------------------------------

#-:WINDOWS
(cffi:defcfun ("Ed3363_affine_mul" _Ed3363-affine-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

#-:WINDOWS
(cffi:defcfun ("Ed3363_projective_mul" _Ed3363-projective-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

#-:WINDOWS
(cffi:defcfun ("Ed3363_projective_add" _Ed3363-projective-add) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8)
  (pt2x :pointer :uint8)
  (pt2y :pointer :uint8)
  (pt2z :pointer :uint8))

#-:WINDOWS
(cffi:defcfun ("Ed3363_to_affine" _Ed3363-to-affine) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8))

;; -----------------------------------------------------------------------

#-:WINDOWS
(cffi:defcfun ("Curve1174_affine_mul" _Curve1174-affine-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

#-:WINDOWS
(cffi:defcfun ("Curve1174_projective_mul" _Curve1174-projective-mul) :void
  (ptx  :pointer :uint8)
  (pty  :pointer :uint8)
  (ptz  :pointer :uint8)
  (nv   :pointer :uint8))

#-:WINDOWS
(cffi:defcfun ("Curve1174_projective_add" _Curve1174-projective-add) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8)
  (pt2x :pointer :uint8)
  (pt2y :pointer :uint8)
  (pt2z :pointer :uint8))

#-:WINDOWS
(cffi:defcfun ("Curve1174_to_affine" _Curve1174-to-affine) :void
  (pt1x :pointer :uint8)
  (pt1y :pointer :uint8)
  (pt1z :pointer :uint8))

;; ----------------------------------------------------------------

(defun ed-neutral-point ()
  (get-cached-symbol-data '*edcurve*
                          :ed-neutral-point *ed-c*
                          (lambda ()
                            (make-ecc-pt
                             :x 0
                             :y *ed-c*))))

(defun ed-neutral-point-p (pt)
  (ed-pt= pt (ed-neutral-point)))

;; -------------------------------------------------------------------

(defun have-fast-impl ()
  #-:WINDOWS
  (fast-ed-curve-p *edcurve*)
  #+:WINDOWS
  nil)

;; -------------------------------------------------------------------

(defmacro modq (form)
  `(with-mod *ed-q* ,form))

(defmacro modr (form)
  `(with-mod *ed-r* ,form))

;; -------------------------------------------------------------------

(defmethod ed-affine ((pt ecc-pt))
  pt)

(defmethod ed-affine ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z)))
    (cond ((= 1 z)
           (make-ecc-pt
            :x  x
            :y  y))
          #-:WINDOWS
          ((have-fast-impl)
           (%ecc-fast-to-affine pt))
          (t
           (with-mod *ed-q*
             (make-ecc-pt
              :x  (m/ x z)
              :y  (m/ y z))))
          )))

(defmethod ed-affine ((pt ecc-cmpr-pt))
  (ed-affine (ed-decompress-pt pt)))

;; -------------------------------------------------------------------

(defmethod ed-projective ((pt ecc-proj-pt))
  pt)

(defmethod ed-projective ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    (make-ecc-proj-pt
     :x x
     :y y
     :z 1)))
  
#|
(defmethod ed-projective ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y))
             (alpha (field-random *ed-q*)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x (m* alpha x)
       :y (m* alpha y)
       :z alpha)
      )))
|#

(defmethod ed-projective ((pt ecc-cmpr-pt))
  (ed-projective (ed-decompress-pt pt)))

;; -------------------------------------------------------------------

(defmethod ed-random-projective ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y))
             (alpha (safe-field-random *ed-q*))
             (declare (integer alpha)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x (m* alpha x)
       :y (m* alpha y)
       :z alpha)
      )))

(defmethod ed-random-projective ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z))
             (alpha (safe-field-random *ed-q*))
             (declare (integer alpha)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x (m* alpha x)
       :y (m* alpha y)
       :z (m* alpha z))
      )))

(defmethod ed-random-projective ((pt ecc-cmpr-pt))
  (ed-random-projective (ed-decompress-pt pt)))

;; -------------------------------------------------------------------

(defmethod ed-pt= ((pt1 ecc-pt) pt2)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) pt1)
             (declare (integer x1 y1))
             (:struct-accessors ecc-pt ((x2 x) (y2 y)) (ed-affine pt2))
             (declare (integer x2 y2)))
    (with-mod *ed-q*
      (and (= x1 x2)
           (= y1 y2))
      )))

#|
(defmethod ed-pt= ((pt1 ecc-proj-pt) (pt2 ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y) (z1 z)) pt1)
             (declare (integer x1 y1 z1))
             (:struct-accessors ecc-pt ((x2 x) (y2 y)) pt2)
             (declare (integer x2 y2)))
    (with-mod *ed-q*
      (and (= (m* z1 x2) x1)
           (= (m* z1 y2) y1))
    )))
|#

(defmethod ed-pt= ((pt1 ecc-proj-pt) pt2)
  (um:bind* ((:struct-accessors ecc-proj-pt ((x1 x) (y1 y) (z1 z)) pt1)
             (declare (integer x1 y1 z1))
             (:struct-accessors ecc-proj-pt ((x2 x) (y2 y) (z2 z)) (ed-projective pt2))
             (declare (integer x2 y2 z2)))
    (with-mod *ed-q*
      (and (= (m* z1 x2) (m* z2 x1))
           (= (m* z1 y2) (m* z2 y1)))
      )))

(defmethod ed-pt= ((pt1 ecc-cmpr-pt) (pt2 ecc-cmpr-pt))
  (= (ecc-cmpr-pt-cx pt1)
     (ecc-cmpr-pt-cx pt2)))

(defmethod ed-pt= ((pt1 ecc-cmpr-pt) pt2)
  (ed-pt= (ed-affine pt1) pt2))

;; -------------------------------------------------------------------

(defmethod ed-satisfies-curve ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
    (with-mod *ed-q*
      (let ((xx (m* x x))
            (yy (m* y y)))
        (= (m+ xx yy)
           (m* *ed-c* *ed-c*
               (m+ 1 (m* *ed-d* xx yy))))
        ))))
  
(defmethod ed-satisfies-curve ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z)))
    ;; z^2*(x^2 + y^2) = c^2*(z^4 + d*x^2*y^2)
    (with-mod *ed-q*
      (let ((xx (m* x x))
            (yy (m* y y))
            (zz (m* z z)))
        (= (m* zz (m+ xx yy))
           (m* *ed-c* *ed-c*
               (+ (m* zz zz)
                  (m* *ed-d* xx yy))))
        ))))

(defmethod ed-satisfies-curve ((pt ecc-cmpr-pt))
  (ed-satisfies-curve (ed-projective pt)))

;; -------------------------------------------------------------------

(defun ed-affine-add (pt1 pt2)
  ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) pt1)
               (declare (integer x1 y1))
               (:struct-accessors ecc-pt ((x2 x) (y2 y)) pt2)
               (declare (integer x2 y2))
               (y1y2  (m* y1 y2))
               (declare (integer y1y2))
               (x1x2  (m* x1 x2))
               (declare (integer x1x2))
               (x1x2y1y2 (m* *ed-d* x1x2 y1y2))
               (declare (integer x1x2y1y2))
               (denx  (m* *ed-c* (1+ x1x2y1y2)))
               (declare (integer denx))
               (deny  (m* *ed-c* (- 1 x1x2y1y2)))
               (declare (integer deny))
               (numx  (+ (m* x1 y2)
                         (m* y1 x2)))
               (declare (integer numx))
               (numy  (- y1y2 x1x2))
               (declare (integer numy)))
      (make-ecc-pt
       :x  (m/ numx denx)
       :y  (m/ numy deny))
      )))

(defun ed-projective-add (pt1 pt2)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ecc-proj-pt ((x1 x)
                                               (y1 y)
                                               (z1 z)) pt1)
               (declare (integer x1 y1 z1))
               (:struct-accessors ecc-proj-pt ((x2 x)
                                               (y2 y)
                                               (z2 z)) pt2)
               (declare (integer x2 y2 z2))
               (a  (m* z1 z2))
               (declare (integer a))
               (b  (m* a a))
               (declare (integer b))
               (c  (m* x1 x2))
               (declare (integer c))
               (d  (m* y1 y2))
               (declare (integer d))
               (e  (m* *ed-d* c d))
               (declare (integer e))
               (f  (- b e))
               (declare (integer f))
               (g  (+ b e))
               (declare (integer g))
               (x3 (m* a f (- (m* (+ x1 y1)
                                  (+ x2 y2))
                               c d)))
               (y3 (m* a g (- d c)))
               (z3 (m* *ed-c* f g)))
      (make-ecc-proj-pt
       :x  x3
       :y  y3
       :z  z3)
      )))

(defun ed-add (pt1 pt2)
  ;; contageon to randomized projective coords for added security
  ;; (reset-blinders)
  ;; (tally :ecadd)
  (let ((ppt1 (ed-projective pt1))  ;; projective add is so much faster than affine add
        (ppt2 (ed-projective pt2))) ;; so it pays to make the conversion
    (cond
     #-:WINDOWS
     ((have-fast-impl)
      (%ecc-fast-add ppt1 ppt2))

     (t 
      ;; since projective add takes about 6 usec, and affine add takes
      ;; about 40 usec, it pays to always convert to projective coords,
      ;; especially since it is so cheap to do so.
      (ed-projective-add ppt1 ppt2))
     )))

;; ----------------------------------------------------------------

(defmethod ed-negate ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    (with-mod *ed-q*
      (make-ecc-pt
       :x  (m- x)
       :y  y)
      )))

(defmethod ed-negate ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x  (m- x)
       :y  y
       :z  z)
      )))

(defmethod ed-negate ((pt ecc-cmpr-pt))
  (ed-negate (ed-projective pt)))

(defun ed-sub (pt1 pt2)
  (ed-add pt1 (ed-negate pt2)))

;; ----------------------------------------------------------------
;; NAF multiplication, 4 bits at a time...
#|
(defun naf4 (k)
  (declare (integer k))
  (labels ((mods (x)
             (declare (integer x))
             (let ((xm (ldb (byte 4 0) x)))
               (declare (fixnum xm))
               (if (>= xm 8)
                   (- xm 16)
                 xm))))
    (um:nlet iter ((k   k)
                   (ans nil))
      (declare (integer k)
               (list ans))
      (if (zerop k)
          ans
        (if (oddp k)
            (let ((di (mods k)))
              (declare (fixnum di))
              (go-iter (ash (- k di) -4) (cons di ans)))
          ;; else
          (go-iter (ash k -1) (cons 0 ans)))
        ))))
        
(defun ed-basic-mul (pt n)
  (declare (integer n))
  (cond ((zerop n) (ed-projective
                    (ed-neutral-point)))
        
        ((or (= n 1)
             (ed-neutral-point-p pt)) pt)
        
        (t (let ((precomp
                  (let* ((r0   (ed-projective pt))
                         (r0x2 (ed-add r0 r0)))
                    (loop for ix fixnum from 1 below 8 by 2
                          for r1 = r0 then (ed-add r1 r0x2)
                          collect (cons ix r1)
                          collect (cons (- ix) (ed-negate r1)))
                    )))
             
             (um:nlet iter ((nns  (naf4 n))
                                 (qans nil))
               (if (endp nns)
                   (or qans (ed-neutral-point))
                 (let ((qsum  (and qans (ed-add qans qans)))
                       (nnhd  (car nns)))
                   (declare (fixnum nnhd))
                   (unless (zerop nnhd)
                     (let ((kpt (cdr (assoc nnhd precomp))))
                       (if qsum
                           (setf qsum (ed-add qsum qsum)
                                 qsum (ed-add qsum qsum)
                                 qsum (ed-add qsum qsum)
                                 qsum (ed-add qsum kpt))
                         ;; else
                         (setf qsum kpt))))
                   (go-iter (cdr nns) qsum))))
             ))
        ))
|#

;; -------------------------------------------------
;; Support for C-level Ed3363 curve

#-:WINDOWS
(defun xfer-to-c (val cvec)
  ;; transfer val to C vector in 6 8-byte words
  (declare (integer val))
  (setf val (mod val *ed-q*))
  (setf (cffi:mem-aref cvec :uint64 0) (ldb (byte 64   0) val)
        (cffi:mem-aref cvec :uint64 1) (ldb (byte 64  64) val)
        (cffi:mem-aref cvec :uint64 2) (ldb (byte 64 128) val)
        (cffi:mem-aref cvec :uint64 3) (ldb (byte 64 192) val))
  (when (eql *edcurve* *curve-ed3363*)
    (setf (cffi:mem-aref cvec :uint64 4) (ldb (byte 64 256) val)
          (cffi:mem-aref cvec :uint64 5) (ldb (byte 64 320) val))
    ))

#-:WINDOWS
(defun xfer-from-c (cvec)
  ;; retrieve val from C vector in 6 8-byte words
  (let ((v 0))
    (declare (integer v))
    (setf v (dpb (cffi:mem-aref cvec :uint64 0) (byte 64   0) v)
          v (dpb (cffi:mem-aref cvec :uint64 1) (byte 64  64) v)
          v (dpb (cffi:mem-aref cvec :uint64 2) (byte 64 128) v)
          v (dpb (cffi:mem-aref cvec :uint64 3) (byte 64 192) v))
    (when (eql *edcurve* *curve-ed3363*)
      (setf v (dpb (cffi:mem-aref cvec :uint64 4) (byte 64 256) v)
            v (dpb (cffi:mem-aref cvec :uint64 5) (byte 64 320) v)))
    #|
      ;; libs now handle conversion properly - DM 03/19
    (assert (and (<= 0 v)
                 (< v *ed-q*)))
    (mod v *ed-q*)
    |#
    v
    ))

#-:WINDOWS
(defmacro with-fli-buffers (buffers &body body)
  (if (endp buffers)
      `(progn
         ,@body)
    (destructuring-bind (name &optional lisp-val) (car buffers)
      `(cffi:with-foreign-pointer (,name 48)
         ,@(when lisp-val
             `((xfer-to-c ,lisp-val ,name)))
         (with-fli-buffers ,(cdr buffers) ,@body))
      )))

#+:LISPWORKS
(editor:setup-indent "with-fli-buffers" 1)

;; -------------------------------------------------------------------

#-:WINDOWS
(defmethod %ecc-fast-mul ((pt ecc-proj-pt) n)
  ;; about a 10% speed penalty over using affine points
  (with-fli-buffers ((cptx (ecc-proj-pt-x pt))  ;; projective in...
                     (cpty (ecc-proj-pt-y pt))
                     (cptz (ecc-proj-pt-z pt))
                     (cwv  n))
    
    (funcall (fast-ed-curve-proj-mul *edcurve*)
             cptx cpty cptz cwv)
    
    (make-ecc-proj-pt
              :x (xfer-from-c cptx) ;; projective out...
              :y (xfer-from-c cpty)
              :z (xfer-from-c cptz))
    ))

#-:WINDOWS
(defmethod %ecc-fast-mul ((pt ecc-pt) n)
  (with-fli-buffers ((cptx (ecc-pt-x pt))  ;; affine in...
                     (cpty (ecc-pt-y pt))
                     (cptz)
                     (cwv  n))

    (funcall (fast-ed-curve-affine-mul *edcurve*)
             cptx cpty cptz cwv)
    
    (make-ecc-proj-pt
     :x (xfer-from-c cptx) ;; projective out...
     :y (xfer-from-c cpty)
     :z (xfer-from-c cptz))
    ))

#-:WINDOWS
(defmethod %ecc-fast-mul ((pt ecc-cmpr-pt) n)
  (%ecc-fast-mul (ed-projective pt) n))

#-:WINDOWS
(defun %ecc-fast-add (pt1 pt2)
  (with-fli-buffers ((cpt1xv (ecc-proj-pt-x pt1)) ;; projective in...
                     (cpt1yv (ecc-proj-pt-y pt1))
                     (cpt1zv (ecc-proj-pt-z pt1))
                     (cpt2xv (ecc-proj-pt-x pt2))
                     (cpt2yv (ecc-proj-pt-y pt2))
                     (cpt2zv (ecc-proj-pt-z pt2)))
    
             (funcall (fast-ed-curve-proj-add *edcurve*)
                      cpt1xv cpt1yv cpt1zv
                      cpt2xv cpt2yv cpt2zv)
    
    (make-ecc-proj-pt ;; projective out...
     :x (xfer-from-c cpt1xv)
     :y (xfer-from-c cpt1yv)
     :z (xfer-from-c cpt1zv))
    ))

#-:WINDOWS
(defun %ecc-fast-to-affine (pt)
  (with-fli-buffers ((cptx (ecc-proj-pt-x pt)) ;; projective in...
                     (cpty (ecc-proj-pt-y pt))
                     (cptz (ecc-proj-pt-z pt)))
    
    (funcall (fast-ed-curve-to-affine *edcurve*)
             cptx cpty cptz)
    
    (make-ecc-pt
     :x (xfer-from-c cptx) ;; affine out...
     :y (xfer-from-c cpty))
    ))

#|
(let ((prf (range-proofs:make-range-proof 1234567890)))
  (time   
   (loop repeat 100 do
       (range-proofs:validate-range-proof prf))))

(time
 (loop repeat 100 do
       (range-proofs:validate-range-proof
        (range-proofs:make-range-proof 1234567890))))
|#
;; -------------------------------------------------------------------
;; 4-bit fixed window method - decent performance, and never more than
;; |r|/4 terms

(defun nibbles (n)
  ;; for debug display to compare with windows4 output
  (let* ((nbits (integer-length n))
         (limt  (* 4 (floor nbits 4))))
    (loop for pos fixnum from limt downto 0 by 4 collect
          (ldb (byte 4 pos) n))))
  
(defun windows (n window-nbits)
  ;; return a big-endian list of balanced bipolar window values for
  ;; each window-nbits nibble in the number. E.g., for window-nbits = 4,
  ;; (windows 123 4) -> (1 -8 -5),
  ;; where each values is in the set (-8, -7, ..., 7)
  (declare (integer n)
           (fixnum window-nbits))
  (let* ((nbits (integer-length n))
         (limt  (* window-nbits (floor nbits window-nbits)))
         (2^wn  (ash 1 window-nbits))
         (wnlim (1- (ash 2^wn -1))))
    (declare (fixnum nbits limt 2^wn wnlim))
    (um:nlet iter ((pos 0)
                   (ans nil)
                   (cy  0))
      (declare (fixnum pos cy))
      (let* ((byt (ldb (byte window-nbits pos) n))
             (x   (+ byt cy)))
        (declare (fixnum byt x))
        (multiple-value-bind (nxt nxtcy)
            (if (> x wnlim)
                (values (- x 2^wn) 1)
              (values x 0))
          (if (< pos limt)
              (go-iter (+ pos window-nbits) (cons nxt ans) nxtcy)
            (list* nxtcy nxt ans)))
        ))))

(defun windows-to-int (wins window-nbits)
  ;; for debugging...
  (let ((ans 0))
    (loop for w in wins do
          (setf ans (+ w (ash ans window-nbits))))
    ans))

(defun ed-projective-double (pt)
  (ed-projective-add pt pt))

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
         (pt*1   (ed-projective pt))
         (pt*m1  (ed-negate pt*1)))
    (declare (fixnum nel offs))
    (setf (aref precv (1+ offs)) pt*1      ;; slot ix = 0 never referenced
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
                        (ed-projective-add pt*m1 (get-prec wc (1+ ix)))
                      (ed-projective-add pt*1 (get-prec wc (1- ix))))
                  ;; else - ix even
                  (ed-projective-double (get-prec wc (ash ix -1))))
                )))))

(defmethod generalized-bipolar-windowed-mul (pt n &key window-nbits)
  ;; ECC point-scalar multiplication using fixed-width bipolar window
  ;; algorithm
  (declare (fixnum window-nbits)
           (integer n))
  (let* ((ws  (windows n window-nbits))
         (wc  (make-bipolar-window-cache
               :nbits window-nbits
               :pt    pt))  ;; affine or projective in...
         (ans nil))
    (loop for w fixnum in ws do
          (when ans
            (loop repeat window-nbits do
                  (setf ans (ed-projective-double ans))))
          (unless (zerop w)
            (let ((pw  (get-prec wc w)))
              (setf ans (if ans
                            (ed-projective-add pw ans)
                          pw)))
            ))
    (or ans  ;; projective out...
        (ed-neutral-point))))

(defmethod generalized-bipolar-windowed-mul ((pt ecc-cmpr-pt) n &key window-nbits)
  (generalized-bipolar-windowed-mul (ed-projective pt) n :window-nbits window-nbits))

;; --------------------------------------------------------------------------------
#|
;; 1-bit NAF form
(defun naf (k)
  (declare (integer k))
  ;; non-adjacent form encoding of integers
  (um:nlet iter ((k k)
                 (ans nil))
    (declare (integer k))
    (if (plusp k)
        (let ((kj (if (oddp k)
                      (- 2 (mod k 4))
                    0)))
          (declare (integer kj))
          (go-iter (ash (- k kj) -1) (cons kj ans)))
      ans)))

(defun ed-basic-mul (pt n)
  ;; this is about 50% faster than not using NAF
  (cond ((zerop n)  (ed-projective
                     (ed-neutral-point)))
        
        ((or (= n 1)
             (ed-neutral-point-p pt))  pt)
        
        (t  (let* ((r0  (ed-random-projective pt)) ;; randomize point
                   (r0n (ed-negate r0))
                   (nns (naf n))
                   (v   r0))
              (loop for nn in (cdr nns) do
                    (setf v (ed-add v v))
                    (case nn
                      (-1  (setf v (ed-add v r0n)))
                      ( 1  (setf v (ed-add v r0)))
                      ))
              v))
        ))
|#

;; --------------------------------------------------------------------------------

(defun ed-mul (pt n)
  #|
  (let* ((alpha  (* *ed-r* *ed-h* (field-random #.(ash 1 48)))))
    (ed-basic-mul pt (+ n alpha)))
  |#
  (declare (integer n))
  ;; (tally :ecmul)
  (let ((nn  (mod n *ed-r*)))
    (declare (integer nn))
    
    (cond ((zerop nn)
           (ed-neutral-point))
          
          ((or (= nn 1)
               (ed-neutral-point-p pt))
           pt)

          #-:WINDOWS
          ((have-fast-impl)
           (%ecc-fast-mul pt nn))
          
          (t
           (generalized-bipolar-windowed-mul pt nn
                                    :window-nbits 4))
          )))

(defun ed-div (pt n)
  (with-mod *ed-r*
    (ed-mul pt (m/ n))))

(defun ed-nth-proj-pt (n)
  (ed-mul *ed-gen* n))

(defun ed-nth-pt (n)
  (ed-affine (ed-nth-proj-pt n)))

;; ---------------------------------------------------------------
;; conversion between integers and little-endian UB8 vectors

(defun ed-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :ed-nbits *edcurve*
                          (lambda ()
                            (integer-length *ed-q*))))

(defun ed-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :ed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-nbits) 8))))

;; ----------------------------------------

(defun ed-compressed-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :ed-compressed-nbits *edcurve*
                          (lambda ()
                            (1+ (ed-nbits)))))

(defun ed-compressed-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :ed-compressed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-compressed-nbits) 8))))

(defun ed-cmpr/h-sf ()
  (get-cached-symbol-data '*edcurve*
                          :ed-cmpr/h-sf *edcurve*
                          (lambda ()
                            (with-mod *ed-r*
                              (m/ *ed-h*)))))

(defun ed-decmpr*h-fn ()
  (get-cached-symbol-data '*edcurve*
                          :ed-decmpr*h-fn *edcurve*
                          (lambda ()
                            (case *ed-h*
                              (1  'identity)
                              (2  (lambda (pt)
                                    (ed-add pt pt)))
                              (4  (lambda (pt)
                                    (let ((pt2 (ed-add pt pt)))
                                      (ed-add pt2 pt2))))
                              (8  (lambda (pt)
                                    (let* ((pt2 (ed-add pt pt))
                                           (pt4 (ed-add pt2 pt2)))
                                      (ed-add pt4 pt4))))
                              (t (error "No decompression function"))
                              ))))

;; -------------------

(defmethod ed-compress-pt ((pt ecc-cmpr-pt) &key enc)
  (if enc
      (ed-compress-pt (ed-projective pt) :enc enc)
    pt))

(defmethod ed-compress-pt (pt &key enc) ;; :bev, :lev, :base58 or nil
  ;;
  ;; Standard encoding for EdDSA is X in little-endian notation, with
  ;; Odd(Y) encoded as MSB beyond X.
  ;;
  ;; If lev is true, then a little-endian UB8 vector is produced,
  ;; else an integer value.
  ;;
  (um:bind* ((cmpr/h  (ed-cmpr/h-sf))
             (:struct-accessors ecc-pt (x y)
              (ed-affine (ed-mul pt cmpr/h))))
    (let ((val  (dpb (ldb (byte 1 0) y)
                     (byte 1 (ed-nbits)) x)))
      (ecase enc
        ((nil)    (make-ecc-cmpr-pt :cx val))
        (:bev     (bevn val (ed-compressed-nbytes)))
        (:lev     (levn val (ed-compressed-nbytes)))
        (:base58  (base58 (bevn val (ed-compressed-nbytes))))
        ))))

;; --------------------------

(defmethod ed-decompress-pt ((x ecc-pt))
  x)

(defmethod ed-decompress-pt ((x ecc-proj-pt))
  x)

(defmethod ed-decompress-pt ((x ecc-cmpr-pt))
  (ed-decompress-pt (ecc-cmpr-pt-cx x)))

(defmethod ed-decompress-pt (x)
  (ed-decompress-pt (int x)))

(defmethod ed-decompress-pt ((v integer))
  (with-mod *ed-q*
    (let* ((decmpr-fn (ed-decmpr*h-fn))
           (nbits (ed-nbits))
           (sign  (ldb (byte 1 nbits) v))
           (x     (ldb (byte nbits 0) v))
           (y     (ed-solve-y x)))
      (unless (= sign (ldb (byte 1 0) y))
        (setf y (m- y)))
      (funcall decmpr-fn
               (make-ecc-proj-pt
                :x  x
                :y  y
                :z  1))
      )))

(defun ed-solve-y (x)
  (with-mod *ed-q*
    (msqrt (m/ (m* (+ *ed-c* x)
                   (- *ed-c* x))
               (- 1 (m* x x *ed-c* *ed-c* *ed-d*))))))

;; -----------------------------------------------------------------

(defmethod ed-valid-point-p ((pt ecc-pt))
  (and (ed-satisfies-curve pt)
       (not (or (zerop (ecc-pt-x pt))
                (zerop (ecc-pt-y pt))))
       pt))

(defmethod ed-valid-point-p ((pt ecc-proj-pt))
  (ed-valid-point-p (ed-affine pt)))

(defmethod ed-valid-point-p ((pt ecc-cmpr-pt))
  (ed-valid-point-p (ed-affine pt)))

(defun ed-validate-point (pt)
  (assert (ed-valid-point-p pt))
  pt)

#|
(loop repeat 10000 do
      (let* ((pt (ed-random-generator)))
        (ed-validate-point pt)))

(loop repeat 10000 do
      (let* ((n (random-between 1 *ed-r*))
             (pt (ed-nth-pt n)))
        (ed-validate-point pt)
        (ed-validate-point (ed-mul pt (- n)))))
 |#
;; -----------------------------------------------------------------

#|
(defmethod hashable ((x ecc-proj-pt))
  (hashable (ed-compress-pt x :enc :lev)))

(defmethod hashable ((x ecc-pt))
  (hashable (ed-projective x)))

(defmethod hashable ((x ecc-cmpr-pt))
  (hashable (ed-projective x)))
|#

;; -----------------------------------------------------------

(defun hash-to-grp-range (&rest args)
  (apply 'hash-to-range *ed-r* args))

(defun hash-to-pt-range (&rest args)
  (apply 'hash-to-range *ed-q* args))

;; -------------------------------------------------

(defun compute-deterministic-skey (seed &optional (index 0))
  (multiple-value-bind (_ hval)
      (hash-to-grp-range seed index)
    (declare (ignore _))
    hval))

(defun make-deterministic-keys (seed)
  (let* ((skey  (compute-deterministic-skey seed))
         (pkey  (ed-mul *ed-gen* skey)))
    (values skey pkey)))

(defun ed-random-pair ()
  "Select a random private and public key from the curve"
  (let* ((seed (ctr-drbg (integer-length (1- *ed-r*))))
         (skey (compute-deterministic-skey seed))
         (pt   (ed-nth-pt skey)))
    (values skey pt)))

;; -----------------------------------------------------
;; Hashing onto curve

(defun ed-pt-from-hash (hintval)
  "Hash onto curve. Treat h as X coord, just like a compressed point.
Then if Y is a quadratic residue we are done.
Else re-probe with (X^2 + 1)."
  (let ((cof-fn  (ed-decmpr*h-fn)))
    (with-mod *ed-q*
      (um:nlet iter ((x  hintval))
        (or
         (um:when-let (y (ignore-errors (ed-solve-y x)))
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
             (and (not (ed-neutral-point-p pt))
                  pt)))
         ;; else - invalid point, so re-probe at x^2+1
         (go-iter (m+ 1 (m* x x)))
         )))))

(defun ed-pt-from-seed (&rest seeds)
  (multiple-value-bind (_ hval)
      (apply 'hash-to-pt-range seeds)
    (declare (ignore _))
    (ed-pt-from-hash hval)))

(defun ed-random-generator ()
  (ed-pt-from-seed (uuid:make-v1-uuid)
                   (ctr-drbg (integer-length *ed-q*))))

;; ---------------------------------------------------
;; The IETF EdDSA standard as a primitive
;;
;; From IETF specifications
;; (we tend toward BEV values everywhere, but IETF dictates LEV)

(defun compute-schnorr-deterministic-random (msgv k-priv)
  (um:nlet iter ((ix 0))
    ;; randomness r from hash to group range of (CTR | SKEY | MSG)
    ;; to make deterministic to avoid PlayStation attacks, yet
    ;; random because of hash - nothing up my sleeve...
    (let ((r   (int (hash-to-grp-range
                     (levn ix 4)
                     (levn k-priv (integer-length (1- *ed-q*))) ;; we want *ed-q* here to avoid truncating skey
                     msgv))))
      (if (plusp r)
          (values r (ed-nth-pt r) ix)
        (go-iter (1+ ix)))
      )))

(defun ed-dsa (msg skey)
  (let* ((msg-enc   (loenc:encode msg))
         (pkey      (ed-nth-pt skey))
         (pkey-cmpr (ed-compress-pt pkey)))
    ;; r = the random challenge value for Fiat-Shamir sigma proof
    ;; - deterministic, to avoid attacks, yet unpredictable via hash
    ;; - a "nothing up my sleeve" proof value
    (multiple-value-bind (r rpt)
        (compute-schnorr-deterministic-random msg-enc skey)
      (let* ((rpt-cmpr  (ed-compress-pt rpt))
             (nbcmpr    (ed-compressed-nbytes))
             (s         (with-mod *ed-r*
                          (m+ r
                              (m* skey
                                  (int
                                   (hash-to-grp-range
                                    (levn rpt-cmpr  nbcmpr)
                                    (levn pkey-cmpr nbcmpr)
                                    msg-enc))
                                  )))))
        (list
         :msg   msg
         :pkey  pkey-cmpr
         :r     rpt-cmpr
         :s     s)
        ))))

(defun ed-dsa-validate (msg pkey r s)
  ;; pkey should be presented in compressed pt form
  ;; r is likewise a compressed pt
  ;; s is a group scalar
  (let ((nbcmpr (ed-compressed-nbytes)))
    (ed-pt=
     (ed-mul (ed-nth-pt s) *ed-h*)
     (ed-add (ed-mul (ed-decompress-pt r) *ed-h*)
             (ed-mul
              (ed-mul (ed-decompress-pt pkey)
                      (int
                       (hash-to-grp-range
                        (levn r    nbcmpr)
                        (levn pkey nbcmpr)
                        (loenc:encode msg))))
              *ed-h*))
     )))

;; -----------------------------------------------------------
;; VRF on Elliptic Curves
;;
;; Unlike the situation with pairing curves and BLS signatures, we
;; must use a Schnorr-like scheme, with a deterministic
;; nonce-protected random challenge value, to guard the secret key.
;;
;; If you ever happened to use the same random challenge value on a
;; different message seed then it becomes possible to compute the
;; secret key.
;;
;; Adapted from Appendix A of paper:
;;  "CONIKS: Bringing Key Transparency to End Users"
;;   by Melara, Blankstein, Bonneau, Felten, and Freedman

(defun ed-vrf (seed skey)
    (let* ((h    (ed-pt-from-seed seed))
           (vrf  (ed-mul h skey)))
      (ed-compress-pt vrf)))
           

(defun ed-prove-vrf (seed skey)
    (let* ((h    (ed-pt-from-seed seed))
           (vrf  (ed-compress-pt (ed-mul h skey)))
           
           ;; r = the random challenge value for Fiat-Shamir sigma proof
           ;; - deterministic, to avoid attacks, yet unpredictable via hash
           ;; - a "nothing up my sleeve" proof value
           (r    (compute-schnorr-deterministic-random seed skey))
           ;; s = H(g, h, P, v, g^r, h^r)
           (s    (int
                  (hash-to-grp-range
                   (ed-compress-pt *ed-gen*)         ;; g
                   (ed-compress-pt h)                ;; h = H(m)
                   (ed-compress-pt (ed-nth-pt skey)) ;; P = pkey
                   vrf                               ;; v
                   (ed-compress-pt (ed-nth-pt r))    ;; g^r
                   (ed-compress-pt (ed-mul h r)))))  ;; h^r
           (tt   (with-mod *ed-r*                    ;; tt = r - s * skey
                   (m- r (m* s skey)))))

      (list :v vrf   ;; the VRF value (a compressed pt)
            :s s     ;; a check scalar
            :t tt))) ;; a check scalar


(defun ed-check-vrf (seed proof pkey)
  ;; pkey should be presented in compressed pt form
  (let* ((v    (getf proof :v))
         (s    (getf proof :s))
         (tt   (getf proof :t))
         (h    (ed-pt-from-seed seed))
         ;; check s ?= H(g, h, P, v, g^r = g^tt * P^s, h^r = h^tt * v^s)
         (schk (int
                (hash-to-grp-range
                 (ed-compress-pt *ed-gen*)
                 (ed-compress-pt h)
                 pkey
                 v
                 (ed-compress-pt
                  (ed-add
                   (ed-nth-proj-pt tt)
                   (ed-mul (ed-decompress-pt pkey) s)))
                 (ed-compress-pt
                  (ed-add
                   (ed-mul h tt)
                   (ed-mul (ed-decompress-pt v) s)))
                 ))))
    (= schk s)))

;; -----------------------------------------------------------
#|
(let* ((*edcurve* *curve41417*)
       ;; (*edcurve* *curve1174*)
       ;; (*edcurve* *curve-e521*)
       )
  (plt:window 'plt
              :xsize 330
              :ysize 340)
  (plt:polar-fplot 'plt `(0 ,(* 2 pi))
                   (lambda (arg)
                     (let* ((s (sin (+ arg arg)))
                            (a (* *ed-d* *ed-c* *ed-c* s s 1/4))
                            (b 1)
                            (c (- (* *ed-c* *ed-c*))))
                       (sqrt (/ (- (sqrt (- (* b b) (* 4 a c))) b)
                                (+ a a)))))
                   :clear t
                   :aspect 1))

(let* ((ans (loop for ix from 1 to 10000
                  for pt = *ed-gen* then (ed-add pt *ed-gen*)
                  collect (ecc-pt-x (ed-affine pt)))))
  (plt:plot 'raw (mapcar (um:curry #'ldb (byte 8 0)) ans)
            :clear t)
  (plt:histogram 'plt (mapcar (um:curry #'ldb (byte 8 0)) ans)
                 :clear t
                 :cum t))

(loop repeat 1000 do
      (let* ((x   (field-random (* *ed-h* *ed-r*)))
             (pt  (ed-mul *ed-gen* x))
             (ptc (ed-compress-pt pt))
             (pt2 (ed-decompress-pt ptc)))
        (assert (ed-validate-point pt))
        (unless (ed-pt= pt pt2)
          (format t "~%pt1: ~A" (ed-affine pt))
          (format t "~%pt2: ~A" (ed-affine pt2))
          (format t "~%ptc: ~A" ptc)
          (format t "~%  k: ~A" x)
          (assert (ed-pt= pt pt2)))
        ))
 |#

;; -----------------------------------------------------------------------------
;; Elligator encoding of curve points

(defun elligator-limit ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-limit *edcurve*
                          (lambda ()
                            (floor (1+ *ed-q*) 2))))

(defun elligator-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-nbits *edcurve*
                          (lambda ()
                            (integer-length (1- (elligator-limit))))))

(defun elligator-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-nbytes *edcurve*
                          (lambda ()
                            (ceiling (elligator-nbits) 8))))

(defun elligator-int-padding ()
  ;; generate random padding bits for an elligator int
  (let* ((enb   (elligator-nbits))
         (nbits (mod enb 8)))
    (if (zerop nbits)
        0
      (ash (ctr-drbg-int (- 8 nbits)) enb))
    ))

(defun compute-csr ()
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (with-mod *ed-q*
    (let* ((dp1  (+ *ed-d* 1))
           (dm1  (- *ed-d* 1))
           (dsqrt (m* 2 (msqrt (- *ed-d*))))
           (c    (m/ (+ dsqrt dm1) dp1))
           (c    (if (quadratic-residue-p c)
                     c
                   (m/ (- dsqrt dm1) dp1)))
           (r    (m+ c (m/ c)))
           (s    (msqrt (m/ 2 c))))
      (list c s r ))))

(defun csr ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve*
                          :elligator-csr *edcurve*
                          'compute-csr))

(defun to-elligator-range (x)
  (ldb (byte (elligator-nbits) 0) x))

(defun elligator-decode (z)
  ;; z in (1,2^(floor(log2 *ed-q*/2)))
  ;; good multiple of bytes for curve-1174 is 248 bits = 31 bytes
  ;;                            curve-E382    376        47
  ;;                            curve-41417   408        51
  ;;                            curve-E521    520        65
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (let ((z (to-elligator-range z)))
    (declare (integer z))
    (cond ((= z 1)
           (ed-neutral-point))
          
          (t
           (with-mod *ed-q*
             (um:bind* (((c s r) (csr))
                        (u     (m/ (- 1 z) (1+ z)))
                        (u^2   (m* u u))
                        (c^2   (m* c c))
                        (u2c2  (+ u^2 c^2))
                        (u2cm2 (+ u^2 (m/ c^2)))
                        (v     (m* u u2c2 u2cm2))
                        (chiv  (mchi v))
                        (xx    (m* chiv u))
                        (yy    (m* (m^ (m* chiv v) (truncate (1+ *ed-q*) 4))
                                   chiv
                                   (mchi u2cm2)))
                        (1+xx  (1+ xx))
                        (x     (m/ (m* (- c 1)
                                       s
                                       xx
                                       1+xx)
                                   yy))
                        (y     (m/ (- (m* r xx)
                                      (m* 1+xx 1+xx))
                                   (+ (m* r xx)
                                      (m* 1+xx 1+xx))))
                        (pt    (make-ecc-proj-pt
                                :x  x
                                :y  y
                                :z  1)))
               ;; (assert (ed-satisfies-curve pt))
               pt
               )))
          )))
 
(defun elligator-encode (pt)
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  ;; return encoding tau for point pt, or nil if pt not in image of phi(tau)
  (if (ed-neutral-point-p pt)
      (logior 1 (elligator-int-padding))
    ;; else
    (with-mod *ed-q*
      (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                 (yp1  (1+ y)))
        (unless (zerop yp1)
          (um:bind* (((c s r) (csr))
                     (etar       (m* r (m/ (- y 1) (m* 2 yp1))))
                     (etarp1     (+ 1 etar))
                     (etarp1sqm1 (- (m* etarp1 etarp1) 1))
                     (scm1       (m* s (- c 1))))
            (when (and (quadratic-residue-p etarp1sqm1)
                       (or (not (zerop (m+ etar 2)))
                           (m= x (m/ (m* 2 scm1 (mchi c))
                                     r))))
              (um:bind* ((xx    (- (m^ etarp1sqm1 (floor (1+ *ed-q*) 4))
                                    etarp1))
                         (z     (mchi (m* scm1
                                          xx
                                          (1+ xx)
                                          x
                                          (+ (m* xx xx) (m/ (m* c c))))))
                         (u     (m* z xx))
                         (enc   (m/ (- 1 u) (1+ u)))
                         (tau   (min enc (m- enc))))
                ;; (assert (ed-pt= pt (elligator-decode enc))) ;; check that pt is in the Elligator set
                ;; (assert (< tau (elligator-limit)))
                (logior tau (elligator-int-padding))
                ))
            ))))
    ))

;; -------------------------------------------------------

(defun compute-deterministic-elligator-skey (seed &optional (index 0))
  ;; compute a private key from the seed that is safe, and produces an
  ;; Elligator-capable public key.
  (let* ((skey (compute-deterministic-skey seed index))
         (pkey (ed-nth-pt skey))
         (tau  (elli2-encode pkey)))
    (if tau
        (values skey tau index)
      (compute-deterministic-elligator-skey seed (1+ index)))
    ))

(defun compute-elligator-summed-pkey (sum-pkey)
  ;; post-processing step after summing public keys. This corrects the
  ;; summed key to become an Elligator-capable public key. Can only be
  ;; used on final sum, not on intermediate partial sums.
  (um:nlet iter ((ix 0))
    (let ((p  (ed-add sum-pkey (ed-nth-pt ix))))
      (or (elli2-encode p)
          (go-iter (1+ ix))))))
#|
(multiple-value-bind (skey1 pkey1) (compute-elligator-skey :dave)
  (multiple-value-bind (skey2 pkey2) (compute-elligator-skey :dan)
    (let ((p  (ed-add pkey1 pkey2)))
      (compute-elligator-summed-pkey p))))

(defun tst (nel)
  (let ((ans nil)
        (dict (make-hash-table)))
    (loop for ix from 0 below nel do
          (multiple-value-bind (skey tau ct)
              (compute-elligator-skey (ed-convert-int-to-lev ix 4))
            (if (gethash skey dict)
                (print "Collision")
              (setf (gethash skey dict) tau))
            (when (plusp ct)
              (push (cons ix ct) ans))))
    ans))
 |#
             
(defun compute-elligator-schnorr-deterministic-random (msgv k-priv)
  (um:nlet iter ((ix 0))
    (let* ((r     (with-mod *ed-r*
                            (mmod
                             (int
                              (hash/512
                               (levn ix 4)
                               (levn k-priv (elligator-nbytes))
                               msgv)))))
           (rpt   (ed-nth-pt r))
           (tau-r (elli2-encode rpt)))
      (if (and (plusp r) tau-r)
          (values r tau-r ix)
        (go-iter (1+ ix)))
      )))

#|
(defun tst (nel)
  (let ((ans  nil)
        (skey (compute-elligator-skey :dave)))
    (loop for ix from 0 below nel do
          (multiple-value-bind (r rpt ct)
              (compute-elligator-schnorr-deterministic-random
               (ed-convert-int-to-lev ix 4) skey)
            (declare (ignore r rpt))
            (when (plusp ct)
              (push (cons ix ct) ans))))
    ans))
 |#

(defun elligator-ed-dsa (msg k-priv)
  (let ((msg-enc (lev (loenc:encode msg)))
        (tau-pub (elli2-encode (ed-nth-pt k-priv))))
    (unless tau-pub
      (error "Not an Elligator key"))
    (multiple-value-bind (r tau-r)
        (compute-elligator-schnorr-deterministic-random msg-enc k-priv)
      (let* ((nbytes (elligator-nbytes))
             (s      (with-mod *ed-r*
                       (m+ r
                           (m* k-priv
                               (int
                                (hash/512
                                 (levn tau-r nbytes)
                                 (levn tau-pub nbytes)
                                 msg-enc))
                               )))))
        (list
         :msg     msg
         :tau-pub tau-pub
         :tau-r   tau-r
         :s       s)
        ))))

(defun elligator-ed-dsa-validate (msg tau-pub tau-r s)
  (let ((nbytes (elligator-nbytes)))
    (ed-pt=
     (ed-nth-pt s)
     (ed-add (elli2-decode tau-r)
             (ed-mul (elli2-decode tau-pub)
                     (int
                      (hash/512
                       (levn tau-r   nbytes)
                       (levn tau-pub nbytes)
                       (lev (loenc:encode msg))))
                     )))))

;; ------------------------------------------------------------

(defun do-elligator-random-pt (fn-gen)
  ;; search for a random multiple of *ed-gen*
  ;; such that the point is in the Elligator set.
  ;; Return a property list of
  ;;  :r   = the random integer in [1,q)
  ;;  :pt  = the random point in projective form
  ;;  :tau = the Elligator encoding of the random point
  ;;  :pad = bits to round out the integer length to multiple octets
  (um:nlet iter ()
    (multiple-value-bind (skey pkey) (ed-random-pair)
      (let ((tau  (and (plusp skey)
                       (funcall fn-gen pkey)))) ;; elligatorable? - only about 50% are
        (if tau
            (list :r   skey
                  :tau tau)
          (go-iter))
        ))))

(defun elligator-tau-vector (tau)
  ;; lst should be the property list returned by elligator-random-pt
  (levn tau (elligator-nbytes)))

(defun do-elligator-schnorr-sig (msg tau-pub k-priv fn-gen)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:nlet iter ()
    (let* ((lst   (funcall fn-gen))
           (vtau  (elligator-tau-vector (getf lst :tau)))
           (h     (int
                   (hash/512
                    vtau
                    (elligator-tau-vector tau-pub)
                    msg)))
           (r     (getf lst :r))
           (s     (with-mod *ed-r*
                    (m+ r (m* h k-priv))))
           (smax  (elligator-limit)))
      (if (>= s smax)
          (progn
            ;; (print "restart ed-schnorr-sig")
            (go-iter))
        (let* ((spad  (logior s (elligator-int-padding)))
               (svec  (elligator-tau-vector spad)))
          (list vtau svec))
        ))))

(defun do-elligator-schnorr-sig-verify (msg tau-pub sig fn-decode)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:bind* (((vtau svec) sig)
             (pt-pub (funcall fn-decode tau-pub))
             (pt-r   (funcall fn-decode (int vtau)))
             (h      (int
                      (hash/512
                       vtau
                       (elligator-tau-vector tau-pub)
                       msg)))
             (s      (to-elligator-range (int svec)))
             (pt     (ed-nth-pt s))
             (ptchk  (ed-add pt-r (ed-mul pt-pub h))))
    (ed-pt= pt ptchk)))

;; -------------------------------------------------------

(defun elligator-random-pt ()
  (do-elligator-random-pt #'elligator-encode))

(defun ed-schnorr-sig (m tau-pub k-priv)
  (do-elligator-schnorr-sig m tau-pub k-priv #'elligator-random-pt))

(defun ed-schnorr-sig-verify (m tau-pub sig)
  (do-elligator-schnorr-sig-verify m tau-pub sig #'elligator-decode))

;; -------------------------------------------------------

#|
(defun chk-elligator ()
  (loop repeat 1000 do
        ;; ix must be [0 .. (q-1)/2]
        (let* ((ix (random-between 0 (floor (1+ *ed-q*) 2)))
               (pt (elligator-decode ix))
               (jx (elligator-encode pt)))
          (assert (= ix jx))
          )))
(chk-elligator)

(let* ((lst     (elligator-random-pt))
       (k-priv  (getf lst :r))
       (pt-pub  (getf lst :pt))
       (tau-pub (elligator-tau-vector lst))
       (msg     (ensure-8bitv "this is a test"))
       (sig     (ed-schnorr-sig msg tau-pub k-priv)))
   (ed-schnorr-sig-verify msg tau-pub sig))

(let* ((lst     (elligator-random-pt))
       (k-priv  (getf lst :r))
       (pt-pub  (getf lst :pt))
       (tau-pub (elligator-tau-vector lst))
       (msg     (ensure-8bitv "this is a test"))
       (sig     (elli2-schnorr-sig msg tau-pub k-priv)))
   (elli2-schnorr-sig-verify msg tau-pub sig))

(let ((arr (make-array 256
                       :initial-element 0)))
  (loop repeat 10000 do
        (let* ((lst (elligator-random-pt))
               (tau (getf lst :tau)))
          (incf (aref arr (ldb (byte 8 200) tau)))))
  (plt:histogram 'xhisto arr
                 :clear t)
  (plt:plot 'histo arr
            :clear t)
  )
        
                      
|#

#|
(with-mod *ed-q*
  (let* ((c4d  (m* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
         (a    (m/ (m* 2 (1+ c4d)) (- c4d 1)))
         (b    1))
    (m* a b (- (m* a a) (m* 4 b))))) ;; must not be zero
|#

;; --------------------------------------------------------

(defun find-quadratic-nonresidue ()
  (um:nlet iter ((n  -1))
    (if (quadratic-residue-p n)
        (go-iter (1- n))
      n)))

(defun compute-elli2-ab ()
  ;; For Edwards curves:  x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  ;; x --> -2*c*u/v
  ;; y --> c*(1+u)/(1-u)
  ;; to get Elliptic curve: v^2 = (c^4*d -1)*(u^3 + A*u^2 + B*u)
  ;; v --> w*Sqrt(c^4*d - 1)
  ;; to get: w^2 = u^3 + A*u^2 + B*u
  ;; we precompute c4d = c^4*d, A = 2*(c^4*d+1)/(c^4*d-1), and B = 1
  ;; must have: A*B*(A^2 - 4*B) != 0
  (with-mod *ed-q*
    (let* ((c4d        (m* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
           (c4dm1      (- c4d 1))
           (sqrt-c4dm1 (msqrt c4dm1)) ;; used during coord conversion
           (a          (m/ (m* 2 (+ c4d 1)) c4dm1))
           (b          1)
           (u          (find-quadratic-nonresidue))
           (dscr       (- (m* a a) (m* 4 b))))
      ;; (assert (not (quadratic-residue-p *ed-q* dscr)))
      (assert (not (zerop (m* a b dscr))))
      (list sqrt-c4dm1 a b u))))
  
(defun elli2-ab ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve*
                          :elligator2-ab *edcurve*
                          'compute-elli2-ab))

(defun montgy-pt-to-ed (pt)
  ;; v = w * Sqrt(c^4*d-1)
  ;; x = -2*c*u/v
  ;; y = c*(1+u)/(1-u)
  ;; w^2 = u^3 + A*u^2 + B =>  x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (um:bind* ((:struct-accessors ecc-pt ((xu x)
                                        (yw y)) pt))
    (if (and (zerop xu)
             (zerop yw))
        (ed-neutral-point)
      ;; else
      (destructuring-bind (sqrt-c4dm1 a b u) (elli2-ab)
        (declare (ignore a b u))
        (with-mod *ed-q*
          (let* ((yv   (m* sqrt-c4dm1 yw))
                 (x    (m/ (m* -2 *ed-c* xu) yv))
                 (y    (m/ (m* *ed-c* (1+ xu)) (- 1 xu)))
                 (pt   (make-ecc-pt
                        :x  x
                        :y  y)))
            (assert (ed-satisfies-curve pt))
            pt))))))
              
(defun elli2-decode (r)
  ;; protocols using the output of elli2-decode must validate the
  ;; results. All output will be valid curve points, but many
  ;; protocols must avoid points of low order and the neutral point.
  ;;
  ;; Elli2-encode will provide a value of 0 for the neutral point.
  ;; But it will refuse to generate a value for the other low order
  ;; torsion points.
  ;;
  ;; However, that doesn't prevent an attacker from inserting values
  ;; for them.  There are no corresponding values for the low order
  ;; torsion points, apart from the neutral point. But certain random
  ;; values within the domain [0..(q-1)/2] are invalid.
  ;;
  (let ((r (to-elligator-range r))) ;; mask off top random
    (declare (integer r))
    (cond  ((zerop r)  (ed-neutral-point))
           (t
            (with-mod *ed-q*
              (um:bind* (((sqrt-c4dm1 a b u) (elli2-ab))
                         (u*r^2   (m* u r r))
                         (1+u*r^2 (1+ u*r^2)))
                
                ;; the following error can never trigger when fed with
                ;; r from elli2-encode. But random values fed to us
                ;; could cause it to trigger the error.
                
                (when (or (zerop 1+u*r^2)     ;; this could happen for r^2 = -1/u
                          (= (m* a a u*r^2)   ;; this can never happen: B=1 so RHS is square
                             (m* b 1+u*r^2 1+u*r^2)))  ;; and LHS is not square.
                  (error "invalid argument"))
                
                (let* ((v    (- (m/ a 1+u*r^2)))
                       (eps  (mchi (+ (m* v v v)
                                      (m* a v v)
                                      (m* b v))))
                       (xu   (- (m* eps v)
                                (m/ (m* (- 1 eps) a) 2)))
                       (rhs  (m* xu
                                 (+ (m* xu xu)
                                    (m* a xu)
                                    b)))
                       (yw   (- (m* eps (msqrt rhs))))
                       ;; now we have (xu, yw) as per Bernstein: yw^2 = xu^3 + A*xu^2 + B*xu
                       ;; Now convert- to our Edwards coordinates:
                       ;;   (xu,yw) --> (x,y): x^2 + y^2 = c^2*(1 + d*x^2*y^2)
                       (yv   (m* sqrt-c4dm1 yw))
                       (x    (m/ (m* -2 *ed-c* xu) yv))
                       (y    (m/ (m* *ed-c* (1+ xu)) (- 1 xu)))
                       (pt   (make-ecc-proj-pt
                              :x  x
                              :y  y
                              :z  1)))
                  #|
                  (assert (ed-satisfies-curve pt)) ;; true by construction
                  (assert (not (zerop (m* v eps xu yw)))) ;; true by construction
                  (assert (= (m* yw yw) rhs))     ;; true by construction
                  |#
                  pt
                  ))))
           )))
  
(defun ed-pt-to-montgy (pt)
  ;; u = (y - c)/(y + c)
  ;; v = -2 c u / x
  ;; w = v / sqrt(c^4 d - 1)
  ;; montgy pt (u,w) in: w^2 = u^3 + A u^2 + B u
  (if (ed-neutral-point-p pt)
      (make-ecc-pt
       :x 0
       :y 0)
    ;; else
    (with-mod *ed-q*
      (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                 ((sqrt-c4dm1 a b u) (elli2-ab))
                 (declare (ignore u))
                 (xu   (m/ (- y *ed-c*) (+ y *ed-c*)))
                 (yv   (m/ (m* -2 *ed-c* xu) x ))
                 (yw   (m/ yv sqrt-c4dm1)))
        (assert (= (m* yw yw)
                   (m+ (m* xu xu xu)
                       (m* a xu xu)
                       (m* b xu))))
        (make-ecc-pt
         :x xu
         :y yw)))))
             
(defun elli2-encode (pt)
  ;; Elligator2 mapping of pt to Zk
  ;; return Zk or nil
  (cond ((ed-neutral-point-p pt)
         (elligator-int-padding))
        (t
         (with-mod *ed-q*
           (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                      ((sqrt-c4dm1 a b u) (elli2-ab))
                      (declare (ignore b))
                      ;; convert our Edwards coords to the form needed by Elligator-2
                      ;; for Montgomery curves
                      (xu   (m/ (- y *ed-c*) (+ y *ed-c*)))
                      (yv   (m/ (m* -2 *ed-c* xu) x ))
                      (yw   (m/ yv sqrt-c4dm1))
                      (xu+a (+ xu a)))
             ;; now we have (x,y) --> (xu,yw) for:  yw^2 = xu^3 + A*xu^2 + B*xu
             #|
             (labels ((esqrt (x)
                        (cond ((= 3 (mod *ed-q* 4))
                               (m^ x (floor (1+ *ed-q*) 4)))
                              ((= 5 (mod *ed-q* 8))
                               (m^ x (floor (+ 3 *ed-q*) 8)))
                              (t
                               (error "NYI"))
                              )))
               (cond ((zerop xu)
                      (elligator-int-padding))
                     ((zerop yw)
                      (elligator-int-padding))
                     ((zerop xu+a)
                      (elligator-int-padding))
                     ((quadratic-residue-p yw)
                      (let ((r (esqrt (- (m/ xu xu+a u)))))
                        (logior (min r (m- r))
                                (elligator-int-padding))))
                     (t
                      (let ((r (esqrt (- (m/ xu+a xu u)))))
                        (logior (min r (m- r))
                                (elligator-int-padding))))
                     |#
                     #||#
                  (when (and (not (zerop xu+a))
                             (or (not (zerop yw))
                                 (zerop xu))
                             (quadratic-residue-p (- (m* u xu xu+a))))
                    (let* ((e2    (if (quadratic-residue-p yw)
                                      (m/ xu xu+a)
                                    (m/ xu+a xu)))
                           (enc   (msqrt (m/ e2 (- u))))
                           (tau   (min enc (m- enc)))
                           ;; (ur2   (m* u tau tau))
                           ;; (1pur2 (1+ ur2))
                           )
                      
                      ;; (assert (< tau (elligator-limit)))
                      #|
                     (when (zerop 1pur2) ;; never happens, by construction
                       (format t "~%Hit magic #1: tau = ~A" tau))
                     (when (= (m* a a ur2) ;; never happens, by construction
                              (m* b 1pur2 1pur2))
                       (format t "~%Hit magic #2: tau = ~A" tau))
                     
                     (unless (or (zerop 1pur2) ;; never happens, by construction
                                 (= (m* a a ur2)
                                    (m* b 1pur2 1pur2)))
                       tau)
                     |#
                      (logior tau (elligator-int-padding))
                      ))
                  #||#
                  )))))

(defun get-shares ()
  (values *ed-sk3* *ed-sk1* *ed-sk2*))

(defun elli2-random-pt ()
  (do-elligator-random-pt #'elli2-encode))

(defun elli2-schnorr-sig (m tau-pub k-priv)
  (do-elligator-schnorr-sig m tau-pub k-priv #'elli2-random-pt))

(defun elli2-schnorr-sig-verify (m tau-pub sig)
  (do-elligator-schnorr-sig-verify m tau-pub sig #'elli2-decode))

(defun distribute-shares (shares)
  (setf *ed-sk1* (first shares)
        *ed-sk2* (second shares)
        *ed-sk3* (third shares)))

;; ------------------------------------------------------------------------------
;; General scheme for creating private / public keys with Elligator encodings...
;; (elli2-random-pt) => property list with (getf ans :r) = private key integer
;;                                         (+ (getf ans :tau)
;;                                            (getf ans :padding)) = public Elligator integer
;; ------------------------------------------------------------------------------
#|
(defun chk-elli2 ()
  (loop repeat 100 do
        (let* ((lst (elli2-random-pt))
               (pt  (ed-affine (getf lst :pt)))
               (tau (getf lst :tau))
               (pt2 (ed-affine (elli2-decode tau))))
          (assert (ed-pt= pt pt2))
          )))
(chk-elli2)

(let ((arr (make-array 256
                       :initial-element 0)))
  (loop repeat 10000 do
        (let* ((lst (elli2-random-pt))
               (tau (getf lst :tau)))
          (incf (aref arr (ldb (byte 8 200) tau)))))
  (plt:histogram 'xhisto arr
                 :clear t)
  (plt:plot 'histo arr
            :clear t)
  )

;; pretty darn close to 50% of points probed
;; result in successful Elligator-2 encodings
(let ((cts 0))
  (loop repeat 1000 do
        (let* ((ix (field-random (* *ed-h* *ed-r*)))
               (pt (ed-mul *ed-gen* ix)))
          (when (elli2-encode pt)
            (incf cts))))
  (/ cts 1000.0))
|#
             
;; ------------------------------------------------------------------------------
;; CORE-CRYPTO:STARTUP and CORE-CRYPTO:SHUTDOWN

(defun startup-edwards ()
  #-:WINDOWS (core-crypto:ensure-dlls-loaded)
  (format t "~%Connecting Edwards Curves")
  (set-ed-curve :curve1174))

(core-crypto:add-to-startups 'startup-edwards)

(defun shutdown-edwards ()
  (format t "~%Disconnecting Edwards Curves")
  (setf *edcurve* nil))

(core-crypto:add-to-shutdowns 'shutdown-edwards)
