
(in-package :edec-ff)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------------------

(defmacro define-ecc-curve (name &rest args)
  (lw:with-unique-names (embed-ff curve-ff)
    (let* ((wm-name         (string-trim '(#\*) name))
           (ff-embed-name   (um:symb wm-name "-ff-embed"))
           (ff-curve-name   (um:symb wm-name "-ff-curve")))
      `(let ((,embed-ff (ffbase ,(getf args :q) :inst-class ',ff-embed-name))
             (,curve-ff (ffbase ,(getf args :r) :inst-class ',ff-curve-name)))
         (define-ffield1 ,ff-embed-name ,embed-ff)
         (define-ffield1 ,ff-curve-name ,curve-ff)
         (defparameter ,name
           (,(if (getf args :affine-mul)
                 'make-fast-ed-curve
               'make-ed-curve)
            :ff-embed ,embed-ff
            :ff-curve ,curve-ff
            ,@args)))
      )))

(defmacro with-embedding-field (&body body)
  `(with-field *ed-ff-embed*
     ,@body))

(defmacro with-curve-field (&body body)
  `(with-field *ed-ff-curve*
     ,@body))


#+:LISPWORKS
(editor:setup-indent "define-ecc-curve" 1)

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

(define-ecc-curve *curve1174*
  ;; rho-security (security by Pollard's rho attack) = 2^124.3
  ;; rho-security = (* 0.886 (sqrt *ed-r*))  (0.886 = (sqrt (/ pi 4)))
  ;; x^2 + y^2 = 1 - 1174 x^2 y^2
  
  :name :Curve1174
  :c    1
  :d    -1174.
  :q    (- (ash 1 251.) 9.)
  :r    904625697166532776746648320380374280092339035279495474023489261773642975601.
   ;; = 2^249 - 11332719920821432534773113288178349711
   :h    4.  ;; cofactor -- #E(K) = h*r
   :gen  (make-ecc-pt
          :x  1582619097725911541954547006453739763381091388846394833492296309729998839514.
          :y  3037538013604154504764115728651437646519513534305223422754827055689195992590.)
   
   :affine-mul '_Curve1174-affine-mul
   :proj-mul   '_Curve1174-projective-mul
   :proj-add   '_Curve1174-projective-add
   :to-affine  '_Curve1174-to-affine)
              
;; ---------------------------

(define-ecc-curve *curve-E382*
  ;; rho-security = 2^188.8

  :name :Curve-E382
  :c    1
  :d    -67254.
  :q    (- (ash 1 382.) 105.)
  :r    2462625387274654950767440006258975862817483704404090416745738034557663054564649171262659326683244604346084081047321.
  ;; = 2^380 - 1030303207694556153926491950732314247062623204330168346855
  :h    4.
  :gen  (make-ecc-pt
         :x  3914921414754292646847594472454013487047137431784830634731377862923477302047857640522480241298429278603678181725699.
         :y  17.) )

;; ---------------------------

(define-ecc-curve *curve41417*
  ;; rho-security = 2^205.3

  :name :Curve41417
  :c    1
  :d    3617.
  :q    (- (ash 1 414.) 17.)
  :r    5288447750321988791615322464262168318627237463714249754277190328831105466135348245791335989419337099796002495788978276839289.
  ;; = 2^411 - 33364140863755142520810177694098385178984727200411208589594759
  :h    8.
  :gen  (make-ecc-pt
         :x  17319886477121189177719202498822615443556957307604340815256226171904769976866975908866528699294134494857887698432266169206165.
         :y  34.) )

(defvar *ed-sk1* '(95246105359107334368835702946360885221712654429149134602107470274728322648.
                   . 871438026695917300513750426093020668134374754750828650996077213558451945727.))

;; ---------------------------

(define-ecc-curve *curve-Ed448* ;; the Goldilocks curve
  ;; rho-security = 2^222.8

  :name :Curve-Ed448
  :c    1
  :d    -39081.
  :q    (- (ash 1 448.) (ash 1 224.) 1)
  :r    (- (ash 1 446.) 13818066809895115352007386748515426880336692474882178609894547503885.)
  :h    4.
  :gen  (make-ecc-pt
         :x  117812161263436946737282484343310064665180535357016373416879082147939404277809514858788439644911793978499419995990477371552926308078495.
         :y  19.))

(defvar *ed-sk2* '(421974047105585348268953778604183148636297453017931755410200849399520674162.
                   . 599385664174222180908461435453721901391720075307525572554742060814390186035.))

;; ---------------------------

(define-ecc-curve *curve-E521*
  ;; rho-security = 2^259.3
  
  :name :curve-E521
  :c    1
  :d    -376014.
  :q    (- (ash 1 521.) 1)
  :r    1716199415032652428745475199770348304317358825035826352348615864796385795849413675475876651663657849636693659065234142604319282948702542317993421293670108523.
  ;; = 2^519 - 337554763258501705789107630418782636071904961214051226618635150085779108655765
  :h    4.
  :gen  (make-ecc-pt
         :x  1571054894184995387535939749894317568645297350402905821437625181152304994381188529632591196067604100772673927915114267193389905003276673749012051148356041324.
         :y  12.)
  :affine-mul '_CurveE521-affine-mul
  :proj-mul   '_CurveE521-projective-mul
  :proj-add   '_CurveE521-projective-add
  :to-affine  '_CurveE521-to-affine)

(defvar *ed-sk3* '(92467223671543291667538537127794393729406965156397932625577105047698643617.
                   . 80308136573684915858899498388575132495612607448470528849337536761764056915.))


;; ---------------------------

(define-ecc-curve *curve-Ed3363*  ;; the High-Five curve from MIRACL Labs
  ;; y^2 + x^2 = 1 + 11111 x^2 y^2
  ;; rho-security = 2^168 to 2^192 ?? just a guess

  :name :curve-Ed3363
  :c    1
  :d    11111.  ;; the High-Five! 
  :q    (- (ash 1 336.) 3.)
  :r    #x200000000000000000000000000000000000000000071415FA9850C0BD6B87F93BAA7B2F95973E9FA805
  ;; = 2^333 + 10345181422283708075595279275609870082194840463365
  :h    8.
  :gen  (make-ecc-pt
         :x  #x0c
         :y  #xC0DC616B56502E18E1C161D007853D1B14B46C3811C7EF435B6DB5D5650CA0365DB12BEC68505FE8632)
  
  :affine-mul '_Ed3363-affine-mul
  :proj-mul   '_Ed3363-projective-mul
  :proj-add   '_Ed3363-projective-add
  :to-affine  '_Ed3363-to-affine)

;; ------------------------------------------------------

(defvar *edcurve* )

(define-symbol-macro *ed-c*        (ed-curve-c        *edcurve*)) ;; c constant in curve equation
(define-symbol-macro *ed-d*        (ed-curve-d        *edcurve*)) ;; d constant in curve equation
(define-symbol-macro *ed-q*        (ed-curve-q        *edcurve*)) ;; prime base of the coordinate field
(define-symbol-macro *ed-r*        (ed-curve-r        *edcurve*)) ;; primt bsae on the curve field
(define-symbol-macro *ed-h*        (ed-curve-h        *edcurve*)) ;; cofactor for the containing field
(define-symbol-macro *ed-gen*      (ed-curve-gen      *edcurve*)) ;; generator point for the curve
(define-symbol-macro *ed-name*     (ed-curve-name     *edcurve*)) ;; keyword symbol name of the curve
(define-symbol-macro *ed-ff-embed* (ed-curve-ff-embed *edcurve*))
(define-symbol-macro *ed-ff-curve* (ed-curve-ff-curve *edcurve*))

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



