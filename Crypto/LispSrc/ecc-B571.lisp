; ecc-b571.lisp -- Elliptic Curve Crypto on NIST B-571 over GF(2^571)
;; DM/Acudora  11/11
;; ----------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------
;; ECC Routines over NIST B-571

(defun ecc-infinity ()
  +ecc-inf+)

(defun ecc-infinite-p (pt)
  (typep pt 'ecc-infinity))

;; ---------------------------------------------

(defun ecc-affine (pt)
  (optima:match pt
    ((ecc-infinite-)  pt)
    ((ecc-pt-)        pt)
    ((ecc-projective-pt- :x x :y y :z z)
       ;; for Jacobian projective coordinates (x, y, z) --> (x/z^2, y/z^3)
       (let* ((zinv (gfinv z))
              (zsq  (gf^2 zinv)))
         (make-ecc-pt
          :x (gf* x zsq)
          :y (gf* y zinv zsq))
         ))
    ))
       
(defun ecc-projective (pt &key (alpha 1))
  (optima:match pt
    ((ecc-infinite-)  pt)
    ((ecc-pt- :x x :y y)
       ;; for Jacobian projective coordinates (x, y) --> (alpha^2*x, alpha^3*y, alpha)
       ;; alpha in K*
       (make-ecc-projective-pt
        :x x
        :y y
        :alpha alpha))
    ((ecc-projective-pt-) pt)))

;; --------------------------------------------------------------

(defun ecc-affine-double (pt)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x)
                                        (y1 y)) pt)
             (s  (gf+ x1 (gf/ y1 x1)))
             (x2 (gf+ (gf^2 s) s *ecc-a*))
             (y2 (gf+ (gf^2 x1) (gf* (gf+ s 1) x2))))
    (make-ecc-pt :x x2 :y y2)) )


(defun ecc-affine-add (pt1 pt2)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x)
                                        (y1 y)) pt1)
             (:struct-accessors ecc-pt ((x2 x)
                                        (y2 y)) pt2))
    (cond
     ((= x1 x2)
      (cond ((= y1 y2)
             (ecc-affine-double pt1))
            ((= y1 (gf+ x2 y2))
             (ecc-infinity))
            (t (error "ecc-affine-add: points not on curve"))
            ))
     (t
        (let* ((x1+x2  (gf+ x1 x2))
               (s      (gf/ (gf+ y1 y2) x1+x2))
               (x3     (gf+ (gf^2 s) s x1+x2 *ecc-a*))
               (y3     (gf+ (gf* s (gf+ x1 x3)) x3 y1)))
          (make-ecc-pt :x x3 :y y3)))
       )))


;; --------------------------------------------------------------
;;
;; Jacobian Projective Coordinates
;; y^2 + x.y = x^3 + a.x^2 + b, a in {0,1}^n
;;

(defun ecc-projective-double (pt)
  ;; for Jacobian projective coordinates (x, y, z)
  (um:bind* ((:struct-accessors ecc-projective-pt ((x1 x)
                                                   (y1 y)
                                                   (z1 z)) pt)
             (z3      (gf* x1 z1 z1))
             (x1sq    (gf^2 x1))
             (tmp1    (gf+ (gf* y1 z1) x1sq))
             (x3      (gf+ (gf^2 tmp1)
                           (gf* z3 (gf+ (gf* *ecc-a* z3)
                                        tmp1))))
             (y3      (gf+
                       (gf* z3
                            (gf+ x3
                                 (gf^2 x1sq)))
                       (gf* tmp1 x3))))
    (make-ecc-projective-pt :x x3 :y y3 :z z3 :alpha 1) ))


(defun ecc-projective-add (pt1 pt2)
  ;; for Jacobian projective coordinates (x, y, z)
  (um:bind* ((:struct-accessors ecc-projective-pt ((x1 x)
                                                   (y1 y)
                                                   (z1 z)) pt1)
             (:struct-accessors ecc-projective-pt ((x2 x)
                                                   (y2 y)
                                                   (z2 z)) pt2)
             (z1sq  (gf* z1 z1))
             (z2sq  (gf* z2 z2))
             (t1    (gf+ (gf* x1 z2sq)
                         (gf* x2 z1sq)))
             (t2    (gf+ (gf* y1 z2 z2sq)
                         (gf* y2 z1 z1sq))))
    (if (zerop t1)
        (if (zerop t2)
            (ecc-projective-double pt1)
          (ecc-infinity))
      ;; else
      (let* ((z3    (gf* z1 z2 t1))
             (x3    (gf+ (gf^ t1 3)
                         (gf* *ecc-a* z3 z3)
                         (gf* t2 z3)
                         (gf^2 t2)))
             (tc    (gf* z2 t1))
             (y3    (gf+ (gf* tc (gf+ (gf* z1 x3)
                                      (gf* y1 tc tc)))
                         (gf* t2 (gf+ x3 (gf* x1 tc tc)))))
             )
        
        (make-ecc-projective-pt :x x3 :y y3 :z z3 :alpha 1)) )
    ))

;; --------------------------------------------------------------

(defun ecc-pt= (pt1 pt2)
  (optima:match pt1
    ((ecc-infinite-)      (ecc-infinite-p pt2))
    ((ecc-pt- :x x1 :y y1)
       (optima:match pt2
         ((ecc-infinite-)   nil)
         ((ecc-pt- :x x2 :y y2)
            (and (= x1 x2)
                 (= y1 y2)))
         ((ecc-projective-pt-)
            (ecc-pt= pt1 (ecc-affine pt2)))
         ))
    ((ecc-projective-pt- :x x1 :y y1 :z z1)
       (optima:match pt2
         ((ecc-infinite-)  nil)
         ((ecc-pt-)
            (ecc-pt= pt1 (ecc-projective pt2)))
         ((ecc-projective-pt- :x x2 :y y2 :z z2)
            (and (= (gf* x1 z2 z2) (gf* x2 z1 z1))
                 (= (gf* y1 z2 z2 z2) (gf* y2 z1 z1 z1))))
         ))
    ))

(defun ecc-negate (pt)
  (optima:match pt
    ((ecc-infinite-)  pt)
    ((ecc-pt- :x x :y y)
     (make-ecc-pt
      :x x
      :y (gf+ x y)))
    ((ecc-projective-pt- :x x :y y :z z)
     (make-ecc-projective-pt
      :x x
      :y (gf+ y (gf* x z))
      :z z
      :alpha 1))
    ))


(defstub c-ecc571-add)

(defun ecc-double (pt)
  (optima:match pt
    ((ecc-infinite-)  pt)
    ((ecc-pt-)
       (case *nbits*
         (571  (um:with-fast-impl (c-ecc571-add)
                               (c-ecc571-add pt pt)
                               (ecc-affine-double pt)))
         
         (t  (ecc-affine-double pt))
         ))
    ((ecc-projective-pt-)
       (ecc-projective-double pt))
    ))

(defun ecc-add (pt1 pt2)
  ;; here addition is affine when both args are affine
  ;; otherwise projective is contageous
  (labels ((basic-affine-add (p1 p2)
             (case *nbits*
               (571  (um:with-fast-impl (c-ecc571-add)
                                     (c-ecc571-add p1 p2)
                                     (ecc-affine-add p1 p2)))
               (t  (ecc-affine-add p1 p2))
               )))
    
    (optima:match pt1
      ((ecc-infinite-)  pt2)
      ((ecc-pt-)
         (optima:match pt2
           ((ecc-infinite-)  pt1)
           ((ecc-pt-)        (basic-affine-add pt1 pt2))
           ((ecc-projective-pt-)
              (ecc-projective-add (ecc-projective pt1) pt2))
           ))
      ((ecc-projective-pt-)
         (optima:match pt2
           ((ecc-infinite-)  pt1)
           ((ecc-pt-)
              (ecc-projective-add pt1 (ecc-projective pt2)))
           ((ecc-projective-pt-)
              (ecc-projective-add pt1 pt2))
           ))
      )))

(defun ecc-sub (pt1 pt2)
  (ecc-add pt1 (ecc-negate pt2)))

;; ---------------------------------------------

(defun ecc-mul-driver (pt n prepfn)
  ;; left-to-right algorithm
  ;; assumes n has been wrapped modulo *ecc-r*
  ;; input pt is in affine coords, result is in affine coords
  (let ((nn (mod n *ecc-r*)))
    (cond ((zerop nn)  (ecc-infinity))
          
          ((or (= nn 1)
               (ecc-infinite-p pt))  pt)
          
          (t  (let* ((r0  (funcall prepfn pt))
                     (l   (1- (integer-length nn)))
                     (v   (vector r0
                                  r0)))
                (loop repeat l do
                      (decf l)
                      (setf (aref v 1)                   (ecc-double (aref v 1))
                            (aref v (ldb (byte 1 l) nn)) (ecc-add (aref v 1) r0)))
                (ecc-affine (aref v 1))))
          )))

(defun ecc-projective-mul (pt n &key (alpha 1))
  ;; left-to-right algorithm
  ;; for projective coordinates
  (ecc-mul-driver pt n
                  (lambda (pt)
                    (ecc-projective pt :alpha alpha)) ))
 


(defstub c-ecc571-mul)

(defun ecc-affine-mul (pt n)
  ;; left-to-right algorithm
  ;; for affine coordinates
  (ecc-mul-driver pt n
                  'identity))

(defun ecc-basic-mul (pt n &key (alpha 1))
  (optima:match pt
    ((ecc-infinite-) pt)
    ((ecc-pt-)
       (case *nbits*
         (571  (um:with-fast-impl (c-ecc571-mul)
                               (c-ecc571-mul pt n :alpha alpha)
                               (ecc-projective-mul pt n :alpha alpha)))
         (t (ecc-projective-mul pt n :alpha alpha))))
    ((ecc-projective-pt-)
       (ecc-projective-mul pt n))
    ))
    
(defun ecc-basic-div (pt n)
  (with-mod *ecc-r*
    (ecc-basic-mul pt (m/ n))))

;; -----------------------------------------------------------------
;; safe version of ecc-mul

(defun ecc-mul (pt n)
  (with-mod *ecc-r*
    (let* ((alpha     (gf-random-k*))
           (gf-alpha  (gfinv alpha))
           (n*alpha   (* n alpha))
           (inv-alpha (m/ alpha)))
      (ecc-basic-mul
       (ecc-basic-mul pt n*alpha :alpha gf-alpha)
       inv-alpha))))

(defun ecc-div (pt n)
  (with-mod *ecc-r*
    (ecc-mul pt (m/ n))))

(defun ecc-halve (pt)
  (optima:match pt
    ((ecc-infinite-) pt)
    ((ecc-pt- :x x :y y)
       (let* ((lam (gf-solve-quadratic (gf+ x *ecc-a*)))
              (xh  (gf-sqrt (gf+ y
                                 (gf* x (gf+ lam 1)))))
              (yh  (gf* xh (gf+ lam xh))))
         (make-ecc-pt
          :x xh
          :y yh)) )
    ((ecc-projective-pt-)
       (ecc-halve (ecc-affine pt)))
    ))

;; ---------------------------------------------

(defun ecc+ (pt &rest args)
  (reduce 'ecc-basic-add args :initial-value pt))

(defun ecc- (pt &rest args)
  (if args
      (reduce 'ecc-sub args :initial-value pt)
    (ecc-negate pt)))

(defun ecc* (pt &rest args)
  (ecc-mul pt (apply '* args)))

(defun ecc/ (pt &rest args)
  (ecc-div pt (apply '* args)))

;; --------------------------------------------------------
;;

(defun ecc-solution-p (pt)
  (optima:match pt
    ((ecc-infinite-)  nil)
    ((ecc-pt- :x x :y y)
       ;; verify: y^2 + x*y = x^3 + a*x^2 + b
       (zerop
        (gf+ (gf* y (gf+ x y))
             (gf* x x (gf+ x *ecc-a*))
             *ecc-b*)))
    ((ecc-projective-pt- :x x :y y :z z)
       ;; (x,y) --> (x',y',z') = (s^2*x, s^3*y, s)
       ;;
       ;; verify: y^2 + x*y = x^3 + a*x^2 + b
       ;; -->  y'^2 + z'*x'*y' = x'^3 + a*z'^2*x'^2 + b*z'^6 
       (let* ((xz  (gf* x z)))
         (zerop
          (gf+ (gf* *ecc-b* (gf^ z 6))
               (gf* xz (gf+ y (gf* xz *ecc-a*)))
               (gf* y y)
               (gf^ x 3))) ))
    ))

(defun ecc-validate-public-key (pt)
  (assert (not (ecc-infinite-p pt)))
  (optima:match pt
    ((ecc-pt- :x x :y y)
       (assert (<= (integer-length x) *nbits*))
       (assert (<= (integer-length y) *nbits*))
       ;; (assert (= (gf-trace x) (gf-trace *ecc-a*)))
       )
    ((ecc-projective-pt- :x x :y y :z z)
       (assert (<= (integer-length x) *nbits*))
       (assert (<= (integer-length y) *nbits*))
       (assert (<= (integer-length z) *nbits*)))
    )
  (assert (ecc-solution-p pt))
  ;; we have to say the next one like this because the mul operator
  ;; performs a modulo *ecc-r* ahead of the point operation.
  ;; Note: this clause also catches those points with
  ;; incorrect Tr(x). Should always have Tr(pt.x) = Tr(*ecc-a*)
  (assert (ecc-infinite-p (ecc-add pt (ecc-basic-mul pt (1- *ecc-r*)))))
  (assert (not (ecc-infinite-p (ecc-basic-mul pt *ecc-h*))))
  pt)

(ecc-validate-public-key *ecc-gen*)

;; --------------------------------------------------------
;;

(defun ecc-random-key ()
  ;; top bits are random, bottom 128 bits are a v.1 UUID nonce
  ;;   bottom 48 bits identify the machine by MAC address
  ;;   next 16 bits are random
  ;;   next 60 bits are timestamp to nearest 100 ns
  ;;   next 4 bits are #b0001
  ;; Attempts to produce a unique random number on each invocation
  (let* ((r  (random-between 1 *ecc-r*)))
    (dpb (uuid:uuid-to-integer (uuid:make-v1-uuid)) (byte 128 0) r)))

;; ----------------------------------------------------------

(defun ecc-make-lagrange-interpolator (shares)
  (with-mod *ecc-r*
    (labels ((lprod (x xs)
               (reduce (lambda (prod x2)
                         (m* prod (m- x2 x)))
                       xs
                       :initial-value 1)))
      (lambda (x0)
        (labels ((term (sum share)
                   (um:bind* ((:struct-accessors crypto-share (x y) share)
                              (xs (mapcar 'crypto-share-x (remove share shares))))
                     (ecc-add sum
                              (ecc-mul y (m/ (lprod x0 xs)
                                             (lprod x xs)) )) )))
          (reduce #'term shares
                  :initial-value (ecc-infinity) ))))))

(defun ecc-solve-lagrange (shares &optional (x 0))
  (let ((fn (ecc-make-lagrange-interpolator shares)))
    (funcall fn x)))

;; -------------------------------------------------------------
#|
(defun gf571-hash (msg &key (key 0))
  (let* ((pt    (ecc-mul *ecc-gen* (+ key key 1)))
         (p1    (ecc-pt-x pt))
         (p2    (ecc-pt-y pt))
         (msg   (ensure-8bitv msg))
         (len   (length msg))
         (ctr   0))

    (labels ((xor-nbr (n)
               (let ((px p1)
                     (nn (gf+ n
                              (ash (incf ctr) (* 8 64)))))
                 (setf p1 (gf* p2 nn)
                       p2 (gf/ px nn)) ))
             
             (xor-vector (src)
               (xor-nbr (convert-bytes-to-int src))))

      (xor-nbr len)
      (dolist (item (um:group msg 64))
        (xor-vector item))
      p2)))


(defun ecc-hash (msg &key (key 0))
  (let* ((p1    (ecc-mul *ecc-gen* (+ key key 1)))
         (p2    (ecc-mul p1 (ecc-pt-x p1)))
         (msg   (ensure-8bitv msg))
         (len   (length msg))
         (ctr   0))

    (labels ((xor-nbr (n)
               (setf p1 (ecc-mul p1 (+ (ecc-pt-x p2)
                                       (ash (incf ctr) (* 8 64))
                                       n))
                     p2 (ecc-mul p2 (ecc-pt-x p1))) )
             
             (xor-vector (src)
               (xor-nbr (convert-bytes-to-int src))))

      (xor-nbr len)
      (dolist (item (um:group msg 64))
        (xor-vector item))
      (ecc-pt-x p2))))
      
(defun ecc-ctr-encrypt (msg key)
  (let* ((delta (ecc-mul *ecc-gen* key))
         (pt    delta)
         (msg   (ensure-8bitv msg))
         (len   (length msg))
         (nblks (truncate len 64))
         (tail  (* nblks 64))
         (out   (make-ub-array len)))

    (labels ((xor (start end)
               (let* ((submsg (subseq msg start end))
                      (mask   (convert-int-to-nbytes
                               (ecc-pt-x pt) (- end start))))
                 (map-into submsg #'logxor submsg mask)
                 (replace out submsg
                          :start1 start)
                 (setf pt (ecc-add pt delta)))))
      
      (loop repeat nblks
            for ix-start from  0 by 64
            for ix-end   from 64 by 64
            do
            (xor ix-start ix-end))
      
      (when (< tail len)
        (xor tail len))

      out)))
|#

;; ------------------------------------------------------------
;; for use in transition of old shares as nested lists
;; to new shares as structures

(defmethod cvt-share ((lst cons))
  (destructuring-bind (abscissa (pt-x pt-y)) lst
    (make-crypto-share
     :x abscissa
     :y (make-ecc-pt
         :x pt-x
         :y pt-y))))

(defmethod cvt-share ((shr crypto-share))
  shr)

(defmethod cvt-point ((lst cons))
  (destructuring-bind (x y) lst
    (make-ecc-pt
     :x x
     :y y)))

(defmethod cvt-point (pt)
  (ecc-affine pt))

;; -----------------------------------------------------------

(defun ecc-compress-pt (pt)
  ;; based on s = y/x, s^2 + s = x + a + b/x^2
  ;; report (x XOR Tr(y/x)), Tr(z) in [0,1]
  ;; Since we know Tr(x) = Tr(a), we can peel off the LSB of x
  ;; and adjust to the same trace as Tr(a).
  (um:bind* ((:struct-accessors ecc-pt (x y)
              (ecc-affine (ecc-validate-public-key pt))))
    (logxor x (gf-trace (gf/ y x))) ))

(defun ecc-decompress-pt (xc)
  ;; based on s = y/x, s^2 + s = x + a + b/x^2
  ;; reported xc is  (x XOR Tr(y/x))
  ;; Solve for s, then report (x,x*s)
  ;; Since we know Tr(x) = Tr(a), we can peel off the LSB of xc
  ;; and adjust to the same trace as Tr(a).
  (handler-case
      (ecc-validate-public-key
       (let* ((trxor (logand xc 1))
              (x     (ensure-trace xc (gf-trace *ecc-a*)))
              (tr    (logxor trxor (logand x 1)))
              (xsq   (gf* x x))
              (s     (ensure-trace
                      (gf-solve-quadratic
                       (gf+ x *ecc-a* (gf/ *ecc-b* xsq)))
                      tr)))
         (make-ecc-projective-pt
          :x x
          :y (gf* x s) )))
    
    (error ()
      (error "Invalid point"))
    ))

;; -----------------------------------------------------------------
;; Test vectors
#|
 Curve: B571
-------------
(defun check-b571-test-vectors ()
  (let* ((tv '(
               :k 1
               :x #x0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABDBDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927E1E7769C8EEC2D19
               :y #x037BF27342DA639B6DCCFFFEB73D69D78C6C27A6009CBBCA1980F8533921E8A684423E43BAB08A576291AF8F461BB2A8B3531D2F0485C19B16E2F1516E23DD3C1A4827AF1B8AC15B

               :k 2
               :x #x01F8BD0B0C77369F3C5A1943C01215CAD8C7018C4AF1A588E6EFE81C0A39E0A50DB8E55BB371D956B15DBCB13AB12AF532B1FC6B7DDF0A13D12DFAA76051132B84020BEC72D2F265
               :y #x044A9E41F77686550649D5D124021AA477516211625BED15762A0729A0D052C71E99CDCDDE7D245C0AB279CD4FD5B554D1F5F5E4CA912C0051DF85DE732DBC5F672C49F12215103F

               :k 3
               :x #x072D033E612CA6DD14C28F1F6689AF9A97BBA7FD88A25BAE969DD1C91E75A9F680442DEA747EAB06E73B746911780505DAB0E03149DE396B1020FBAF55580CF4D6EB9738CE0D26F8
               :y #x0494BA50070CA0FC97E89F3EA55DE5AED4D5BD139B3725618DFEB90152230AA53FA37B6E39BAAD54A77771F0DF01BFFEE82FBB0BAD45F2D5C89F704C6EBA6722B1E4433638D87DDA

               :k 4
               :x #x01717E2A5C3EE28FE6DB3F13B087FB3E9EFBE38772FFC1F1B03E0F2BBCCC016EDE33153D46B7BA33618C056CB2F35EDA083AF75B17115E138483DC59AD83FE1077A2829E20BFFE5F
               :y #x00C2A90DA370B1B8963DFE950C975806F6DE5A01E5152B4850B2FF99D1F8B0726D5D2F0BB850ABA987ACAFC99B70C581ABE9DD39E7BFA4ACFB303E76738ED001DEC5032359C3AD2C

               :k 5
               :x #x077AAC79DC0D8891F8C383A000B6457A6E29ADB61B6AA83C32F3815F4D0173085599A42D9DD5529022EEDAA849397809B8CE613CD800E1676559AD2DAB43338E7E10C1C768C8D209
               :y #x0131F402BF46187D38270BBEA69AC9673A66116E11DD7A08FBC222BF513871A7D58798FC0EA22336961C76BEB380A7299904DBFD62C1D534D232D6DF42795EFA601152D2B57C5A7E

               :k 6
               :x #x0557287AC0922872935C3971223112A6345B8DE8C112D37061F6643DE94D758CE34552FDD6B0F21992F06E2E5DD6B3A69028D35C5C8F1C934177D64506EC14212727C6C1FA11EDDA
               :y #x0295FD54014C36DA83518A23C15F00037200697B8CB38B2F81AFE4843E7954C00DD125AE7D6CCCEBE9EEFA2B2653D8999421E9969544A2A5C534CE63037D44B77C01209AB18C04A8

               :k 7
               :x #x055323D3EE8D3D62CC94055288C2D30DA13F2E74908678C1927A607A0B03AC847B909E3612E916601E3A9B00D69E6DD5FD2CB6A7987F7CC485C277ECCBD2C2EE345B0CDE067075E8
               :y #x04E1A011452C7A543B3FD3969720C6B88E92FB1F2A71A8B8BB25B959B8CBD2106C20FAF7C847A038EBBA6BA145C24A4CA0FCF4DBEABB3CE744153F64FFCC2F8894B73EFC11A2A662

               :k 8
               :x #x00D9BAE2A98471270CD81EEC78D6C906D5DB35CDF049F56822B5910FA9B03BFBB07BFF09CCEC3831C4E4B04485FE70A460EF8E0A88F6BE4957822BF3C997F34DBA2864C6CD47E0B1
               :y #x05C08664F55B7DDCE2277F251725BC76EA968EF0C4E4A8522EF920D9DCD36D68704B1C7F03DD54154FE741E1970C4CF3BCB055B4938CA787179AEADA021E2D7795F52484CF504837

               :k 9
               :x #x02E28EB85C4DB405E6AF8722219BFC7E0DEE5F63973A8538D539B851F6DF2E141B51043BAC7D67C57A33C159C07D6310403DB2C5AB647673C41EB3A5FE84A85E2795D2D15EA19671
               :y #x0639755A25DC91FFF9DD128EEB7945EA9135BA288747D302C1CF2056B2B26E22C31B345F9C83DFB68F2910D6DE27BD07F305A082ABF9B62AEDE66B7EC459AAFEA8C1817E10A4524D

               :k 10
               :x #x02E7F80713D6620DFD571EF392E231940FDDF15DA67AD31FCE4808D4C54F955A566BFC1E8A10E481CF0EC353560A7997D1B562B518585D6DB8EC99131A9209D6126A5B6FC54BE8FD
               :y #x0095B4A8879821FF2B54F9B3ED927DD3A04DB637AA5CD40C727C6EDE3030E07B1B7ACA812118BF9F377CA98A1336CDF6B83A7122BAA1BEE70AC221948FA5E54674A072AF86025952

               :k 11
               :x #x020F60EE1C635708BDEC8F0BD8767372D145402CDC614CECEF0C3033E804E5E5A042D6DC0DD31F93078C883727B711D748FD71B6D60088192DDA285763C0B2F9B2EC2C8EC8E0476D
               :y #x0050B43DDCAE16670D10B22547810642F38CCE90623CB3A9DD95EA575867C092FF77E17B4216B11CCDF4100D529951B99C6E0C8742BF376B8DCF521961C2A5819D5C579A33A8C025

               :k 12
               :x #x0219AE0B766B17D3B310E1EB4AEBF6C135C39BBAC05C20EED04158A7B39E69CBA55CA9A3F5BD3131B208AC253D693E138941FB391022776B7C0E520AF96AA527CC503ABF2F399C86
               :y #x01D2217DAD1F83C99B4C9C1AE426E1CF8A415AE10C9DFC930A0EF13AE9185CDA2DAFF3916988CF247DB6D4215B6DF23F109077D952A6FD60EA17B82095604EC9849934C6D365867A

               :k 13
               :x #x0554EF00BC8894C9D60751D6510D6706636C252A8F4B31391AE95B8438FC927B1069C6ECA2C309DB6072A80116F2781FE03DD064DCD0F199FD43F3997B21CB74B21E094A6C3CE7EF
               :y #x01199F942E4DA8CD55127D8CC18ADEC221FC53E4B94570BABD6C22696520E79A7135727DBBC4DEE1B7162ECBF261427550919E78B555867C007ED8E7001C4CA77ECCCA4E4BD38C63

               :k 14
               :x #x02929E07DB1229E3906CE242C56A9A02208DBB4D6397BBB3CC1C3E53C311CB99BE470ED8B6191B59B5071E84C000428E7B74A456775836C3B58930B4FA20751E6266411F3BEFF3A5
               :y #x0343B10B2B39A9905683ABDCF1470DCBA7F76A3F237DC89792E6EB2EFDD1549CF6973ABD69B98BFFB0D4FBB88AF7A644928BE60C2AAF2F690D5945C1792398028EE58F977A8F2BE2

               :k 15
               :x #x038CCBEFC042385C751E02DDAD50ECC57F23FA4235F8A27CCC31C1290380B7478A7976C7395DD21035251AAB28355D665D3545CEC6739F500C739CED2E1FD87277FD5FC7E92ED19A
               :y #x07C7B76EAE12A050F9C54489AD3F33E6428E5EA5D8AB627E93C805DD805F80210E9FD41B20EA2B6FABA178EA17D22FF1C3011FC57B3800D2EBC59E1DB84A841C99FD299721EA21C8

               :k 16
               :x #x068989557C872F0F3B33EAF57E55F7D5853A2B61C368AE0B602B587B6ED600856DAD98D83FC0FE370FF6F229B3A0AA3A5FC9CE9D05EBF7F8F531071479C643F0F914226DF4A875DA
               :y #x00069C79079321474FAB6BC7D302EAF60EC2DDE78023A2865F2D428BE48C8C24FCDEF0C049D2C9D6E4F67D37A00B6542813D485D3B3C04A6292513775D4F86D8A288741074D5C88A

               :k 17
               :x #x056DB896DA3370648244B873AF7EE95003F9AEA515DD00632EF513EC3C4251428A6FD6BD6A74307060B422957DB1E7E27474E0CAD8397FAAC618EB19D6EF0DF6CA293E1DE7B539A7
               :y #x05F79F9D736F7C7E6C21B05486006B51D19D05CA0BA7658AC51811165035DC074454CE7575C37B2CBAC0F27C1D6CB75818AE258DC3D0EBD99821C444BB9EA4B3CEBA3762A80E8413

               :k 18
               :x #x03DA1431DE0ACBD3F4276109E9A4ED3A748245A14EFA7F1DA84FE917073959D8AC6FDFB5DE3F90EF77FB76E133FFCCFABF64309521C84F7529C765FCA9D02D4268219616D8BA712B
               :y #x00AA944AB72CD18C65866A69D4A70431B40350B2468BFFD2E761BC74C80CF5B0E0EF646FE6E9F7A300DCC098999375ED5DB310C37355656E99A13FB9CE1446395B0DD30E2200646E

               :k 19
               :x #x02232CDB414DF19ED46D9EB492B4ED4FAB0AB96918783D44503526598EC6786597CB824412D93B5DF55FA475AAE122AF56FEAEBBE41FD111103AACD235C7E5C055DD1CE0780067C9
               :y #x041C3844CE19D15F08E745F62542D9BB31D1E8E6AD0D874CF188753CC1CE8B104BE05B9600A5F74C8B1B5C6398C9F8AB5C79FBB73118FE270FE257F4ED5408E2865E22999F372B10

               :k 20
               :x #x040F4A06EDBF702E8F868A157D6D04C76B47CF1CD637CCA94DA858B2EE52C1FE290880CD513A93E60D16CE2E58C855F9882C259493254162D731A239ADE99FC785FB97FDFC72A4F4
               :y #x04802390FDAC1D9B59EC4937F6443EF938EE44CFE96F34987CBBD822226325625A5AF3C629591F2EEB8541BAE72FCCD657F00737FBF80D82C401F8A39B915E160688CFE5AC6F3CCA

               :k 112233445566778899
               :x #x077F5D821A238B7AF9C23B5D447E2393D28A308AA3433355B0E73BD23A3A72FAB81FE9FD5A6C8F8891DDD9A7056BB205E6DA1AE42D9713651AD1207678D64D56BF50F5D05F72E085
               :y #x0200A5258D53C3A756BFEE3E8DCBEAFD7F781D7C05FCEAFFCE2117E69B4272E92636D23239BB74AEDA63E708F94C3D777E175FFE346C98AF11397F4B462A38C0164D06C438023555

               :k 112233445566778899112233445566778899
               :x #x04BD101FF2D6F621E53D1D63F7011C53E9956D840DB2510E667890AF2B85112B6FB427B5918F5AEA4CB68F4D5AA532A89EA1CC1C22F3EC5D98226F5AABFCBDE5AC3FEBF1F621040F
               :y #x02164396DAFB6B61FB6957E433782AAA75124BD4280F6458B17B3B07D109FB92EC859C6F09B75B01145F1CCE6974FD6C3CB7DA6DAE5F41402837E8EFE1DDA3E279992B4FB7B7EAD6

               :k 1992623597601820334275444784098161341414252502647858850477940617876196735335367504892511147225880341545498168999090021223223595237741903315110794472594029953181583356723327
               :x #x04FD746F4265664E038357FA540A1517DAF2F71BB8097BDAD93CC9010F25D84C2F0FE9234D6E3AA79A60932892D3CE574A5FB4D4788071E5F21586C5357908EE4839544F1B8D2285
               :y #x0728E938B6D100C3A67FE2738C41F9802692E847A1E4FEECAA7D7F21F9DB0596C85A00AEEDA069162C9D96F31B577B047E139342FDBFB93FBF2ED8ED29379A6B41DB637FE8D71ED9

               :k 3861235304097731366395849020295506793580025501573752466082694720823659889999449120687268962697352252653604400025856795876469657119139444635908785358460460317088877788544768
               :x #x04435FC297946D031A28AAB3DA06915A6C5AE07DC1E71B58AF7167358A0629E95A95D795592514C5371A007A6AFB9CE1D87069BD181CFD1457972FA7FA781C4CA008633EABD28AA2
               :y #x01FE909CB108F22AD29DC1570606B173085E1FF5710736224E6CB0024A141B9B297665D2ACC15B11E899A5EAC43DC16C71619634BDF6BAC3E7C0009F9C5CAB3AAE803992812515D7

               :k 29484053325707075925799971880648520225129267203907309744947579482621481989803910526623412325649301163282163977203931103437234275639190412051242172756191821780639287295
               :x #x07620AEC44028C64CE94E567A147FAB0E840482EADC69E9AD2764CD396E72A141215167F82934286867A723B1D2B2AC92730276B20247EAF85CE2A77EBE6189188AC89342D13133D
               :y #x072EF9C9248B5105C4F5182DA464F413408B6C7767C7633DD6858756A885E08FCCA425F84EAF579F58D42CB9CF1AD3DB39DBF1F2BEE88DC927ABE3E7C07C3732EE2F560F21FC6828

               :k 3856989612564136079392730580999641434043086815740351063506109991771247389759040179364506010801907655119601519159801507707836288138946973600108373332732886149272671889129479
               :x #x067C4061E0450C221ECCFEB6FE02B893DB383161904059550BF241FCCFF3F75147E2BDF4A9A2D9D5CB70EB22296E8D012DE972A7776430293A5C70392B62264C4C0C5A104B21B17E
               :y #x00DD71F5F2FD56B3F686019E390222A5FF45E996859B6222C50FEC153E4D21017792DA948BA4D397F43E68356611C7635BF27F12364922A7A96FE059F8734841379E2EB3861E40E6

               :k 115168896320404645651432041179098839414378138206812196561404436546770474625174718418325552238695410399889241880699884998980722656882424204062175554372303917066223615
               :x #x0729CEFEE07A890F0F44DE052F1E777B54E9BA32125C8824B32F8E1DB97F8637B5EA4B30722F49C7F91BA7D349FC59604E782515C245EBBCF0C32D937ACFE21CBDCE73C6D6A2291C
               :y #x03C737F717C113818813CA3756E13C2B437ECE0822D9B03A4152D693FCD69CEA3045E07D1156D643EFC4C47255C78E72C0F20D59538A60929C8328CDC9139875EC3724A60CD721CD

               :k 3864537523003199239087417163945278577471667133170388636024057769266287662208867993319214525206175292032772076860100999103054058369004034553550030668101507007173144516497376
               :x #x06CFE40EBD84A2497D4DAC409C8C34EB290A26265C1FFA6B4BDDC0C66235D72D23E1804D6E8B516DAE4CD0CAF88D96593510A6870CF507868A0FF447590B76F0867691D8FCDF5E95
               :y #x02FA9B7B50184638A15183FC77DA1F1D8FCDC08BDA171084142EA3DF9148878D22D30F692F9D5F53D63B16AB29D4B27C004588D32AB15AA46DA03D7B2EDE6C55DE8104A33F23C629

               :k 230121197121011964829492956410962317332440724987436349361278165103269765744684971897427792562867723805262067593176384089090183615361873148246784747143625214720344064
               :x #x049B52AD292175C56C6DD0A2131E53745E18392249B583D477A2A224A3085C634E540C6AAE202F4A72BF07E0E77AE93C90A20FD11F9590B15D628C4A079EF216FD3111844FE9002E
               :y #x028832D22775E3124716723417F51DBA494BB8958B217BD169B9C8A76AD5E3AEFA5DF4AA651A02BE35EEF27BF695E506D4808D8A4AF11560B81B004923489BB21C9EDECDE6798B5C

               :k 3864522781876319144444990411489928498745425588762374758896440897920023110585946263236417010936645116291777139009696600304790060614733522620138525597803759712486299307872239
               :x #x01E1552C7C69C3525DA7DDBA7DB7715BB847ABC2E0AADA92600E518C17820A51B8EAA322CCE4C090C51C40DA4BCB3F4C5D277E25B1CC1E863D432DF78DFCAD1EA2A75D3745103B17
               :y #x0623DD4A4920ED76A5A6615D0DD94BCFBC7196D15DA871BE597FBF99CB62A51705169C2BBA37ED077FE82A3944307AC2F8A021067B97F6CCF4AFAEFA37CE7F6AB3146BCE61BAE852

               :k 1815275926114215700701028333874259168175794556292427843778276145855757450023893249648446870539448899577350096228970948933440013595519109366285051171291091396578474272488967
               :x #x0624C5EF23046534905D62544290F0B3E33D1624BF11EFBE11F46CA1EFCB07019389E6F7BA16CA54AD2D92B4D1C2F2CE4EC230661838D2C14418DE64540F2FB4EB95E0526EFCDA04
               :y #x05751A95A70169C074B4FF7670CB2E9EF7C742474C35417D5613CE37183BB2A7E86094C91CF89B20B5E5A2C677CBB6EBF968F8770285E20FDAA6C68A9B3D9F108C642028C1EFC11E

               :k 1287149561276048671542980749585292895031172887885203268478243566010436807420646737938361337191803564987388184003920857755944740028427123811551562332234582015
               :x #x0363C4953279AEABA18705B01A9E5254BA7099294ECC1C102DE0AF6042C2DBC1DC958764974D8D23E740E945238772FB0DAA5446F42011C1434BC81899D9BBE58BC85F7E3FB18BF5
               :y #x002B9EB340C7ED992F0C23727176ABF49DF09E278EBD66BB80C0BEF98BCB741AFC0F927378CA29167F2999544EBA44E2DB769F86ABCB785B9A1BF0DF0D91D9E65419F8CB5FC8FB79

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285683
               :x #x040F4A06EDBF702E8F868A157D6D04C76B47CF1CD637CCA94DA858B2EE52C1FE290880CD513A93E60D16CE2E58C855F9882C259493254162D731A239ADE99FC785FB97FDFC72A4F4
               :y #x008F699610136DB5D66AC3228B293A3E53A98BD33F58F83131138090CC31E49C7352730B78638CC8E6938F94BFE7992FDFDC22A368DD4CE013305A9A3678C1D183735818501D983E

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285684
               :x #x02232CDB414DF19ED46D9EB492B4ED4FAB0AB96918783D44503526598EC6786597CB824412D93B5DF55FA475AAE122AF56FEAEBBE41FD111103AACD235C7E5C055DD1CE0780067C9
               :y #x063F149F8F5420C1DC8ADB42B7F634F49ADB518FB575BA08A1BD53654F08F375DC2BD9D2127CCC117E44F8163228DA040A87550CD5072F361FD8FB26D893ED22D3833E79E7374CD9

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285685
               :x #x03DA1431DE0ACBD3F4276109E9A4ED3A748245A14EFA7F1DA84FE917073959D8AC6FDFB5DE3F90EF77FB76E133FFCCFABF64309521C84F7529C765FCA9D02D4268219616D8BA712B
               :y #x0370807B69261A5F91A10B603D03E90BC0811513087180CF4F2E5563CF35AC684C80BBDA38D6674C7727B679AA6CB917E2D72056529D2A1BB0665A4567C46B7B332C4518FABA1545

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285686
               :x #x056DB896DA3370648244B873AF7EE95003F9AEA515DD00632EF513EC3C4251428A6FD6BD6A74307060B422957DB1E7E27474E0CAD8397FAAC618EB19D6EF0DF6CA293E1DE7B539A7
               :y #x009A270BA95C0C1AEE650827297E8201D264AB6F1E7A65E9EBED02FA6C778D45CE3B18C81FB74B5CDA74D0E960DD50BA6CDAC5471BE994735E392F5D6D71A9450493097F4FBBBDB4

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285687
               :x #x068989557C872F0F3B33EAF57E55F7D5853A2B61C368AE0B602B587B6ED600856DAD98D83FC0FE370FF6F229B3A0AA3A5FC9CE9D05EBF7F8F531071479C643F0F914226DF4A875DA
               :y #x068F152C7B140E4874988132AD571D238BF8F686434B0C8D3F061AF08A5A8CA191736818761237E1EB008F1E13ABCF78DEF486C03ED7F35EDC1414632489C5285B9C567D807DBD50

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285688
               :x #x038CCBEFC042385C751E02DDAD50ECC57F23FA4235F8A27CCC31C1290380B7478A7976C7395DD21035251AAB28355D665D3545CEC6739F500C739CED2E1FD87277FD5FC7E92ED19A
               :y #x044B7C816E50980C8CDB4654006FDF233DADA4E7ED53C0025FF9C4F483DF376684E6A2DC19B7F97F9E8462413FE772979E345A0BBD4B9F82E7B602F096555C6EEE007650C8C4F052

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285689
               :x #x02929E07DB1229E3906CE242C56A9A02208DBB4D6397BBB3CC1C3E53C311CB99BE470ED8B6191B59B5071E84C000428E7B74A456775836C3B58930B4FA20751E6266411F3BEFF3A5
               :y #x01D12F0CF02B8073C6EF499E342D97C9877AD17240EA73245EFAD57D3EC09F0548D03465DFA090A605D3E53C4AF7E4CAE9FF425A5DF719AAB8D075758303ED1CEC83CE884160D847

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285690
               :x #x0554EF00BC8894C9D60751D6510D6706636C252A8F4B31391AE95B8438FC927B1069C6ECA2C309DB6072A80116F2781FE03DD064DCD0F199FD43F3997B21CB74B21E094A6C3CE7EF
               :y #x044D709492C53C0483152C5A9087B9C4429076CE360E4183A78579ED5DDC75E1615CB4911907D73AD76486CAE4933A6AB0AC4E1C698577E5FD3D2B7E7B3D87D3CCD2C30427EF6B8C

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285691
               :x #x0219AE0B766B17D3B310E1EB4AEBF6C135C39BBAC05C20EED04158A7B39E69CBA55CA9A3F5BD3131B208AC253D693E138941FB391022776B7C0E520AF96AA527CC503ABF2F399C86
               :y #x03CB8F76DB74941A285C7DF1AECD170EBF82C15BCCC1DC7DDA4FA99D5A86351188F35A329C35FE15CFBE78046604CC2C99D18CE042848A0B9619EA2A6C0AEBEE48C90E79FC5C1AFC

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285692
               :x #x020F60EE1C635708BDEC8F0BD8767372D145402CDC614CECEF0C3033E804E5E5A042D6DC0DD31F93078C883727B711D748FD71B6D60088192DDA285763C0B2F9B2EC2C8EC8E0476D
               :y #x025FD4D3C0CD416FB0FC3D2E9FF7753022C98EBCBE5DFF453299DA64B06325775F3537A74FC5AE8FCA78983A752E406ED4937D3194BFBF72A0157A4E020217782FB07B14FB488748

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285693
               :x #x02E7F80713D6620DFD571EF392E231940FDDF15DA67AD31FCE4808D4C54F955A566BFC1E8A10E481CF0EC353560A7997D1B562B518585D6DB8EC99131A9209D6126A5B6FC54BE8FD
               :y #x02724CAF944E43F2D603E7407F704C47AF90476A0C260713BC34660AF57F75214D11369FAB085B1EF8726AD9453CB461698F1397A2F9E38AB22EB8879537EC9066CA29C04349B1AF

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285694
               :x #x02E28EB85C4DB405E6AF8722219BFC7E0DEE5F63973A8538D539B851F6DF2E141B51043BAC7D67C57A33C159C07D6310403DB2C5AB647673C41EB3A5FE84A85E2795D2D15EA19671
               :y #x04DBFBE2799125FA1F7295ACCAE2B9949CDBE54B107D563A14F69807446D4036D84A306430FEB873F51AD18F1E5ADE17B3381247009DC05929F8D8DB3ADD02A08F5453AF4E05C43C

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285695
               :x #x00D9BAE2A98471270CD81EEC78D6C906D5DB35CDF049F56822B5910FA9B03BFBB07BFF09CCEC3831C4E4B04485FE70A460EF8E0A88F6BE4957822BF3C997F34DBA2864C6CD47E0B1
               :y #x05193C865CDF0CFBEEFF61C96FF375703F4DBB3D34AD5D3A0C4CB1D675635693C030E376CF316C248B03F1A512F23C57DC5FDBBE1B7A19CE4018C129CB89DE3A2FDD40420217A886

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285696
               :x #x055323D3EE8D3D62CC94055288C2D30DA13F2E74908678C1927A607A0B03AC847B909E3612E916601E3A9B00D69E6DD5FD2CB6A7987F7CC485C277ECCBD2C2EE345B0CDE067075E8
               :y #x01B283C2ABA14736F7ABD6C41FE215B52FADD56BBAF7D079295FD923B3C87E9417B064C1DAAEB658F580F0A1935C27995DD0427C72C44023C1D74888341EED66A0EC322217D2D38A

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285697
               :x #x0557287AC0922872935C3971223112A6345B8DE8C112D37061F6643DE94D758CE34552FDD6B0F21992F06E2E5DD6B3A69028D35C5C8F1C934177D64506EC14212727C6C1FA11EDDA
               :y #x07C2D52EC1DE1EA8100DB352E36E12A5465BE4934DA1585FE05980B9D734214CEE947753ABDC3EF27B1E94057B856B3F04093ACAC9CBBE3684431826059150965B26E65B4B9DE972

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285698
               :x #x077AAC79DC0D8891F8C383A000B6457A6E29ADB61B6AA83C32F3815F4D0173085599A42D9DD5529022EEDAA849397809B8CE613CD800E1676559AD2DAB43338E7E10C1C768C8D209
               :y #x064B587B634B90ECC0E4881EA62C8C1D544FBCD80AB7D234C931A3E01C3902AF801E3CD1937771A6B4F2AC16FAB9DF2021CABAC1BAC13453B76B7BF2E93A6D741E019315DDB48877

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285699
               :x #x01717E2A5C3EE28FE6DB3F13B087FB3E9EFBE38772FFC1F1B03E0F2BBCCC016EDE33153D46B7BA33618C056CB2F35EDA083AF75B17115E138483DC59AD83FE1077A2829E20BFFE5F
               :y #x01B3D727FF4E533770E6C186BC10A3386825B98697EAEAB9E08CF0B26D34B11CB36E3A36FEE7119AE620AAA529839B5BA3D32A62F0AEFABF7FB3E22FDE0D2E11A96781BD797C5373

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285700
               :x #x072D033E612CA6DD14C28F1F6689AF9A97BBA7FD88A25BAE969DD1C91E75A9F680442DEA747EAB06E73B746911780505DAB0E03149DE396B1020FBAF55580CF4D6EB9738CE0D26F8
               :y #x03B9B96E66200621832A1021C3D44A34436E1AEE13957ECF1B6368C84C56A353BFE756844DC40652404C0599CE79BAFB329F5B3AE49BCBBED8BF8BE33BE26BD6670FD40EF6D55B22

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285701
               :x #x01F8BD0B0C77369F3C5A1943C01215CAD8C7018C4AF1A588E6EFE81C0A39E0A50DB8E55BB371D956B15DBCB13AB12AF532B1FC6B7DDF0A13D12DFAA76051132B84020BEC72D2F265
               :y #x05B2234AFB01B0CA3A13CC92E4100F6EAF96639D28AA489D90C5EF35AAE9B262132128966D0CFD0ABBEFC57C75649FA1E344098FB74E261380F27F79137CAF74E32E421D50C7E25A

               :k 3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285702
               :x #x0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABDBDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927E1E7769C8EEC2D19
               :y #x0078F26E766235B201DA3F2ABA01BEA286FFF67495C313C0BC74F79BE25AC21B39A707134E7058C4AF46BE2C1D7C49BC2AFD7D2C829130A25C5D52E5A673041BFBAF51339566EC42))
         (grp (um:group tv 6))
         (k   (mapcar (um:rcurry 'getf :k) grp))
         (x   (mapcar (um:rcurry 'getf :x) grp))
         (y   (mapcar (um:rcurry 'getf :y) grp))
         (pt  (mapcar (lambda (x y)
                        (make-ecc-pt :x x :y y))
                      x y))
         (gen  *ecc-gen*))
    (with-random-ecc-curve
      (let ((genr (to-random-curve gen)))
        (loop for kix in k
              for ptix in pt
              do
              (assert (ecc-pt= (ecc-validate-public-key (to-random-curve ptix))
                               (ecc-mul genr kix)))))
      t
      )))

(defun check-b163-test-vectors ()
  (with-b163
    (let* ((tv '(
                 :k 1
                 :x #x03F0EBA16286A2D57EA0991168D4994637E8343E36
                 :y #x00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1
                 
                 :k 2
                 :x #x01AEB33FED9C49E0200A0C561EA66D5AB85BD4C2D4
                 :y #x0530608192CD47D0C24C20076475FD625CC82895E8

                 :k 3
                 :x #x0634000577F86AA315009D6F9B906691F6EDD691FE
                 :y #x0401A3DE0D6C2EC014E6FBA5653587BD45DC2230BE

                 :k 4
                 :x #x04053748C8CCD84AF888D3E7623F4FF3B75D153F39
                 :y #x064B0908949B6A838153953B06CD169CC311F5FDA7

                 :k 5
                 :x #x07205899683630522F4C657BB52764867DA449F864
                 :y #x0302537FF55DADA096DB01CA79007AF3013550CB9C

                 :k 6
                 :x #x065AD02C42180EA317348FFE342FB1CF2A3E896195
                 :y #x0054D6F924A2880B5507C59B5B768ABDD6883CC94F

                 :k 7
                 :x #x043EAAAF4BEA5A8C0A3EB105B31A0CF6ABAD87B13A
                 :y #x05FAD8CE53A9D7FD436C988C7A932B0BD27289A17F

                 :k 8
                 :x #x04547BD66270DF7A9601351A616FEF080D44528B03
                 :y #x019303302D63359036B047497DC2F1BB94BB3D93C4

                 :k 9
                 :x #x04802FB7306AE7CAA87F08815BABDFEEBBA9E7A7D3
                 :y #x051887A199573D8C5E2E54FA7FB6859C9F5ABA0256

                 :k 10
                 :x #x0507E541410F581B0D6914C2183C9313E7CAA10915
                 :y #x0303C6D2DE69D3EFDBD20961BB97E25F1B22748341

                 :k 11
                 :x #x0696E27054D49E19B15ED4240AA2F5942A06F25BB5
                 :y #x0213B6F9C3F5E68EFCAF26248A008FF6009343C77C

                 :k 12
                 :x #x05AA3CAE634590B66A3F18A64E47B1C9B3B509E80C
                 :y #x0258234401076C0E16379AA32DF09503285A1EEBE6

                 :k 13
                 :x #x07C565F87A02BFBAD2E0F3517F74392AC60036A5EB
                 :y #x02C556E1E3BD22E6A623BF4033B3E3551409365955

                 :k 14
                 :x #x03566B99AE5EEEB921C9618E514A8AD50506A73F75
                 :y #x04A1B9E316EF87FF578DF5FB8514BEBDDD270CD91B

                 :k 15
                 :x #x01880F725B918ABA057E6DE329ABDFEEF475AE9483
                 :y #x0220415EF494AAD1C937EB6143B18090BF4A2E0516

                 :k 16
                 :x #x041FBD3ADBAB2C4349F5518C8BC4BD531F079DC92B
                 :y #x00611E336597E3A9C3AB428144731DC459A5500F1E

                 :k 17
                 :x #x0714E4DADA0AB682D036AF06DDBA3CCAD123E5734B
                 :y #x06491E8A4DAC775E8E35B3B78172795D6D10EE5200

                 :k 18
                 :x #x01F2DC4C1A649043F1622F611986E84074EBE3F692
                 :y #x03F9DC4F698E25F11F007F5176C2E9822346425217

                 :k 19
                 :x #x01A55C68CE800F55118C74751EE8F99770A65B14F6
                 :y #x01B16D316F36C3491D4AE8B3AFC1DCCCE4141754E2

                 :k 20
                 :x #x00AED08C6DDCF8E345006BD2F6989C3F92CB508A82
                 :y #x0253947FD52A1D327DCAF5224172C24E81BE22C2B3

                 :k 112233445566778899
                 :x #x04B78128BD2F1E46147BA6AE8A2E96679C6082CBFE
                 :y #x0369576EAA212EE4A82F9113139B242ECA44824065

                 :k 112233445566778899112233445566778899
                 :x #x04CCFB0246A0B44EDBBA64C3CFBFA5EFE0175B254B
                 :y #x00C7686F7DBB7A03A655A6200FD00F9289027069D5

                 :k 3014303919301082363471676974008142323349532409856
                 :x #x0206CA58241AD954F6B6499349210DE70DF6D41C69
                 :y #x04B4A47F3360FC8B33C0FC52B924F140ED51DE761E

                 :k 5845660887092509954614822264108743716794242435071
                 :x #x03F9A8E75A930CE4BD76B1E083B7C59AF3559E2332
                 :y #x0321A42923F23C7EBFC6215FFBE3736A424BD67EE1

                 :k 37218388952580046387530102177743765472
                 :x #x059CDB14182D8CC0D721FAE32685C56E26F8941349
                 :y #x060CD9BA37CC91B795716275F2559722B652AB2052

                 :k 178405792776285083668453598138797165866123391
                 :x #x01CA424374AD787A77880627B101FD5A03983BCD93
                 :y #x034DE28FD18358F0ABC44399FB3A95506A00725048

                 :k 5846006375109673349306752832313931793753188073472
                 :x #x006AC93B0829A7F27E89AD517C94DE9D76AA012228
                 :y #x05706AE6A918E2C4D3D7014C4E3A2D1571BBD15B5C

                 :k 5754662784102716162046055105144722111089719902208
                 :x #x063DEEACAD43ECB57FD95FE22577203D893003F524
                 :y #x016429B4458C1B81EAF791DA58F430C0F8BE7BD4B0

                 :k 11417981371511606755217326180000330384402284544
                 :x #x00C69951B883CA6B9716931DF7478E6F400CE1614D
                 :y #x0792D0662006CA484AA33865037BB579FD98A7CECD

                 :k 87091267516532971911250469212156927672320
                 :x #x049DBF57D4CFEC9B141CB7CDC177405C147BF436A1
                 :y #x056C13C2827701030573BB9DC074DB66A90E085D05

                 :k 2809981007300141238972830272943674589706190848
                 :x #x038DEB7A7E42FB8295E9481DFE05FFBA469CD0B391
                 :y #x07B31564545FFF632486977C41871D18D78667ACEA

                 :k 2854409634255779570371480294187805694377328639
                 :x #x0215294902A8129A363003B2AC87E1FED210EE2BFF
                 :y #x03FDE9AF1734A1C84F53A44169D4750B59FD5ABE41

                 :k 5846006549323611672814742442876390689256843201567
                 :x #x00AED08C6DDCF8E345006BD2F6989C3F92CB508A82
                 :y #x02FD44F3B8F6E5D138CA9EF0B7EA5E711375724831

                 :k 5846006549323611672814742442876390689256843201568
                 :x #x01A55C68CE800F55118C74751EE8F99770A65B14F6
                 :y #x00143159A1B6CC1C0CC69CC6B129255B94B24C4014

                 :k 5846006549323611672814742442876390689256843201569
                 :x #x01F2DC4C1A649043F1622F611986E84074EBE3F692
                 :y #x020B000373EAB5B2EE6250306F4401C257ADA1A485

                 :k 5846006549323611672814742442876390689256843201570
                 :x #x0714E4DADA0AB682D036AF06DDBA3CCAD123E5734B
                 :y #x015DFA5097A6C1DC5E031CB15CC84597BC330B214B

                 :k 5846006549323611672814742442876390689256843201571
                 :x #x041FBD3ADBAB2C4349F5518C8BC4BD531F079DC92B
                 :y #x047EA309BE3CCFEA8A5E130DCFB7A09746A2CDC635

                 :k 5846006549323611672814742442876390689256843201572
                 :x #x01880F725B918ABA057E6DE329ABDFEEF475AE9483
                 :y #x03A84E2CAF05206BCC4986826A1A5F7E4B3F809195

                 :k 5846006549323611672814742442876390689256843201573
                 :x #x03566B99AE5EEEB921C9618E514A8AD50506A73F75
                 :y #x07F7D27AB8B1694676449475D45E3468D821ABE66E

                 :k 5846006549323611672814742442876390689256843201574
                 :x #x07C565F87A02BFBAD2E0F3517F74392AC60036A5EB
                 :y #x0500331999BF9D5C74C34C114CC7DA7FD20900FCBE

                 :k 5846006549323611672814742442876390689256843201575
                 :x #x05AA3CAE634590B66A3F18A64E47B1C9B3B509E80C
                 :y #x07F21FEA6242FCB87C08820563B724CA9BEF1703EA

                 :k 5846006549323611672814742442876390689256843201576
                 :x #x0696E27054D49E19B15ED4240AA2F5942A06F25BB5
                 :y #x04855489972178974DF1F20080A27A622A95B19CC9

                 :k 5846006549323611672814742442876390689256843201577
                 :x #x0507E541410F581B0D6914C2183C9313E7CAA10915
                 :y #x060423939F668BF4D6BB1DA3A3AB714CFCE8D58A54

                 :k 5846006549323611672814742442876390689256843201578
                 :x #x04802FB7306AE7CAA87F08815BABDFEEBBA9E7A7D3
                 :y #x0198A816A93DDA46F6515C7B241D5A7224F35DA585

                 :k 5846006549323611672814742442876390689256843201579
                 :x #x04547BD66270DF7A9601351A616FEF080D44528B03
                 :y #x05C778E64F13EAEAA0B172531CAD1EB399FF6F18C7

                 :k 5846006549323611672814742442876390689256843201580
                 :x #x043EAAAF4BEA5A8C0A3EB105B31A0CF6ABAD87B13A
                 :y #x01C4726118438D7149522989C98927FD79DF0E1045

                 :k 5846006549323611672814742442876390689256843201581
                 :x #x065AD02C42180EA317348FFE342FB1CF2A3E896195
                 :y #x060E06D566BA86A842334A656F593B72FCB6B5A8DA

                 :k 5846006549323611672814742442876390689256843201582
                 :x #x07205899683630522F4C657BB52764867DA449F864
                 :y #x04220BE69D6B9DF2B99764B1CC271E757C911933F8

                 :k 5846006549323611672814742442876390689256843201583
                 :x #x04053748C8CCD84AF888D3E7623F4FF3B75D153F39
                 :y #x024E3E405C57B2C979DB46DC64F2596F744CE0C29E

                 :k 5846006549323611672814742442876390689256843201584
                 :x #x0634000577F86AA315009D6F9B906691F6EDD691FE
                 :y #x0235A3DB7A94446301E666CAFEA5E12CB331F4A140

                 :k 5846006549323611672814742442876390689256843201585
                 :x #x01AEB33FED9C49E0200A0C561EA66D5AB85BD4C2D4
                 :y #x049ED3BE7F510E30E2462C517AD39038E493FC573C

                 :k 5846006549323611672814742442876390689256843201586
                 :x #x03F0EBA16286A2D57EA0991168D4994637E8343E36
                 :y #x0325F41D0EF702DC310254C42D65851A3B91471AC7

                 ))
           (grp (um:group tv 6))
           (k   (mapcar (um:rcurry 'getf :k) grp))
           (x   (mapcar (um:rcurry 'getf :x) grp))
           (y   (mapcar (um:rcurry 'getf :y) grp))
           (pt  (mapcar (lambda (x y)
                          (make-ecc-pt :x x :y y))
                        x y))
           (gen *ecc-gen*))
      (with-random-ecc-curve
        (with-random-ecc-curve
          (let ((genr (to-random-curve gen)))
            (ecc-validate-public-key genr)
            (loop for kix in k
                  for ptix in pt
                  do
                  (assert (ecc-pt= (ecc-validate-public-key
                                    (to-random-curve ptix))
                                   (ecc-mul genr kix)))
                  )))
        t
        ))))

;; -------------------------------------------------------------

(defun tst-pt (x)
  (let* ((xsq (gf* x x))
         (s   (handler-case
                  (gf-solve-quadratic
                   (gf+ x *ecc-a*
                        (gf/ *ecc-b* xsq)))
                (error ()
                  (error "X not on curve")))))
    (make-ecc-pt
     :x x
     :y (gf* x s))))

(defvar *ecc-curve127*
  (make-ecc-curve
                   :a 1
                   :b 3
                   :h 2
                   :r 71
                   :nbits 7
                   :gf    $prim-7
                   :h 2
                   :gen (make-ecc-pt
                         :x 3
                         :y 87)))

(defun random-attack (n)
  (with-ecc-curve *ecc-curve127*
    (let ((ans nil))
      (loop for ix from 1 below n do
            (let ((pt (ignore-errors
                        ;; (ecc-pt-for-x ix)
                        (tst-pt ix))))
              (when (and pt
                         ;; (not (ecc-infinite-p (ecc-basic-mul pt *ecc-h* 1)))
                         ;; (ecc-infinite-p (ecc-basic-mul pt *ecc-r* 1))
                         ;; (ecc-infinite-p (ecc-basic-mul pt 29 1))
                         )
                (push (list pt
                            (loop for jx from 2 below 200
                                  when (ecc-infinite-p (ecc-basic-mul pt jx 1))
                                  collect jx))
                      ans))))
      ans)))

(defun find-polyroot (coffs)
  (let* ((ord (1- (length coffs))))
    (labels ((fn (x)
               (do ((ans 0)
                    (ix  0  (1+ ix)))
                   ((> ix ord) ans)
                 (setf ans (+ (aref coffs ix)
                              (* ans x))))))
      (multiple-value-bind (xmin xmax) (roots::zbrac #'fn 0.5 2)
        (max 0 (roots::find-root #'fn xmin xmax))))))


(let* ()
  (flet ((fn (thet)
           (let* ((cisthet (cis thet))
                  (c       (realpart cisthet))
                  (s       (imagpart cisthet))
                  (a3      (expt c 3))
                  (a2      (- (expt c 2)
                              (expt s 2)
                              (* c s)
                              ))
                  (a1      0)
                  (a0      3)
                  (rho     (find-polyroot (vector a3 a2 a1 a0))))
             rho)))
    
    (plt:polar-fplot 'fplt (list (* pi 0) (* pi 2.0)) #'fn
                   :clear t
                   :xrange '(-3 3)
                   :yrange '(-5 5))
    ))

 |#
