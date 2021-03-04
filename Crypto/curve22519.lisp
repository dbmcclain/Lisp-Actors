;;; ecc.lisp -- Elliptic curve cryptography
;;; Mainly the curve25519 algorithm

(require :rg-utils)
(require :rg-syntax)
(require :parcil)
(in-readtable infix)

; Tweak infix syntax so that ^ can be used for exponentiation insted of logxor
(setf (cdr (assoc '^ $binop-translations)) '^)
(setf $binary-ops
  '((\. ->) (^) (* / %) (+ -) (<< >>) (< > <= >=) (== !=) (&) (\|) (&&) (\|\|)
    (= += -= *= /= %= &= ^= \|= >>= <<=)))

(defmacro modp (p expr)
  `(let ((p ,p))
     (flet ((+ (a b) (mod (+ a b) p))
            (- (a b) (mod (- a b) p))
            (* (a b) (mod (* a b) p))
            (/ (a b) (mod (* a (inverse-mod-p b p)) p))
            (^ (a n) (expt-mod-n a n p))
            (sqrt (n) (sqrt-mod-p n p)))
       ,expr)))

(defun expt-mod-n (a x n)
  (let ( (result 1) )
    (while (> x 0)
      (if (oddp x)
        (setf result (mod (* result a) n)))
      (setf x (ash x -1))
      (setf a (mod (* a a) n)))
    result))

(defun extended-gcd (a b)
  (if (zerop b) (return-from extended-gcd (values 1 0)))
  (bb :mv (q r) (floor a b)
      :mv (s v) (extended-gcd b r)
      (values v (- s (* q v)))))

(defun inverse-mod-p (n p) (extended-gcd n p))

; Rabin-Miller primality test
; http://en.wikipedia.org/wiki/Miller-Rabin_primality_test
(defun probably-prime (n &optional (k 10))
  (bb s 0 d (1- n)
      (loop while (evenp d) do (progn (incf s) (setf d (ash d -1))))
      (dotimes (i k)
        (block loop-body
          (bb a (+ 2 (random (- n 4)))
            x (expt-mod-n a d n)
            (if (or (= x 1) (= x (1- n))) (return-from loop-body))
            (dotimes (r (- s 1))
              (setf x (mod (* x x) n))
              (if (= x 1) (return-from probably-prime nil))
              (if (= x (1- n)) (return-from loop-body))))
          (return-from probably-prime nil))))
  t)

(defun is-square-mod-p (n p)
  (if (not (probably-prime p)) (error "~S is not prime" p))
  (if (>= n p) (error "N must be less than P"))
  (or (member n '(0 1))
      ; Fermat's little theorem: (expt-mod-n a (/ (- p 1) 2) p) == 1 if a is non-square in Fp
      (= 1 modp(p, n^((p-1)/2)))))

(defun sqrt-mod-p (n p)
  (unless (is-square-mod-p n p) (return-from sqrt-mod-p nil))
  (if (zerop n) (return-from sqrt-mod-p 0))
  ; Tonelli-Shanks algorithm (http://planetmath.org/encyclopedia/ShanksTonelliAlgorithm.html)
  (bb s 0 q (1- p)
      (loop while (evenp q) do (progn (incf s) (setf q (ash q -1))))
      w 1
      (loop while (is-square-mod-p w p) do (incf w))
      v (expt-mod-n w q p)
      nprime (inverse-mod-p n p)
      r (expt-mod-n n (/ (1+ q) 2) p)
      (loop
        (bb r2nprime modp(p, r*r*nprime)
            i 0
            (loop until (= 1 (expt-mod-n r2nprime (expt 2 i) p)) do (incf i))
            (if (zerop i) (return-from sqrt-mod-p r))
            (setf r (modp p (* r (expt-mod-n v (expt 2 (- s (+ i 1))) p))))))))

(defc p (- (expt 2 255) 19))

(defun c25519y (x) modp(p, sqrt(x^3 + 486662*x^2 + x)))

;;; curve25519 based on python reference implementation by by Matthew Dempsky
;;; http://cr.yp.to/highspeed/naclcrypto-20090310.pdf

(defun pt-add (xzn xzm base)
  (bb :db (xn zn) xzn :db (xm zm) xzm
      x modp(p, 4 * (xm * xn - zm * zn) ^ 2)
      z modp(p, 4 * (xm * zn - zm * xn) ^ 2 * base)
      (list x z)))

(defun pt-double (xzn)
  (bb :db (xn zn) xzn
      x modp(p, (xn ^ 2 - zn ^ 2) ^ 2)
      z modp(p, 4 * xn * zn * (xn ^ 2 + 486662 * xn * zn + zn ^ 2))
      (list x z)))

(defun curve25519 (n &optional (base 9))
  (bb one (list base 1)
      two (pt-double one)
      :fn f (m) (if (= m 1)
                  (list one two)
                  (bb :db (pm pm1) (f (ash m -1))
                      (if (= 1 (logand m 1))
                        (list (pt-add pm pm1 base) (pt-double pm1))
                        (list (pt-double pm) (pt-add pm pm1 base)))))
      :db ((x z) _) (f n)
      modp(p, x*inv(z))))

(defun clamp (n)
  (logior (logand n ; Should maybe be (ash n 3) so we don't lose LSBs?
                  #.(logand (lognot 7) (lognot (ash 128 (* 8 31)))))
          #.(ash 64 (* 8 31))))

; Diffie-helman test
(defun dh-test (&optional (sk1 (clamp (random p))) (sk2 (clamp (random p))))
  (bb pk1 (curve25519 sk1)
      pk2 (curve25519 sk2)
      ss1 (curve25519 sk1 pk2) ; Shared Secret
      ss2 (curve25519 sk2 pk1)
      (values ss1 pk1 pk2 (= ss1 ss2))))


;;;;;;;;;;;;;;;
;;;
;;;  C version
;;;

; gcc -m64 -bundle -o curve25519.dylib curve25519-donna.c

(require :rffi)
(ff-load "/Users/ron/devel/crypto/curve25519/curve25519-donna-1.1/curve25519.bundle")
(defff "curve25519_donna" (:ptr :ptr :ptr) :ptr)

(defun n->ivector (n)
  (bb :mv (v p) (make-heap-ivector 32 '(unsigned-byte 8))
      (dotimes (i 32)
        (setf (aref v i) (logand n 255))
        (setf n (ash n -8)))
      (values v p)))

(defun ivector->n (v)
  (bb n 0
      (loop for i from 31 downto 0 do (setf n (+ (ash n 8) (aref v i))))
      n))

(defun c25519d (n1 &optional (n2 9))
  (bb :mv (_ p1) (n->ivector n1)
      :mv (_ p2) (n->ivector n2)
      :mv (r rp) (make-heap-ivector 32 '(unsigned-byte 8))
      (curve25519_donna rp p1 p2)
      (ivector->n r)))

(defun c25519d-test (&optional (n1 (random p)) (n2 (random p)))
  (= (c25519d n1 n2) (curve25519 n1 n2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ed25519 - digital signatures based on curve25519
;;;  Code cribbed from http://ed25519.cr.yp.to/python/ed25519.py
;;;  Speeded up considerably by switching to extended euclidian for computing x^-1 mod q
;;;

(defc b 256)
(defc q (- (expt 2 255) 19))
(defc l (+ (expt 2 252) 27742317777372353535851937790883648493)) ; ???

(defun expmod (b e m) (expt-mod-n b e m))

(defun inv (x) (extended-gcd x q))

(defc dd modp(q, 0-121665/121666))
(defc i (expmod 2 (/ (1- q) 4) q))

(defun xrecover (y)
  (bb xx (* (1- (* y y)) (inv (1+ (* dd y y))))
      x (expmod xx #.(/ (+ q 3) 8) q)
      (if (not (zerop (mod (- (* x x) xx) q)))
        (setf x (mod (* x i) q)))
      (if (not (zerop (mod x 2)))
        (setf x (- q x)))
      x))

(defun isoncurve (pt)
  (bb :db (x y) pt (zerop mod(-x*x + y*y - 1 - dd*x*x*y*y, q))))

#|
; NOTE: XRECOVER will return points not on the curve!

(defun xrecover1 (y) ; simpler, more correct, but MUCH slower
  modp(q, sqrt((y*y-1)/(dd*y*y+1))))

(defun xrecover-test (&optional (y (random q)))
  (bb x (xrecover y)
      x1 (xrecover1 y)
      oncurve (isoncurve (list x y))
      oncurve1 (and x1 (isoncurve (list x1 y)))
      (eq oncurve oncurve1)))
|#

(defc bp (bb y modp(q, 4/5) (list (xrecover y) y))) ; Basepoint

; Reference scalar multiplication code, simple but slow
(defun edwards (p1 p2)
  (bb :db (x1 y1) p1
      :db (x2 y2) p2
      k modp(q, dd*x1*x2*y1*y2)
      x3 modp(q, (x1*y2+x2*y1)/(1+k))
      y3 modp(q, (y1*y2+x1*x2)/(1-k))
      (list x3 y3)))

(defun slow-scalarmult (pt e)
  (if (zerop e)
    '(1 0)
    (bb _ (slow-scalarmult pt (ash e -1))
        _ (edwards _ _)
        (if (oddp e) (edwards _ pt) _))))

; Faster (!) version based on:
; http://www.hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html

(defun xpt-add (pt1 pt2)
  ; add-2008-hwcd-4
  (bb :db (x1 y1 z1 t1) pt1
      :db (x2 y2 z2 t2) pt2
      A modp(q, (Y1-X1)*(Y2+X2))
      B modp(q, (Y1+X1)*(Y2-X2))
      C modp(q, Z1*2*T2)
      D modp(q, T1*2*Z2)
      E modp(q, D+C)
      F modp(q, B-A)
      G modp(q, B+A)
      H modp(q, D-C)
      X3 modp(q, E*F)
      Y3 modp(q, G*H)
      Z3 modp(q, F*G)
      T3 modp(q, E*H)
      (list x3 y3 z3 t3)))

(defun xpt-double (pt)
  ; dbl-2008-hwcd
  (bb :db (x1 y1 z1 _) pt
      A modp(q, X1*X1)
      B modp(q, Y1*Y1)
      C modp(q, 2*Z1*Z1)
      D (mod (- A) q) ; a*A, with a=-1 for curve25519
      J (+ x1 y1)
      E modp(q, J*J-A-B)
      G modp(q, D+B)
      F modp(q, G-C)
      H modp(q, D-B)
      X3 modp(q, E*F)
      Y3 modp(q, G*H)
      Z3 modp(q, F*G)
      T3 modp(q, E*H)
      (list x3 y3 z3 t3)))

(defun pt-xform (pt)
  (bb :db (x y) pt (list x y 1 modp(q, x*y))))

(defun pt-unxform (pt)
  (bb :db (x y z _) pt modp(q, list(x/z, y/z))))

(defun xpt-mult (pt n)
  (if (zerop n)
    '#.(pt-xform '(0 1))
    (bb _ (xpt-mult pt (ash n -1))
        _ (xpt-double _)
        (if (oddp n) (xpt-add _ pt) _))))

(defun scalarmult (pt e) (pt-unxform (xpt-mult (pt-xform pt) e)))

#|
; Test if faster scalar mult routine returns same results as slower reference
; implementation.  Turns out it doesn't if pt is not on curve, e.g. y=59
; But that's OK because all computations start from the basepoint!
(defun sm-test ()
  (bb y (random q)
      pt (list (xrecover y) y)
      m (random q)
      (or (equal (scalarmult pt m) (slow-scalarmult pt m))
          (list pt m))))

(loop (bb r (sm-test)
          (if (not (eq r t))
            (print (list r (isoncurve (car r))))
            (print 'OK))))
|#

(defun vector->int (v &optional (big-endian nil))
  (bb v (if big-endian v (reverse v))
      n 0
      (loop for m across v do (setf n (+ m (ash n 8))))
      n))

(defun vector->string (v) (map 'string 'code-char v))

(defun encodeint (n)
  (bb v (make-array 32)
      (dotimes (i 32)
        (setf (aref v i) (logand n 255)
              n (ash n -8)))
      v))

(defun decodeint (s)
  (vector->int s))

(defun encodepoint (pt)
  (bb :db (x y) pt
      (encodeint (+ y (ash (logand x 1) 255)))))

(defun decodepoint (v)
  (bb hi-order-bit (ash (aref v 31) -7)
      y (logand (vector->int v) (1- (expt 2 (- b 1))))
      x (xrecover y)
      (if (/= (logand x 1) hi-order-bit) (setf x (- q x)))
      pt (list x y)
      (if (isoncurve pt) pt (error "Point is not on curve"))))


(require :hashlib) ; For sha512

(defun inthash (s) (vector->int (sha512 s)))

(defun publickey (sk)
  (bb h (inthash sk)
      a (logior #.(ash 1 (- b 2)) (logand #.(ash (1- (expt 2 251)) 3) h))
      (encodepoint (scalarmult bp a))))

(defun signature (m sk &optional (pk (publickey sk)))
  (bb h (sha512 sk)
      hs (vector->string h)
      hi (vector->int h)
      pk (vector->string pk)
      a (logior #.(ash 1 (- b 2)) (logand #.(ash (1- (expt 2 251)) 3) hi))
      r (inthash (strcat (subseq hs (/ b 8) (/ b 4)) m))
      rp (scalarmult bp r)
      s (mod (+ r (* a (inthash (strcat (vector->string (encodepoint rp)) pk m)))) l)
      (concatenate 'vector (encodepoint rp) (encodeint s))))

(defun checksig (sig m pk)
  (if (not (= (length sig) (/ b 4))) (error "Signature is the wrong length"))
  (if (not (= (length pk) (/ b 8))) (error "Public key is the wrong length"))
  (bb rpe (subseq sig 0 (/ b 8))
      r (decodepoint rpe)
      a (decodepoint pk)
      s (decodeint (subseq sig (/ b 8) (/ b 4)))
      h (inthash (strcat (vector->string rpe) (vector->string pk) m))
      v1 (scalarmult bp s)
      v2 (pt-unxform (xpt-add (pt-xform r) (pt-xform (scalarmult a h))))
      (equal v1 v2)))

(defun test (&optional (msg "msg") (sk "secret"))
  (bb pk (publickey sk)
      (checksig (signature msg sk pk) msg pk)))

(defun ed-test ()
  (bb msg (princ-to-string (random q))
      sk (princ-to-string (random q))
      pk (publickey sk)
      sig (signature msg sk pk)
      (checksig sig msg pk)))

#|
Try to compute Ed25519 using Curve25519 computations by going through the following coordinate
transformation (taken from the Ed25519 paper, pages 7-8):

Curve 25519:
v^2 = u^3 + 486662*u^2 + u

Is equivalent to Ed25519:
x^2 + y^2 = 1 + (121665/121666)*x^2*y^2

via:
u = -(y+1)/(y-1)
v = sqrt(486664)*u/x
x = sqrt(486664)*u/v
y = (u-1)/(u+1)

It doesn't work.  We lose the sign of X because xrecover takes a square root.

(defun sm1 (pt n)
  (bb :db (_ y) pt
      u modp(p, (1+y)/(1-y))
      un (curve25519 n u)
      y modp(p, (un-1)/(un+1))
      (list (xrecover y) y)))

(defun randpt ()
  (bb y (random q)
      x (xrecover y)
      pt (list x y)
      (if (isoncurve pt) pt (randpt))))

(defun test1 (&optional (pt (randpt)) (n (random q)))
  (bb :db (x1 y1) (scalarmult pt n)
      :db (x2 y2) (sm1 pt n)
      (pprint (list pt n (= y1 y2) (= x1 x2) (= x1 (- q x2))))))
|#
