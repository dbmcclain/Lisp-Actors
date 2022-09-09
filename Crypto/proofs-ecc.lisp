;; proofs.lisp -- Using the G1 (smaller) group of the pairing curves
;; to provide Pedersen commitments with proofs. Also provide proofs on
;; cloaked values.
;;
;; DM/Emotiq 04/18
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

(in-package :edec)

#|
Any commitment, written as C = H^x, in bent nomenclature, can be
rewritten in log form as log C = x*log H. where log H = alpha * log G
= alpha, considering group element G as the identity element for
exponentiation, or base of the log i.e., the base generator of the
cryptosystem over the curve on which C and H are defined.

So, a Pedersen commitment of the form C = H^gamma * G^x can be
rewritten as log C = gamma * log H + x * log G = gamma * log H + x, for
independent generators G, H.

In un-bent nomenclature that Pedersen commitment would be written as
C = gamma * H + x * G, where H = alpha * G for some randomly selected
alpha. So unbent nomenclature is already in log form.

We share knowledge of H, G, C, and keep x, gamma hidden. When asked to
open the commitment for C we rturn x, always keeping randomness gamma
hidden.

A commitment in the form C = x*G is computationally binding, but not
hiding, because anyone capable of performing ECDLP can solve for x. A
commitment of the form C' = gamma*H + x*G, for random gamma and
independent generator H, is both computationally binding and hiding
because the ability to perform ECDLP does not help in the face of
unknmown randomness gamma, and there is no known relationship between
generators H and G.

To build a proof that x = v, we need to provide a commitment to x and
offer a function of random variable z, such that the challenger cnn
predict the answer to the function F(z) given our commitment, as well
as have us perform the computation and return an answer that can be
matched, without giving away any information about the binding value
for x.
 |#

(defun rand (&rest ignored)
  (declare (ignore ignored))
  "Return a random integer from Z_r over the domain of the curve
field. 0 < z_rand < group order, N.

A \"safe\" random value lies in the interval (Sqrt[N], N-Sqrt[N])
to discourage brute force searches."
  (safe-field-random *ed-r*))

;; ------------------------------------------------------------
;; A better approach that allows a simple Pedersen commitment to
;; directly handle all possible messages, including small integers.

(defstruct ped-proof
  "NIZKP for proof of a Pedersen commitment. We are proving that we
know an opening."
  ;; -----------------
  ;; Proof ID
  curve   ;; name of ECC curve
  seed    ;; for basis gen and challenge seeding
  ;; -----------------
  ;; Initial Commits
  cmt     ;; the commitment C = x*G + gamma*H
  apt     ;; proof point A = d*G + r*H
  ;; -----------------
  ;; After Challenge Values
  mp      ;; proof reveal:  mp*G + rp*H == z*C + A
  rp)

(defstruct ped-secrets
  "The secrets used in the Pedersen commitment"
  seed    ;; identifies the Pedersen commitment
  x       ;; value committed
  gamma   ;; cloaking used
  d r)    ;; proof params:   mp = z*x + d, rp = z*gamma + r

;; --------------------------------------------------------------------

(defun gen-basis (n seed)
  ;; Generate n ECC points, filter duplicates, and refresh until we
  ;; have n unique basis points.
  ;;
  ;; Points are generated deterministically from random seed using
  ;; iterative hashing.
  (labels ((gen  (n)
             (do ((ix    n    (1- ix))
                  (basis nil))
                 ((zerop ix) basis)
               (setf seed (vec (hash/256 seed)))
               (push (ed-pt-from-seed seed) basis))
             ))
    (um:nlet iter ((basis  (gen n)))
      (let* ((rem-basis (remove-duplicates basis :test 'ed-pt=))
             (nb        (length rem-basis)))
        (if (< nb n)
            (go-iter (append (gen (- n nb)) rem-basis))
          basis)
        ))))

(defun generate-pedersen-basis (seed)
  (values-list (gen-basis 2 seed)))

;; ---------------------------------------------------------

(defun make-pedersen-proof (x)
  "Make a Pedersen commitment to x with computational binding, and
random hiding, gamma:

    C = x*G + gamma*H

for randomly selected, independent generators G, H.
Publish G, H, and C.

Prover supplies random point A = d*G + r*H for random values d,r.

Verifier supplies random probe z, which by Fiat-Shamir will be H(G, H, C, A).

Prover then supplies opening (m',r') = (z*x + d, z*gamma + r) and
verifier sees that:

     m'*G + r'*H = z*C + A

This keeps original message (x, gammma) hidden, but proves that we can
open a related commitment. Since x is cloaked by gamma and then d,
there are no concerns about x being in small range."
  (modr
    (let ((seed  (vec (hash/256 (uuid:make-v1-uuid))))
          (gamma (rand)))
      (multiple-value-bind (gpt hpt)
          (generate-pedersen-basis seed)
        (let* ((cmt   (ed-add (ed-mul hpt gamma)
                              (ed-mul gpt x)))
               (vcmt  (vec cmt))
               (d     (rand))
               (r     (rand))
               (apt   (ed-add (ed-mul gpt d)
                              (ed-mul hpt r)))
               (vapt  (vec apt))
               ;; challenge z = hash of public info
               (z     (int (hash/256 seed *ed-name* vcmt vapt)))
               (mp    (m+ (m* z x) d))
               (rp    (m+ (m* z gamma) r)))
          (values
           (make-ped-proof
            :curve *ed-name*
            :seed  seed
            :cmt   vcmt
            :apt   vapt
            :mp    (vec mp)
            :rp    (vec rp))
           (make-ped-secrets
            :seed  seed
            :x     x
            :gamma gamma
            :d     d
            :r     r))
          )))
    ))

(defmethod validate-pedersen-proof ((proof ped-proof))
  "Return t if the proof checks out."
  (with-accessors  ((curve  ped-proof-curve)
                    (seed   ped-proof-seed)
                    (vcmt   ped-proof-cmt)
                    (vapt   ped-proof-apt)
                    (vmp    ped-proof-mp)
                    (vrp    ped-proof-rp)) proof
    (ignore-errors
      (with-ed-curve curve
        (multiple-value-bind (gpt hpt)
            (generate-pedersen-basis seed)
          (destructuring-bind (cmt apt)
              (mapcar 'ed-valid-point-p (list vcmt vapt))
            (modr
              (let* ((z     (int (hash:hash/256 seed curve vcmt vapt)))
                     (gzpt  (ed-add (ed-mul hpt (int vrp))
                                    (ed-mul gpt (int vmp))))
                     (gzcmt (ed-add (ed-mul cmt z)
                                    apt)))
                ;; verify Pedersen commitment
                (ed-pt= gzpt gzcmt)
                ))))
        ))))

#|
(let ((pf (make-pedersen-proof 15)))
  (inspect pf)
  (validate-pedersen-proof pf))
 |#

;; ---------------------------------------------------------------
;; Vector Proofs

(defun left (v n)
  (subseq v 0 n))

(defun right (v n)
  (subseq v n))

(defun vdot (av bv)
  (reduce 'm+
          (map 'vector 'm* av bv)))

(defun ptdot (ptv xv)
  (reduce 'ed-add
          (map 'vector 'ed-mul ptv xv)))
                         
(defun vscale (v x)
  (map 'vector (um:rcurry 'm* x) v))

(defun vadd (v1 v2)
  (map 'vector 'm+ v1 v2))

(defun ptv-scale (ptv x)
  (map 'vector (um:rcurry 'ed-mul x) ptv))

(defun zerov (n)
  (make-array n
              :initial-element 0))

;; --------------------------------------------------------------------

(defun vcommit (gv hv u av bv c)
  ;; av•Gv + bv•Hv + c*U
  (ptdot (vector (ptdot gv av)
                 (ptdot hv bv)
                 u)
         (vector 1 1 c)))

;; --------------------------------------------------------------------

(defun generate-dotprod-basis (n seed)
  (let* ((nel   (+ 2 (* 2 n)))
         (basis (gen-basis nel seed))
         (g     (pop basis))
         (h     (pop basis))
         (gv    (coerce (left  basis n) 'vector))
         (hv    (coerce (right basis n) 'vector)))
    (values gv hv g h)))

;; --------------------------------------------------------------------
;; Vector Dot-Product NIZKP:  av • bv = c
#|
  Size:     n
  Values:   av[1..n]
  Coeffs:   bv[1..n]
  Value:    c = av•bv
  Blinding: rv[1..n], sv[1..n] random
  Basis:    Gv[1..n], Hv[1..n], G, H
  Compute:  
          A  = av•Gv + bv•Hv + α*H   ;; α random blinding
          S  = rv•Gv + sv•Hv + β*H   ;; β random blinding
          c  = av•bv
          t1 = (av•sv) + (bv•rv)
          t2 = (rv•sv)
          T1 = t1*G + tau1*H        ;; tau1 random
          T2 = t2*G + tau2*H        ;; tau2 random

  Publish: A, S, c, T1, T2
  
  Challenge: x random

  Compute:   lv = av + rv*x
             rv = bv + sv*x

  Publish:   C    = lv•Gv + rv•Hv
             tx   = c + t1*x + t2*x^2
             taux = tau1*x + tau2*x^2   ;; blinding for tx
             µ    = α + β*x             ;; α,β blinding for A,S
             Inner-Prod Proof P = C + tx*H => (lv•rv == tx)

  Verify:   (1) tx*G + taux*H =?= c*G + x*T1 + x^2*T2
            (2) A + x*S =?= µ*H + C
            (3) validate inner-prod proof P

  Inner-prod proof, P, has compact size O(log2 n), but not absolutely
  hiding. That's okay because lv, rv not secret.

 |#

(defstruct dotprod-proof
  ;; ---------------------------------
  ;; Proof ID
  curve     ;; name of ECC curve
  n         ;; size of proof vectors
  seed      ;; for basis generation, and make Fiat-Shamir safe
  ;; ----------------------------------
  ;; Initial Commits
  c         ;; the ostensible value of the (av • bv)
  a-cmt     ;; av•Gv + bv•Hv + α*H
  s-cmt     ;; rv•Gv + sv•Hv + β*H
  t1-cmt    ;; t1*G + tau1*H
  t2-cmt    ;; t2*G + tau2*H
  ;; ----------------------------------
  ;; After Challenge Values
  taux      ;; blinding for tx
  tx        ;; c + t1*x + t2*x^2
  mu        ;; blindings
  lr-cmt    ;; lv•Gv + rv•Hv
  tx-proof) ;; recursive inner-product proof on lv•rv = tx

(defun dotprod-proof (av bv)
  ;; Dot-Product Proofs
  ;;
  ;; For vector av of values, and vector bv of coefficients, we claim
  ;; to prove, with zero knowledge, that (av • bv) = c.  Values and
  ;; coffs are kept secret.  Dotprod value, c = (av • bv) is made
  ;; public.
  ;;
  ;; We cloak the values and coffs using vectors of random values in parallel
  ;; with the value and coff vectors.
  ;;
  ;; E.g., suppose we claim to know a, b such that a + 2*b = 15. Then
  ;; vectors av = #(a b), bv = #(1 2). And we should have (av • bv) =
  ;; c = 15. We don't presuppose a value for c, but rather honestly
  ;; compute it from (av • bv).
  ;;
  (let ((n  (length av)))
    (assert (plusp n))            ;; non-empty
    (assert (= n (length bv)))    ;; av same length as bv
    (assert (every 'integerp av)) ;; all av and bv are integers
    (assert (every 'integerp bv))
    (unless (= 1 (logcount n))
      ;; pad to pwr2 length
      (let* ((npwr2  (um:ceiling-pwr2 n))
             (npad   (- npwr2 n))
             (zv     (zerov npad)))
        (setf n  npwr2
              av (concatenate 'vector av (map 'vector 'rand zv))
              bv (concatenate 'vector bv zv))
        ))
    (modr
      (let* ((rv    (map 'vector 'rand av))  ;; generate blinding values
             (sv    (map 'vector 'rand bv))
             (alpha (rand))
             (beta  (rand))
             (c     (vdot av bv))
             (vc    (vec c))
             (seed  (vec (hash/256 (uuid:make-v1-uuid)))))
        (multiple-value-bind (gv hv g h)
            (generate-dotprod-basis n seed)
          (let* ((a-cmt  (vcommit gv hv h av bv alpha))
                 (s-cmt  (vcommit gv hv h rv sv beta))
                 (va-cmt (vec a-cmt))
                 (vs-cmt (vec s-cmt))
                 (t1     (m+ (vdot av sv) (vdot bv rv)))
                 (t2     (vdot rv sv))
                 (tau1   (rand))
                 (tau2   (rand))
                 (t1-cmt (ptdot (vector g  h)
                                (vector t1 tau1)))
                 (vt1-cmt (vec t1-cmt))
                 (t2-cmt (ptdot (vector g  h)
                                (vector t2 tau2)))
                 (vt2-cmt (vec t2-cmt))
                 
                 ;; Fiat-Shamir challenge, x
                 ;; safe: fold in seed, plus size, c and proof
                 (x      (int (hash-to-grp-range seed *ed-name* n
                                                 vc va-cmt vs-cmt
                                                 vt1-cmt vt2-cmt)))

                 (lv     (vadd av (vscale rv x)))
                 (rv     (vadd bv (vscale sv x)))
                 (tx     (m+ c (m* x (m+ t1 (m* x t2)))))
                 (taux   (m* x (m+ tau1 (m* x tau2))))
                 (mu     (m+ alpha (m* beta x))))
            (multiple-value-bind (lr-cmt proof)
                (inner-prod-proof seed (vec x) gv hv h lv rv)
              (make-dotprod-proof
               :curve    *ed-name*
               :seed     seed
               :n        n
               :c        vc
               :a-cmt    va-cmt
               :s-cmt    vs-cmt
               :t1-cmt   vt1-cmt
               :t2-cmt   vt2-cmt
               :taux     (vec taux)
               :tx       (vec tx)
               :mu       (vec mu)
               :lr-cmt   (vec lr-cmt)
               :tx-proof proof)
              )))))))

(defstruct terminal-proof
  a b)

(defstruct subproof
  lf rt sub)

(defun inner-prod-proof (seed vx gv hv u av bv)
  ;; recursive proof of (av•bv = c)
  ;; basis Gv, Hv, U
  ;;
  ;; P = av•Gv + bv•Hv
  ;; challenge x
  ;; prove P' = P + c*x*U
  (let* ((c    (vdot av bv))
         (cmt  (vcommit gv hv u av bv 0))
         ;; Fiat-Shamir challenge, x
         (xh   (hash-to-grp-range seed vx
                                  (vec c) (vec cmt)))
         (vx   (vec xh))
         (x    (int xh))
         (ux   (ed-mul u x))
         (cmtx (ptdot (vector cmt ux)
                      (vector 1   c))))
    (values
     cmt
     (inner-subproof seed vx gv hv ux cmtx av bv))
    ))

(defun half-basis (gvl gvr hvl hvr x xinv)
  (values
   ;; Hadamard "products" in Curve group
   (map 'vector 'ed-add   ;; Gvl/x º x*Gvr
        (ptv-scale gvl xinv)
        (ptv-scale gvr x))
   (map 'vector 'ed-add   ;; x*Hvl ª Hvr/x
        (ptv-scale hvl x)
        (ptv-scale hvr xinv))
   ))
   
(defun inner-subproof (seed vx gv hv u cmt av bv)
  ;; Recursively divide and conquer till final scalar proof
  (let ((n/2  (ash (length av) -1)))
    (if (zerop n/2)
        (make-terminal-proof
         :a   (vec (aref av 0))
         :b   (vec (aref bv 0)))
      ;; else
      (let ((vlst (list av bv gv hv)))
        (destructuring-bind (avl bvl gvl hvl)
            (mapcar (um:rcurry 'left n/2) vlst)
          (destructuring-bind (avr bvr gvr hvr)
              (mapcar (um:rcurry 'right n/2) vlst)
            (let* ((lf   (vcommit gvr hvl u avl bvr (vdot avl bvr)))
                   (rt   (vcommit gvl hvr u avr bvl (vdot avr bvl)))
                   (vlf  (vec lf))
                   (vrt  (vec rt))
                   ;; Fiat-Shamir challenge, x
                   (xh   (hash-to-grp-range seed vx vlf vrt))
                   (vx   (vec xh))
                   (x    (int xh))
                   (xinv (m/ x))
                   (xsq  (m* x x))
                   (cmt/2 (ptdot (vector lf cmt rt)  ;; x^2*L + P + R/x^2
                                 (vector xsq 1 (m/ xsq))))
                   (av/2  (map 'vector 'm+           ;; x*avl + avr/x
                               (vscale avl x)
                               (vscale avr xinv)))
                   (bv/2  (map 'vector 'm+           ;; bvl/x + x*bvr
                               (vscale bvl xinv)
                               (vscale bvr x))))
              (multiple-value-bind (gv/2 hv/2)
                  (half-basis gvl gvr hvl hvr x xinv)
                (make-subproof
                 :lf  vlf
                 :rt  vrt
                 :sub (inner-subproof seed vx gv/2 hv/2 u cmt/2 av/2 bv/2))
                ))))
        ))))

#|
(defun tst (av bv)
  (let* ((proof (dotprod-proof av bv))
         (enc   (loenc:encode proof)))
    (inspect proof)
    (assert (validate-dotprod-proof proof))
    (format t "~%Validation Okay")
    (format t "~%Proof size = ~D" (length enc))
    (terpri)
    (values)))

(tst #(15) #(1))
(tst #(5 5) #(1 2))
(tst #(5 5 4) #(1 2 3))
(tst #(5 5 4 6) #(1 2 3 4))
(tst #(5 5 4 6 7) #(1 2 3 4 5))
|#

(defun validate-dotprod-proof (proof)
  ;; return T/F on proof
  (with-accessors ((curve    dotprod-proof-curve)
                   (seed     dotprod-proof-seed)
                   (n        dotprod-proof-n)
                   (vc       dotprod-proof-c)
                   (va-cmt   dotprod-proof-a-cmt)
                   (vs-cmt   dotprod-proof-s-cmt)
                   (vt1-cmt  dotprod-proof-t1-cmt)
                   (vt2-cmt  dotprod-proof-t2-cmt)
                   (vtaux    dotprod-proof-taux)
                   (vtx      dotprod-proof-tx)
                   (vmu      dotprod-proof-mu)
                   (vlr-cmt  dotprod-proof-lr-cmt)
                   (tx-proof dotprod-proof-tx-proof)) proof
    (ignore-errors
      (with-ed-curve curve
        (destructuring-bind (a-cmt s-cmt t1-cmt t2-cmt lr-cmt)
            (mapcar 'ed-valid-point-p (list va-cmt vs-cmt vt1-cmt vt2-cmt vlr-cmt))
          (destructuring-bind (c tx taux mu)
              (mapcar 'int (list vc vtx vtaux vmu))
            (multiple-value-bind (gv hv g h)
                (generate-dotprod-basis n seed)
              (modr
                ;; Fiat-Shamir challenge, x
                (let ((x  (int (hash-to-grp-range seed curve n
                                                  vc va-cmt vs-cmt
                                                  vt1-cmt vt2-cmt))))
                  (and (ed-pt=
                        (ptdot (vector g  h)
                               (vector tx taux))
                        (ptdot (vector g t1-cmt t2-cmt)
                               (vector c x      (m* x x))))
                       (ed-pt= (ptdot (vector a-cmt s-cmt)
                                      (vector 1     x))
                               (ptdot (vector h  lr-cmt)
                                      (vector mu 1)))
                       (validate-inner-proof tx-proof seed (vec x) gv hv h tx lr-cmt))
                  ))
              ))))
      )))

(defun validate-inner-proof (proof seed vx gv hv u c cmt)
  ;; Fiat-Shamir challenge, x
  (let* ((xh   (hash-to-grp-range seed vx (vec c) (vec cmt)))
         (vx   (vec xh))
         (x    (int xh))
         (ux   (ed-mul u x))
         (cmtx (ptdot (vector cmt ux)
                      (vector 1   c))))
    (validate-subproof proof seed vx gv hv ux cmtx)
    ))

(defun validate-subproof (sub seed vx gv hv u cmt)
  ;; recursive divide and conquer till final scalar validation
  (cond ((terminal-proof-p sub)
         (with-accessors ((va  terminal-proof-a)
                          (vb  terminal-proof-b)) sub
           (destructuring-bind (a b)
               (mapcar 'int (list va vb))
             (destructuring-bind (g h)
                 (mapcar (um:rcurry 'aref 0) (list gv hv))
               (ed-pt= cmt
                       (ptdot (vector g h u)
                              (vector a b (m* a b))
                              ))
               ))))

        (t
         (with-accessors ((vlf  subproof-lf)
                          (vrt  subproof-rt)
                          (sub  subproof-sub)) sub
           (destructuring-bind (lf rt)
               (mapcar 'ed-valid-point-p (list vlf vrt))
             (let* ((n      (length gv))
                    ;; Fiat-Shamir challenge, x
                    (xh     (hash-to-grp-range seed vx
                                               vlf vrt))
                    (vx     (vec xh))
                    (x      (int xh))
                    (xsq    (m* x x))
                    (cmt/2  (ptdot (vector lf cmt rt)
                                   (vector xsq 1 (m/ xsq))))
                    (xinv  (m/ x))
                    (n/2   (ash n -1))
                    (vlst  (list gv hv)))
               (destructuring-bind (gvl hvl)
                   (mapcar (um:rcurry 'left n/2) vlst)
                 (destructuring-bind (gvr hvr)
                     (mapcar (um:rcurry 'right n/2) vlst)
                   (multiple-value-bind (gv/2 hv/2)
                       (half-basis gvl gvr hvl hvr x xinv)
                     (validate-subproof sub seed vx gv/2 hv/2 u cmt/2)
                     )))
               ))))
        ))

;; --- end of file --- ;;
