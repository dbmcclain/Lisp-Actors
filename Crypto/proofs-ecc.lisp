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
field. 1 <= z_rand < group order"
  (safe-field-random *ed-r*))

;; ------------------------------------------------------------
;; A better approach that allows a simple Pedersen commitment to
;; directly handle all possible messages, including small integers.

(defstruct ped-proof
  "Public knowlege for proof of a Pedersen commitment."
  curve   ;; name of ECC curve
  seed    ;; for basis gen and challenge seeding
  cmt     ;; the commitment C = x*G + gamma*H
  apt     ;; proof point
  mp      ;; proof reveal:  mp*G + rp*H = z*C + A
  rp)

(defstruct ped-secrets
  "The secrets used in the Pedersen commitment"
  seed    ;; identifies the Pedersen commitment
  x       ;; value committed
  gamma   ;; cloaking used
  d r)    ;; proof params:   mp = z*x + d, rp = z*gamma + r

;; --------------------------------------------------------------------

(defun gen-basis (n seed)
  (labels ((gen  (n)
             (do ((ix    n    (1- ix))
                  (basis nil))
                 ((zerop ix) basis)
               (push (ed-pt-from-seed seed) basis)
               (setf seed (vec (hash/256 seed))))
             ))
    (um:nlet iter ((basis  (gen n)))
      (let* ((rem-basis (remove-duplicates basis :test 'ed-pt=))
             (nb        (length rem-basis)))
        (if (< nb n)
            (go-iter (append (gen (- n nb)) rem-basis))
          basis)
        ))
    ))

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

(defun ptv-scale (ptv x)
  (map 'vector (um:rcurry 'ed-mul x) ptv))

(defun zerov (n)
  (make-array n
              :initial-element 0))

;; ------------------------------------------------------------------

(defstruct dotprod-opening
  seed    ;; identifies the proof
  av      ;; values committed
  bv      ;; coefficients committed
  rv)     ;; cloaking used

(defstruct dotprod-proof
  curve    ;; name of ECC curve
  n        ;; size of proof vectors
  seed     ;; for basis generation, and make Fiat-Shamir safe
  c        ;; the ostensible value of the (av • bv)
  cmt      ;; first layer commitment = av • Gv + bv • Hv + rv • Fv, i.e., no U term here
  proofs)  ;; to the recursive sub-proofs

(defstruct dotprod-subproof
  lf rt proofs) ;; recursive Lf, Rt points
       
(defstruct dotprod-terminal-proof
  a b r)  ;; final witness values

;; --------------------------------------------------------------------

(defun vcommit (gv hv fv u av bv rv c)
  #|
  (format t "~%AV = ~S" av)
  (format t "~%BV = ~S" bv)
  (format t "~%RV = ~S" rv)
  |#
  (ed-add (ptdot gv av)
          (ed-add (ptdot hv bv)
                  (ed-add (ptdot fv rv)
                          (ed-mul u c))
                  )))

;; --------------------------------------------------------------------

(defun generate-dotprod-basis (n seed)
  (let* ((nel   (1+ (* 3 n)))
         (basis (gen-basis nel seed))
         (u     (pop basis))
         (gv    (coerce (um:take n basis) 'vector))
         (hv    (coerce (um:take n (um:drop n basis)) 'vector))
         (fv    (coerce (right basis (* 2 n)) 'vector)))
        (values gv hv fv u)))

;; --------------------------------------------------------------------

(defun dotprod-proof (av bv)
  ;; Dot-Product Proofs
  ;;
  ;; For vector av of values, and vector bv of coefficients, we claim
  ;; to prove that (av • bv) = c.  Values and coffs are kept secret.
  ;; Dotprod value, c = (av • bv) is made public.
  ;;
  ;; We cloak the values using a vector of random values in parallel
  ;; with the value and coff vectors.  All vectors are padded to
  ;; power-of-2 length to permit recursive halving proof. Gives us
  ;; log2(N) proof size.
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
    (modr
      (cond ((= n 1)
             ;; simple product proof...
             ;; this case has dangeraous reveal in L, R
             ;; force one round of cloaking on way to final reveal
             (setf n  2
                   av (vector (aref av 0) (rand))
                   bv (vector (aref bv 0) 0)))
            
            ((> (logcount n) 1)
             ;; pad to pwr2 size
             (let* ((np2    (um:ceiling-pwr2 n))
                    (nextra (- np2 n))
                    (zv     (zerov nextra)))
               (setf n np2
                     av (concatenate 'vector av (map 'vector 'rand zv))
                     bv (concatenate 'vector bv zv))
               )))
      (let* ((rv    (map 'vector 'rand av))  ;; generate cloaking factors
             (c     (vdot av bv))
             (vc    (vec c))
             (seed  (vec (hash/256 (uuid:make-v1-uuid)))))
        (multiple-value-bind (gv hv fv u)
            (generate-dotprod-basis n seed)
          (let* ((cmt    (vcommit gv hv fv u av bv rv 0))
                 ;; P = av•Gc + bv•Hv + rv•Fv
                 ;; (i.e., no U term here)
                 (vcmt   (vec cmt))
                 
                 ;; Fiat-Shamir challenge, x
                 ;; safe: fold in seed, plus size, c and proof
                 (xh    (hash-to-grp-range seed *ed-name* n vc vcmt))
                 (vx    (vec xh))
                 (x     (int xh))
                 (ux    (ed-mul u x))
                 (cmtx  (ptdot (vector cmt ux)
                               (vector 1 c))))
            (values
             (make-dotprod-proof
              :curve  *ed-name*
              :seed   seed
              :n      n
              :cmt    vcmt
              :c      vc
              :proofs (protocol-2 seed vx gv hv fv ux cmtx av bv rv))
             (make-dotprod-opening
              :seed   seed
              :av     av
              :bv     bv
              :rv     rv))
            ))))))

(defun protocol-2 (seed prev-vx gv hv fv u cmt av bv rv)
  (let ((n/2  (ash (length av) -1)))
    (cond ((zerop n/2)
           (let* ((a  (aref av 0))
                  (b  (aref bv 0))
                  (r  (aref rv 0)))
             (make-dotprod-terminal-proof
              :a  (vec a)
              :b  (vec b)
              :r  (vec r))
             ))
          
          (t
           (let ((vlst (list av bv rv gv hv fv)))
             (destructuring-bind (avl bvl rvl gvl hvl fvl)
                 (mapcar (um:rcurry 'left n/2) vlst)
               (destructuring-bind (avr bvr rvr gvr hvr fvr)
                   (mapcar (um:rcurry 'right n/2) vlst)
                 (let* ((lf   (vcommit gvr hvl fvr u avl bvr rvl (vdot avl bvr)))
                        (rt   (vcommit gvl hvr fvl u avr bvl rvr (vdot avr bvl)))
                        (vcmt (vec cmt))
                        (vlf  (vec lf))
                        (vrt  (vec rt))
                        ;; Fiat-Shamir challenge, x.
                        ;; safe: fold in seed and previous challenge,
                        ;; along with proof, left, right
                        (xh     (hash-to-grp-range seed prev-vx vcmt vlf vrt))
                        (new-vx (vec xh))
                        (x      (int xh))
                        (xinv   (m/ x))
                        (xsq    (m* x x))
                        (gv/2   (map 'vector 'ed-add
                                     (ptv-scale gvl xinv)
                                     (ptv-scale gvr x)))
                        (hv/2   (map 'vector 'ed-add
                                     (ptv-scale hvl x)
                                     (ptv-scale hvr xinv)))
                        (fv/2   (map 'vector 'ed-add
                                     (ptv-scale fvl xinv)
                                     (ptv-scale fvr x)))
                        (cmt/2  (ptdot (vector lf cmt rt)
                                       (vector xsq 1 (m/ xsq))))
                        (av/2   (map 'vector 'm+
                                     (vscale avl x)
                                     (vscale avr xinv)))
                        (bv/2   (map 'vector 'm+
                                     (vscale bvl xinv)
                                     (vscale bvr x)))
                        (rv/2   (map 'vector 'm+
                                     (vscale rvl x)
                                     (vscale rvr xinv))))
                   (make-dotprod-subproof
                    :lf  vlf
                    :rt  vrt
                    :proofs (protocol-2 seed new-vx gv/2 hv/2 fv/2 u cmt/2 av/2 bv/2 rv/2))
                   )))))
          )))

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
  (with-accessors ((curve dotprod-proof-curve)
                   (seed  dotprod-proof-seed)
                   (n     dotprod-proof-n)
                   (vcmt  dotprod-proof-cmt)
                   (vc    dotprod-proof-c)
                   (sub   dotprod-proof-proofs)) proof
    (ignore-errors
      (with-ed-curve curve
        (let ((cmt (ed-valid-point-p vcmt))
              (c   (int vc)))
          (multiple-value-bind (gv hv fv u)
              (generate-dotprod-basis n seed)
            (modr
              (let* ((xh   (hash-to-grp-range seed curve n vc vcmt))
                     (vx   (vec xh))
                     (x    (int xh))
                     (ux   (ed-mul u x))
                     (cmtx (ptdot (vector cmt ux)
                                  (vector 1  c))))
                (validate-protocol-2 sub seed vx gv hv fv ux cmtx)
                )))
          )))
    ))

(defun validate-protocol-2 (sub seed prev-vx gv hv fv u cmt)
  (cond  ((dotprod-terminal-proof-p sub)
          (with-accessors ((va  dotprod-terminal-proof-a)
                           (vb  dotprod-terminal-proof-b)
                           (vr  dotprod-terminal-proof-r)) sub
            (destructuring-bind (a b r)
                (mapcar 'int (list va vb vr))
              (destructuring-bind (g h f)
                  (mapcar (um:rcurry 'aref 0) (list gv hv fv))
                (equalp (vec cmt) (vec (ptdot (vector g h f u)
                                              (vector a b r (m* a b))
                                              )))
                ))))
         
         ((dotprod-subproof-p sub)
          (with-accessors ((vlf     dotprod-subproof-lf)
                           (vrt     dotprod-subproof-rt)
                           (spsub   dotprod-subproof-proofs)) sub
            (let* ((n      (length gv))
                   (lf     (ed-valid-point-p vlf))
                   (rt     (ed-valid-point-p vrt))
                   (xh     (hash-to-grp-range seed prev-vx (vec cmt) vlf vrt))
                   (new-vx (vec xh))
                   (x      (int xh))
                   (xsq    (m* x x))
                   (cmt/2  (ptdot (vector lf cmt rt)
                                  (vector xsq 1 (m/ xsq))))
                   (xinv   (m/ x))
                   (n/2    (ash n -1))
                   (vlst   (list gv hv fv)))
              (destructuring-bind (gvl hvl fvl)
                  (mapcar (um:rcurry 'left n/2) vlst)
                (destructuring-bind (gvr hvr fvr)
                    (mapcar (um:rcurry 'right n/2) vlst)
                  (let* ((gv/2  (map 'vector 'ed-add
                                     (ptv-scale gvl xinv)
                                     (ptv-scale gvr x)))
                         (hv/2  (map 'vector 'ed-add
                                     (ptv-scale hvl x)
                                     (ptv-scale hvr xinv)))
                         (fv/2  (map 'vector 'ed-add
                                     (ptv-scale fvl xinv)
                                     (ptv-scale fvr x))))
                    (validate-protocol-2 spsub seed new-vx gv/2 hv/2 fv/2 u cmt/2)
                    ))))
            ))
         (t
          (error "subproof expected"))
         ))

;; --- end of file --- ;;
