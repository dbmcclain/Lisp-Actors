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
  gpt hpt cmt apt mp rp)

(defstruct ped-secrets
  "The secrets used in the Pedersen commitment"
  x gamma d r)

;; ---------------------------------------------------------

(defun make-pedersen-proof (x &key
                              (gpt (ed-random-generator))
                              (hpt (ed-random-generator)))
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
    (let* ((gamma (rand))
           (cmt   (ed-add (ed-mul hpt gamma)
                          (ed-mul gpt x)))
           (d     (rand))
           (r     (rand))
           (apt   (ed-add (ed-mul gpt d)
                          (ed-mul hpt r)))
           ;; challenge z = hash of public info
           (z     (int (hash:hash/256 gpt hpt cmt apt)))
           (mp    (m+ (m* z x) d))
           (rp    (m+ (m* z gamma) r)))
      (values
       (make-ped-proof
        :gpt   (int gpt)
        :hpt   (int hpt)
        :cmt   (int cmt)
        :apt   (int apt)
        :mp    mp
        :rp    rp)
       (make-ped-secrets
        :x     x
        :gamma gamma
        :d     d
        :r     r))
      )))

(defmethod validate-pedersen-proof ((proof ped-proof))
  "Return t if the proof checks out."
  (with-accessors  ((gpt   ped-proof-gpt)
                    (hpt   ped-proof-hpt)
                    (cmt   ped-proof-cmt)
                    (apt   ped-proof-apt)
                    (mp    ped-proof-mp)
                    (rp    ped-proof-rp)) proof
    (ignore-errors
      (destructuring-bind (hpt gpt cmt apt)
          (mapcar 'ed-validate-point
                  (list hpt gpt cmt apt))
        (modr
         (let* ((z     (int (hash:hash/256 gpt hpt cmt apt)))
                (gzpt  (ed-add (ed-mul hpt rp)
                               (ed-mul gpt mp)))
                (gzcmt (ed-add (ed-mul cmt z)
                               apt)))
           ;; verify Pedersen commitment
           (= (int gzpt) (int gzcmt))
           )))
      )))

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
  av bv)

(defstruct dotprod-proof
  gv hv u pf c x proofs)

(defstruct dotprod-subproof
  gv hv pf l r x proofs)
       
(defstruct dotprod-terminal-proof
  g h pf a b)

;; --------------------------------------------------------------------

(defun gen-basis (n)
  (let ((basis  (loop repeat n collect (ed-random-generator))))
    (um:nlet iter ((basis  basis))
      (let* ((basis2 (remove-duplicates basis :test 'ed-pt=))
             (nel2   (length basis2)))
        (if (< nel2 n)
            (go-iter (append (gen-basis (- n nel2)) basis2))
          basis2))
      )))

(defun vcommit (gv hv u av bv)
  ;; (format t "~%AV = ~S" av)
  ;; (format t "~%BV = ~S" bv)
  (ed-add (ptdot gv av)
          (ed-add (ptdot hv bv)
                  (ed-mul u (vdot av bv)))
          ))

(defun dotprod-proof (av bv)
  ;; Dot-Product Proofs
  ;;
  ;; For vector av of values, and vector bv of coefficients, we claim
  ;; to prove that (av • bv) = c.  Values and coffs are kept secret.
  ;; Dotprod value, c = (av • bv) is made public.
  ;;
  ;; We cloak the values by appending an equal number of random values
  ;; whose coefficients are zero. Then both vectors are padded to
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
    (cond ((= n 1)
           ;; this case has dangeraous reveal in L, R
           ;; so we cloak to avoid
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
    ;; add cloaking values
    (setf av  (concatenate 'vector av (map 'vector 'rand av))
          bv  (concatenate 'vector bv (map 'vector (constantly 0) bv))
          n   (ash n 1))
    (modr
     (let* ((basis (gen-basis (+ 1 n n)))
            (u     (pop basis))
            (gv    (coerce (left  basis n) 'vector))
            (hv    (coerce (right basis n) 'vector))
            (c     (vdot av bv))
            (p     (vcommit gv hv u av bv))
            (vc    (vec c))
            (vp    (vec p))
            ;; Fiat-Shamir challenge, x
            (x     (int (hash-to-grp-range vp vc)))
            (ux    (ed-mul u x))
            (cx    (m* c (m- 1 (m/ x)))) ;; c' = c*(1-1/x)
            (px    (ed-add p (ed-mul ux cx))))
       (values
        (make-dotprod-proof
         :gv     (map 'vector 'vec gv)
         :hv     (map 'vector 'vec hv)
         :u      (vec u)
         :pf     vp
         :c      vc
         :x      (vec x)
         :proofs (protocol-2 gv hv ux px av bv))
        (make-dotprod-opening
         :av     av
         :bv     bv))
       ))))

(defun protocol-2 (gv hv u p av bv)
  (let ((n/2  (ash (length av) -1)))
    (modr 
     (cond ((zerop n/2)
            (let* ((a  (aref av 0))
                   (b  (aref bv 0))
                   (g  (aref gv 0))
                   (h  (aref hv 0)))
              (make-dotprod-terminal-proof
               :g  (vec g)
               :h  (vec h)
               :pf (vec p)
               :a  (vec a)
               :b  (vec b))
              ))
           
          (t
           (let ((vlst (list av bv gv hv)))
             (destructuring-bind (avl bvl gvl hvl)
                 (mapcar (um:rcurry 'left n/2) vlst)
               (destructuring-bind (avr bvr gvr hvr)
                   (mapcar (um:rcurry 'right n/2) vlst)
                 (let* ((l   (vcommit gvr hvl u avl bvr))
                        (r   (vcommit gvl hvr u avr bvl))
                        (vp  (vec p))
                        (vl  (vec l))
                        (vr  (vec r))
                        ;; Fiat-Shamir challenge, x
                        (x     (int (hash-to-grp-range vp vl vr)))
                        (xinv  (m/ x))
                        (xsq   (m* x x))
                        (gv/2  (map 'vector 'ed-add
                                    (ptv-scale gvl xinv)
                                    (ptv-scale gvr x)))
                        (hv/2   (map 'vector 'ed-add
                                     (ptv-scale hvl x)
                                     (ptv-scale hvr xinv)))
                        (p/2    (ed-add (ed-mul l xsq)
                                        (ed-add p
                                                (ed-mul r (m/ xsq)))))
                        (av/2   (map 'vector 'm+
                                     (vscale avl x)
                                     (vscale avr xinv)))
                        (bv/2   (map 'vector 'm+
                                     (vscale bvl xinv)
                                     (vscale bvr x))))
                   (make-dotprod-subproof
                    :gv  (map 'vector 'vec gv)
                    :hv  (map 'vector 'vec hv)
                    :pf  vp
                    :l   vl
                    :r   vr
                    :x   (vec x)
                    :proofs (protocol-2 gv/2 hv/2 u p/2 av/2 bv/2))
                   )))))
          ))))

(defun validate-dotprod-proof (pf)
  (let* ((pgv  (dotprod-proof-gv pf))
         (phv  (dotprod-proof-hv pf))
         (pu   (dotprod-proof-u  pf))
         (pp   (dotprod-proof-pf pf))
         (pc   (dotprod-proof-c  pf))
         (px   (dotprod-proof-x  pf))
         (psub (dotprod-proof-proofs pf)))
    (let ((gv (map 'vector 'ed-valid-point-p pgv))
          (hv (map 'vector 'ed-valid-point-p phv))
          (u  (ed-valid-point-p pu))
          (p  (ed-valid-point-p pp))
          (c  (int pc)))
      (modr
       (and (every 'identity gv)
            (every 'identity hv)
            u
            p
            (equalp px (vec (hash-to-grp-range pp pc)))
            (let* ((x   (int px))
                   (ux  (ed-mul u x))
                   (cx  (m* c (m- 1 (m/ x))))
                   (px  (ed-add p
                                (ed-mul ux cx))))
              (validate-protocol-2 psub gv hv ux px)
              ))))))

(defun validate-protocol-2 (sub gv hv u p)
  (cond  ((dotprod-terminal-proof-p sub)
          (with-accessors ((tpg  dotprod-terminal-proof-g)
                           (tph  dotprod-terminal-proof-h)
                           (tpp  dotprod-terminal-proof-pf)
                           (tpa  dotprod-terminal-proof-a)
                           (tpb  dotprod-terminal-proof-b)) sub
            (and (= 1 (length gv))
                 (= 1 (length hv))
                 (equalp (vec (aref gv 0)) tpg)
                 (equalp (vec (aref hv 0)) tph)
                 (equalp (vec p) tpp)
                 (let ((g  (ed-valid-point-p tpg))
                       (h  (ed-valid-point-p tph))
                       (a  (int tpa))
                       (b  (int tpb)))
                   (and tpg
                        tph
                        (equalp tpp (vec (ed-add (ed-mul g a)
                                                 (ed-add (ed-mul h b)
                                                         (ed-mul u (m* a b)))))))
                   ))))
         
         ((dotprod-subproof-p sub)
          (with-accessors ((spgv  dotprod-subproof-gv)
                           (sphv  dotprod-subproof-hv)
                           (spp   dotprod-subproof-pf)
                           (spl   dotprod-subproof-l)
                           (spr   dotprod-subproof-r)
                           (spx   dotprod-subproof-x)
                           (spsub dotprod-subproof-proofs)) sub
            (let ((n  (length gv)))
              (and  (= n (length hv))
                    (= n (length spgv))
                    (= n (length sphv))
                    (every (lambda (pt1 pt2)
                             (equalp (vec pt1) pt2))
                           gv spgv)
                    (every (lambda (pt1 pt2)
                             (equalp (vec pt1) pt2))
                           hv sphv)
                    (equalp (vec p) spp)
                    (let ((l (ed-valid-point-p spl))
                          (r (ed-valid-point-p spr)))
                      (and l r
                           (equalp spx (vec (hash-to-grp-range spp spl spr)))
                           (let* ((x    (int spx))
                                  (x2   (m* x x))
                                  (pf   (ed-add (ed-mul l x2)
                                                (ed-add p
                                                        (ed-mul r (m/ x2)))))
                                  (xinv  (m/ x))
                                  (n/2   (ash n -1))
                                  (vlst  (list gv hv)))
                             (destructuring-bind (gvl hvl)
                                 (mapcar (um:rcurry 'left n/2) vlst)
                               (destructuring-bind (gvr hvr)
                                   (mapcar (um:rcurry 'right n/2) vlst)
                                 (let* ((gv/2  (map 'vector 'ed-add
                                                    (ptv-scale gvl xinv)
                                                    (ptv-scale gvr x)))
                                        (hv/2   (map 'vector 'ed-add
                                                     (ptv-scale hvl x)
                                                     (ptv-scale hvr xinv))))
                                   (validate-protocol-2 spsub gv/2 hv/2 u pf)
                                   ))))
                           ))))))
         (t
          (error "subproof expected"))
         ))
#|
(defun tst (av bv)
  (let* ((proof (dotprod-proof av bv))
         (enc   (loenc:encode proof)))
    (inspect proof)
    (assert (validate-dotprod-proof proof))
    (format t "~%Proof size = ~D" (length enc))))

(tst #(15) #(1))
(tst #(5 5) #(1 2))
(tst #(5 5 4) #(1 2 3))
(tst #(5 5 4 6) #(1 2 3 4))
(tst #(5 5 4 6 7) #(1 2 3 4 5))
|#