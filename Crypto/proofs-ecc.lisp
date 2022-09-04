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

(defun rand ()
  "Return a random integer from Z_r over the domain of the curve
field. 1 <= z_rand < group order"
  (safe-field-random *ed-r*))

;; --------------------------------------------------------

(defstruct ped-proof
  "Public knowlege for proof of a Pedersen commitment."
  hpt gpt cmt alpha lf rt)

(defstruct ped-secrets
  "The secrets used in the Pedersen commitment"
  gamma x)

;; ---------------------------------------------------------

(defun make-pedersen-proof (x &key
                              (hpt (ed-random-generator))
                              (gpt (ed-random-generator)))
  "Make a Pedersen commitment to x with computational binding, and
random hiding, gamma:

    C = gamma * H + x * G

for randomly selected, independent generators H, G.
Publish C, H, and G.

Proof is by way noting that for random challenge, z, prover supplies
values for alpha, L, and R, where:

    alpha = z * gamma + (1/z) * x
    L = x * H
    R = gamma * G

Then verifier computes:

    G' = (1/z) * H + z * G
    C' = (1/z^2) * L + C + z^2 * R

and sees that

    alpha * G' = C'

which proves knowledge of both factors, gamma and x.

We invoke Shamir conversion to NIZKP by using the hash of the
transcript as the random challenge value, z = Hash/256(H, G, C).

Note, that if x comes from a small domain, and by way of knowing both
H and L = x*H, it is possible for a brute force search to determine x.
So small domain values must be cloaked. See below..."
  
  (modr
    (let* ((gamma (rand))
           (cmt   (ed-add (ed-mul hpt gamma)
                          (ed-mul gpt x)))
           ;; challenge z = hash of public info
           (z     (int (hash:hash/256 hpt gpt cmt)))
           (alpha (m+ (m* gamma z)
                      (m/ x z)))
           (lf    (ed-mul hpt x))
           (rt    (ed-mul gpt gamma)))
      (values
       (make-ped-proof
        :hpt   (int hpt)
        :gpt   (int gpt)
        :cmt   (int cmt)
        :alpha alpha
        :lf    (int lf)
        :rt    (int rt))
       (make-ped-secrets
        :x     x
        :gamma gamma))
      )))

(defmethod validate-pedersen-proof ((proof ped-proof))
  "Return t if the proof checks out."
  (with-accessors  ((hpt   ped-proof-hpt)
                    (gpt   ped-proof-gpt)
                    (cmt   ped-proof-cmt)
                    (alpha ped-proof-alpha)
                    (lf    ped-proof-lf)
                    (rt    ped-proof-rt)) proof
    (ignore-errors
      (multiple-value-bind (hpt gpt cmt lf rt)
          (values (ed-validate-point hpt)
                  (ed-validate-point gpt)
                  (ed-validate-point cmt)
                  (ed-validate-point lf)
                  (ed-validate-point rt))
        (modr
         (let* ((z     (int (hash:hash/256 hpt gpt cmt)))
                (gzpt  (ed-add (ed-mul hpt (m/ z))
                               (ed-mul gpt z)))
                (zsq   (m* z z)))
           ;; verify Pedersen commitment
           (= (int (ed-mul gzpt alpha))
              (int (ed-add (ed-mul lf (m/ zsq))
                           (ed-add cmt
                                   (ed-mul rt zsq)))))
           ))))
    ))

#|
(let ((pf (make-pedersen-proof 15)))
  (inspect pf)
  (validate-pedersen-proof pf))
 |#
;; --------------------------------------------------------

(defstruct clk-proof
  "Public knowlege for the cloaked proof - two Pedersen proofs plus
additional sum commitment and adjustment factor."
  a-proof ax-proof
  cmt-x hadj)

(defstruct clk-secrets
  "Secrets of the cloaked proof"
  a-secrets ax-secrets
  x gam-x)

;; --------------------------------------------------------

(defun make-cloaked-proof (x)
  "Make a proof of cloaked value with binding commitments on x and a
random cloaking value, a.

We provide two Pedersen commitments with proofs, for a and (x-a) using
independent generators G, H. Pedersen commitments have the form

   C = gamma*H + v*G,

for a binding commitment to value v, with random hiding factor
gamma.

Since cloaking value, a, is randomly selected from the entire domain
of the curve field, it is safe to reveal partial knowledge through
L = a*H during the proof. And similarly for (x - a)*H. Brute force
search is infeasible.

We publish the two Pedersen commitment proofs. Then we provide a proof
on the Pedersen commitment for x by showing that

   Cx = gamma-x * H + x * G
      = Cx-a + Ca + hAdj * H.

Since

   Ca = gamma-a * H + a * G

and

   Cx-a = gamma-ax * H + (x - a) * G

the sum in G is obvious. But to match the (gamma-x * H) term we need an
adjustment in the H curve, of the form:

   hAdj = (gamma-x - (gamma-ax + gamma-a))

We have already proven binding knowledge of hiding factors gamma-a
and gamma-ax, and also on a and (x-a), in the two Pedersen commitments
with proofs.

Given that H and G are independent, the sum of commitments proof shows
that knowledge of all three gamma are present.  And the fact that
adjustment is needed only on the H curve shows that x = (x - a) + a in
the G curve.

All three gamma are random values sampled from the entire domain of
the curve. So publishing hAdj reveals nothing. Brute force search
remains infeasible.

Note that we cannot provide a Pedersen proof for x alone, as that
would show a value of L = x*H during the proof. And since x may be
from a small domain, and knowing H, it would be simple to perform
brute-force search to find x."
  
  (modr
   (multiple-value-bind (ped-a sec-a)
       (make-pedersen-proof (rand)) ;; commit to random cloaking, a
     (let ((a     (ped-secrets-x sec-a))
           (gam-a (ped-secrets-gamma sec-a))
           (hpt   (ed-decompress-pt (ped-proof-hpt ped-a)))
           (gpt   (ed-decompress-pt (ped-proof-gpt ped-a))))
       (multiple-value-bind (ped-ax sec-ax)
           (make-pedersen-proof (m- x a) ;; commit to difference (x - a)
                                :gpt gpt ;; same independent generators G, H
                                :hpt hpt)
         (let* ((gam-ax  (ped-secrets-gamma sec-ax))
                (gam-x   (rand))  ;; choose random hiding factor
                (hadj    (m- gam-x (m+ gam-ax gam-a)))
                (cmt-x   (ed-add (ed-mul hpt gam-x) ;; form Pedersen commitment on x
                                 (ed-mul gpt x))))  ;; same generators G, H
           (values
            (make-clk-proof
             :a-proof   ped-a
             :ax-proof  ped-ax
             :cmt-x     (int cmt-x)
             :hadj      hadj)
            (make-clk-secrets
             :a-secrets   sec-a
             :ax-secrets  sec-ax
             :x           x
             :gam-x       gam-x))
           ))))))

(defmethod validate-cloaked-proof ((prf clk-proof))
  "Return t if the proof checks out."
  (with-accessors ((cmt-x   clk-proof-cmt-x)
                   (hadj    clk-proof-hadj)
                   (prf-a   clk-proof-a-proof)
                   (prf-ax  clk-proof-ax-proof)) prf
    (ignore-errors
      (when (and (= (ped-proof-gpt prf-a) (ped-proof-gpt prf-ax))
                 (= (ped-proof-hpt prf-a) (ped-proof-hpt prf-ax))
                 (validate-pedersen-proof prf-a)
                 (validate-pedersen-proof prf-ax))
        (with-accessors ((cmt-a  ped-proof-cmt)
                         (hpt    ped-proof-hpt)) prf-a
          (with-accessors ((cmt-ax  ped-proof-cmt)) prf-ax
            (multiple-value-bind (hpt cmt-ax cmt-a)
                (values (ed-validate-point hpt)
                        (ed-validate-point cmt-ax)
                        (ed-validate-point cmt-a))
              (modr
               (= (int cmt-x)
                  (int (ed-add (ed-mul hpt hadj)
                               (ed-add cmt-ax cmt-a)))
                  )))
            ))))
    ))

#|
(let ((pf (make-cloaked-proof 15)))
  (inspect pf)
  (validate-cloaked-proof pf))
 |#
