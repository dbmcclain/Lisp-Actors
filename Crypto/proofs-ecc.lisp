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
        :hpt   (int hpt)
        :gpt   (int gpt)
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
  (with-accessors  ((hpt   ped-proof-hpt)
                    (gpt   ped-proof-gpt)
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
