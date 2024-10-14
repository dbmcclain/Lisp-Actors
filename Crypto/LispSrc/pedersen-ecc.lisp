;; pedersen-ecc.lisp - a look at Pedersen Commitments
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------
#|
The MIT License

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

;; Needs Edwards.lisp compiled and loaded...
(user::asdf :core-crypto)

(defpackage :pedersen-ecc
  (:use :common-lisp :crypto-mod-math)
  (:import-from :edwards-ecc
   :ed-mul
   :ed-add
   :ed-sub
   :ed-div
   :ed-negate
   :ed-affine
   :*edcurve*
   :*ed-r*
   :*ed-q*
   :*ed-gen*
   :ed-curve-name
   :random-between
   :ed-pt=
   :ed-compress-pt
   :ed-decompress-pt
   :convert-bytes-to-int
   :with-ed-curve
   :ed-nth-pt
   :ed-random-pair
   )
  (:import-from :ecc-crypto-b571
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:export
   ))

(in-package :pedersen-ecc)

(defun rand-val ()
  ;; random value in Z_r
  (random-between (ash *ed-r* -1) *ed-r*))

(defun hashem-int (&rest args)
  (convert-bytes-to-int
   (apply 'sha3-buffers args)))

(defstruct commitment
  ;; points stored in compressed form for network transmission
  curve  ;; the ECC curve
  gens   ;; the ECC generators used for dot-prod basis
  hgen   ;; random point on curve
  com    ;; the commitment value (a point on the curve)
  )

(defun commit-vector (a-vec g-vec h-gen blinding-s)
  ;; h-gen = (pf-expt *pf-rg* secret-a)
  ;; keep secrets: secret-a, blinding-s
  (make-commitment
   :curve  (ed-curve-name *edcurve*)
   :gens   (mapcar 'ed-compress-pt g-vec)
   :hgen   (ed-compress-pt h-gen)
   :com    (ed-compress-pt
            ;; s*h + (a . g), for a, g vectors
            (ed-add
             ;; shield dot prod with blinding
             (ed-mul h-gen blinding-s)
             ;; compute g^(alpha .dot. a)
             (reduce 'ed-add
                     (mapcar 'ed-mul g-vec a-vec))))
   ))

;; ---------------------------------------------------------------
;; COMMIT - For a vector of values a-vec, create a Pedersen Commitment
;;
;; This current version effectively forms the dot-product between
;; a-vec and a random blinding vector formed as g-vec. When ready to
;; reveal the commitment, share the a-vec and the random blinding-s
;; value formed during the commitment.
;;
(defun commit (a-vec)
  ;; send com portion, keep secret-a & blinding-s secret for now
  ;; when ready to reveal the commited values, send a-vec and blinding-s
  (let* ((secret-a   (rand-val)) ;; never shared
         (blinding-s (apply 'hashem-int secret-a a-vec)) ;; (rand-val))
         (g-vec      (make-gens (length a-vec)))
         (h-gen      (ed-mul *ed-gen* secret-a))
         (com        (commit-vector a-vec g-vec h-gen blinding-s)))
    (list :com com ;; public share
          :secret-a secret-a  ;; keep secret
          :blinding-s blinding-s))) ;; keep secret
  
  ;; -------------------------------------------------------------------

(defun make-gens (nel)
  ;; form a random blinding vector of generators g^alpha_i
  ;; ensure no duplicates (very small chance of that)
  (um:nlet iter ((nel nel)
                      (ans nil))
    (if (plusp nel)
        (let ((g-alpha (ed-mul *ed-gen* (rand-val))))
          (if (member g-alpha ans :test 'ed-pt=)
              (go-iter nel ans)
            (go-iter (1- nel) (cons g-alpha ans))))
      ans)))

(defun make-coffs (nel)
  ;; choose some random values for test a-vec
  (loop repeat nel collect
        (rand-val)))

;; ------------------------------------------------------------------

(defun hash-length-bits ()
  (1+ (integer-length *ed-q*)))

(defun select-sha3-hash ()
  (let ((nb  (hash-length-bits)))
    (cond ((> nb 384) :sha3)
          ((> nb 256) :sha3/384)
          (t          :sha3/256)
          )))

(defun sha3-buffers (&rest bufs)
  ;; assumes all buffers are UB8
  (let ((dig  (ironclad:make-digest (select-sha3-hash))))
    (dolist (buf bufs)
      (ironclad:update-digest dig buf))
    (ironclad:produce-digest dig)))

(defun convert-pt-to-v (pt)
  (let ((nb (ceiling (1+ (integer-length *ed-q*)) 8)))
    (convert-int-to-nbytesv (ed-compress-pt pt) nb)))

(defun hash-pt-pt (pt1 pt2)
  (let ((v1 (convert-pt-to-v pt1))
        (v2 (convert-pt-to-v pt2)))
    (convert-bytes-to-int (sha3-buffers v1 v2))))

(defun hash-pts (&rest pts)
  (convert-bytes-to-int
   (apply 'sha3-buffers (mapcar 'convert-pt-to-v pts))))

;; --------------------------------------------------------------------

#|
;; try it out... 
(let* ((nel  4)
       (av   (make-coffs nel)))
  (inspect (commit av)))
 |#
#|
;; -----------------------------------------------------------------------
;; ZKP to prove N >= 0, select the 4 integers from Lagrange 4-square,
;; whose sum of squares equals our N. Hence, sum >= 0.
;;
;; Make a commitment on these 4 numbers:
;;    (n1 n2 n3 n4) -> Sum(n_i * p_i, i = 1..4) for p_i prime
;;
;; If the p_i are all coprime and > largest possible n_i then this sum is unique
;; per ordering of n_i. Random blinding value v, secret key s,
;; commitment (msg = Sum*P - V, c = H(V,msg),  r = v - c*s)
;;   so R = r*G = v*G - c*s*G = v*G - c*P = V - c*P, or V = r*G + c*P
;;      check H(V,msg) = c
;;      Sum*P = msg + V

(inspect
 (sort
  (loop for ix from 0 below 11 nconc
        (loop for iy from 0 below 11 collect (+ (* ix 11) (* iy 13))))
  '<))
|#
    