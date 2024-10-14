;; proof-of-skey.lisp
;;
;; DM/RAL  2024/10/10 12:46:25 UTC
;; ----------------------------------

(defpackage #:proof-of-skey
  (:use #:common-lisp #:edec #:vec-repr #:hash #:finite-field))

(in-package #:proof-of-skey)

;; ----------------------------------

(defstruct Schnorr-signature
  Curve Cpt Hk z)

(defun Schnorr-signature (item s)
  ;;
  ;; Produce ZKP triple:
  ;;
  ;;    (C, Hk, z),
  ;;
  ;; where,
  ;;
  ;;    C  = a commitment on s,
  ;;    Hk = a random hash challenge, Hk = H(K,C,item) for random point Kpt
  ;;    z  = the response to the challenge.
  ;;
  ;; --------------------------------------
  ;; First we select a random value, k, and compute the Fiat-Shamir
  ;; challenge, Hk, as:
  ;;
  ;;    Hk = Hash/256(K=k*G, C=s*G, item)
  ;;
  ;; Random, k, is based on a hash of a nonce, item, and field scalar, s.
  ;;
  ;; Commitment, C = s*G, is a commitment to the field element, s,
  ;; used in construction of the ZKP. And k*G is a committment to the
  ;; randomness k.
  ;;
  ;; Finally, we compute challenge response, z, as:
  ;;
  ;;   z = k - Hk*s
  ;;
  ;; so that, during verification
  ;;
  ;;   z*G + Hk*C = K
  ;;
  ;; and so verifier can ascertain that:
  ;;
  ;;   H(z*G + Hk*C, C, item) = Hk
  ;;
  ;; IOW, We provide a pre-image of hash, Hk, that is based on itself.
  ;; Could never happen unless we know field element, s, and random
  ;; value, k.
  ;;
  ;; Note that, because random, k, is based on the hash of a nonce and
  ;; time to the nearest 100 ns, for a fixed item and fixed scalar, s,
  ;; two distinct requests for ZKP produce entirely different values
  ;; for Hk, z. Hence, it becomes impossible to reverse engineer the
  ;; two ZKP's to find randomness, k, nor scalar, s.
  ;;
  (let* ((Cp     (ed-nth-pt s))         ;; committment, Cp
         (k      (int (hash/256 Cp item))) ;; random scalar, k
         (Kp     (ed-nth-pt k))         ;; random point committment, Kp
         (Hk     (hash/256 Kp Cp item)) ;; challenge, Hk
         (z      (with-curve-field      ;; proof response, z
                   (ff- k (ff* (int Hk) s))))) ;; challenger should see that: z*G = A - Hk*P
    (make-Schnorr-signature
     :Curve *ed-name*
     :Cpt   Cp
     :Hk    Hk
     :z     z)
    ))

(defmethod validate-Schnorr-signature (item (sig Schnorr-signature))
  (with-slots (Curve Cpt Hk z) sig
    (with-ed-curve Curve
      (let ((H  (hash/256 (ed-add
                           (ed-nth-pt z)
                           (ed-mul Cpt (int Hk)))
                          Cpt item)))
        (hash:hash= Hk H)
        ))))

;; --------------------------------------------
;; ZKP that I know field element, skey.

(defun proof-of-skey (skey)
  ;; In this case, ZKP (C, Hk, z) becomes ZKP (Pkey, Hk, z)
  (Schnorr-signature :skey skey))

(defmethod validate-proof-of-skey ((proof Schnorr-signature))
  (validate-Schnorr-signature :skey proof))

;; --------------------------------------------

(defstruct Scalar-proof
  Curve seed Cpt Apt d e)

(defun Scalar-proof (u)
  ;; Using Pedersen Committment on u.
  ;; Safe for any size of scalar, u.
  ;;
  ;;   C = u*G + γ*H
  ;;   A = r*G + s*H
  ;;
  ;;   Challenge
  ;;   z = Hash(C A)
  ;;
  ;;   Response
  ;;   d = (r + z*u)*G
  ;;   e = (s + z*γ)*H
  ;;   ∴ z*C + A = d*G + e*H
  ;;
  ;;  Produce (seed. C, A, d, e)
  ;;  G, H random pts from seed.
  ;;
  (with-curve-field
    (let* ((seed  (hash/256 (uuid:make-v1-uuid))))
      (multiple-value-bind (Gpt Hpt)
          (edec-ff::generate-pedersen-basis seed)
        (let* ((γ     (edec-ff::rand))
               (r     (edec-ff::rand))
               (s     (edec-ff::rand))
               (Cpt   (ed-add (ed-mul Gpt u)
                              (ed-mul Hpt γ)))
               (Apt   (ed-add (ed-mul Gpt r)
                              (ed-mul Hpt s)))
               (z     (int (hash/256 Cpt Apt)))
               (d     (ff+ r (ff* z u)))
               (e     (ff+ s (ff* z γ))))
          (make-scalar-proof
           :Curve *ed-name*
           :seed  seed
           :Cpt   Cpt
           :Apt   Apt
           :d     d
           :e     e)
          )))))
    
(defmethod validate-scalar-proof ((proof Scalar-proof))
  (with-slots (Curve seed Cpt Apt d e) proof
    (with-ed-curve Curve
      (multiple-value-bind (Gpt Hpt)
          (edec-ff::generate-pedersen-basis seed)
        (let* ((z    (int (hash/256 Cpt Apt)))
               (zCpt (ed-mul Cpt z))
               (Dpt  (ed-mul Gpt d))
               (Ept  (ed-mul Hpt e)))
          (ed-pt= (ed-add Dpt Ept)
                  (ed-add zCpt Apt))
          )))))

;; --------------------------------------------
#|    
(validate-scalar-proof (Scalar-proof 1))

 
(C, Hk, z)
---------
k = rand
γ = rand
A = rand pt
z = k - Hk*s
C = s*G + γ*A
K = k*G
Hk = H(K)
∴ Hk = H(z*G + Hk*(C - γ*A))

;; Pedersen committment on u, cloaking γ 
;; choose r, s random
;;    Basis (G, H), C = u*G + γ*H, R = r*G, S = s*H
;;
;; challenge z
;;   d = r + z*u
;;   e = s + z*γ
;;   --> (d e)

    z*C = z*u*G + z*γ*H
        = d*G - R + e*H - S

    z*C + R + S = d*G + e*H
;; C = u*G
;; challenge z
;; d = r + z*u
;; e = s + z*t
(C, r, s)
C = x*G + γ*H
pf: 

C = u*G + γ*H
α = z*u + γ/z
z*u = α - γ/z
z*γ = z^2*(α - z*u)
z*C = z*u*G + z*γ*H
    = (z*α-γ)*G + 
|#

