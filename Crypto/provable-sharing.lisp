
(in-package :edec-ff)

#|

We seek an (N,M) sharing system where we pass along N values, M <= N of
which are the minimmum necessary shares to combine to reveal a secret
value.

We also want to provide proof of commitment to the secret value, and
proof that each of the shares are correct.

Generating polynomial, order M:

  P(x) = a0 + k*x*Prod[(x-x_i), i = 1..M-1], k random > 0, x_i random > N

  such that,

  P(0) = a0 == our shared secret value.

  Shares are:

    (i, P(i)), i = 1..N

  Any collectxion of size M, of the shares is sufficient to solve for
the shared secret by way of a Lagrange interpolating polynomial formed
from the share values.

   L(x) = Sum[y_i*Prod[x-x_j, j != i]/Prod[x_i-x_j, j != i],i=1..M]
   L(0) => a0

So, clearly, L(x) and P(x) are the same polynomial.

We know that if we write the polynomial as:

   P(x) = a0 + a1*x + a2*x^2 + ... + a_M-1*x^(M-1)

   then

   a_n = (1/n!)*d^n/dx^n P(x) | x=0.

-------------------------------------
Proof of Pedersen Commitment - 2 ways

1. C = x*G + γ*H, x is secret value, γ is random cloaking
   Provide committment C, witnesses L = x*H, R = γ*G.
   For any challenge, z, prover furnishes α = x/z + γ*z.
   Challenger then sees that:
     G' = 1/z*H + z*G
     C' = 1/z^2*L + C + z^2*R
     α*G' = C'  - this is only possible if prover really knows x, γ.

   If DLP is solvable, then secret x is vulnerable in L, γ vulnerable in R.

2. C = x*G + γ*H, x is secret value, γ is random cloaking
   Provide committment C, witness D = y*G + s*H, random y,s
   For any challenge e, prover furnishes u = y + e*x, v = s + e*γ.
   Challenger sees that:
     u*G + v*H = D + e*C - this is only possible if prover really knows x, γ, y, s.

   If DLP is solvable, then secret x remains cloaked by γ, and y cloaked by s.
   But if DLP were broken, then you could not trust dealer who could manipulate
    x -> x', γ -> γ', such that x'*G + γ'*H = x*G + γ*H.


For provably sharing secret a0, given polynomial
   P(x) = a0 + a1*x + a2*x^2 ... + a_(M-1)*x^(M-1),
requiring M of N >= M shares, each share is (x_i, y_i = P(x_i)).

Provide committments on coefficients a_i:
     C_i = a_i*G + γ*H, random cloaking γ
witness D = y*G + s*H, random y,s.
Challenge e = H(C_0, C_1, ... C_(M-1), D).
Provide verifiers {u_i = y + e*a_i, v = s + e*γ}
Coeff Proofs:  u_i*G + v*H = D + e*C_i.

Each coefficient committment provable with (C_i,D) using method 2
above, using x = a_i.

For each participant U_j, j = 1..N:
  Provide witness W_j to j'th participant, along with his share.
     W_j = γ*Sum(x_j^i, i=0..M-1)*H
  Then complete provable share is: ({C_i}, D, x_j, y_j = P(x_j), W_j).
  Proof of share (x_j, y_j):
      y_j*G + W_j = Sum[C_i * x_j^i, i=0..M-1] 

  

|#
#|
Consider checksum on coeffs.

(A0, A1, ..., An, Chk)

P(x) = A0 + A1 x + A2 x^2 + ... An x^n
Q(x) = (x-2) P(x) with root at x=2.
Q(x): (B0,B1, ..., B_n+1)
B_i = A_i-1 - 2 A_i
A_-1  = Chk
A_n+1 = 0

B0 = Chk - 2 A0
B1 = A0 - 2 A1
B2 = A1 - 2 A2
...
B_n = A_n-1 - 2 A_n
B_n+1 = A_n

Q(2) = 0
Chk + 2 P(2) = 0
Chk = -2 P(2)
;; --------------------------------------------

For N-way sharing, generate N-1 random coeffs, plus secret value A0.
F(x) = A0 + Sum(x^i A_i, i=1..N-1)
Offer committments on C_i = A_i*G, and C_sum = Sum(A_i, i = 0..N-1)*G
Generate shares (k, F(k), {C_i}, C_Sum) k = 1..N
See that for share k: F(k)*G = Sum(k^j*C_j, j=0..N-1), and C_Sum = Sum(C_j, j=0..N-1)
To uncover secret A0, bring together all shares and form Lagrangian polynomial L(x|{j,F(j)},j=1..N),
Solve A0 = L(0), see that A0*G = C_0

Bringing together of shares: Send (k, F(k), C_Sum_k) k = 1..N
See that C_Sum_i = C_Sum_j ∀ i,j in 1..N
See that k are all distinct, no duplicates
See that F(k)*G = C_k
|#

(defun lagrange (shares)
  ;; Denominators are constant with respect to x.
  ;; Pre-compute the inverse denominators.
  (let ((1/dens (mapcar (lambda (share)
                        (destructuring-bind (k . fk) share
                          (declare (ignore fk))
                          (cons k
                                (ff/ (reduce (lambda (acc share)
                                               (let ((m (car share)))
                                                 (if (= m k)
                                                     acc
                                                   (ff* acc (ff- k m)))
                                                 ))
                                             shares
                                             :initial-value 1)))
                          ))
                        shares)))
    (lambda (x)
      (reduce (lambda (acc share)
                (destructuring-bind (k . fk) share
                  (let* ((num   (reduce (lambda (acc share)
                                          (let ((m (car share)))
                                            (if (= m k)
                                                acc
                                              (ff* acc (ff- x m)))
                                            ))
                                        shares
                                        :initial-value 1))
                         (1/den (cdr (assoc k 1/dens))))
                    (ff+ acc (ff* fk num 1/den))
                    )))
              shares
              :initial-value 0)
      )))

#|
(with-curve-field
  (let* ((yfn (lambda (x) (ff+ 1 (ff* 3 x))))
         (ks  (um:range 1 10))
         (fks (mapcar yfn ks))
         (shares (pairlis ks fks))
         (lfn (lagrange shares)))
    (int (funcall lfn 0))))
|#

(defun gen-shares (secret nneeded &optional (nshares nneeded))
  ;; Each share consists of:
  ;;
  ;;    ((abscissa-index . ordinate-value)
  ;;     coff-committments-list)
  ;;
  (check-type nneeded (integer 2))
  (check-type nshares (integer 2))
  (assert (>= nshares nneeded))
  (with-curve-field
    (let* ((ks    (um:range 1 (1+ nshares)))
           (coffs (mapcar (lambda (j)
                            (declare (ignore j))
                            (edec-ff::rand))
                          (um:range 1 nneeded)))
           (fpoly (lambda (x)
                    (um:nlet iter ((x^n   x)
                                   (coffs coffs)
                                   (acc   secret))
                      (if (endp coffs)
                          acc
                        (go-iter (ff* x x^n)
                                 (cdr coffs)
                                 (ff+ acc (ff* x^n (car coffs))))
                        ))))
           (fks   (mapcar fpoly ks))
           (cjs   (mapcar #'ed-nth-pt (cons secret coffs))))
      (values (hash/256 cjs)
              ;; hash is sensitive to both value and order of
              ;; coeffients
              (mapcar (lambda (k fk)
                        (list (cons k fk) cjs))
                      ks fks))
      )))

(defun validate-share (problem-hash share)
  ;; Validate a share as belonging to a problem set and having
  ;; self-consistent values.
  ;;
  ;; If a share does not pertain to the problem designated by
  ;; problem-hash, then reject it. It's problem set is the hash of
  ;; its coefficient committments.
  ;;
  ;; If a share carries a bogus abscissa or ordinate, reject it.
  ;;
  (and
   ;; hash of share coeffs matches the problem hash we expect?
   (hash= (hash/256 cjs) problem-hash)
   ;; fk commitment equal to poly sum over coff committments?
   (validate-share-point share)))

(defun validate-share-point (share)
  ;; Validate a share as having self-consistent values.
  ;;
  ;; While we are blind to the implied shared secret, we have
  ;; committments of the polynomial coeffients. Performing the
  ;; polynomial evaluation at our abscissa with those committments
  ;; should produce the committment of our designated ordinal value.
  ;;
  ;; In this sense, the share represents a homomorphic encryption of
  ;; the sharing polynomial.
  ;;
  (with-curve-field
    (destructuring-bind ((k . fk) cjs) share
      (ed-pt= (ed-nth-pt fk)
              (um:nlet iter ((cjs cjs)
                             (k^j 1)
                             (acc (ed-neutral-point)))
                (if (endp cjs)
                    acc
                  (let ((term (ed-mul (car cjs) k^j)))
                    (go-iter (cdr cjs)
                             (ff* k k^j)
                             (ed-add acc term))
                    ))
                ))
      )))

(defun combine-shares (problem-hash shares)
  ;; Filter out bogus shares and deduce the hidden secret value.
  ;; Working only on the recovery problem designated by problem-hash.
  (with-curve-field
    (let* ((rejecting       (complement
                             (um:curry #'validate-share problem-hash)))
           (good-shares     (remove-if rejecting shares))
           (coffs           (second (first good-shares)))
           (ncoffs          (length coffs))
           (selected-shares
            (um:nlet iter ((shares good-shares)
                           (acc    nil))
              (unless (endp shares)
                (let* ((new-shares (adjoin (car shares) acc
                                           ;; abscissa
                                           :key #'caar)))
                  ;; filter out duplicate abscissa shares
                  (if (>= (length new-shares) ncoffs)
                      new-shares
                    (go-iter (cdr shares) new-shares))
                  )))
            ))
      (unless selected-shares
        (error "Too few good shares"))
      (let ((share-points (mapcar #'first selected-shares)))
        (int (funcall (lagrange share-points) 0))
        ))))

#|
(gen-shares 15 2 2)

(multiple-value-bind (chash shares)
    (gen-shares 15 2 5)
  (combine-shares chash (cdddr shares)))

(multiple-value-bind (chash shares)
    (gen-shares 15 2 5)
  (let ((fst    (car shares))
        (snd    (cadr shares)))
    (combine-shares chash (list fst snd))))

(multiple-value-bind (chash shares)
                (gen-shares 15 3 5)
  (assert (reduce (lambda (acc share)
                    (and acc (validate-share chash share)))
                  shares
                  :initial-value t)))

D[x^n,x,m] = m!/(m-n)! x^(n-m) when n > m, else 0.

Using a polynomial as a database. Successive values of interest
stored at increasing abscissae.

So if database has y_m at x=m, then its polynomial coeff
ought to be 1/m! y_m.

Then y_n is revealed at x = 0 in D[P(x),x,n].


|#
