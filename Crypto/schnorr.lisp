
(in-package :edwards-ecc)

;; ----------------------------------------------
;; Schnorr Signatures

(defvar *ssig-nonce*  (int (ecc::make-nonce)))

(defun ssig-nonce ()
  (sys:atomic-incf *ssig-nonce*))

(defun gen-sig-random (msg)
  ;; making r depend on message ensures that no two messages will have the same r, sans hash collisions
  ;; making r depend on incrementing nonce thwarts the evil collective signature coordinator attack
  (int (hash/256 (ssig-nonce) *my-skey* msg)))

(defun ssig-sign (msg)
  ;; generate det random, r
  ;; K = r*G
  ;; e = H(K || M)
  ;; u = r - e * skey
  ;; publish sig = (u e)
  (with-mod *ed-r*
    (let* ((r  (gen-sig-random msg)) ;; deterministic pseudo-randomness
           (K  (ed-compress-pt (ed-mul *ed-gen* r)))
           (e  (hash/256 K msg))
           (u  (m- r (m* (int e) *my-skey*))))
      (list msg
            (ed-compress-pt (ed-mul *ed-gen* *my-skey*))
            u e))))

(defun ssig-verify (msg pkey u e)
  (let* ((Kv  (ed-compress-pt
               (ed-add (ed-mul *ed-gen* u)
                       (ed-mul (ed-decompress-pt pkey) (int e)))))
         (ev  (hash/256 Kv msg)))
    (hash= ev e)))

;; ------------------------------------------------------------
;; Collective Schnorr Signatures

(defun collective-sig-cloaker (&optional (nonce (ssig-nonce)))
  (values  (int (hash/256 nonce *my-skey*))
           nonce))

(defun collective-ssig-setup (msg)
  ;; coordinator provides msg
  (let* ((r (gen-sig-random msg))
         (K (ed-mul *ed-gen* r)))
    (multiple-value-bind (cloak nonce)
        (collective-sig-cloaker)
      ;; send this triple to coordinator
      ;; he will send back H(K_sun||msg) and the nonce with cloaked r value
      (list K
            nonce
            (logxor cloak r))
      )))

(defun collective-ssig-finish (nonce cloaked-r hashsum)
  ;; coordinator provides cloaked-r and hashsum
  (let* ((cloak (collective-sig-cloaker nonce))
         (r     (logxor cloaked-r cloak)))
    (with-mod *ed-r*
      ;; send this u value back to coordinator
      (m- r (m* (int hashsum) *my-skey*))
      )))

#|
(let* ((msg  "This is a test!"))
  (let ((sig (ssig-sign msg)))
    (print sig)
    (destructuring-bind (msg pkey u e) sig
      (assert (ed-pt= pkey (ed-compress-pt *my-pkey*)))
      (ssig-verify msg pkey u e)
      )))
 |#
;; ----------------------------------------------------------------------------
#|
 For collective signatures:
  1. Coordinator node sends msg to all participant nodes
  2. Each participant forms r_i, i = 1..N participants,
     and sends K_i = r_i * G back to coordinator.
  3. Coordinator node adds up all K_i, i = 1..N to produce K_sum,
     and forms hash H_sum = H(K_sum | msg), and sends this back to all participants
  4. Each node forms u_i = r_i - H(K_sum || msg) * skey_i, and sends u_i back to coordinator
  5. Coordinator sums u_i, i = 1..N to produce u_sum. Publishes u_sum and H_sum as the signature
     along with sum of all participant Pkey_i, Pkey_sum, as the validating public key.

;; ----------------------------------------------------

Evil Collective Signature Coordinator Attack
  - Coordinator sends MSG1 to all participants
  - Coordinator collects randomness K_i, forms H(K_sum | msg)
  - Coordinator sends H(K_sum | msg) ta all participants
  - Participants form u_i and send back to coordinator
  -- So far so good...
  - Now coordinator sends same msg to subset of participants
  - on collecting the K_i, the same r_i will have been used at each participant, since same message
  - but K_sum will be different, and so will H(K_sum | msg)
  - on collecting new u_i, we have the situation where same r_i was used, but now have different
    multiplier on the secret keys.
  - So... simple algebra allows Coordinator to see each participant's secret key.

  Countermeasure is to include an incrementing count at each node, and generate randomness as
    r = H(++ctr || skey || msg)
|#
;; -----------------------------------------------------------------------------------------
