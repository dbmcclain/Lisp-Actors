;; srp6-ecc.lisp -- SRP-6a Protocol in ECC
;;
;; DM/RAL 03/21
;; ------------------------------------------------

(defpackage #:srp6-ecc
  (:use #:cl #:ac)
  (:import-from #:core-crypto
   #:int
   #:hash/256
   #:hash=
   #:vec
   #:with-mod
   #:m+
   #:m*)
  (:import-from #:edwards-ecc
   #:*ed-gen*
   #:*ed-r*
   #:*ed-q*
   #:ed-affine
   #:ed-compress-pt
   #:ed-random-pair
   #:make-deterministic-keys
   #:ed-add
   #:ed-sub
   #:ed-mul)
  (:import-from #:usec
   #:get-time-usec)
  (:import-from #:actors.security
   #:$VERSION
   #:signature-mismatch-error
   #:crypto
   #:init-crypto-for-hmac
   #:init-crypto-for-input
   #:init-crypto-for-output
   #:init-crypto-for-renegotiation)
  (:import-from #:actors.network
   #:client-request-negotiation-ecc
   #:intf-srp-ph2-reply
   #:intf-srp-ph2-begin-ecc
   #:intf-srp-ph3-begin)
  (:export
   #:client-negotiate-security-ecc
   #:server-negotiate-security-ecc
   ))

(in-package #:srp6-ecc)

(defvar *member-tbl* (make-hash-table
                      :test #'equal))

(defun add-member (info)
  (setf (gethash (car info) *member-tbl*) (cdr info)))

#|
(defun member-data (machine-instance)
  (let* ((salt (int (hash/256 (usec:get-time-usec))))
         (seed (int (hash/256 salt (hash/256 machine-instance $VERSION)))))
    (multiple-value-bind (x v)
        (make-deterministic-keys seed)
      (list machine-instance salt x (ed-compress-pt v)))))
|#

(progn
  (add-member '("Arroyo.local"
                64312043832863619747953468365437559830437998134641746103290477938253457572300
                891938153290200541895437830188057625448175311417807227544513453588451873458
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 2281124396847643339496930478758037867194371184857201838485742174386118537564)))
  (add-member '("Rincon.local"
                115162469593219643095529344846467810336764392449392003640786646358322559337381
                685238382847641274182683657843007532897107465333895821456505441193694774431
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 4618639225348649571859847622871416385806382551022274362125457945368830526970))))

(defmethod client-negotiate-security-ecc ((crypto crypto) intf)
  ;; Phase-I: send local node ID
  (let ((node-id (machine-instance)))
    (destructuring-bind (salt x gxc)
        (gethash node-id *member-tbl*)
      (declare (ignore salt))
      (=wait ((bbc) :timeout 5)
          (client-request-negotiation-ecc intf =wait-cont node-id)
        ;; Phase-II: receive N,g,s,B
        ;; Compute: x = H32(s,ID,PassPhrase)
        ;;          a = 1 < random < N
        ;;          A = g^a mod N
        ;;          u = H32(A,B)
        ;;          S = (B - 3*g^x)^(a+u*x) mod N
        ;;          M1 = H32(A,B,S)
        ;; Send A,m1
        ;; Hold as secret: x, a, u, S
        
        ;; Public key B might not be a *valid* public key.
        ;; But we don't use it directly.
        ;; Do the subtraction first, then check for validity.
        (let* ((k   (int (hash/256 *ed-r* *ed-q*)))
               (bbb (ed-sub bbc
                            (ed-mul gxc k))))
          
          (multiple-value-bind (a aa)
              (ed-random-pair)
            (let* ((aac (ed-compress-pt aa))
                   (u   (int (hash/256 aac bbc)))
                   (sc  (ed-compress-pt
                         (ed-mul bbb
                                 (with-mod *ed-r*
                                   (m+ a (m* u x))))))
                   (m1  (hash/256 aac bbc sc)))
              (=wait ((m2) :timeout 5)
                  (funcall (intf-srp-ph2-reply intf) =wait-cont aac m1)
                ;; Phase 3: receive M2
                ;; Compute: Chk2  = H32(A,M1,S), check Chk2 = M2
                ;;          Key   = H32(S) 
                ;;          InitV = H32(M2,S)
                ;; Init crypto with Key, InitV
                (let* ((chk2      (hash/256 aac m1 sc))
                       (key-in    (vec (hash/256 aac sc bbc)))
                       (key-out   (vec (hash/256 bbc sc aac)))
                       (his-initv (vec (hash/256 m1 sc)))
                       (my-initv  (vec (hash/256 m2 sc))))
                  
                  (unless (hash= chk2 m2)
                    (signature-mismatch-error))
                  
                  (init-crypto-for-hmac   crypto his-initv my-initv)
                  (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                  (init-crypto-for-output crypto key-out (subseq my-initv 0 16))
                  (init-crypto-for-renegotiation crypto (vec sc))
                  ))
              ))))
      )))

(define-condition no-member-info (error)
  ((node-id  :reader no-member-info-node-id :initarg :node-id))
  (:report (lambda (c stream)
             (format stream "No member info: ~A"
                     (no-member-info-node-id c)))
   ))

(defmethod server-negotiate-security-ecc ((crypto crypto) intf node-id)
  ;; ensure that our good numbers have not been altered...
  (let ((gxc (third (gethash node-id *member-tbl*))))
    (unless gxc
      (error 'no-member-info :node-id node-id))
    ;; Phase II: Compute: s = random 256-bit
    ;;                    x = H32(s,ID,PassPhrase)
    ;;                    v = g^x mod N
    ;;                    b = 1 < random < N
    ;;                    B = 3*v + g^b mod N
    ;; Write: N,g,s,B
    ;; Hold as secret: x, v, b
    (multiple-value-bind (b bbb)
        (ed-random-pair)
      (let* ((k   (int (hash/256 *ed-r* *ed-q*)))
             (bbc (ed-compress-pt
                   (ed-add bbb
                           (ed-mul gxc k)))))
        (=wait ((aac m1) :timeout 5)
            (funcall (intf-srp-ph2-begin-ecc intf) =wait-cont bbc)
          ;; Phase III: Receive A,M1
          ;; Compute: u     = H32(A,B)
          ;;          S     = (A*v^u)^b mod N
          ;;          Chk1  = H32(A,B,S) check Chk1 = M1
          ;;          M2    = H32(A,M1,S)
          ;;          Key   = H32(S)    ;; H16(S)
          ;;          InitV = H32(M1,S) ;; H16(M1,S)
          ;; Send: M2
          ;; Hold as secret: u,S
          ;; Init crypto with Key, initv
          (let* ((u         (int (hash/256 aac bbc)))
                 (sc        (ed-compress-pt
                             (ed-mul
                              (ed-add aac
                                      (ed-mul gxc u))
                              b)))
                 (chk1      (hash/256 aac bbc sc))
                 (m2        (hash/256 aac m1 sc))
                 (key-in    (vec (hash/256 bbc sc aac)))
                 (key-out   (vec (hash/256 aac sc bbc)))
                 (his-initv (vec (hash/256 m2 sc)))
                 (my-initv  (vec (hash/256 m1 sc))))
            
            (unless (hash= chk1 m1)
              (signature-mismatch-error))
            
            (funcall (intf-srp-ph3-begin intf) m2
                     (lambda ()
                       (init-crypto-for-hmac   crypto my-initv his-initv)
                       (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                       (init-crypto-for-output crypto key-out (subseq my-initv 0 16))
                       (init-crypto-for-renegotiation crypto (vec sc))
                       ))
            ))
        ))))
