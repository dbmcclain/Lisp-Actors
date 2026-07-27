;; srp6-ecc.lisp -- SRP-6a Protocol in ECC
;;
;; DM/RAL 03/21
;; ------------------------------------------------

(defpackage #:stactors/srp6-ecc
  (:use #:cl #:ac)
  (:import-from #:core-crypto
   #:int
   #:vec
   #:hash/256
   #:hash=
   #:m+
   #:m*)
  (:import-from #:edwards-ecc
   #:*ed-r*
   #:*ed-q*
   #:modr
   #:ed-neutral-point-p
   #:ed-compress-pt
   #:ed-random-pair
   #:make-deterministic-keys
   #:ed-add
   #:ed-sub
   #:ed-mul)
  (:import-from #:usec
   #:get-time-usec)
  (:import-from #:stactors/security
   #:$VERSION
   #:signature-mismatch-error
   #:crypto
   #:init-crypto-for-hmac
   #:init-crypto-for-input
   #:init-crypto-for-output
   #:init-crypto-for-renegotiation)
  (:import-from #:stactors/network
   #:client-request-negotiation-ecc
   #:intf-srp-ph2-begin-ecc
   #:intf-srp-ph2-reply
   #:intf-srp-ph3-begin)
  (:export
   #:client-negotiate-security-ecc
   #:server-negotiate-security-ecc
   ))

(in-package #:stactors/srp6-ecc)

(defvar *k* (modr
             (m+ 0 (int (hash/256 *ed-r* *ed-q*)))))

;; If this assertion fails, you are in big trouble. Choose a different
;; curve. Or choose a different way to produce *K*.
(assert (plusp *k*))

(defvar *member-tbl* (make-hash-table
                      :test #'equal))

(defun add-member (info)
  ;; info = (machine-id, salt, pubkey)
  (setf (gethash (car info) *member-tbl*) (cdr info)))

(defun gen-info (machine-instance salt)
  (let ((id  (uuid:make-v1-uuid)))
    (make-deterministic-keys (int (hash/256 salt
                                            (uuid:uuid-mac id)
                                            machine-instance
                                            $VERSION)))))

(progn
  (add-member '("RAMBO"
                45856201371453790338684476625774037518018348616596977538111467812341557771426
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 4109462784809415756608148087493293195494253484169838025065489294992891936759)))
  (add-member '("Rincon.local"
                7374906492199582768186917522243095222278956196110340325819761741469008931898
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 3701637139218678945650869305201356630109196577234174017024796712469667985034)))
  (add-member '("Arroyo.local"
                106029884373403939073501875292406545473531818604404964185342374082620578855260
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 4723142539931161713001162285185296666487924957940488481520777516455453944074))))

#|
(let ((*print-readably* t))
  (let ((salt (int (hash/256 (usec:get-time-usec))))
        (mach (machine-instance)))
    (multiple-value-bind (x gx)
        (gen-info mach salt)
      (pprint (list mach salt (ed-compress-pt gx))))))

(multiple-value-bind (x gx)
    (gen-info (machine-instance)
              58092113895438756482702715951169183950349033817880824399631344333284986728915)
  (ed-compress-pt gx))
|#

(define-condition no-member-info (error)
  ((node-id  :reader no-member-info-node-id :initarg :node-id))
  (:report report-no-member-info))

(defun report-no-member-info (c stream)
  (format stream "No member info: ~A"
          (no-member-info-node-id c)))

(defun get-keying (mach-id)
  (or (gethash mach-id *member-tbl*)
      (error 'no-member-info :node-id mach-id)))

(defmethod client-negotiate-security-ecc ((crypto crypto) intf &optional cont)
  ;; No second chances - any error shuts down the connection
  ;; immediately.
  (let* ((node-id (machine-instance))
         (salt    (car (get-keying node-id)))) ;; salt as int
    ;;
    ;; Phase-I: send local node ID
    ;;
    (=bind (bbc)
        (client-request-negotiation-ecc intf =bind-cont node-id)
      ;;
      ;; Phase-II: receive B                  -- a random compressed ECC point on Curve1174
      ;;
      ;; PreComputed: (x, V=x*G) as deterministic key pair, using
      ;;                         seed H32(salt,MAC-addr,ID,PassPhrase))
      ;;
      ;; Compute: (a, A=a*G) as random key pair on curve1174
      ;;          u = H32(A,B)                -- A,B in compressed form
      ;;          k = H32(*ed-r*,*ed-q*)
      ;;          S = (B - k*V)*(a + u*x)     -- a point on Curve1174
      ;;          M1 = H32(A,B,S)             -- A,B,S in compressed form
      ;;
      ;; Send A,M1                            -- A in compressed form, M1 as Hash/256
      ;; Hold as secret: x, a, u, S
      ;;
      ;; Public key B might not be a *valid* public key.  Conversion
      ;; from compressed form to affine or projective performs
      ;; validity checking.
      ;;
      (multiple-value-bind (x gx)
          (gen-info node-id salt)
        (let ((bb  (ed-sub bbc
                           (ed-mul gx *k*))))
          
          (when (ed-neutral-point-p bb)
            (signature-mismatch-error)) ;; someone sent us a fishy B point
          
          (multiple-value-bind (aac sc m1)
              (um:nlet iter ()
                (multiple-value-bind (a aa)
                    (ed-random-pair)
                  (let* ((aac (ed-compress-pt aa))
                         (u   (int (hash/256 aac bbc)))
                         (s   (ed-mul bb
                                      (modr
                                       (m+ a (m* u x))))))
                    (if (or (zerop u)
                            (ed-neutral-point-p s))
                        (go-iter)
                      (let* ((sc  (ed-compress-pt s))
                             (m1  (hash/256 aac bbc sc)))
                        (values aac sc m1))
                      ))
                  ))
            (=bind (m2)
                (funcall (intf-srp-ph2-reply intf) =bind-cont aac m1)
              ;;
              ;; Phase 3: receive M2 -- a Hash/256
              ;;
              ;; Compute: Chk2      = H32(A,M1,S), check Chk2 == M2
              ;;          Key-in    = H32(A,S,B) -- A,S,B in compressed form
              ;;          Key-out   = H32(B,S,A) -- A,S,B in compressed form
              ;;          His InitV = H32(M1,S)[0:16)  -- M1 as Hash256, S in compressed form
              ;;          My InitV  = H32(M2,S)[0:16)  -- M2 as Hash256, S in compressed form
              ;;
              ;; Init crypto with (Key-in, His-InitV)      -- input channel
              ;;                  (Key-out, My-InitV)      -- output channel
              ;;                  H32(His-InitV, My-InitV) -- HMAC keying
              ;; For symmetric AES/256/HMAC applied asymmetrically.
              ;;
              (let* ((chk2      (hash/256 aac m1 sc))
                     (key-in    (vec (hash/256 aac sc bbc)))
                     (key-out   (vec (hash/256 bbc sc aac)))
                     (his-initv (vec (hash/256 m1 sc)))
                     (my-initv  (vec (hash/256 m2 sc))))
                
                (unless (hash= chk2 m2)
                  (signature-mismatch-error))
                
                (init-crypto-for-hmac   crypto his-initv my-initv)
                (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                (init-crypto-for-output crypto key-out (subseq my-initv  0 16))
                (init-crypto-for-renegotiation crypto (vec sc))
                (when cont
                  (funcall cont))
                ))
            ))))
    ))

(defmethod server-negotiate-security-ecc ((crypto crypto) intf node-id)
  ;; No second chances - any error shuts down the connection
  ;; immediately.
  ;;
  ;; We start in Phase II on receipt of his node-id.
  (let ((gxc (cadr (get-keying node-id))))
    ;;
    ;; Phase II:
    ;;
    ;; PreComputed: (x, V=x*G) as deterministic key pair, using
    ;;                         seed H32(salt,MAC-addr,ID,PassPhrase))
    ;;
    ;; Compute: (b, b*G) = random key pair on Curve1174
    ;;          k = H32(*ed-r*, *ed-q*)
    ;;          B = k*V + b*G
    ;;
    ;; Write: B in compressed point form
    ;; Hold as secret: V, b
    ;;
    (multiple-value-bind (b bb)
        (ed-random-pair)
      (let ((bbc (ed-compress-pt
                  (ed-add bb
                          (ed-mul gxc *k*)))))
        (=bind (aac m1)
            (funcall (intf-srp-ph2-begin-ecc intf) =bind-cont bbc)
          ;;
          ;; Phase III: Receive A,M1 -- A as compressed point, M1 as Hash/256
          ;;
          ;; Compute: u         = H32(A,B)    -- A,B as compressed points
          ;;          S         = (A + u*V)*b
          ;;          Chk1      = H32(A,B,S), check Chk1 == M1
          ;;          M2        = H32(A,M1,S) -- A,S as compressed points, M1 as Hash/256 
          ;;          Key-in    = H32(B,S,A)  -- A,B,S as compressed points
          ;;          Key-out   = H32(A,S,B)  -- A,B,S as compressed points   
          ;;          His-InitV = H32(M2,S)[0:16)   -- M2 as Hash/256, S as compressed point
          ;;          My-InitV  = H32(M1,S)[0:16)   -- M1 as Hash/256, S as compressed point
          ;;
          ;; Send: M2
          ;; Hold as secret: u,S
          ;;
          ;; Init crypto with (Key-in, His-InitV), -- for input channel
          ;;                  (Key-out, My-InitV), -- for output channel
          ;;                  H32(My-InitV, His-InitV) -- HMAC keying
          ;; For symmetric AES/256/HMAC applied asymmetrically.
          ;;
          
          (when (ed-neutral-point-p aac) ;; someone sent us a fishy A point
            (signature-mismatch-error))
          
          (let* ((u  (int (hash/256 aac bbc)))
                 (s  (ed-mul
                      (ed-add aac
                              (ed-mul gxc u))
                      b)))
            (when (or (zerop u) ;; someone cooked up an A
                      (ed-neutral-point-p s))
              (signature-mismatch-error))
            
            (let* ((sc        (ed-compress-pt s))
                   (chk1      (hash/256 aac bbc sc)))
              (unless (hash= chk1 m1)
                (signature-mismatch-error))
              
              (let* ((m2        (hash/256 aac m1 sc))
                     (key-in    (vec (hash/256 bbc sc aac)))
                     (key-out   (vec (hash/256 aac sc bbc)))
                     (his-initv (vec (hash/256 m2 sc)))
                     (my-initv  (vec (hash/256 m1 sc))))
                
                (funcall (intf-srp-ph3-begin intf) m2
                         (lambda ()
                           (init-crypto-for-hmac   crypto my-initv his-initv)
                           (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                           (init-crypto-for-output crypto key-out (subseq my-initv  0 16))
                           (init-crypto-for-renegotiation crypto (vec sc))
                           ))
                ))))
        ))))
