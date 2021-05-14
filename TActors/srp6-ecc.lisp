;; srp6-ecc.lisp -- SRP-6a Protocol in ECC
;;
;; DM/RAL 03/21
;; ------------------------------------------------

(defpackage #:actors/srp6-ecc
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
   #:ed-mul
   #:ed-pt=)
  (:import-from #:usec
   #:get-time-usec)
  (:import-from #:actors/security
   #:$VERSION
   #:signature-mismatch-error
   #:crypto
   #:init-crypto-for-hmac
   #:init-crypto-for-input
   #:init-crypto-for-output
   #:init-crypto-for-renegotiation)
  (:export
   #:client-negotiate-security-ecc
   #:server-negotiate-security-ecc
   ))

(in-package #:actors/srp6-ecc)

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
                111359185718774473234263171213664782667961806995809606040329125855375068237677
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 3048372172690304076654247831317757393464732363531197621440925794873999139018)))
  (add-member '("Rincon.local"
                97308094470404514392702034881350532955848808853024587654007127531464649675822
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 675674428011803626943160706574540523843302588141853758067300457361484064080)))
  (add-member '("Arroyo.local"
                77520660937938258103724384941954485037525808631321044717521707989795738576118
                #S(EDWARDS-ECC::ECC-CMPR-PT
                   :CX 4035517786989866824095025259200618926751666752792981150207975910989323175725))))

#|
(let ((*print-readably* t))
  (let ((salt (int (hash/256 (usec:get-time-usec))))
        (mach (machine-instance)))
    (multiple-value-bind (x gx)
        (gen-info mach salt)
      (pprint (list mach salt (ed-compress-pt gx))))))

(let ((*print-readably* t))
  (let ((salt (int (hash/256 (usec:get-time-usec))))
        (mach "RAMBO"))
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

;; Client Side initiates
;; :request-srp-negotiation -> intf -> :request-srp-negotiation @cust node-id -> socket
;; @cust <-- @rcust bbc
;; @cust aac m1 -> @rcust
;; @cust <-- m2
;; :srp-done --> intf

;; Server Side responds
;; 

(defmethod client-negotiate-security-ecc ((crypto crypto) intf cust)
  ;; No second chances - any error shuts down the connection
  ;; immediately.
  (let* ((node-id (machine-instance))
         (salt    (car (get-keying node-id)))) ;; salt as int
    ;;
    ;; Phase-I: send local node ID
    ;;
    (β (@rcust bbc)
        (send intf :request-srp-negotiation β node-id)
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
            (β (m2)
                (send intf :sec-send @rcust β aac m1)

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
                (send intf :srp-done)
                (send cust)))
            ))))
    ))

(defmethod server-negotiate-security-ecc ((crypto crypto) intf @rcust node-id)
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
        (β (@rcust aac m1)
            (send intf :sec-send @rcust β bbc)
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
                
                (β ()
                    (send intf :srp-ph3-begin @rcust β m2)
                  (init-crypto-for-hmac   crypto my-initv his-initv)
                  (init-crypto-for-input  crypto key-in  (subseq his-initv 0 16))
                  (init-crypto-for-output crypto key-out (subseq my-initv  0 16))
                  (init-crypto-for-renegotiation crypto (vec sc))
                  (send intf :srp-done))
                ))))
        ))))
