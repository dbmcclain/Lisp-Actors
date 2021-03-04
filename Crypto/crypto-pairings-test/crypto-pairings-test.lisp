
(in-package :core-crypto-test)

(define-test basis-consistency
  (assert-true (hash:hash-check pbc::*pairing-default-ar160-params* pbc::*chk-pairing-default-ar160-params*))
  (assert-true (hash:hash-check pbc::*pairing-fr256-params-old*     pbc::*chk-pairing-fr256-params-old*))
  (assert-true (hash:hash-check pbc::*pairing-fr256-params*         pbc::*chk-pairing-fr256-params*))
  (assert-true (hash:hash-check pbc::*pairing-fr449-params*         pbc::*chk-pairing-fr449-params*))
  )

(define-test keying
  (let ((k (make-key-pair :test)))
    (assert-true (check-public-key (keying-triple-pkey k)
                                   (keying-triple-sig  k)))))

(define-test child-keying
  (let* ((k (make-key-pair :test))
         (c (bev (hash/256 :another-test))))
    (multiple-value-bind (cskey cchain)
        (ckd-secret-key (keying-triple-skey k) c 1)
      (multiple-value-bind (cpkey cchain2)
          (ckd-public-key (keying-triple-pkey k) c 1)
        (assert-true (int= (public-of-secret cskey) cpkey))
        (assert-true (int= cchain cchain2))))))

(define-test signature
  (let* ((k  (make-key-pair :test))
         (sig (sign-message "this is a test"
                            (keying-triple-pkey k)
                            (keying-triple-skey k))))
    (assert-true (check-message sig))))

(define-test encryption
  (let* ((msg  "This is a test")
         (k    (make-key-pair :test))
         (cmsg (ibe-encrypt msg (keying-triple-pkey k) :test-id))
         (dmsg (ibe-decrypt cmsg (keying-triple-skey k))))
    (assert-true (string= dmsg msg))))

(define-test multisignature
  (let* ((k1  (make-key-pair :test-1))
         (k2  (make-key-pair :test-2))
         (msg "this is a test")
         (sig1 (sign-message msg
                             (keying-triple-pkey k1)
                             (keying-triple-skey k1)))
         (sig2 (sign-message msg
                             (keying-triple-pkey k2)
                             (keying-triple-skey k2)))
         (msig (combine-signatures sig1 sig2)))
    (assert-true (check-message msig))))

(define-test vrf
  (let* ((k    (make-key-pair :test))
        (vrf   (compute-vrf :test-seed (keying-triple-skey k))))
    (assert-true (validate-vrf (vrf-proof vrf) (vrf-y vrf)))
    (assert-true (validate-vrf-mapping :test-seed
                                       (vrf-proof vrf)
                                       (keying-triple-pkey k)
                                       (vrf-y vrf)))))

(define-test pedersen-proof
  (let* ((proof (make-pedersen-proof 15)))
    (assert-true (validate-pedersen-proof proof))))

(define-test cloaked-proof
  (let* ((proof (make-cloaked-proof 15)))
    (assert-true (validate-cloaked-proof proof))))

#|
(define-test confidential-purchase
  (let* ((kb  (make-key-pair :buyer))
         (kv  (make-key-pair :vendor))
         (cost  100)
         (fees   10)
         (paid  200)
         (change 90)
         ;; make a private transaction between kb and kv
         (purch (confidential-purchase paid change
                                       (keying-triple-pkey kb)
                                       (keying-triple-skey kb)
                                       (keying-triple-pkey kv)))
         ;; make a publicly verifiable transaction from kb
         (ppurch (confidential-purchase paid change
                                        (keying-triple-pkey kb)
                                        (keying-triple-skey kb))))
    (assert-true (check-confidential-purchase purch
                                              cost fees
                                              (keying-triple-skey kv)))
    (assert-true (check-confidential-purchase ppurch
                                              cost fees))))
|#



;;;; Base58 Tests

;;; Adapted from https://github.com/bitcoin/bitcoin/blob/master/src/test/data/base58_encode_decode.json

(defparameter *base58-test-inputs-and-expected-outputs*
  '(("" "")
    ("61" "2g")
    ("626262" "a3gV")
    ("636363" "aPEr")
    ("73696d706c792061206c6f6e6720737472696e67" "2cFupjhnEsSn59qHXstmK2ffpLv2")
    ("00eb15231dfceb60925886b67d065299925915aeb172c06647" "1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L")
    ("516b6fcd0f" "ABnLTmg")
    ("bf4f89001e670274dd" "3SEo3LWLoPntC")
    ("572e4794" "3EFU7m")
    ("ecac89cad93923c02321" "EJDM8drfXA6uyA")
    ("10c8511e" "Rt5zm")
    ("00000000000000000000" "1111111111")
    ("000111d38e5fc9071ffcd20b4a763cc9ae4f252bb4e48fd66a835e252ada93ff480d6dd43dc62a641155a5" "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
    ("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff" "1cWB5HCBdLjAuqGGReWE3R3CguuwSjw6RHn39s2yuDRTS5NsBgNiFpWgAnEx6VQi8csexkgYw3mdYrMHr8x9i7aEwP8kZ7vccXWqKDvGv3u1GxFKPuAkn8JCPPGDMf3vMMnbzm6Nh9zh1gcNsMvH3ZNLmP5fSG6DGbbi2tuwMWPthr4boWwCxf7ewSgNQeacyozhKDDQQ1qL5fQFUW52QKUZDZ5fw3KXNQJMcNTcaB723LchjeKun7MuGW5qyCBZYzA1KjofN1gYBV3NqyhQJ3Ns746GNuf9N2pQPmHz4xpnSrrfCvy6TVVz5d4PdrjeshsWQwpZsZGzvbdAdN8MKV5QsBDY")))

(setq lisp-unit:*print-failures* t)     ; <= consider removing or move
                                        ; elsewhere when dust settles?
                                        ; -mhd, 5/29/18

(defun base58-tests ()
  (loop for (in-hex-string expected-out-base58-string)
          in *base58-test-inputs-and-expected-outputs*
        as in-bv = (ironclad:hex-string-to-byte-array in-hex-string)
        as out-base58-string = (vec-repr:base58-str in-bv)
        initially (format t "~%Base58-test~%")
        do (format t "In: ~s~%  => (expect:) ~s~%" 
                   in-hex-string expected-out-base58-string)
           ;; make less verbose after the dust settles! -mhd, 5/29/18
           (unless (equal out-base58-string 
                          expected-out-base58-string)
             (cerror "Continue as usual"
                     "Unexpected results.~%   ~s => ~s~%  Expected: ~s."
                     in-hex-string out-base58-string
                     expected-out-base58-string))
           (assert-equal out-base58-string 
                         expected-out-base58-string)))


(define-test base58-tests
  (base58-tests))
