(in-package :core-crypto-test)

(define-test base58 ()
  (let ((iterations 100))
    (loop
       :repeat iterations
       :doing (let* ((val (random (ash 1 256)))
                     (b58  (vec-repr:base58-str val))
                     (b582 (vec-repr:base58-str val)))
                (assert-true (string= b58 b582))))))

;;;; Base58 Tests

;;; Adapted from https://github.com/bitcoin/bitcoin/blob/master/src/test/data/base58_encode_decode.json

(defparameter *base58-test-inputs-and-expected-outputs*
  '(("" "1")
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
    ("00000000000000000000" "11111111111")
    ("000111d38e5fc9071ffcd20b4a763cc9ae4f252bb4e48fd66a835e252ada93ff480d6dd43dc62a641155a5" "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
    ("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff" "1cWB5HCBdLjAuqGGReWE3R3CguuwSjw6RHn39s2yuDRTS5NsBgNiFpWgAnEx6VQi8csexkgYw3mdYrMHr8x9i7aEwP8kZ7vccXWqKDvGv3u1GxFKPuAkn8JCPPGDMf3vMMnbzm6Nh9zh1gcNsMvH3ZNLmP5fSG6DGbbi2tuwMWPthr4boWwCxf7ewSgNQeacyozhKDDQQ1qL5fQFUW52QKUZDZ5fw3KXNQJMcNTcaB723LchjeKun7MuGW5qyCBZYzA1KjofN1gYBV3NqyhQJ3Ns746GNuf9N2pQPmHz4xpnSrrfCvy6TVVz5d4PdrjeshsWQwpZsZGzvbdAdN8MKV5QsBDY")))

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


