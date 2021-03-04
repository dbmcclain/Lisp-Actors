
(in-package :core-crypto-test)

(define-test hashes
  (assert-true (and
                (string-equal
                 (hex-str (hash/256 ""))
                 "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a")
                (string-equal
                 (hex-str (hash/384 ""))
                 "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004")
                (string-equal
                 (hex-str (hash/512 ""))
                 "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26")
                )))

#|
(lisp-unit:run-tests :all :crypto/test)
|#
