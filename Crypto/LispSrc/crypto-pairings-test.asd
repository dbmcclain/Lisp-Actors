
(asdf:defsystem "crypto-pairings-test"
  :depends-on (lisp-unit
               core-crypto)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-test
                                  :all :crypto-pairings-test))
  :components ((:module package
                :pathname "crypto-pairings-test/"
                :components ((:file "package")))
               (:module tests
                :pathname "crypto-pairings-test/"
                :depends-on (package)
                :components ((:file "crypto-pairings-test"))
                )))
