
(asdf:defsystem "core-crypto-test"
  :depends-on (lisp-unit
               core-crypto)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests
                                  :all :core-crypto-test))
  :components ((:module package
                :pathname "core-crypto-test/"
                :components ((:file "package")))
               (:module tests
                :pathname "core-crypto-test/"
                :depends-on (package)
                :components ((:file "test-modmath")
                             (:file "test-vecrepr")
                             (:file "test-hash")
                             (:file "test-edwards"))
                )))
