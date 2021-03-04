(defsystem "core-crypto-test"
  :depends-on (core-crypto
               lisp-unit)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :core-crypto-test))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module tests
                :depends-on (package)
                :pathname "./"
                :components ((:file "hash")
                             (:file "test-modmath")
                             (:file "test-vecrepr")
                             (:file "test-hash")
                             (:file "test-edwards")
                             (:file "crypto-pairings-test"))
                )))


                       
