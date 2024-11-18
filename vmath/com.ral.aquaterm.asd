
#+:MAC
(asdf:defsystem "com.ral.aquaterm"
  :description "aquaterm: an interface to the Aquaterm graphic display terminal"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "aquaterm-macros")
                (:file "aquaterm-dff" :depends-on ("aquaterm-macros")))
  :depends-on  (;; "data-objects"
                "com.ral.regex"))

