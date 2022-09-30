
(asdf:defsystem "tolstoy-aont"
  :description "tolstory-aont: all or nothing encode / decode"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "mimic")
                (:file "aont-messaging"))
  :serial       t
  :depends-on   ("aont"
                 ))

