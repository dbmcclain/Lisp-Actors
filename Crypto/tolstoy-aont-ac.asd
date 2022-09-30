
(asdf:defsystem "tolstoy-aont-ac"
  :description "tolstory-aont-ac: all or nothing encode / decode (Actors-based)"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "mimic")
                (:file "aont-messaging-ac"))
  :serial       t
  :depends-on   ("actors"
                 "aont"
                 ))

