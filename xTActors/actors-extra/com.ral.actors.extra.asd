(asdf:defsystem "com.ral.actors.extra"
  :description "Everything is an Actor..."
  :version     "3.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2021-2022 by Refined Audiometrics Laboratory. MIT License terms apply."
  :components  ((:file "debugging")
                (:file "transactional-db")
                (:file "reactive")
                (:file "resource")
                ;; (:file "sponsors")
                )
  :SERIAL T
  :depends-on   ("com.ral.actors"
                 "com.ral.rb-trees"              ;; maps for transactional db
                 "com.ral.lisp-object-encoder"   ;; encoding for transactional db
                 ))

#|
(asdf :doctools)
(doctools:gen-docs
 :asdf-system-name :com.ral.actors.extra
 :package-name     :com.ral.actors
 :directory        (translate-logical-pathname "PROJECTS:LISP;xTActors;actors-extra")
 :subtitle         "Extra Goodies for Hewitt Actors in Lisp")
|#
