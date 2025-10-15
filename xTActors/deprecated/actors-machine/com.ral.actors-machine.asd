
(asdf:defsystem "com.ral.actors-machine"
  :description "An Emulator for a true Actors Machine"
  :version     "3.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2021-2022 by Refined Audiometrics Laboratory. MIT License terms apply."
  :components  ((:file "packages")
                (:file "actors-machine"))
  :SERIAL T
  :depends-on   ("com.ral.useful-macros"))


#|
(asdf :doctools)
(doctools:gen-docs
 :asdf-system-name :com.ral.actors-machine
 :package-name     :com.ral.actors-machine
 :directory        (translate-logical-pathname "PROJECTS:LISP;xTActors;actors-machine")
 :subtitle         "An Emulator for a true Actors Machine")
|#
