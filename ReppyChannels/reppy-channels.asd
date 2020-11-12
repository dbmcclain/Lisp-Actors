
(asdf:defsystem "reppy-channels"
  :description "reppy: John Reppy's Channels for Sync & Async Comms"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  (
                ;; (:file "reppy-actors-channel")
                (:file "reppy-channels")
                ;; (:file "reppy-lf")
                (:file "imcell")
                (:file "spm")
                #+:LISPWORKS (:file "topgui")
                )
  :serial t
  :depends-on   ("useful-macros"
                 "actors"
                 "data-objects"
                 "mpcompat"
                 "optima"
                 ))

