
(asdf:defsystem "com.ral.scigraph"
  :description "scigraph: a graphical plotting system"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components (
               #+:WIN32
               (:MODULE "Win"
                :COMPONENTS ((:file "win_scigraph_intf")
                             (:file "win-scigraph"
                              :depends-on ("win_scigraph_intf"))))
               #+:MACOSX
               (:MODULE "Mac"
                :COMPONENTS ((:file "mac-scigraph-macros")
                             (:file "mac_scigraph_intf"
                              :depends-on ("mac-scigraph-macros"))
                             (:file "mac-scigraph"
                              :depends-on ("mac_scigraph_intf"))))
               
               (:file "surfplot")
               (:file "images"))
  :depends-on  ("com.ral.vmath"
                #+:MACOSX "com.ral.aquaterm"))
