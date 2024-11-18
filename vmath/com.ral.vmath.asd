
(asdf:defsystem "com.ral.vmath"
  :description "vmath: a system for vectorized math"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "simple-vector-ops")
                (:file "vmath")
                (:file "matrix")
                (:file "lmfit")
                (:file "linfit")
                (:file "kalman")
                (:file "locate")
                (:file "interpolate")
                (:file "monotonic-spline")
                (:file "integrate")
                (:file "roots")
                (:file "fft-tools")
                (:file "kaiser")
                (:file "simplex")
                
                #+:MACOSX
                (:MODULE "Mac"
                 :COMPONENTS ((:file "mac_fft_intf")
                              (:file "fft-structs")
                              (:file "mac-fft"   :depends-on ("mac_fft_intf"))
                              (:file "fft-twids" :depends-on ("mac_fft_intf"))
                              (:file "mac-sfft")
                              (:file "mac-dfft")
                              (:file "mac-fft2d")
                              ))

                #+:win32
                (:MODULE "Win"
                 :components ((:file "win_fft_intf")
                              (:file "win-fft"  :depends-on ("win_fft_intf"))))

                (:file "nr-glue")
                #+:MACOSX (:file "burg")
                #+:MACOSX (:file "dgesvd"))
  :serial t
  :depends-on  (
                ;; "data-objects"
                "com.ral.c-arrays"))

