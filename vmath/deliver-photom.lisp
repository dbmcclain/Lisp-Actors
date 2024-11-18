(change-directory "d:/projects/lispworks/")
(load "defsys.lisp")
(load-system "data-objects")
(load-system "icom")
(load-system "scids")
(load-system "vmath")
;(compile-system "photom" :load t)
(load-system "photom")
(deliver 'phot:photom
	 "photom" 0

         :interface :capi
	 :keep-gc-cursor t

         :keep-foreign-symbols t

         :in-memory-delivery nil
         
	 ;; :quit-when-no-windows t
         ;; :keep-debug-mode :keep-packages
         ;; :packages-to-keep :all
	 )
(quit)
