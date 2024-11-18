;; To use this file, get out of LispWorks environment and enter the
;; following command on a shell command line (Bash):
;;   rawlisp -init "/projects/lispworks/vmath/saver.lisp"
;;
(change-directory "/projects/lispworks/")
(load "defsys.lisp")

(load-system "scids")        ;; SciDS data files
(load-system "vmath")        ;; vectorized math
(load-system "data-objects") ;; reppy channels
(load "tools/lazy")          ;; lazy eval

(compile-system "photom" :load t)
(save-image "photom-image"
            :restart-function 'phot:photom
	    :console :input
	    :environment t
	    )
(quit)
