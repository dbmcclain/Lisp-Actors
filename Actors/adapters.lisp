
(in-package :actors)

;; For global binidings that are never dynamically rebound
#+:LISPWORKS
(defmacro defglobal-var (name &rest args)
  `(hcl:defglobal-variable ,name ,@args))
#-:LISPWORKS
(defmacro defglobal-var (name &rest args)
  `(defvar ,name ,@args))
