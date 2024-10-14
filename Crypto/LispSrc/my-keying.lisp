(in-package :edec-mm)

;; -----------------------------------------------
;; this part would be replaced by node state info

(defvar *my-pkey* #S(ECC-PT :X 2149324148663821067438903471695820159629583774816876923518478901568591623108.
                            :Y 1876947084269847797465766041362586077620995675266084905736781013606318774630.))


(define-symbol-macro *my-skey* (multiple-value-call #'skey (get-shares)))

