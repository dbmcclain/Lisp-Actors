;; (ql:quickload :emotiq)
(ql:quickload :alexandria)

(in-package :cl-user)

(defun parse-systems-into (file) 
  (let* ((all-tests.sexp
          (asdf:system-relative-pathname :core-crypto "./etc/all-tests.sexp"))
         (s (alexandria:read-file-into-string all-tests.sexp))
         (systems (read-from-string s)))
    (with-open-file (o file :direction :output :if-exists :supersede)
      (format o "~{~s ~}" systems))))

