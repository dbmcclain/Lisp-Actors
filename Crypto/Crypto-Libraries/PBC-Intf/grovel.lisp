(ql:quickload :cffi-grovel)
(defparameter *pwd* (make-pathname :name nil :type nil :defaults *load-pathname*))
(defpackage :hoge.ffi)
(with-open-file (out (merge-pathnames "tmp.lisp" *pwd*)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
  (format out "~{~S~%~}" '((in-package :hoge.ffi)
                           (define "____hoge___" "sizeof(int)")
                           (constant (+what+ "____hoge___")))))
(load (cffi-grovel:process-grovel-file (merge-pathnames "tmp.lisp" *pwd*)))
