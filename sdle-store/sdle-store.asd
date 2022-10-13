;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-user)

(defpackage #:sdle-store.system
  (:use #:cl #:asdf) ;; ASDF3 explicitly states that it will :use both :asdf and :uiop
  (:export #:non-required-file))

(in-package #:sdle-store.system)

#-(or lispworks mcl cmu clisp sbcl allegro ecl openmcl abcl)
(error "This is an unsupported lisp implementation.
Currently only MCL, OpenMCL, Lispworks, CMUCL, SBCL,
CLISP, ECL and AllegroCL are supported.")

(defclass non-required-file (cl-source-file) ()
  (:documentation
   "File containing implementation dependent code which may or may not be there."))

(defun lisp-system-shortname ()
  #+mcl :mcl #+lispworks :lispworks #+cmu :cmucl #+clisp :clisp #+sbcl :sbcl
  #+allegro :acl #+ecl :ecl #+openmcl :openmcl #+abcl :abcl)

(defmethod component-pathname ((component non-required-file))
  (let ((pathname (call-next-method))
        (name (string-downcase (lisp-system-shortname))))
    (merge-pathnames
     (make-pathname :directory (list :relative name))
     pathname)))

(defmethod perform ((op compile-op) (component non-required-file))
  (when (probe-file (component-pathname component)) 
    (call-next-method)))

(defmethod perform ((op load-op) (component non-required-file))
  (when (probe-file (component-pathname component))
    (call-next-method)))

(defmethod operation-done-p ((o operation) (c non-required-file))
  (when (probe-file (component-pathname c))
    (call-next-method)))

(defsystem "sdle-store"
  :name "SDLE-STORE"
  :author "Sean Ross <sross@common-lisp.net>; hacked by DM/SD 09/08"
  :maintainer "Sean Ross <sross@common-lisp.net>"
  :version "0.9.0"
  :description "Serialization package"
  :long-description "Portable CL Package to serialize data"
  :licence "MIT"
  :in-order-to ((test-op (test-op "sdle-store/tests")))
  :depends-on (com.ral.useful-macros)
  :serial t
  :components ((:file "package")
               (:file "utils")
               #+(or abcl (and clisp (not mop)))
               (:file "mop")
               (:file "backends")
               (:file "plumbing")
               (:file "circularities")
               (:file "default-backend-decls")
               (:file "default-backend")
               (:non-required-file "custom")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sdle-store))))
  (funcall (find-symbol "SETUP-SPECIAL-FLOATS" :sdle-store))
  (provide 'sdle-store))

(defsystem "sdle-store/tests"
  :depends-on (rt sdle-store cl-store)
  :components ((:module tests
                        :pathname "./"
                        :components ((:file "tests")))))
