
(in-package #:packrat-readers)

(lw:defadvice (defrule non-inherited-sym :before)
    (call-form env)
  ;; Ensure that all of our rules have names using symbols that have
  ;; been locally interned and not inherited from other packages.
  ;;
  ;; We assume that as long as their symbol names were derived in the
  ;; current package, then they are unique internal symbols and will
  ;; not be shared for redefinition with any other package that might
  ;; also be using ESRAP.
  ;;
  ;; Hence the names "PR-NUMBER", "PR-REAL", and "PR-COMPLEX", since
  ;; NUMBER, REAL, and COMPLEX are inherited from Common Lisp.
  ;;
  (declare (ignore env))
  (let ((name (second call-form)))
    (unless (eql (symbol-package name) *package*)
      (error "Rule definitions should have non-inherited symbol names: ~A:~A" 
             (package-name (symbol-package name)) (symbol-name name)))
    ))
           

