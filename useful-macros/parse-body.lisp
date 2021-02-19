
(in-package :useful-macros)

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
     (setf current (car body))
     (when (and documentation (stringp current) (cdr body))
       (if doc
           (error "Too many documentation strings in ~S." (or whole body))
         (push (pop body) doc)) ;; Alexandira fails here...
       (go :declarations))
     (when (and (listp current) (eql (first current) 'declare))
       (push (pop body) decls)
       (go :declarations)))
    (values body (nreverse decls) doc)))

