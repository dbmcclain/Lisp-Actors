
(in-package #:user)

;; --------------------------------------------------
;; Qi Support...

(defmacro with-readtable-case (ccase &body body)
  `(call-with-readtable-case ,ccase (lambda () ,@body)))

(defun call-with-readtable-case (ccase fn)
  (let ((old-case (shiftf (readtable-case *readtable*) ccase)))
    (unwind-protect
        (funcall fn)
      (setf (readtable-case *readtable*) old-case))))

(defun asdfpc (lib)
  "Shortcut for ASDF with :PRESERVE readtable-case"
  (with-readtable-case :PRESERVE
                       (asdf:oos 'asdf:load-op lib)))

(defvar *qi-readtable* (with-readtable-case :PRESERVE
                                            (copy-readtable *readtable*)))

(defvar *lisp-readtable* (copy-readtable *readtable*))

(defun run-qi ()
  (cd (translate-logical-pathname "PROJECTS:LISP;"))
  (with-readtable-case :PRESERVE ;; let ((*readtable* *qi-readtable*))
                       (load "./QiII1.05/Lisp/run_lispworks")))

(dolist (ftype '(".fasl" ".ofasl" ".xfasl" ".64xfasl"))
  (pushnew ftype editor:*ignorable-file-suffices* :test 'string-equal))

(editor:bind-key "Overwrite Mode" "Ctrl-Help" :global :emacs)

