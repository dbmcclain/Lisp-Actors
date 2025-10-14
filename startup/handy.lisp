
(in-package #:user)
;; ----------------------------------------------------------------

(defun redir ()
  (cd "/Volumes/My Passport for Mac/projects/Lispworks/"))

(export '(cwd pwd ?action ?packages ?avoid-syms ?specials
              error-if-special
              with-working-directory
              editor-indentation
              ))
        
(defun cwd (path) 
  (let ((*default-pathname-defaults* (get-working-directory)))
    (cd path)))

(defun pwd ()
  (get-working-directory))

(defun ?action ()
  (let ((action (capi:prompt-with-list
                 (mapcar 'first system::*all-action-lists*) 
                 "Choose an Action")))
    (when action
      (print action)
      (print-actions action))
    ))

(defun ?packages ()
  (let ((pkg (capi:prompt-with-list
              (sort (mapcar 'package-name
                            (list-all-packages))
                    'string-lessp)
              "Choose a Package")
             ))
    (when pkg
      (inspect
       (find-package pkg)))
    ))

(defun editor-indentation (&optional sym)
  (let ((tbl (slot-value editor::*default-syntax-table* 'editor::operators)))
    (if sym
        (list sym (gethash (string sym) tbl))
      (let ((keys nil))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (push k keys))
                 tbl)
        (let ((chosen (capi:prompt-with-list
                       (sort keys 'string-lessp)
                       "Choose a Symbol")))
          (when chosen
            (editor-indentation chosen))
          )))))

;; NOTE:
;; (hcl:variable-information '*read-eval*) => (values :special nil nil)
;; (sys:declared-special-p '*read-eval*) => T

(defun ?avoid-syms ()
  (let ((lst nil))
    (do-symbols (s)
      (when (sys:declared-special-p s)
        (push s lst)))
    (capi:prompt-with-list
     (sort lst #'string-lessp)
     "Avoid these Symbols!")))

(defun ?specials (syms)
  ;; give us a list of symbols
  ;; return a list of those which are
  ;; declared special as viewed from current package
  (loop for sym in syms
        when (sys:declared-special-p sym)
        collect sym))

(defmacro error-if-special (&rest syms)
  ;; place (user:error-if-special ...syms...) in your code
  ;; Compiler will issue an error if any of them are SPECIAL bindings
  ;; Most useful with multithreaded code to be sure we don't accidentally
  ;; refer to a special binding instead of a lexical binding.
  (let ((specs (?specials syms)))
    (when specs
      (error "Symbols are SPECIAL: ~A" specs))))

(defun call-with-working-directory (path fn)
  (let ((old-path (get-working-directory)))
    (change-directory path)
    (unwind-protect
        (funcall fn))
    (change-directory old-path)))
  
(defmacro with-working-directory (path &body body)
  `(call-with-working-directory ,path
                                (lambda ()
                                  ,@body)))

