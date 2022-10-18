;; project-packages-lw.lisp -- Project package mapping for LispWorks
;;
;; DM/RAL 10/22
;; -----------------------------------------------------------

(defpackage #:com.ral.project-packages
  (:use #:common-lisp)
  (:export
   #:defproject
   #:map-name
   #:show-mappings
   ))

(in-package #:com.ral.project-packages)

(defvar *mappings*  (make-hash-table :test #'string=))
(defvar *map-lock*  (mp:make-lock :sharing t))
(defvar *bypass-mapping*   nil)

(defun normalize (name)
  (cond ((stringp name)
         (string-upcase name))
        ((symbolp name)
         (normalize (symbol-name name)))
        (t
         (error "What!? (~S)" name))
        ))

(defun do-defproject (pairs)
  (mp:with-exclusive-lock (*map-lock*)
    (dolist (pair pairs)
      (destructuring-bind (from-name to-name) pair
        (let ((from-name (normalize from-name)))
          (setf (gethash from-name *mappings*) to-name)
          )))
    ))

(defmacro defproject (&rest pairs)
  `(do-defproject ',pairs))

(defun map-name (name &optional froms)
  (cond ((or *bypass-mapping*
             (packagep name))
         name)
        (t
         (let ((norm-name (normalize name))
               to-name)
           (when (find norm-name froms :test #'string=)
             (error "Cyclic mappong ~A" norm-name))
           (mp:with-sharing-lock (*map-lock*)
             (if (and ;; (char= #\= (char norm-name 0))
                      (setf to-name (gethash norm-name *mappings*)))
                 (map-name to-name (cons norm-name froms))
               name))))
        ))

(lw:defadvice (cl:find-package project-packages :around)
    (name/package)
    (declare (optimize speed))          ;this is critical code
    (lw:call-next-advice
     (map-name name/package)) )

(lw:defadvice (sys::find-package-without-lod project-packages :around)
    (name)
  ;; used by editor to set buffer package
  (declare (optimize speed))
  ;; (format t "find-package-without-lod: ~S" (editor:variable-value 'editor::current-package) )
  (lw:call-next-advice (map-name name)))

(lw:defadvice (sys::find-global-package project-packages :around)
    (name)
  ;; used by editor to set buffer package
  (declare (optimize speed))
  ;; (format t "find-package-without-lod: ~S" (editor:variable-value 'editor::current-package) )
  (lw:call-next-advice (map-name name)))

(lw:defadvice (sys::%in-package project-packages :around)
    (name &rest args)
  (declare (optimize speed))
  (apply #'lw:call-next-advice (map-name name) args))

(defmethod in-quicklisp-p (filename)
  (find "quicklisp" (pathname-directory filename)
        :test #'string=))

(defmethod in-quicklisp-p ((stream stream))
  nil)

(lw:defadvice (load project-packages :around)
    (filename &rest args)
  (let ((*bypass-mapping* (in-quicklisp-p filename)))
    (apply #'lw:call-next-advice filename args)))

(lw:defadvice (compile-file project-packages :around)
    (filename &rest args)
  (let ((*bypass-mapping* (in-quicklisp-p filename)))
    (apply #'lw:call-next-advice filename args)))

;; ------------------------------------------------------

(defun show-mappings ()
  (let (lst)
    (mp:with-sharing-lock (*map-lock*)
      (with-hash-table-iterator (gen *mappings*)
        (loop
           (multiple-value-bind (more? key value) (gen)
             (unless more? (return))
             (push `(,key ,value) lst)))))
    (with-standard-io-syntax
      (pprint (sort lst #'string< :key #'car)))
    (values)))

