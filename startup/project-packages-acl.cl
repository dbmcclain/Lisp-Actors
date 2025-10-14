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

(defvar *mappings*  (make-hash-table :test 'string=))
(defvar *map-lock*  (mp:make-process-lock))
(defvar *bypass-mapping*   nil)

(defun normalize (name)
  (declare (optimize speed))          ;this is critical code
  (cond ((stringp name)
         (string-upcase name))
        ((symbolp name)
         (normalize (symbol-name name)))
        (t
         (error "What!? (~S)" name))
        ))

(defun do-defproject (pairs)
  (mp:with-process-lock (*map-lock*)
    (dolist (pair pairs)
      (destructuring-bind (from-name to-name) pair
        (let ((from-name (normalize from-name)))
          (setf (gethash from-name *mappings*) to-name)
          )))
    ))

(defmacro defproject (&rest pairs)
  `(do-defproject ',pairs))

(defun map-name (name &optional froms)
  (declare (optimize speed))          ;this is critical code
  (cond ((or *bypass-mapping*
             (packagep name))
         name)
        (t
         (let ((norm-name (normalize name))
               to-name)
           (when (find norm-name froms :test #'string=)
             (error "Cyclic mappong ~A" norm-name))
           (mp:with-process-lock (*map-lock*)
             (if (setf to-name (gethash norm-name *mappings*))
                 (map-name to-name (cons norm-name froms))
               name))))
        ))

;; ------------------------------------------------

(excl:def-fwrapper wrapped-find-package (name/package)
  (declare (optimize speed))          ;this is critical code
  (setf name/package (map-name name/package))
    (excl:call-next-fwrapper))

(excl:fwrap 'find-package 'wfp1 'wrapped-find-package)

(excl:def-fwrapper wrapped-package-name-to-package (name &rest args)
  (declare (ignore args))
  (setf name (map-name name))
  (excl:call-next-fwrapper))

(excl:fwrap 'excl::package-name-to-package 'wpntp1 'wrapped-package-name-to-package)

#|
(lw:defadvice (sys::find-package-without-lod project-packages :around)
    (name)
  ;; used by editor to set buffer package
  (declare (optimize speed))
  ;; (format t "find-package-without-lod: ~S" (editor:variable-value 'editor::current-package) )
  (lw:call-next-advice (map-name name)))
|#
#||#
#+:LISPWORKS
(lw:defadvice (sys::find-global-package project-packages :around)
    (name)
  ;; used by editor to set buffer package
  (declare (optimize speed))
  ;; (format t "find-package-without-lod: ~S" (editor:variable-value 'editor::current-package) )
  (lw:call-next-advice (map-name name)))
#||#

;; ------------------------------------------------

#|
(lw:defadvice (sys::%in-package project-packages :around)
    (name &rest args)
  (declare (optimize speed))
  (apply #'lw:call-next-advice (map-name name) args))
|#

(excl:def-fwrapper wrapped-in-package (&rest args)
  (setf (cadar args) (map-name (cadar args)))
  (excl:call-next-fwrapper))

(excl:fwrap 'in-package 'wip1 'wrapped-in-package)

(excl:def-fwrapper wrapped-use-package (pkgs &rest args)
  (declare (ignore args))
  (setf pkgs (if (listp pkgs)
                 (mapcar #'map-name pkgs)
               (map-name pkgs)))
  (excl:call-next-fwrapper))

(excl:fwrap 'use-package 'wup1 'wrapped-use-package)

;; ------------------------------------------------

(defmethod in-quicklisp-p (filename)
  (find "quicklisp" (pathname-directory filename)
        :test #'string=))

(defmethod in-quicklisp-p ((stream stream))
  nil)

(excl:def-fwrapper wrapped-load (filename &rest args)
  (declare (ignore args))
  (let ((*bypass-mapping* (in-quicklisp-p filename)))
    (excl:call-next-fwrapper)))

(excl:fwrap 'load 'wld1 'wrapped-load)

(excl:def-fwrapper wrapped-compile-file (filename &rest args)
  (declare (ignore args))
  (let ((*bypass-mapping* (in-quicklisp-p filename)))
    (excl:call-next-fwrapper)))

(excl:fwrap 'compile-file 'wcf1 'wrapped-compile-file)

;; ------------------------------------------------------

(defun show-mappings ()
  (let (lst)
    (mp:with-process-lock (*map-lock*)
      (with-hash-table-iterator (gen *mappings*)
        (loop
           (multiple-value-bind (more? key value) (gen)
             (unless more? (return))
             (push `(,key ,value) lst)))))
    (with-standard-io-syntax
      (pprint (sort lst #'string< :key #'car)))
    (values)))

#|
(show-mappings)
 |#
