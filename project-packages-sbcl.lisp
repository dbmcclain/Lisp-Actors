;; project-packages-sbcl.lisp -- Package mapping for SBCL
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

(defvar *mappings*  (make-hash-table))
(defvar *map-lock*  (sb-thread:make-mutex))
(defvar *bypass-mapping*   nil)

(defun normalize (name)
  (cond ((stringp name)
         (intern (string-upcase name) :keyword))
        ((symbolp name)
         (normalize (symbol-name name)))
        (t
         (error "What!? (~S)" name))
        ))

(defun do-defproject (pairs)
  (sb-thread:with-recursive-lock (*map-lock*)
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
           (sb-thread:with-recursive-lock (*map-lock*)
             (if (and ;; (char= #\= (char norm-name 0))
                      (setf to-name (gethash norm-name *mappings*)))
                 (map-name to-name (cons norm-name froms))
               name))))
        ))

(defun package-mapper (next-fn name)
  (declare (optimize speed))
  (funcall next-fn (map-name name)))

(defun in-quicklisp-p (filename)
  (find "quicklisp" (pathname-directory filename)
        :test #'string=))

(defun loader (next-fn filename &rest args)
  (let ((*bypass-mapping* (in-quicklisp-p filename)))
    (apply next-fn filename args)))

(sb-ext:with-unlocked-packages (:cl)
  (cl-advice:make-advisable 'cl:find-package
                            :arguments '(name)
                            :force-use-arguments t)
  (cl-advice:make-advisable 'cl:in-package
                            :arguments '(name)
                            :force-use-arguments t)
  (cl-advice:make-advisable 'cl:load
                            :arguments '(name &rest args)
                            :force-use-arguments t)
  (cl-advice:make-advisable 'cl:compile-file
                            :arguments '(name &rest args)
                            :force-use-arguments t)
  (cl-advice:add-advice :around 'cl:find-package #'package-mapper)
  (cl-advice:add-advice :around 'cl:in-package   #'package-mapper)
  (cl-advice:add-advice :around 'cl:load         #'loader)
  (cl-advice:add-advice :around 'cl:compile-file #'loader))

;; ------------------------------------------------------

(defun show-mappings ()
  (let (lst)
    (sb-thread:with-recursive-lock (*map-lock*)
      (with-hash-table-iterator (gen *mappings*)
        (loop
           (multiple-value-bind (more? key value) (gen)
             (unless more? (return))
             (push `(,key ,value) lst)))))
    (with-standard-io-syntax
      (pprint (sort lst #'string< :key #'car)))
    (values)))

