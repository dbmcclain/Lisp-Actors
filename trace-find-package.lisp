;; trace-find-package.lisp -- Trace FIND-PACKAGE and record who uses
;; what package refefences?
;;
;; Try to adhere to basic Common Lisp so that this can be loaded
;; as early as possible.
;;
;; DM/RAL  10/22
;; -------------------------------------------------------------------------

(defpackage #:com.ral.trace-find-package
  (:use #:common-lisp)
  (:export
   #:*traces*
   #:*resided-in-key*
   #:*visited-key*
   #:show-traces
   #:who-uses
   ))

(in-package #:com.ral.trace-find-package)

;; -------------------------------------------------------------------------

(defvar *traces*         (make-hash-table :test 'string=))
(defvar *trace-lock*     
    #+:LISPWORKS (mp:make-lock)
  #+:ALLEGRO (mp:make-process-lock))
(defvar *resided-in-key* (string '{99d34928-4b2d-11ed-88b3-787b8acbe32e}))
(defvar *visited-key*    (string '{a365158e-4b2d-11ed-88b3-787b8acbe32e}))

(defun normalize (ref)
  (cond ((stringp ref)
	 (string-upcase ref))
	((symbolp ref)
	 (normalize (symbol-name ref)))
	((packagep ref)
	 (normalize (package-name ref)))
	(t
	 (error "What!? (~S)" ref))
	))

(defun lookup (key lst)
  (find key lst :test #'string=))

(defun trace-find-package (pkg)
  (let ((current-package (normalize *package*))
        (ref-package     (normalize pkg)))
    (#+:LISPWORKS mp:with-lock #+:ALLEGRO mp:with-process-lock (*trace-lock*)
      (let ((resided-in (gethash *resided-in-key* *traces*))
            (visited-in (gethash *visited-key* *traces*)))
        (unless (lookup current-package resided-in)
          (setf (gethash *resided-in-key* *traces*) (cons current-package resided-in)))
        (unless (lookup ref-package visited-in)
          (setf (gethash *visited-key* *traces*) (cons ref-package visited-in)))
        (unless (lookup ref-package '("CL" "COMMON-LISP" "KEYWORD"))
          (let ((refs (gethash current-package *traces*)))
            (unless (lookup ref-package refs)
              (setf (gethash current-package *traces*) (cons ref-package refs)))
            ))
        ))
    ))

#+:LISPWORKS
(lw:defadvice (find-package find-package-tracker :after)
    (name)
  (declare (optimize speed))
  (trace-find-package name))

#+:LISPWORKS
(lw:defadvice (sys::find-package-without-lod project-packages :after)
    (name)
  ;; used by editor to set buffer package
  (declare (optimize speed))
  ;; (format t "find-package-without-lod: ~S" (editor:variable-value 'editor::current-package) )
  (trace-find-package name))

;; -------------------------------------------------------------------------

(defun translate-key (key)
  (if (string= key *resided-in-key*)
      "All Host Packages"
    (if (string= key *visited-key*)
        "All Packages Visited"
      key)))

(defun show-traces ()
  (let (lst)
    (#+:LISPWORKS mp:with-lock #+:ALLEGRO mp:with-process-lock (*trace-lock*)
      (with-hash-table-iterator (gen *traces*)
        (loop
           (multiple-value-bind (more? key value) (gen)
             (unless more? (return))
             (let ((vals (sort value #'string<))
                   (key  (translate-key key)))
               (push `(:from ,key :to ,@vals) lst))))))
    (with-standard-io-syntax
      (pprint (sort lst #'string< :key #'cadr)))
    (values)))

(defun who-uses (pkg)
  (let ((pkg  (normalize pkg))
        lst)
    (#+:LISPWORKS mp:with-lock #+:ALLEGRO mp:with-process-lock (*trace-lock*)
      (with-hash-table-iterator (gen *traces*)
        (loop
           (multiple-value-bind (more? key value) (gen)
             (unless more? (return))
             (unless (lookup key (list pkg *visited-key* *resided-in-key*))
               (when (find pkg value :test #'string=)
                 (push key lst)))
             ))))
    (sort lst #'string<)))
