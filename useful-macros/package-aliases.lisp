;; package-aliases.lisp -- Package aliases and hierarchical package names
;;
;; DM/SD  04/11
;; --------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package #:useful-macros)

(defun true-package-name (pkg/name)
  (package-name pkg/name))

(export 'true-package-name)

;; --------------------------------------------------------
#|
(defvar *package-aliases* nil)
(defvar *avoid-alias-processing* nil) ;; used locally to control lookup during aliasing

(defun alias-package (from-name to-name)
  (let ((from-name (string from-name))
        (to-name   (and to-name
                        (if (packagep to-name)
                            (true-package-name to-name)
                          (string to-name))) ))
    (if (and to-name
             (string/= from-name to-name))
        (let ((nicks nil)
              (*avoid-alias-processing* t))
          ;; if package already exists, rename it and strip out
          ;; nicknames
          (let ((pkg (find-package from-name)))
            (when pkg
              ;; original package name becomes a new side-channel nickname too.
              (setf nicks (cons (true-package-name pkg) (package-nicknames pkg)))
              (rename-package pkg to-name))) ;; leave out nicknames
          (dolist (name (cons from-name nicks))
            (setf (sys:cdr-assoc (string name)
                                 *package-aliases*
                                 :test 'string=)
                  to-name)))
      
      ;; else -- to-name = from-name, or to-nome = NIL
      ;; just remove the alias
      (let ((pair (assoc from-name *package-aliases*
                         :test 'string=)))
        (setf *package-aliases* (delete pair *package-aliases*))) )))

(defun lookup-package-alias (key)
  (declare (optimize speed))
  (if (packagep key)
      key
    ;; else
    (or
     (sys:cdr-assoc (string key) *package-aliases*
                    :test 'string=)
     key)))

(defun delete-package-alias (name)
  (alias-package name nil))

(defun delete-associated-aliases (to-name)
  (loop for pair = (rassoc to-name *package-aliases*
                           :test 'string=)
        while pair
        do
        (setf *package-aliases* (delete pair *package-aliases*)) ))

(defun alias-p (name)
  (assoc (string name) *package-aliases*
         :test 'string=))

;; --------------------------------------------------------
;; package-name

(defun true-package-name (pkg/name)
  (let* ((pkg (find-package pkg/name))
         (*avoid-alias-processing* t))
    (package-name pkg)))

(export 'true-package-name)

(lw:defadvice (cl:package-name :handle-package-aliases :around)
    (pkg/name)
  (let ((real-name (lw:call-next-advice pkg/name)))
    (if *avoid-alias-processing*
        real-name
      ;; else
      (or (car (rassoc real-name *package-aliases*
                       :test 'string=))
          real-name))))

(lw:defadvice (sys::package-prompt :handle-package-aliases :around)
    (package)
  (or (car (rassoc (true-package-name package) *package-aliases*
                   :test 'string=))
      (lw:call-next-advice package)))

;; --------------------------------------------------------
;; find-package

(lw:defadvice (cl:find-package :handle-package-aliases :around)
    (name/package)
  (declare (optimize speed))          ;this is critical code
  (lw:call-next-advice
   (if *avoid-alias-processing*
       name/package
     (lookup-package-alias name/package))))

;; -------------------------------------------------------
;; sys::find-package-without-lod -- used by LW tools

(lw:defadvice (sys::find-package-without-lod :handle-package-aliases :around)
    (name)
  ;; used by editor to set buffer package
  (declare (optimize speed))
  (lw:call-next-advice
   (if *avoid-alias-processing*
       name
     (lookup-package-alias name))))

;; ------------------------------------------------------------
;; delete-package

(lw:defadvice (cl:delete-package :handle-package-aliases :around)
    (name)
  (let ((pkg (find-package name)))
    (if pkg
        (let* ((real-name (true-package-name pkg))
               (ans       (lw:call-next-advice pkg)))
          (delete-associated-aliases real-name)
          ans)
	;; else
	(lw:call-next-advice name))))

;; --------------------------------------------------------
;; rename-package

(lw:defadvice (cl:rename-package :handle-package-aliases :around)
    (pkg new-name &rest args)
    (declare (optimize speed))          ;this is critical code
    (let ((xpkg (find-package pkg)))
      (if xpkg
          (let ((prev-name  (true-package-name xpkg))
                (ans (apply #'lw:call-next-advice xpkg new-name args)))
            (unless (string= (string prev-name) (string new-name))
              (loop for pair = (rassoc prev-name *package-aliases*
                                       :test 'string=)
                    while pair
                    do
                    (setf (cdr pair) (string new-name))))
            ans))
      ;; else
      (apply #'lw:call-next-advice pkg new-name args)))

;; --------------------------------------------------------
;; make-package

(lw:defadvice (cl:make-package :handle-package-aliases :around)
    (name &rest args)
  (if (alias-p name)
      (error "Package would shadow a package alias")
    (apply #'lw:call-next-advice name args)))

;; --------------------------------------------------------
;; in-package

(lw:defadvice (cl:in-package :handle-package-aliases :around)
    (form env)
  (lw:call-next-advice `(,(car form)
                         ,(lookup-package-alias (cadr form)))
                       env))

;; --------------------------------------------------------
;; defpackage

(lw:defadvice (cl:defpackage :handle-package-aliases :around)
    (form env)
  (let* ((op          (car form))
         (name        (string (cadr form)))
         (real-name   (lookup-package-alias name))
         (alias-p     (not (eq name real-name)))
         (args        (cddr form))
         (nicks       (find :nicknames args
                            :key 'car))
         (modargs     (if alias-p
                          (remove :nicknames args
                                  :key 'car)
                        args))
         (ans  (lw:call-next-advice `(,op ,real-name ,@modargs)
                                    env)))
    (when alias-p
      ;; The intent here is that when a package is aliased, we are
      ;; doing so because we want to avoid package name clashes in
      ;; some foreign Lisp session.
      ;;
      ;; The real name is generally chosen as a UUID, which is highly
      ;; unlikely to collide with another package name in the foreign
      ;; system.
      ;;
      ;; Hence we can't permit any extant nicknames in the real
      ;; package either. But we provide them as additional aliases for
      ;; our own development convenience.
      (dolist (nick (cdr nicks))
        (alias-package nick real-name)))
    
    ans))

;; ------------------------------------------------------------

(defun remove-package-alias-system ()
  (dolist (sym '(cl:find-package
		 sys::find-package-without-lod
                 cl:delete-package
                 cl:rename-package
                 cl:make-package
                 cl:in-package
                 cl:defpackage))
    (lw:remove-advice sym ':handle-package-aliases))
  (let ((fn (lambda (&rest args)
              (declare (ignore args))
              (error "Package-alias system deactivated"))))
    (setf *package-aliases* nil
          (symbol-function 'alias-package) fn
          (symbol-function 'delete-package-alias) fn)
    (dolist (sym '(*package-aliases*
                   alias-package
                   delete-package-alias
                   delete-associated-aliases
                   lookup-package-alias
                   remove-package-alias-system))
      (unintern sym)) ))
          
;; --------------------------------------------------------

(export '(alias-package
          delete-package-alias
          remove-package-alias-system))


#||#
;; --------------------------------------------------------
#|
(alias-package :bfmsg :{284B6F36-5E56-11E0-9A81-0017F2CCD25E})
|#
|#

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


