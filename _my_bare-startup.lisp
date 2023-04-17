;; -*- Mode: Lisp; coding: UTF-8 -*-

(in-package "USER")

(pushnew :asdf-unicode *features*)
#+:LISPWORKS
(progn
  (pushnew :LATIN-1 system:*specific-valid-file-encodings*)
  (pushnew :utf-8 system:*specific-valid-file-encodings*)

  (lw:set-default-character-element-type 'cl:character))

(setf *read-default-float-format* 'double-float)

#+:LISPWORKS
(load-all-patches)

#|
(let ((prjdir "/Volumes/My Passport for Mac/projects"))
  (setf (environment-variable "PROJECTS")
	(if (probe-file prjdir)
	    prjdir
	    #P"~/projects")))
|#

(pushnew :COM.SD  *features*)
(pushnew :COM.RAL *features*)
#+:LISPWORKS
(compile-file-if-needed "startup/dongle" :load t)
#+:ALLEGRO
(progn
  (compile-file-if-needed "startup/dongle")
  (load "startup/dongle"))

(require "asdf")

#+:MACOSX
(asdf:initialize-source-registry
 `(:SOURCE-REGISTRY
   (:EXCLUDE "dbm-git-repo")
   ;; (:TREE #P"~/projects/Emotiq/src/")
   (:TREE #P"~/projects/Lispworks/")
   ;; (:TREE #P"~/Documents/GitHub/")
   ;; (:TREE #P"/usr/local/lisp/source/")
   :IGNORE-INHERITED-CONFIGURATION))

#+:MSWINDOWS
(asdf:initialize-source-registry
 `(:SOURCE-REGISTRY
   (:EXCLUDE "dbm-git-repo")
   ;; (:TREE #P"y:/projects/Emotiq/src/")
   (:TREE #P"y:/projects/Lispworks/")
   ;; (:TREE #P"y:/Documents/GitHub/")
   ;; (:TREE #P"/usr/local/lisp/source/")
   :IGNORE-INHERITED-CONFIGURATION))

#+:LINUX
(asdf:initialize-source-registry
 `(:SOURCE-REGISTRY
   (:EXCLUDE "dbm-git-repo")
   ;; (:TREE #P"/media/psf/Home/projects/Emotiq/src/")
   (:TREE #P"/media/psf/Home/projects/Lispworks/")
   ;; (:TREE #P"/usr/local/lisp/source/")
   :IGNORE-INHERITED-CONFIGURATION))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init #-:MSWINDOWS (merge-pathnames "quicklisp/setup.lisp" 
                                                    (user-homedir-pathname))
                      #+:MSWINDOWS "y:/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun asdf (lib &rest args &key &allow-other-keys)
  "Shortcut for ASDF."
  (apply 'asdf:oos 'asdf:load-op lib args))

;; (push "~/quicklisp/dists/emotiq/software" ql:*local-project-directories*)
;; (ql:quickload :asdf)
