;; -*- Mode: Lisp -*-

(in-package "USER")

#||# ;; DM/RAL 03/24
(compile-file-if-needed "startup/advice-trace"        :load t)
(compile-file-if-needed "startup/trace-find-package"  :load t)
(compile-file-if-needed "startup/project-packages-lw" :load t)
(load "startup/project-mappings")
#||#

;; (trace editor:bind-key)

(load "startup/_my_bare-startup")

#||# ;; DM/RAL 03/24
(compile-file-if-needed "startup/gui-colors-dark" :load t)
(compile-file-if-needed "startup/handy"           :load t)
#||#

(setf *inspect-through-gui* t)

(setf *print-pretty*  t)
(setf *print-length*  10)
(setf *print-level*   6)
(setf *print-circle*  t)

(setf sys:*extended-spaces* t)

(require "dynamic-complete")

;; (asdf :named-readtables)

(asdf :lw-add-ons)
(setf lw-add-ons:*show-doc-string-when-showing-arglist* t)
(asdf:load-system :lw-plugins)

;; -------------------------------------------------------------------------------

;; (asdf "space-shows-arglist")

(lw:defadvice (sys::display-times fix-allocated-memory-display :around)
    (entry-times exit-times allocation start-real end-real xxx page-faults ncalls)
  (lw:call-next-advice entry-times exit-times
                       (with-standard-io-syntax
                         (format nil "~:D" allocation)) ;; use comma separated groups of digits
                         start-real end-real xxx page-faults ncalls))

;; (load "startup/qi-support")

#+:nil
(named-readtables:defreadtable :ral-syntax
                               (:merge :current))

(define-action "Initialize LispWorks tools"
               "Make an Output Browser Tool"
               #'(lambda (screen)
                   (capi:find-interface 'lw-tools:output-browser
                                        :screen screen))
               :after "Create default the tools")

;; ---------------------------------------------------------------

#||# ;; DM/RAL 03/24
(asdf :illogical-pathnames)

;; (load "./yt-loader.lisp")

(asdf :com.ral.actors)
(pushnew :actors *features*)

(asdf :com.ral.useful-macros/ext)
(compile-file-if-needed "startup/editor-enhancements" :load t)

(asdf :plotter)
(asdf :com.ral.actors.extra) ;; pick up KVDB
(asdf :com.ral.actors.secure-channel)
(asdf :refstore)
(asdf :com.ral.astro)
#||#

(setf (environment-variable "HOMEBREW_PREFIX") "/opt/homebrew")
