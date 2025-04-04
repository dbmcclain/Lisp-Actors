;; SMULT delivery script
;;
#|;; To use:
#|
The MIT License

Copyright (c) 2017-2023 Refined Audiometrics Laboratory, LLC

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

;; For Mac-64
pushd /Applications/LispWorks\ 8.0\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS
"$PWD/Lispworks-8-0-0-macos64-universal" -build ~/projects/lispworks/Crypto/deliver-echo.lisp
popd

;; For Mac
pushd /Applications/LispWorks\ 6.0/LispWorks.app/Contents/MacOS
./Lispworks-6-0-0-macos-universal -init ~/projects/lispworks/Godzilla/deliver.lisp
popd

;; for Windows (Dawson)
pushd h:/projects/Lispworks
"d:/program Files/Lispworks/lispworks-5-1-0-x86-win32.exe" -init ./Godzilla/deliver.lisp

;; for Vista (Slate)
pushd c:/Users/Public/projects/Lispworks
"c:/program Files/Lispworks/lispworks-5-1-0-x86-win32.exe" -init ./Godzilla/deliver.lisp

;; for Vista (Citrine-Vista & Topaz-Vista)
pushd c:/projects/Lispworks
"c:/program Files/Lispworks/lispworks-7-0-0-x64-windows.exe" -init ./VTuning/crypto/tools/deliver-aont.lisp
popd

|#

(load-logical-pathname-translations "PROJECTS")
(cd (translate-logical-pathname "PROJECTS:LISP;"))
;; (cd #P"~/projects/Lispworks/")
(compile-file-if-needed "startup/project-packages-lw"   :load t)
(load "startup/project-mappings")
(load "startup/_my_bare-startup")

#|
#+:MACOSX
(let ((prjdir "/Volumes/My Passport for Mac/projects"))
  (setf (environment-variable "PROJECTS")
	(if (probe-file prjdir)
	    prjdir
	    (namestring #P"~/projects"))))

(load-logical-pathname-translations "PROJECTS")
(change-directory (translate-logical-pathname "PROJECTS:LISP;"))

(load "dongle")

(let ((mi (machine-instance)))
  (cond ((string-equal "CITRINE-VISTA" mi) (load "Win32-Citrine-ASDF-Starter"))
	((string-equal "SLATE"         mi) (load "Win32-Citrine-ASDF-Starter"))
        ((string-equal "TOPAZ-VISTA"   mi) (load "Win32-Topaz-ASDF-Starter"))
        ((string-equal "DAWSON"        mi) (load "Win32-Dawson-ASDF-Starter"))
        ((string-equal "RAMBO"         mi) (load "Win32-Citrine-ASDF-Starter"))
        (t                                 (load "ASDF-Starter"))
        ))
|#

;; ------------------------------------------------------------------------------
;; Get the bundle maker for OS X

;; #+:MACOSX
;; (compile-file-if-needed "macos-application-bundle" :load t)

;; ------------------------------------------------------------------------------
;; Get the components we need in the base Lisp

;; ------------------------------------------------------------------------------
;; Compile the application
;; (asdf:operate 'asdf:load-op :godzilla :force t) ;; force full recompile

;; (require "inspector-values")

;; ------------------------------------------------------------------------------
;; Change to the resources folder

;; (change-directory (translate-logical-pathname "PROJECTS:LISP;godzilla;"))

(defun echo ()
  (format t "Nr Args = ~d~%" (length sys:*line-arguments-list*))
  (loop for ix from 1
        for arg in sys:*line-arguments-list*
          do
          (format t "~D: ~A~%" ix arg))
  (lw:quit :status 0))

(compile 'echo)

(deliver 'echo
         "lwecho"
         #|
         #+:Macosx
         (let ((this-dir (translate-logical-pathname "PROJECTS:LISP;Crypto;")))
           (create-macos-application-bundle
            "/Applications/Tolstoy-AONT.app"
            :signature  "ACUD"
            :identifier "com.ral.aont"
            :application-icns (merge-pathnames "calculator.icns" this-dir)
            :document-types nil
            ))

	 #+:WIN32 "Tolstoy-AONT.exe"
         |#
         
         0 ;; delivery level
         
         ;; #+:WIN32 :icon-file #+:WIN32 "Resources/Godzilla.png"

         :multiprocessing t
         :keep-lisp-reader t
         ;; :keep-conditions :all
         :quit-when-no-windows t
         :kill-dspec-table nil
         ;; :keep-pretty-printer t

         ;; :editor-style :emacs
         ;; :keep-complex-numbers t
         ;; :keep-eval t
         ;; :keep-load-function t
         ;; :keep-modules t
         ;; :keep-package-manipulation t
         ;; :keep-trans-numbers t
         ;; :redefine-compiler-p nil
         
         ;; :keep-foreign-symbols t
	 ;; :keep-gc-cursor t

	 :versioninfo
         (list
          :binary-version #x0001000100000010
	  :version-string "Version 1.01 build 16"
	  :company-name   "Refined Audiometrics Laboratory, LLC"
	  :product-name   "SMULT"
	  :file-description "Use to provide TPM multiplication of user points"
          :legal-copyright  "Copyright (c) 2023 by Refined Audiometrics Laboratory, LLC. All rights reserved.")
	 ;; :keep-debug-mode t
	 ;; :packages-to-keep :all

         :startup-bitmap-file nil
         :split nil
         ;; :split :resources
         )
(quit)
