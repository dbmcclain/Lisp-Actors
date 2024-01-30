;; lib-loads.lisp -- Controlled Crypto Library load/unload
;;
;; DM/Emotiq 08/18
;; ---------------------------------------------------------
#|
Copyright (c) 2018 Emotiq AG

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


(in-package :crypto-lib-loader)

;; -----------------------------------------------------------------------

;; The Lisp runtime load might help us do this differently, but explicit
;; initialization is much easier to understand

(defun define-dev-dlls ()
  "loads the DLLs (.so and .dylib) at runtime, from pre-specified directories"
  (pushnew (asdf:system-relative-pathname :core-crypto "./var/local/lib/")
           cffi:*foreign-library-directories* :test 'equal)
  (cffi:define-foreign-library
      :libEd3363 
    (:darwin     "libLispEd3363.dylib")
    (:linux      "libLispEd3363.so")
    (t (:default "libLispEd3363")))
  (cffi:define-foreign-library
      :libCurve1174
    (:darwin     "libLispCurve1174.dylib")
    (:linux      "libLispCurve1174.so")
    (t (:default "libLispCurve1174")))
  (cffi:define-foreign-library
      :libCurveE521
    (:darwin     "libLispCurveE521.dylib")
    (:linux      "libLispCurveE521.so")
    (t (:default "libLispCurveE521")))
  #|
  (cffi:define-foreign-library
      :libLispPBC 
    (:darwin     "libLispPBCIntf.dylib")
    (:linux      "libLispPBCIntf.so")
    (t (:default "libLispPBCIntf")))
  |#
  )

(defun define-production-dlls ()
  "loads the DLLs (.so and .dylib) at runtime, from the current directory"
  (cffi:define-foreign-library
      :libEd3363
   (:darwin     "libLispEd3363.dylib")
   (:linux      "libLispEd3363.so")
   (t (:default "libLispEd3363")))
  (cffi:define-foreign-library
      :libCurve1174
   (:darwin     "libLispCurve1174.dylib")
   (:linux      "libLispCurve1174.so")
   (t (:default "libLispCurve1174")))
  (cffi:define-foreign-library
      :libCurveE521
    (:darwin     "libLispCurveE521.dylib")
    (:linux      "libLispCurveE521.so")
    (t (:default "libLispCurveE521")))
  #|
  (cffi:define-foreign-library
      :libLispPBC
   (:darwin     "libLispPBCIntf.dylib")
   (:linux      "libLispPBCIntf.so")
   (t (:default "libLispPBCIntf")))
  |#
  )

;; -----------------------------------------------------------------------------

#+(AND :LISPWORKS (:OR :MACOS :LINUX))
(defun do-with-aimed-load-paths (fn)
  ;;
  ;; LW specifies that lib load has search order:
  ;;
  ;;  On MacOS
  ;;  ---------
  ;;   1. LD_LIBRARY_PATH
  ;;   2. DYLD_LIBRARY_PATH
  ;;   3. ~/lib
  ;;   4. /usr/local/lib
  ;;   5. /usr/lib
  ;;
  ;;  On Linux
  ;;  ---------
  ;;   1. LD_LIBRARY_PATH
  ;;   2. list of libs specified in /etc/ld.so.cache
  ;;   3. /usr/lib
  ;;   4. /lib
  ;;
  ;; On Windows, it first searches in the directory of the executable.
  ;;
  ;; So for MacOS and Linux we arrange to prefix LD_LIBRARY_PATH with
  ;; our own specific path
  ;;
  (labels ((pref-env-var (var-name pref)
             (let ((prev (lw:environment-variable var-name)))
               (setf (lw:environment-variable var-name)
                     (if prev
                         (um:paste-strings #\: pref prev)
                       pref))
               prev)))
    (let* ((ld-name   "LD_LIBRARY_PATH")
           (pref      #|
                        (if (emotiq:production-p)
                            ;; use current app folder for lib path
                            (namestring
                             (make-pathname
                              :directory (pathname-directory (lw:lisp-image-name))))
                          ;; else - use our ../var/local/lib path
                          (asdf:system-relative-pathname :core-crypto "./var/local/lib/"))
                        |#
                        (asdf:system-relative-pathname :core-crypto "./var/local/lib/"))
           (sav-ld    (pref-env-var ld-name pref)))
      (unwind-protect
          (funcall fn)
        (setf (lw:environment-variable ld-name) sav-ld))
      )))

#-(AND :LISPWORKS (:OR :MACOS :LINUX))
(defun do-with-aimed-load-paths (fn)
  (funcall fn))


(defmacro with-aimed-load-paths (&body body)
  `(do-with-aimed-load-paths (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-aimed-load-paths" 0)


(defvar *load-counter*  0)

(defmethod load-dlls ()
  "load the dev or production dlls at runtime"
  (format t "~%CRYPTO-LIB-LOADER:LOAD-DLL Load Counter = ~A" (incf *load-counter*))
  (cond
   ((= 1 *load-counter*)
    (format t " -- Loading libraries")
    #|
    (if (emotiq:production-p)
        (define-production-dlls)
      (define-dev-dlls))
    |#
    (define-dev-dlls)
    (with-aimed-load-paths
      ;; (cffi:use-foreign-library :libLispPBC)
      (cffi:use-foreign-library :libEd3363)
      (cffi:use-foreign-library :libCurve1174)
      (cffi:use-foreign-library :libCurveE521)
      ))
   
   (t
    (format t " -- Skip re-loading libraries"))
   ))

(defmethod unload-dlls ()
  (when (plusp *load-counter*)
    (format t "~%Unloading DLL's")
    ;; (cffi:close-foreign-library :libLispPBC)
    (cffi:close-foreign-library :libed3363)
    (cffi:close-foreign-library :libcurve1174)
    (cffi:close-foreign-library :libCurveE521)
    (setf *load-counter* 0)
    ))

(defun core-crypto:ensure-dlls-loaded ()
  (load-dlls))

(core-crypto:add-to-startups  'load-dlls)
(core-crypto:add-to-shutdowns 'unload-dlls)


