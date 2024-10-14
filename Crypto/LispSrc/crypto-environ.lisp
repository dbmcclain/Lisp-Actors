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

(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------------
;; File Locations Environment

(defstruct environ
  home-path
  shared-files-path
  pwd-file
  pubkey-file)

(defun make-crypto-environ ()
  (let* ((home   (probe-file
                  #-(or :macosx :win32) "~/"
                  #+:MACOSX "~/" ;; #+:WIN32 (lw:environment-variable "USERPROFILE")
                  #+:WIN32 (sys:get-user-profile-directory)))
         (shared
                 #-(or :macosx :win32) "/var/tmp/" ;; FIXME
                 #+:MACOSX (or (probe-file "~/Dropbox/Team Share/")
                               (probe-file "~/Documents/Dropbox/Team Share/"))
                 #+:WIN32  (probe-file (merge-pathnames "Dropbox/Team Share/" home)))
         (pwd    (merge-pathnames "_acudora-ecc-passwds" home))
         (pub    (merge-pathnames "Acudora-public-keys.txt" shared)))
    (make-environ
     :home-path         home
     :shared-files-path shared
     :pwd-file          pwd
     :pubkey-file       pub)))

(def-cached-var crypto-environ (make-crypto-environ))

#|
(defvar *crypto-environ* nil)

(defun crypto-environ ()
  (or *crypto-environ*
      (setf *crypto-environ* (init-crypto-environ))))
|#

(defun shared-acudora-file (fname)
  (merge-pathnames fname (environ-shared-files-path (crypto-environ))))

(defun pwd-file ()
  (environ-pwd-file (crypto-environ)))

(defun pubkey-file ()
  (environ-pubkey-file (crypto-environ)))

(defun home-path ()
  (environ-home-path (crypto-environ)))

;; -------------------------------------------------------------------------------------------
