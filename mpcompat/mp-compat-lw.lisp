;; mp-compat-lw.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
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

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))
;; --------------------------------------------------
;; Compatibility Layer

(defmacro defglobal (name val)
  `(hcl:defglobal-variable ,name ,val))

#|
(defun current-process ()
  "Get the current Lisp process."
  (mp:get-current-process))
|#

(defun current-process ()
  "Get the current Lisp process."
  mp:*current-process*)

;; --------------------------------------------------------------------------

;; --------------------------------------------------------------------------

(defun generate-uuid ()
  (uuid:make-v1-uuid))
#|
(defun generate-uuid ()
  #+:MACOSX  (uuid:byte-array-to-uuid (uuidgen:generate))
  #+:WIN32   (uuid:make-v1-uuid))
|#
;; --------------------------------------------------------------------------

(defmacro CAS (place old new)
  `(sys:compare-and-swap ,place ,old ,new))
