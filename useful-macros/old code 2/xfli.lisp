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

(in-package :xfli)

;; flags for dlopen()
(defconstant +dlopen-rtld-lazy+       #x1)
(defconstant +dlopen-rtld-now+        #x2)
(defconstant +dlopen-rtld-local+      #x4)
(defconstant +dlopen-rtld-global+     #x8)
(defconstant +dlopen-rtld-noload+    #x10)
(defconstant +dlopen-rtld-nodelete+  #x80)
(defconstant +dlopen-rtld-first+    #x100)

;; special handle arguments for dlsym()
(defconstant +dlsym-rtld-next+      -1)
(defconstant +dlsym-rtld-default+   -2)
(defconstant +dlsym-rtld-self+      -3)
(defconstant +dlsym-rtld-main-only+ -5)

(fli:define-foreign-function (_dlclose "dlclose" :source)
    ((handle :long))
  :result-type :long
  :language :ansi-c)

(fli:define-foreign-function (_dlopen "dlopen" :source)
    ((path (:reference-pass (:ef-mb-string :null-terminated-p t)))
     (flags :long))
  :result-type :long
  :language :ansi-c)

(fli:define-foreign-function (_dlsym "dlsym" :source)
    ((handle :long)
     (symname (:reference-pass (:ef-mb-string :null-terminated-p t))))
  :result-type :long
  :language :ansi-c)


#|
(defparameter h
  (_dlopen "/usr/local/lib64/libHsIIR.dylib"
           (logior +dlopen-rtld-local+)))

(defparameter _tstadr
  (_dlsym h "hsiir_test"))

(fli:define-foreign-funcallable call-with-void
    ()
  :result-type :long
  :language :ansi-c)

(call-with-void (fli:make-pointer :type :function :address _tstadr))

(_dlclose h)
 |#
;; ----------------------------------------------------------------------

(defvar *loaded-dylibs* (make-hash-table))

(defstruct dylib-info
  libname handle bindings)

(defun register-module (module &key real-name (connection-style :lazy))
  (unless (gethash module *loaded-dylibs*)
    (let* ((handle  (_dlopen (namestring real-name)
                             (logior +dlopen-rtld-local+
                                     (ecase connection-style
                                       (:lazy      +dlopen-rtld-lazy+)
                                       (:immediate +dlopen-rtld-now+)
                                       )))))
      (when (zerop handle)
        (error "Can't load module ~A" real-name))
      (setf (gethash module *loaded-dylibs*) (make-dylib-info
                                              :libname  real-name
                                              :handle   handle
                                              :bindings nil)))
    ))

(defun disconnect-module (module)
  (let ((info (gethash module *loaded-dylibs*)))
    (when info
      (remhash module *loaded-dylibs*)
      (_dlclose (dylib-info-handle info))
      (do ((tl  (cdr (dylib-info-bindings info)) (cddr tl)))
          ((endp tl))
        (setf (car tl) fli:*null-pointer*))
      )))

(defun lookup-dylib-binding (module c-name)
  ;; returns the cons cell whose car is the FLI function pointer
  ;; this indirection is nulled out on disconnect-module
  (let ((info (gethash module *loaded-dylibs*)))
    (if info
        (let ((lst  (member c-name (dylib-info-bindings info)
                            :key  (lambda (v)
                                    (and (stringp v)
                                         v))
                            :test 'string=)))
          (if lst
              (cdr lst)
            ;; else
            (let ((addr (_dlsym (dylib-info-handle info) c-name)))
              (when (zerop addr)
                (error "No entry point ~A" c-name))
              (let ((ptr  (fli:make-pointer :type :function :address addr)))
                (cdr (setf (dylib-info-bindings info) (list* c-name ptr (dylib-info-bindings info))))
                ))
            ))
      ;; else
      (error "No module: ~A" module))))

(defmacro define-foreign-function ((lisp-name foreign-name) args &rest rest &key module &allow-other-keys)
  (let* ((ptr           (gensym))
         (callable-name (intern (string (gensym))))
         (caller-args   (gensym)))
    `(let ((,ptr (lookup-dylib-binding ,module ,foreign-name)))
       (fli:define-foreign-funcallable ,callable-name ,args ,@rest)
       (defun ,lisp-name (&rest ,caller-args)
         (apply ',callable-name (car ,ptr) ,caller-args)))
    ))

;; -------------------------------------------------------------------
