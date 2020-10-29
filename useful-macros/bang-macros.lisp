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

(in-package :useful-macros)

;; --------------------------------------------
;; Bang-symbols
(defun bang-symbol-p (prefix s)
  (and (symbolp s)
       (eql (mismatch (symbol-name s) prefix) 2)))


;; The WALK-FORM function takes three arguments: a form to walk, an
;; initial environment to walk it in, and a walker function to perform
;; whatever action is necessary along the walk.
;;
;; That walker function itself takes three arguments: a form, a
;; context and an environment. The walker arranges for it to be called
;; on every macroexpanded or evaluated subform in the original form.
;;
;; The walker function should return a replacement form for the
;; subform it is given (or the subform itself if it doesn't
;; want to take any action), and a secondary value of t if no further
;; walking of that form should take place.

#-:LISPWORKS
(defun get-bang-symbols (prefix body)
  ;; NOTE: this will not be technically correct if the user has any bang-symbols
  ;; bound in let-bindings within body. We can't know this without performing a
  ;; code walker with lexical binding accounting...
  (remove-duplicates
   (remove-if #'(lambda (item)
                  (not (bang-symbol-p prefix item)))
              (flatten body))))

#+:LISPWORKS
(defun get-bang-symbols (prefix body)
  (let (bang-symbols)
    ;; NOTE: non-portable -- depends on LW implementation of WALKER
    (flet ((bang-walker (subform context env)
             (declare (ignore context))
             (when (and (bang-symbol-p prefix subform)
                        (not (walker:variable-lexical-p subform env)))
               (pushnew subform bang-symbols))
             subform))
      (walker:walk-form body nil #'bang-walker)
      bang-symbols)))

(defun bang-symbol-name (s)
  (subseq (symbol-name s) 2))

;; --------------------------------------------
;; A-Bang symbols -- create anaphoric symbol names
;; in package of macro expansion

(defmacro defmacro/a! (name args &body body)
  (let ((syms (get-bang-symbols #.(symbol-name :A!) `(progn ,@body))))
    (if syms
        `(defmacro ,name ,args
           (let ,(mapcar #`(,a1 (intern ,(bang-symbol-name a1))) syms)
             ,@body))
      `(defmacro ,name ,args ,@body))))

;; --------------------------------------------
;; G-Bang symbols -- auto generated gensyms

(defmacro defmacro/g! (name args &body body)
  (let ((syms (get-bang-symbols #.(symbol-name :G!) `(progn ,@body))))
    (if syms
        `(defmacro/a! ,name ,args
           (let ,(mapcar #`(,a1 (gensym ,(bang-symbol-name a1))) syms)
             ,@body))
      `(defmacro/a! ,name ,args ,@body))))

  
;; --------------------------------------------
;; O-Bang symbols -- once-only eval gensyms

(defun o!-symbol-to-g!-symbol (s)
  (symb #.(symbol-name :G!)
        (bang-symbol-name s)))

(defmacro defmacro! (name args &body body)
  (let* ((os (get-bang-symbols #.(symbol-name :O!) `(list ,@args)))
         (gs (mapcar 'o!-symbol-to-g!-symbol os)))
    ;; o-bang symbols can interfere with find-source
    (if os
        `(defmacro/g! ,name ,args
           `(let ,(mapcar 'list (list ,@gs) (list ,@os))
              ,(progn
                 ,@body)))
      `(defmacro/g! ,name ,args ,@body)) ))

#+:LISPWORKS
(dspec:define-dspec-alias defmacro! (name)
  `(defmacro ,name))

#+:LISPWORKS
(dspec:define-form-parser (defmacro!
                              (:parser dspec:name-only-form-parser)))
