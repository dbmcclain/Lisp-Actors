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

;; ----------------------------------------------------------------

(declaim (inline unchecked-pwr2
                 unchecked-ceiling-log2 unchecked-ceiling-pwr2
                 unchecked-floor-log2   unchecked-floor-pwr2
                 unchecked-align-pwr2))

(defun pwr2-q (n)
  (and (plusp n)
       (= 1 (logcount n))))

(defun unchecked-pwr2 (n)
  #F
  (declare (fixnum n))
  ;; (dpb 1 (byte 1 n) 0)
  (ash 1 n)) ;; ASH is 100 times faster than DPB in LWM

(defun pwr2 (n)
  (check-type n (fixnum 0))
  (unchecked-pwr2 n))

(defun unchecked-ceiling-log2 (n)
  #F
  (declare (integer n))
  (- (integer-length n)
     (if (= 1 (logcount n)) 1 0)))

(defun ceiling-log2 (n)
  (check-type n (integer 1))
  (unchecked-ceiling-log2 n))

(defun unchecked-ceiling-pwr2 (n)
  (unchecked-pwr2 (unchecked-ceiling-log2 n)))

(defun ceiling-pwr2 (n)
  (unchecked-pwr2 (ceiling-log2 n)))

(defun unchecked-floor-log2 (n)
  #F
  (declare (integer n))
  (1- (integer-length n)))

(defun floor-log2 (n)
  (check-type n (integer 1))
  (unchecked-floor-log2 n))

(defun unchecked-floor-pwr2 (n)
  (unchecked-pwr2 (unchecked-floor-log2 n)))

(defun floor-pwr2 (n)
  (unchecked-pwr2 (floor-log2 n)))

(defun unchecked-align-pwr2 (val pwr2)
  #F
  (declare (fixnum pwr2)
           (integer val))
  (let ((pwr2m1 (1- pwr2)))
    (logandc2 (+ val pwr2m1) pwr2m1)))

(defun align-pwr2 (val pwr2)
  (check-type val (integer 0))
  (check-type pwr2 (fixnum 0))
  (unchecked-align-pwr2 val pwr2))

;; -----------------------------------------------------------
#| ;; use Alexandria's version of curry and rcurry
;; in ML these are referred to as sections
;; these actually correspond to the Dylan operators
;; secr ::= rcurry, secl ::= curry
(defun curry (fn &rest pref-args)
  (lambda (&rest suf-args)
    ;; using M-V-C we don't need to append the arg lists
    (multiple-value-call fn (values-list pref-args) (values-list suf-args))))

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (multiple-value-call fn (values-list pref-args) (values-list suf-args))))
|#

(defun make-rubber-vector (&key (length 16) element-type)
  (make-array length
              :fill-pointer 0
              :adjustable   t
              :element-type element-type))

(defmacro named-lambda (name lambda-list &body body)
  `(labels ((,name ,lambda-list ,@body))
     #',name))

  ;; --------------------------------------------

(defun raw-mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) 
      (princ a s))
    ))
  
(defun mkstr (&rest args)
  (with-standard-io-syntax
    (apply 'raw-mkstr args)))

;; ----------------------------------------------
;; Symbology...

(defun correct-for-symbol-character-case (str)
  ;; a portable way to make symbol strings
  ;; Modern Mode vs ANSI
  (if (eql #\a (char (string :a) 0))
      (string-downcase (string str))
    (string-upcase (string str))))

(defun intern-symbol (str &rest package)
  (apply 'intern (correct-for-symbol-character-case str) package))

(defun symb (&rest args)
  (values (intern-symbol (apply #'mkstr args))))

(defun kwsymb (&rest args)
  (values (intern-symbol (apply #'mkstr args) (find-package :keyword))))

(defmethod kwsymbol ((name string))
  (intern-symbol name (find-package :keyword)))
(defmethod kwsymbol ((sym symbol))
  (if (keywordp sym)
      sym
    (intern (symbol-name sym) (find-package :keyword))))
(defmethod kwsymbol ((cons cons))
  (if (eq 'quote (car cons))
      (kwsymbol (cadr cons))
    (call-next-method)))

(defun symbol-gensym (s)
  (gensym (format nil "~A-" (symbol-name s))))
  
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (sym)
                   `(,sym  (symbol-gensym ',sym)))
                 syms)
     ,@body))

(defun gensyms (lst)
  (mapcar (lambda (_)
            (declare (ignore _))
            (gensym))
          lst))

(unless (fboundp 'with-unique-names)
  ;; Lispworks already has this, but SBCL does not
  (defmacro with-unique-names ((&rest names) &body body)
    `(symbol-macrolet ,(mapcar (lambda (name)
                                 `(,name ',(gensym (string name))))
                               names)
       ,@body)))

(unless (fboundp 'rebinding)
  ;; Lispworks already has this, but SBCL does not
  (defmacro rebinding (variables &body body)
    (loop with prefix = (symbol-name '#:re)
          for var in variables
          for g = (gensym prefix)
          for temp = `(gensym ,(string var))
          collect `(,g ,temp) into gensyms
          collect ``(,,g ,,var) into temps
          collect `(,var ,g) into renames
          finally (return `(let ,gensyms
                             `(let (,,.temps)
                                ,(let ,renames
                                   ,@body)))))))

;; --------------------------------------------------------

#+sbcl
(if (string-lessp (lisp-implementation-version) "1.2.2")
    (pushnew :safe-sbcl *features*)
    (setq *features* (remove :safe-sbcl *features*)))

(defun flatten (x)
  ;; this is really a deep flatten
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   #+(and sbcl (not safe-sbcl))
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                   ((atom x) (cons x acc))
                   (t  (rec (car x) (rec (cdr x) acc)))
                   )))
    (rec x nil)))

;; #+sbcl code above adapted from
;; #https://github.com/thephoeron/let-over-lambda/blob/a202167629cb421cbc2139cfce1db22a84278f9f/let-over-lambda.lisp

(defmacro perform (name bindings &body body)
  (let ((args (mapcar 'first bindings))
        (vals (mapcar 'second bindings)))
    `(labels ((,name ,args ,@body))
       (,name ,@vals))
    ))

#|
(defmacro nlet (name bindings &body body)
  ;; NLET = Named LET
  `(perform ,name ,bindings ,@body))
|#

#| ;; use Alexandria:parse-body instead of this
(defun collect-decls (forms)
  (nlet iter ((forms forms)
              (decls nil))
    (declare (dynamic-extent #'iter))
    (let ((form (car forms)))
      (if (or (stringp form)
              (and (consp form)
                   (eq (car form) 'declare)))
          (iter (cdr forms) (cons form decls))
        (values forms (nreverse decls))
        ))))
|#

;; -------------------------------------------



;;; DEFCONSTANT+: Defconstant PLUS a bit of common sense.

(defmacro defconstant+ (name value &optional doc)
  "Like DEFCONSTANT but does not allow you to (easily) change the
  value once you've evaluated the form."
  ;; In return, if you do something like 
  ;;
  ;;   (progn (defconstant foo "bar") (defconstant foo "bar"))
  ;;
  ;; SBCL (justified by Common Lisp) will not say things like
  ;;
  ;;   The constant FOO is being redefined (from "bar" to "bar")
  `(defconstant ,name 
     (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(if doc (list doc))))
