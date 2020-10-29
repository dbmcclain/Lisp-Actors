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

(in-package :lexb)

;;---------------------------------------------------------------------
;;
;; The macro ENSURE-LEXICAL-BINDING in this version accomplishes its
;; actions by way of alpha conversion of the body code over the
;; detected global references obtained by code-walking the body form.
;;
;; Lexical bindings are assured by using uninterned symbols as
;; replacements for extant binding names. That way, unlike with
;; symbol-macrolet, extant global special values can exist without
;; signaling an error during compiling. The current runtime binding
;; values of global symbols will be picked up and bound lexically to
;; uninterned symbols which cannot possibly be already globally bound.
;;
;; The scanning and rewriting is accomplished with the help of the LW
;; Walker. Lexical capture bindings are inserted after any LET
;; rebinding of globals within the scope of ENSURE-LEXICAL-BINDING.
;; Lambda args are also alpha converted to avoid referring to special
;; bindings.
;;
;; DM/RAL 10/17
;;---------------------------------------------------------------------

;; NOTE:
;; (hcl:variable-information '*read-eval*) => (values :special nil nil)
;; (sys:declared-special-p '*read-eval*) => T

(defun ?specials (syms)
  ;; give us a list of symbols
  ;; return a list of those which are
  ;; declared special as viewed from current package
  (loop for sym in syms
        when (sys:declared-special-p sym)
        collect sym))

;; clever hack to discern binding nature of symbol
(defmacro test-special (sym)
  (let ((gf (gensym)))
    `(let ((,gf (let ((,sym :lexical))
                  (lambda ()
                    ,sym))))
       (let ((,sym :dynamic))
         (funcall ,gf)))))

;; -----------------------------------------------------------------
;; ENSURE-LEXICAL-BINDINGS & MP-LAMBDA macros and their walking
;; scanner/rewriter. This version uses OPTIMA:MATCH (much easier!)

(defun prognify (form)
  ;; form is always a (PROGN ...) or a (LOCALLY ...)
  (optima:match form
    ((list (or 'PROGN 'LOCALLY) clause)  clause)
    (_                                   form)
    ))

#|
(rebind-global-free-vars '(progn (block mytag (doit toit mytag))) nil nil)
(ensure-lexical ()
  (block mytag
    (doit toit mytag)))
|#

(defun bound-value (sym)
  (when (boundp sym)
    (symbol-value sym)))

(defun rebind-global-free-vars (form global-exceptions env)
  ;; Dual-pass scan and rewrite, looking for symbols that have global
  ;; special bindings. We build up a dictionary as an alist and use
  ;; that to perform symbol substitutions and lexical capture
  ;; bindings.
  ;;
  ;; global-exceptions is a list of symbols that should be excluded
  ;; from rewriting.
  ;;
  (let (last-form
        free-vars
        (dbg nil))
    (labels
        ((get-replacement (sym)
           (or (sys:cdr-assoc sym free-vars)
               (if (symbol-package sym)
                   (let ((gname (gensym (string sym))))
                     (setf free-vars (acons sym gname free-vars))
                     gname)
                 sym)))
         
         (convert-pair-to-revlist (pair)
           (destructuring-bind (sym . gsym) pair
             `(,gsym ,sym)))

         (get-bindings-form ()
           (mapcar #'convert-pair-to-revlist free-vars))

         #|
         (convert-outer-pair-to-revlist (pair)
           (destructuring-bind (sym . gsym) pair
             `(,gsym (bound-value ',sym))))
         
         (get-outer-bindings-form ()
           (mapcar #'convert-outer-pair-to-revlist free-vars))
         |#
         
         (get-binding-sym (binding)
           (if (consp binding)
               (car binding)
             binding))

         (is-global (sym env)
           (and (symbolp sym)
                (symbol-package sym)
            ;;
            ;; DEFCONSTANT produces a special binding,
            ;; but it can't be accidentally rebound or
            ;; redefined, so it is okay for other
            ;; threads to use this truly global fixed
            ;; values
            ;;
            (not (constantp sym))
            ;;
            ;; once declared special with DEFVAR or DEFPARAMETER you
            ;; can never bind a symbol lexically. The only way to
            ;; remove its special property is to unintern it.
            ;;
            (or ;; (sys:declared-special-p sym)
                ;;
                ;; and using (DECLARE SPECIAL) can make
                ;; dynamic bindings, even if there is
                ;; no global declaration with that
                ;; symbol. But these local special
                ;; bindings can be overridden by an
                ;; inner lexical binding with the same
                ;; symbol.
                ;;
                (walker:variable-special-p sym env)
                (not (walker:variable-lexical-p sym env))
                )
            ;;
            ;; make sure we aren't supposed to ignore this symbol
            ;;
            (not (member sym global-exceptions))))

         (show-input-form (form)
           (when dbg
             (ignore-errors
               (with-standard-io-syntax
                 (pprint `(<= ,form))))))
         
         (set-last-form (form)
           (when dbg
             (ignore-errors
               (with-standard-io-syntax
                 (pprint `(=> ,form))
                 (terpri))))
           (setf last-form form)))

      (macrolet ((match-avoiding-walker-loops (subform &rest clauses)
                   `(if (eq ,subform last-form)
                        ,subform
                      ;; else
                      (progn
                        (show-input-form ,subform)
                        (set-last-form
                         (optima:match ,subform ,@clauses)))
                      )))
        (labels
            ((alpha-convert (subform context env)
               (declare (ignore context))
               (match-avoiding-walker-loops subform
                 
                 ((list* (or 'MACROLET 'SYMBOL-MACROLET) _ body)
                  (prognify `(LOCALLY ,@body)))
                 
                 (sym
                  (cond ((is-global sym env)
                         ;; basic alpha conversion
                         (get-replacement sym))
                        
                        (t  subform) ;; otherwise, just return the subform as-is
                        ))
                 ))
             
             (insert-bindings (subform context env)
               ;;
               ;; insert lexical capture bindings where needed, and clean
               ;; up the symbol-macrolet leftovers
               ;;
               (declare (ignore context))
               (match-avoiding-walker-loops subform

                 ((list* (and letsym (or 'LET 'LET*)) bindings body)
                  (labels
                      ((is-global-rebinding (binding)
                         (let ((sym (get-binding-sym binding)))
                           (when (is-global sym env)
                             (let ((pair (assoc sym free-vars))) ;; did we need to alpha convert?
                               (when pair
                                 `(,pair))) ;; enclose pair for MAPCAN
                             ))))
                    (let ((pairs (mapcan #'is-global-rebinding bindings)))
                      (if pairs
                          (let* ((capture-bindings (mapcar #'convert-pair-to-revlist pairs))
                                 (gnames (mapcar #'cdr pairs)))
                            `(,letsym ,bindings
                                      (LET ,capture-bindings
                                        (DECLARE (IGNORABLE ,@gnames))
                                        ,@body)))
                        ;; else
                        `(,letsym ,bindings ,@body)))))
                 
                 (_   subform)
                 )))
          
          ;; Expansion is always a (PROGN ...) form
          ;;
          ;; Two walk-throughs: first to alpha convert and mark global LET
          ;; re-bindings and LAMBDA arg lists, the second to strip markers
          ;; and place capture bindings.
          ;;
          (let ((pre-expansion (walker:walk-form form env))) ;; reify macrolets
            (when dbg
              (print "Starting alpha conversion...."))
            (let ((expansion (walker:walk-form pre-expansion env #'alpha-convert)))
              (when dbg
                (print "Starting cleanup walkthrough...."))
              (let ((new-expansion (walker:walk-form expansion env #'insert-bindings)))
                #|
                (with-standard-io-syntax
                  (pprint expansion)) ;; for debugging
                |#
                (if free-vars
                    (let* ((capture-bindings (get-bindings-form))
                           (gnames (mapcar #'car capture-bindings)))
                      `(LET ,capture-bindings
                         (DECLARE (IGNORABLE ,@gnames))
                         ,@(cdr new-expansion))) ;; remove the PROGN
                  ;; else
                  (prognify new-expansion)) ))) )))))

(editor:setup-indent "match-avoiding-walker-loops" 1)

(defun mklist (arg)
  (if (listp arg)
      arg
    (list arg)))

(defvar *in-scan* nil) ;; prevents all but outermost scan from occurring

(defmacro ensure-lexical ((&key global) &body body &environment env)
  ;; keyword arg global is really a symbol or a list of symbols that
  ;; should be excluded from lexical rebinding, remaining special
  ;; bindings in the form. (so-called global-exceptions). These should
  ;; be rare, so the default case is to assume that globals require
  ;; lexical rebindings around the form.
  (let ((form `(PROGN ,@body)))
    (if *in-scan*
        (prognify form)
      ;; else
      (let ((*in-scan* t))
        (rebind-global-free-vars form (mklist global) env)) )))

;; ------------------------------------------------------------

(defun process-args (args)
  (let (global-exceptions)
    (labels ((is-&global (item)
               (and (symbolp item)
                    (string-equal #.(string '&global) item)))
               
             (trim (lst)
               (unless (endp lst)
                 (cond ((is-&global (car lst))
                        (setf global-exceptions (append (mklist (cadr lst)) global-exceptions))
                        (trim (cddr lst)))

                       (t
                        (cons (car lst)
                              (trim (cdr lst))))
                       ))))
      (let ((new-args (trim args)))
        (values new-args global-exceptions))
      )))

#|
(defmacro mp-lambda (args &body body)
  (multiple-value-bind (new-args global-exceptions)
      (process-args args)
    `(ensure-lexical-bindings (:global ,global-exceptions)
       (lambda ,new-args ,@body)) ))
|#

(editor:setup-indent "mp-lambda" 1)
(editor:setup-indent "ensure-lexical-bindings" 1)

;; ---------------------------------------------------------------
;; (LEXICAL args-list &body body) -- generate an error if any args have special binding
#|
(defmacro lexical-1 (args &rest body)
  (destructuring-bind (ghd &rest gtl) args
    `(symbol-macrolet ((,ghd ,ghd))
       ,@(if gtl
             `((lexical-1 ,gtl ,@body))
           body))
    ))

(defmacro lexical (args &rest body)
  (if args
      `(lexical-1 ,args ,@body)
    (prognify `(progn ,@body))))

(editor:setup-indent "lexical" 1)

(defun record-lexical-symbols (form global-exceptions env)
  ;; Dual-pass scan and rewrite, looking for symbols that have global
  ;; special bindings. We build up a dictionary as an alist and use
  ;; that to perform symbol substitutions and lexical capture
  ;; bindings.
  ;;
  ;; global-exceptions is a list of symbols that should be excluded
  ;; from rewriting.
  ;;
  (let (last-form
        recorded-symbols
        (dbg nil))
    (labels
        ((show-input-form (form)
           (when dbg
             (with-standard-io-syntax
               (pprint `(<= ,form)))))
         
         (set-last-form (form)
           (when dbg
             (with-standard-io-syntax
               (pprint `(=> ,form))
               (terpri)))
           (setf last-form form)))

      (macrolet ((do-avoiding-walker-loops (subform &body body)
                   `(if (eq ,subform last-form)
                        ,subform
                      ;; else
                      (progn
                        (show-input-form ,subform)
                        (set-last-form (progn ,@body)))
                      )))

        (labels
            ((symbol-scan (subform context env)
               (declare (ignore context))
               (do-avoiding-walker-loops subform
                 (cond ((and (symbolp subform)
                             (not (constantp subform))
                             (not (member subform global-exceptions))
                             (not (walker:variable-lexical-p subform env)))
                        (pushnew subform recorded-symbols)
                        subform)

                       ((and (consp subform)
                             (member (car subform) '(MACROLET SYMBOL-MACROLET)))
                        (prognify `(LOCALLY ,@(cddr subform))))

                       (t subform)))
               ))
          (let ((expanded (walker:walk-form form env))) ;; allow to enact macrolets
            (values (walker:walk-form expanded env #'symbol-scan)
                    recorded-symbols)
          )) )) ))
             
(defmacro ensure-lexical ((&key global) &body body &environment env)
  (let ((form `(progn ,@body)))
    (if *in-scan*
        (prognify form)
      (let* ((*in-scan* t))
        (multiple-value-bind (new-form recorded-symbols)
            (record-lexical-symbols form (mklist global) env)
          `(lexical ,recorded-symbols ,@(cdr new-form)))
        ))))
|#
(editor:setup-indent "ensure-lexical" 1)

(defmacro mp-lambda (args &body body)
  (multiple-value-bind (new-args global-exceptions)
      (process-args args)
    `(ensure-lexical (:global ,global-exceptions)
       (lambda ,new-args ,@body))))

(defmacro mp-labels (bindings &body body)
  `(labels ,(mapcar (lambda (binding)
                      (destructuring-bind (sym args &rest body) binding
                        `(,sym ,args
                               (ensure-lexical ()
                                 ,@body))))
                    bindings)
     ,@body))

(defmacro mp-flet (bindings &body body)
  `(flet ,(mapcar (lambda (binding)
                    (destructuring-bind (sym args &rest body) binding
                      `(,sym ,args
                             (ensure-lexical ()
                               ,@body))))
                  bindings)
     ,@body))

(editor:setup-indent "mp-labels" 1 nil nil :FLET)
(editor:setup-indent "mp-flet"   1 nil nil :FLET)

#|
(defvar x 15)
(defvar diddly 32)
(unintern 'x)

;; ------------------------------------------
;; Showing that a lambda arg is not necessarily a lexical binding

(defvar x 15)

(defun print-x ()
  (print x))

(defun tstx ()
  (mp:funcall-async (lambda (x) (print-x)) 32))

(tstx) ;; => 32

;; -------------------------------------------
;; Try a local special binding

(defun print-y ()
  (declare (special y))
  (print y))

(defun tsty ()
  (let ((y 15))
    (declare (special y))
    (print-y)
    (mp:funcall-async (lambda (y) (print-y)) 32)))

(tsty) ;; => 15, Y is Unbound!

;; ---------------------------------------------

(defun tsty ()
  (let ((y 15))
    (declare (special y))
    (ensure-lexical ()
      (print y)
      (mp:funcall-async (lambda ()
                          (declare (special y))
                          (format t "~&y = ~A" y)))
      )))
  
(tsty)

;; ---------------------------------------------

(let ((x 15))
  (ensure-lexical ()
    (lambda (a b)
      (+ x a b))))

(ensure-lexical ()
 (let ((u 1)
       ;; (x 32)
       (y 15))
   (declare (special y))
   (locally ;; let ((y 77))
     (ensure-lexical ()
      (let ((v 2)
            ;; (x "twently")
            )
        (ensure-lexical ()
         (symbol-macrolet ((c 3))
           (lambda (x a b)
             (+ x y a b c)
             (lambda (u v)
               (+ x u v z))))))))))

(let ((u 1)
      (x 32)
      (y 15))
  (declare (special y))
  (locally ;; let ((y 77))
    (ensure-lexical-bindings ()
      (let ((v 2)
            (x "twently"))
        (ensure-lexical-bindings ()
          (symbol-macrolet ((c 3))
            (mp-lambda (x a b)
              (+ x y a b c))))))))

(defun print-x ()
  (print x))

(defun tst ()
  ((lambda (x) (print-x)) 32))

(defun print-y ()
  (declare (special y))
  (print y))

(defun tst ()
  (let ((y 15))
    (declare (special y))
    (let ((y 77))
      (print y)
      (print-y))))

(mp-lambda (a b c)
  (declare (ignore c))
  (+ x a b))

(mp-lambda (a b c &global xx)
	(declare (ignore c))
	(+ x a b))

(mp-lambda (a b &global diddly)
           (+ x diddly (* a y) b))

|#
