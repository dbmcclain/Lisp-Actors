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
;; Walker.
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
;; scanner/rewriter

(defun prognify (lst)
  ;; lst is always a (PROGN ...) or a (LOCALLY ...)
  (if (cddr lst)
      lst
    (cadr lst)))

(defun convert-pair-to-revlist (pair)
  (destructuring-bind (sym . gsym) pair
    `(,gsym ,sym)))

(defun get-binding-sym (binding)
  (if (consp binding)
      (car binding)
    binding))

(defconstant *marker* (gensym (string :marker)))

(defun insert-capture-bindings-at-markers (tree dict env global-exceptions)
  (labels ((rewrite (subform context env)
             (declare (ignore context))
             (labels ((is-global (sym)
                        (and
                         ;; DEFCONSTANT produces a special binding,
                         ;; but it can't be accidentally rebound or
                         ;; redefined, so it is okay for other
                         ;; threads to use this truly global fixed
                         ;; values
                         (not (constantp sym))
                         ;; once declared special with DEFVAR or DEFPARAMETER
                         ;; you can never bind a symbol lexically.
                         (or (sys:declared-special-p sym)
                             ;; and using (DECLARE SPECIAL) can make
                             ;; dynamic bindings, even if there is
                             ;; no global declaration with that
                             ;; symbol. But these local special
                             ;; bindings can be overridden by an
                             ;; inner lexical binding with the same
                             ;; symbol.
                             (walker:variable-special-p sym env))
                         (not (member sym global-exceptions))))
                      
                      (is-global-rebinding (binding)
                        (let ((sym (get-binding-sym binding)))
                          (when (is-global sym)
                            (let ((pair (assoc sym dict))) ;; did we need to alpha convert?
                              (when pair
                                `(,pair)))
                            )))

                      (is-marked-let (form)
                        (and (consp form)
                             (eql 'PROGN (car form))
                             (consp (cadr form))
                             (eq *marker* (caadr form))))

                      (is-marked-lambda (form)
                        (and (consp form)
                             (eq *marker* (car form))
                             (consp (caddr form))
                             (eql 'LOCALLY (caaddr form)))))
           
               (cond ((is-marked-lambda subform)
                      (destructuring-bind (mark args (locallysym &rest body)) subform
                        (declare (ignore mark locallysym))
                        `(FUNCTION (LAMBDA ,args ,@body))))
                     
                     ((is-marked-let subform)
                      (destructuring-bind (prognsym
                                           (mark letsym bindings)
                                           (locallysym &rest body)) subform
                        (declare (ignore prognsym mark locallysym))
                        
                        (let ((pairs (mapcan #'is-global-rebinding bindings)))
                          (if pairs
                              `(,letsym ,bindings
                                        (let ,(mapcar #'convert-pair-to-revlist pairs)
                                          (declare (ignorable ,@(mapcar #'cdr pairs)))
                                          ,@body))
                            ;; else
                            `(,letsym ,bindings ,@body))) ))
                     
                     (t
                      subform)
                     ))))
    (walker:walk-form tree env #'rewrite)))
                    
(defun rebind-global-free-vars (form global-exceptions env)
  ;; Single-pass scan and rewrite, looking for symbols that have
  ;; global special bindings. We build up a dictionary as an alist and
  ;; hand it back to the caller so he can generate lexical bindings
  ;; around our newly macro-expanded and rewritten form.
  ;;
  ;; global-exceptions is a list of symbols that should be excluded
  ;; from rewriting.
  ;;
  (let (free-vars
        needs-insertions)
    (labels ((get-replacement (sym)
               (or (sys:cdr-assoc sym free-vars)
                   (let ((gname (gensym (string sym))))
                     (setf free-vars (acons sym gname free-vars))
                     gname)))

             (get-bindings-form ()
               (mapcar #'convert-pair-to-revlist free-vars))
             
             (rewrite (subform context env)
               (declare (ignore context))

               #||#
               (with-standard-io-syntax
                 (pprint subform))
               #||#
               
               (labels ((is-global (sym)
                          (and
                           ;; DEFCONSTANT produces a special binding,
                           ;; but it can't be accidentally rebound or
                           ;; redefined, so it is okay for other
                           ;; threads to use this truly global fixed
                           ;; values
                           (not (constantp sym))
                           ;; once declared special with DEFVAR or DEFPARAMETER
                           ;; you can never bind a symbol lexically.
                           (or (sys:declared-special-p sym)
                               ;; and using (DECLARE SPECIAL) can make
                               ;; dynamic bindings, even if there is
                               ;; no global declaration with that
                               ;; symbol. But these local special
                               ;; bindings can be overridden by an
                               ;; inner lexical binding with the same
                               ;; symbol.
                               (walker:variable-special-p sym env))
                           (not (member sym global-exceptions)))))
                 
                 (cond (;; basic symbol alpha conversion
                        (and (symbolp subform)
                             (is-global subform))
                        (get-replacement subform))
                       
                       ;; Here we alpha convert lambda args in case
                       ;; one or more of them identify global special
                       ;; names.  Failing to do this step means that
                       ;; the body of the lambda will become alpha
                       ;; converted, while the args needing to be
                       ;; referred to still have their global special
                       ;; symbols. Kaboom!
                       ;;
                       ;; As with LET bindings, we need to modify the
                       ;; source tree with a marker so that it can be
                       ;; readily identified in the final rewrite
                       ;; phase, and also avoid causing an infinite
                       ;; walker loop.
                       ;;
                       ;; So here, pattern:
                       ;;    (FUNCTION (LAMBDA args &body body))
                       ;; becomes:
                       ;;    (MARKER new-args (LOCALLY ,@body))
                       ;; in order to keep declares at the start of body legal.
                       ;;
                       ;; We have to do this in this manner because we
                       ;; otherwise are never shown any lambda arg
                       ;; lists, nor lambda expressions, during the
                       ;; walkthrough.
                       ;;
                       ((and (consp subform)
                             (eql 'FUNCTION (car subform))
                             (consp (cadr subform))
                             (eql 'LAMBDA (caadr subform)))
                        (destructuring-bind (functionsym (lambdasym args &rest body)) subform
                          (declare (ignore functionsym lambdasym))
                          (labels ((iter (arg)
                                     (cond ((and (symbolp arg)
                                                 (is-global arg))
                                            (get-replacement arg))

                                           ((consp arg)
                                            (mapcar #'iter arg))

                                           (t arg))))
                            
                            (setf needs-insertions t)
                            (let ((new-args (mapcar #'iter args)))
                              `(,*marker* ,new-args (LOCALLY ,@body))))))

                       ;; This shouldn't be our problem... but when the
                       ;; walker encounters a symbol-macrolet form, it
                       ;; macroexpands all the symbols, but leaves the
                       ;; symbol-macrolet and its bindings in place. A
                       ;; subsequent repeat walk through from the
                       ;; compiler will trigger a warning that none of
                       ;; the symbol-macrolet bindings were referenced.
                       ;;
                       ;; So, we take the liberty of liberating the
                       ;; walked form of the symbol-macrolet and its
                       ;; bindings and plant the rest of the clauses
                       ;; into a locally. That keeps the system quiet.
                       ;; (c.f., LOCALLY is needed instead of PROGN,
                       ;; in order to accommodate possible leading
                       ;; DECLARE clauses.)
                       ;;
                       ((and (consp subform)
                             (eql 'symbol-macrolet (car subform)))
                        (prognify `(LOCALLY ,@(cddr (walker:walk-form subform env)))))

                       ;; look for let-bindings of globals and mark
                       ;; them for later fixup
                       ((and (consp subform)
                             (member (car subform) '(LET LET*))
                             (some #'is-global (mapcar #'get-binding-sym (cadr subform))))
                        (setf needs-insertions t)
                        (destructuring-bind (letsym bindings &rest body) subform
                          ;; we need this elaborate marking in order
                          ;; to avoid warnings in inappropriately
                          ;; placed declarations
                          `(PROGN
                             (,*marker* ,letsym ,bindings)
                             (LOCALLY ,@body))
                          ))
                       
                       (t
                        subform)
                       ))))
      
      ;; expansion is always a (PROGN ...) form
      ;;
      ;; Two walk throughs: first to alpha convert and mark global LET
      ;; re-bindings, second to strip markers and place capture
      ;; bindings.
      (let* ((expansion (walker:walk-form form env #'rewrite))
             (new-expansion (if needs-insertions
                                (insert-capture-bindings-at-markers expansion
                                                                    free-vars env
                                                                    global-exceptions)
                              ;; else
                              expansion)))
        #|
        (with-standard-io-syntax
          (pprint expansion))
        |#
        (if free-vars
            (let* ((capture-bindings (get-bindings-form))
                   (gnames (mapcar #'car capture-bindings)))
              `(LET ,capture-bindings
                 (DECLARE (ignorable ,@gnames))
                 ,@(cdr new-expansion))) ;; remove the PROGN
          ;; else
          (prognify new-expansion))) )))

(defun mklist (arg)
  (if (listp arg)
      arg
    (list arg)))

(defvar *in-scan* nil) ;; prevents all but outermost scan from occurring

(defmacro ensure-lexical-bindings ((&key global) &body body &environment env)
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

(defmacro mp-lambda (args &body body)
  (multiple-value-bind (new-args global-exceptions)
      (process-args args)
    `(ensure-lexical-bindings (:global ,global-exceptions)
       (lambda ,new-args ,@body)) ))

(editor:setup-indent "mp-lambda" 1)
(editor:setup-indent "ensure-lexical-bindings" 1)


#|
(defvar x 15)
(defvar diddly 32)
(unintern 'x)

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
