;; optima-bridge.lisp
;;
;; DM/RAL  2024/03/23 17:51:32 UTC
;; ----------------------------------

(defpackage #:optima-bridge
  (:use #:common-lisp)
  (:export
   #:match
   #:match-fail
   #:bind-tail))

(in-package #:optima-bridge)

;; ----------------------------------

(defun convert-pat (pat &optional tlsyms tls)        
  (if (or (atom pat)
          (eq (car pat) 'quote))
      (values pat tlsyms tls)
    ;; else
    (multiple-value-bind (hd tlsyms1 tls1)
        (xconvert-pat (car pat) tlsyms tls)
      (multiple-value-bind (tl tlsyms2 tls2)
          (convert-pat (cdr pat) tlsyms1 tls1)
        (cond ((null tl)
               (values `(list ,hd)
                       tlsyms2 tls2))
              ((consp tl)
               (values `(,(car tl) ,hd ,@(cdr tl))
                       tlsyms2 tls2))
              (t
               (values `(list* ,hd ,tl)
                       tlsyms2 tls2))
              )))
    ))

(defun xconvert-pat (pat &optional tlsyms tls)
  ;; Convert lambda-list-keywords into a pattern that Optima can understand
  ;;   (a b c &optional d) => (a b c . #:gd)
  ;;   (a b c &rest d)     => (a b c . d)
  (let ((pos  (and (alexandria:proper-list-p pat)
                   (position-if (um:rcurry #'member lambda-list-keywords) pat))))
    (if pos
        (let ((hd  (um:take pos pat))
              (tl  (um:drop pos pat)))
          (optima:match tl
            ((list* '&rest x y)
             (convert-pat (nconc hd x)
                          (if y
                              (cons x tlsyms)
                            tlsyms)
                          (if y
                              (cons y tls)
                            tls)))
            (_
             (let ((tlsym   (gensym)))
               (convert-pat (nconc hd tlsym)
                            (cons tlsym tlsyms)
                            (cons tl tls))
               ))
            ))
      ;; else
      (convert-pat pat tlsyms tls))
    ))
        
#|
(xconvert-pat 15)
(xconvert-pat '(x 15 'x y))
(xconvert-pat '(x 15 'x . y))
(xconvert-pat '(x 15 'x &rest y))
(xconvert-pat '(x 15 'x &rest y &key a b c))
(xconvert-pat '((a &optional (b 1)) &rest args &key (x 15)))
 |#

(defmacro bind-tail (args tail-var &body body)
  `(apply (lambda ,args
            ,@body)
          ,tail-var))

#+:LISPWORKS
(editor:indent-like 'bind-tail 'multiple-value-bind)

(defun translate-clause (clause)
  (um:let+ ((pat  (car clause))
            (rest (cdr clause))
            (:mvb (tst body) (if (member (car rest) '(when /))
                                 (values (cadr rest) (cddr rest))
                               (values nil rest)))
            (body (if tst
                      `((unless ,tst
                          (optima:fail))
                        ,@body)
                    body))
            (:mvb (opat tlsyms tls) (xconvert-pat pat))
            (body (um:nlet iter ((body body)
                                 (tlsyms tlsyms)
                                 (tls    tls))
                    (if (endp tlsyms)
                        body
                      ;; bindings to the right can refer to bindings on their left
                      `((bind-tail ,(car tls)
                            ,(car tlsyms)
                           ,@(iter body (cdr tlsyms) (cdr tls))))
                      ))))
    `(,opat
      ,@body)
    ))


(defmacro match (msg &rest clauses)
  `(optima:match ,msg ,@(mapcar #'translate-clause clauses)))

(defmacro match-fail ()
  `(optima:fail))

#|
(let ((tag :me))
  (match '(:me :save)
    ((cust :ask key) (list cust key))
    ((atag :save) / (eq atag tag) (list atag))
    (msg msg)))

(disassemble (lambda (msg)
               (let ((tag :me))
                 (match msg
                   ((cust :ask key) (list cust key))
                   ((atag :save) / (eq atag tag) (list atag))
                   (msg msg)))))

(translate-clause '((a b . c) / (eql c 15) (doit a b c)))
(translate-clause '((a b &optional (c 32)) / (eql a 15) (doit a b c)))
(translate-clause '(((a &optional (b 1)) &rest args &key (x 15)) / (eql a 15) (doit a b c)))
(match msg
  ((a b . c) / (eql c 15) (doit a b c)))
 |#

