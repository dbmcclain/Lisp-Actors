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

(defun convert-pat (pat)        
  (cond ((atom pat)
         pat)
        ((eq (car pat) 'quote)
         pat)
        (t
         (let ((hd (convert-pat (car pat)))
               (tl (convert-pat (cdr pat))))
           (cond ((null tl)
                  `(list ,hd))
                 ((consp tl)
                  `(,(car tl) ,hd ,@(cdr tl)))
                 (t
                  `(list* ,hd ,tl))
                 )))
        ))

(defun xconvert-pat (pat)
  ;; Convert lambda-list-keywords into a pattern that Optima can understand
  (let ((pos  (and (alexandria:proper-list-p pat)
                   (position-if (um:rcurry #'member lambda-list-keywords) pat))))
    (if pos
        (let* ((hd  (um:take pos pat))
               (tl  (um:drop pos pat))
               (tlsym (gensym))
               (new-pat (nconc hd tlsym)))
          (values (convert-pat new-pat) tlsym tl))
      (convert-pat pat))
    ))
        
#|
(xconvert-pat 15)
(xconvert-pat '(x 15 'x y))
(xconvert-pat '(x 15 'x . y))
 |#

(defun translate-clause (clause)
  (um:let+ ((pat  (car clause))
            (rest (cdr clause))
            (:mvb (tst body) (if (member (car rest) '(when /))
                                 (values (cadr rest) (cddr rest))
                               (values nil rest)))
            (:mvb (opat tlsym tl) (xconvert-pat pat))
            (body (if tlsym
                      `((bind-tail ,tl
                            ,tlsym
                          ,@body))
                    body)))
    (if tst
        `(,opat (unless ,tst
                  (optima:fail))
                ,@body)
      `(,opat
        ,@body))
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
(match msg
  ((a b . c) / (eql c 15) (doit a b c)))
 |#

(defmacro bind-tail (args tail-var &body body)
  `(apply (lambda ,args
            ,@body)
          ,tail-var))

#+:LISPWORKS
(editor:indent-like 'bind-tail 'multiple-value-bind)