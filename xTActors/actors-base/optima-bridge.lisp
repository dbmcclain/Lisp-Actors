;; optima-bridge.lisp
;;
;; DM/RAL  2024/03/23 17:51:32 UTC
;; ----------------------------------

(defpackage #:optima-bridge
  (:use #:common-lisp)
  (:export
   #:match))

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
#|
(convert-pat 15)
(convert-pat '(x 15 'x y))
(convert-pat '(x 15 'x . y))
 |#

(defun translate-clause (clause)
  (um:let+ ((pat  (car clause))
            (rest (cdr clause))
            (:mvb (tst body) (if (member (car rest) '(when /))
                                 (values (cadr rest) (cddr rest))
                               (values nil rest)))
            (opat (convert-pat pat)))
    (list opat (if tst
                   `(progn
                      (unless ,tst
                        (optima:fail))
                      ,@body)
                 `(progn
                    ,@body)))
    ))
    
(defmacro match (msg &rest clauses)
  `(optima:match ,msg ,@(mapcar #'translate-clause clauses)))

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
 |#