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

(defun convert-pat (pat &optional tlpairs)
  ;; tlpairs - a list of (tail-symbol, tail-form) encountered along the way
  (labels
      ((inner-convert (pat tlpairs)
         ;; Provides atoms and quoted symbols, and otherwise converts
         ;; dotted lists to (LIST* ...), and other lists to (LIST ...).
         (if (or (atom pat)
                 (eq (car pat) 'quote))
             (values pat tlpairs)
           ;; else
           (multiple-value-bind (hd tlpairs)
               (convert-pat (car pat) tlpairs)
             (multiple-value-bind (tl tlpairs)
                 (inner-convert (cdr pat) tlpairs)
               (cond ((null tl)
                      (values `(list ,hd)
                              tlpairs))
                     ((consp tl)
                      (values `(,(car tl) ,hd ,@(cdr tl))
                              tlpairs))
                     (t
                      (values `(list* ,hd ,tl)
                              tlpairs))
                     )))
           )))
    (declare (dynamic-extent #'inner-convert))
    
    ;; Convert lambda-list-keywords into a pattern that Optima can understand:
    ;;
    ;;   (a b c &optional d) => (a b c . #:gd)    => (values (LIST* a b c #:gd)
    ;;                                                       ((#:gd . (&optional d)) . tlpairs))
    ;;   (a b c &key d e f)  => (a b c . #:gtail) => (values (LIST* a b c #:gail)
    ;;                                                       ((#:gail . (&key d e f)) . tlpairs))
    ;;   (a b c &rest d)     => (a b c . d)       => (values (LIST* a b c d)
    ;;                                                       tlpairs)
    ;;   (a b c d)           =>                      (values (LIST  a b c d)
    ;;                                                       tlpairs)
    (let ((pos  (and (alexandria:proper-list-p pat)
                     (position-if (um:rcurry #'member lambda-list-keywords) pat))))
      (if pos
          (let ((hd  (um:take pos pat))
                (tl  (um:drop pos pat)))
            (optima:match tl
              ((list* '&rest x y)
               (inner-convert (nconc hd x)
                              (if y
                                  (acons x y tlpairs)
                                tlpairs)))
              (_
               (let ((tlsym   (gensym)))
                 (inner-convert (nconc hd tlsym)
                                (acons tlsym tl tlpairs))))
              ))
        ;; else
        (inner-convert pat tlpairs))
      )))
        
#|
(convert-pat 15)
(convert-pat '(x 15 'x y))
(convert-pat '(x 15 'x . y))
(convert-pat '(x 15 'x &rest y))
(convert-pat '(x 15 'x &rest y &key a b c))
(convert-pat '((a &optional (b 1)) &rest args &key (x 15)))
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
            (:mvb (opat tlpairs) (convert-pat pat))
            (body (um:nlet iter ((body  body)
                                 (pairs tlpairs))
                    (if (endp pairs)
                        body
                      ;; bindings to the right can refer to bindings on their left
                      `((bind-tail ,(cdar pairs)
                            ,(caar pairs)
                           ,@(iter body (cdr pairs))))
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

