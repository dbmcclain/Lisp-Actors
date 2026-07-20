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
  ;; Convert list patterns into patterns that Optima can understand.
  ;; Proper lists get converted:     '(a b c)   => '(LIST a b c),
  ;; and dotted lists get converted: '(a b . c) => '(LIST* a b c).
  ;;
  ;; We also convert lists with lambda-list-keywords into dotted lists
  ;; ahead of the above conversion.
  ;;
  ;; tlpairs - a list of (tail-symbol, tail-form) encountered along
  ;; the way, which correspond to lambda-list keywords. Those will
  ;; need an application of a lambda form to untnangle the bindings.
  ;;
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
    
    ;;   (a b c &optional d ...) => (a b c . #:gd) => (values (LIST* a b c #:gd)
    ;;                                                        '((#:gd . (&optional d ...)) . tlpairs))
    ;;   (a b c &key d ...)  => (a b c . #:gtail)  => (values (LIST* a b c #:gtail)
    ;;                                                        '((#:gtail . (&key d ...)) . tlpairs))
    ;;   (a b c &rest d ...) => (a b c . d)        => (values (LIST* a b c d)
    ;;                                                        '((d . (...)) . tlpairs))
    ;;   (a b c . d)                               => (values (LIST* a b c d)
    ;;                                                        tlpairs)
    ;;   (a b c d)                                 => (values (LIST  a b c d)
    ;;
    (um:accumx (acc dot ans)
      (um:nlet iter ((p pat))
        (if (atom p)
            ;; true for symbol (as in symbols, or ending cdr of dotted list),
            ;; as well as for terminating NIL of proper list.
            ;; Either way, no lambda-list-keywords encountered.
            (inner-convert pat tlpairs)
          ;; else
          (let ((tok (car p)))
            (case tok
              (&rest
               (let ((sym (cadr p))
                     (tl  (cddr p)))
                 (dot sym)
                 (inner-convert (ans)
                                (if tl
                                    (acons sym tl tlpairs)
                                  tlpairs))
                 ))
              ((&optional &key)
               (let ((tlsym (gensym)))
                 (dot tlsym)
                 (inner-convert (ans)
                                (acons tlsym p tlpairs))
                 ))
              (t
               ;; else - no lambda-list-keyword yet
               (acc tok)
               (go-iter (cdr p)))
              ))))
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

