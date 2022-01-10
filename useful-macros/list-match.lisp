;; list-match.lisp -- a minimal list pattern matcher
;;
;; DM/RAL  11/21
;; ----------------------------------------------------

(in-package :list-match)

(defun match-pat (msg pat)
  ;; collect binding values in reverse order
  #F
  (um:nlet iter ((pat  pat)
                 (msg  msg)
                 (vals nil))
    (cond  ((atom pat) ;; NIL (as in ENDP) is also an atom
            (cond ((null pat)               (values (null msg) vals)) ;; NIL is also a symbol (!!)
                  ((um:is-underscore? pat)  (values t vals))
                  ((keywordp pat)           (values (eql msg pat) vals))
                  ((symbolp pat)            (values t (cons msg vals)))
                  (t                        (values (equalp msg pat) vals))
                  ))
           ((eql 'quote (car pat))
            (values (equalp msg (cadr pat)) vals))
           ((eql 'function (car pat))
            (let ((desig (cadr pat)))
              (values (and (symbolp desig)
                           (eq msg (symbol-function desig)))
                      vals)))
           ((consp msg)
            (multiple-value-bind (ok new-vals)
                (iter (car pat) (car msg) vals)
              (when ok
                (go-iter (cdr pat) (cdr msg) new-vals))
              ))
           )))

#|
(match-pat '(1 2 3 (4 15 16 17 18 19) 20 21 22)
           '(a b _ (c 15 d . e) . f))
(match-pat (list 1 #'1+ 3) '(a #'1+ b))
(match-pat 1 'x)
|#

(defun match-clause (msg pat tst fn)
  #F
  (multiple-value-bind (ok vals)
      (match-pat msg pat)
    (when (and ok
               (or (null tst)
                   (apply tst vals)))
      (apply fn vals))
    ))

(defun collect-args (pat)
  ;; collect binding args in reverse order
  (um:nlet iter ((pat  pat)
                 (args nil)
                 (lsts nil))
    (cond ((atom pat)
           (cond ((null pat)              (values args lsts))
                 ((um:is-underscore? pat) (values args lsts))
                 ((keywordp pat)          (values args lsts))
                 ((symbolp pat)           (values (cons pat args) lsts))
                 (t                       (values args lsts))
                 ))
          ((eql 'quote (car pat))    (values args lsts))
          ((eql 'function (car pat)) (values args lsts))
          (t
           (let ((hd (car pat))
                 (tl (cdr pat)))
             (multiple-value-bind (new-args new-lsts)
                 (iter hd args lsts)
               (when (and tl
                          (symbolp tl)
                          (not (um:is-underscore? tl)))
                 (push tl new-lsts))
               (go-iter tl new-args new-lsts)
               )))
          )))

#|
(collect-args '(a b _ (c 15 d . e) . f))
|#

(defun duplicates-exist-p (lst)
  (let ((nel (length lst)))
    (not (eql nel (length (remove-duplicates lst))))))

(defun transform-&rest (pat)
  ;; It is an all too easy mistake to make in writing patterns, that
  ;; one uses &REST for the tail of a list, instead of writing a
  ;; dotted list pattern.  We translate those mistakes here to dotted
  ;; list patterns.
  (cond ((atom pat) pat)
        ((and (eql '&rest (car pat))
              (eql (length pat) 2)
              (symbolp (cadr pat)))
         (cadr pat))
        (t
         (cons (transform-&rest (car pat))
               (transform-&rest (cdr pat))))
        ))

(defun parse-match-clause (lbl fail msg clause)
  (destructuring-bind (pat . body) clause
    (let* ((pat  (transform-&rest pat))
           (tst  nil))
      (multiple-value-bind (args lsts)
          (collect-args pat)
        (when (duplicates-exist-p args)
          (warn "duplicate binding names in match pattern: ~A" args))
        (when (some 'um:is-lambda-list-keyword? args)
          (warn "lambda list keywords are not valid pattern elements"))
        (when (member (car body) '(when /))
          (setf tst  `(lambda ,args
                        (declare (ignorable ,@args))
                        ,@(if lsts
                             `((declare (list ,@lsts))))
                        ,(cadr body))
                body (cddr body)))
        `(block ,fail
           (match-clause ,msg ',pat ,tst
                         (lambda ,args
                           (declare (ignorable ,@args))
                           ,@(if lsts
                                `((declare (list ,@lsts))))
                           (return-from ,lbl
                             (progn
                               ,@body)))
                         ))
        ))))

(defmacro match (msg &body clauses)
  (lw:with-unique-names (lbl fail gmsg)
    `(block ,lbl
       (let ((,gmsg ,msg))
         (macrolet ((match-fail ()
                      `(return-from ,',fail)))
           ,@(mapcar (um:curry #'parse-match-clause lbl fail gmsg) clauses))))
    ))

#+:LISPWORKS
(editor:setup-indent "match" 1)

#|
(match msg
  ((a b . c) (doit a b c))
  )
(match '(2 :a 15)
  ((x :a y) when (oddp x)
   (+ x y))
  ((x :a y)
   (- x y))
  ((a b a)
   :what)
  )
  
(MATCH #:MSG4106
              ((ACTORS/BASE::CUST :PRUNE) (SEND ACTORS/BASE::CUST :PRUNED SELF-BEH))
              ((CUST :SEND VERB . MSG) WHEN (EQL VERB NAME) (SEND* HANDLER CUST MSG))
              ((CUST :ADD-SERVICE ANAME NEW-HANDLER)
               WHEN
               (EQL ANAME NAME)
               (BECOME (SERVICE-LIST-BEH NAME NEW-HANDLER NEXT))
               (SEND CUST :OK))
              ((CUST :REMOVE-SERVICE ANAME) WHEN (EQL ANAME NAME) (PRUNE-SELF NEXT) (SEND CUST :OK))
              ((CUST :AVAILABLE-SERVICES LST) (SEND NEXT CUST :AVAILABLE-SERVICES (CONS NAME LST)))
              ((CUST :LIST LST) (SEND NEXT CUST :LIST (CONS NAME LST)))
              (_ (REPEAT-SEND NEXT)))
|#

