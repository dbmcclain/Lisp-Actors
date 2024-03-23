;; list-match.lisp -- a minimal list pattern matcher
;;
;; DM/RAL  11/21
;; ----------------------------------------------------

(defpackage :com.ral.useful-macros.list-match
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.list-match)

;; ----------------------------------------------------
;; Runtime behavior

(defun match-pat (msg pat)
  ;; collect binding values in reverse order
  #F
  (nlet iter ((pat  pat)
              (msg  msg)
              (vals nil))
    (cond  ((atom pat) ;; NIL (as in ENDP) is also an atom
            (cond ((null pat)            (values (null msg) vals)) ;; NIL is also a symbol (!!)
                  ((is-underscore? pat)  (values t vals))
                  ((keywordp pat)        (values (eq msg pat) vals))
                  ((symbolp pat)         (values t (cons msg vals)))
                  (t                     (values (equalp msg pat) vals))
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

;; ----------------------------------------------------
;; Compiling behavior - Patterns are implicitly quoted compile-time
;; constant expressions, where symbols refer to just a binding symbol.
;; At runtime these pattern constants are implicitly quoted constants
;; used during matching.
;;
;; Duplicate binding symbols are disallowed. If you need a pattern to
;; match the same in more than one position, use a WHEN clause. E.g.,
;;
;;   Prolog Pattern: (A A) => Match Pattern: (A B) WHEN (EQL A B)
;;
;; Patterns cannot refer to runtime bindings. E.g.,
;;
;;    `(A ,B C) should be written as (A X C) WHEN (EQL X B)
;;
;; If you need to match against a constant symbol use a quote:
;;
;;    (A 'X B) -- will bind A to first arg of message, B to third arg,
;;    and match only if second arg is 'X.
;;
;; Any non-symbol patten element matches against a message arg using
;; EQUALP. This is of no concern for numbers, keywords, lists,
;; vectors, but Characters and Strings will become case-agnostic
;; matches. If you need other forms of equality testing, use a binding
;; and a WHEN clause.  E.g.,
;;      (A S B) WHEN (STRING= S "this")
;;
;; Matching does not descend into vectors, apart from EQUALP on
;; constant item matching. Only lists and lists of lists. So don't put
;; expected binding symbols inside of vectors. Arbitrarily complicated
;; list trees are supported.
;;
;; Matching uses '_ as a wildcard element, matching anything. E.g.,
;;  (A _ B . _) binds A to first arg, B to third arg, and don't care for
;;  second arg, and matches any tail (including none).
;;
;; Prefer using dotted list notation instead of &REST. We transform
;; &REST to dotted form, to be user friendly. But no other Lambda-list
;; keywords (&OPTIONAL, &KEY, etc) are permitted.
;;
;; There are no default values on binding symbols.

(defun system-reserved-sym-p (x)
  (or (member x '(t nil))
      (member x lambda-list-keywords)
      ))

(defun acceptable-placeholder-p (x)
  (and (symbolp x)
       (not (keywordp x))
       (not (system-reserved-sym-p x))
       ))

(defun matchable-argpat-p (x)
  (and (acceptable-placeholder-p x)
       (not (is-underscore? x))))

(defun classify-pat (pat)
  (cond ((null pat) :NULL)
        ((symbolp pat)
         (cond ((system-reserved-sym-p pat) :SYSTEM)
               ((keywordp pat)              :KEYWORD)
               ((is-underscore? pat)        :WILD)
               (t                           :VAR)
               ))
        ((atom pat) :ATOM)
        ((member (car pat) '(quote function)) :QUOTE)
        (t :CONS)))

(defun collect-args (pat)
  ;; Collect binding args in reverse order.
  ;; Second value is a list of args that represent lists.
  
  (let ((class (classify-pat pat)))

    ;; An acceptable pattern is either a list of sub-patterns
    ;; (:CONS), or a single symbol representing the whole arg list.
    ;; That symbol can either be the underscore (:WILD), or a
    ;; bindable symbol (:VAR).
    
    (case class
      (:VAR
       (values pat (list pat)))
      (:WILD
       (values))
      (:CONS
       (nlet iter ((class class)
                   (pat   pat)
                   (args  nil)
                   (lsts  nil))
         (ecase class
           ((:NULL :ATOM :WILD :KEYWORD :QUOTE)
            ;; :NULL occurs at end of a list pattern.
            ;; The other possibilities serve as items for literal matching.
            (values args lsts))
           (:VAR
            ;; we have a bindable symbol
            (values (cons pat args) lsts))
           (:SYSTEM
            (error "Invalid pattern element: ~S" pat))
           (:CONS
            ;; we have a list sub-pattern
            (let ((hd (car pat))
                  (tl (cdr pat)))
              (multiple-value-bind (new-args new-lsts)
                  (iter (classify-pat hd) hd args lsts)
                (let ((tl-class (classify-pat tl)))
                  (go-iter tl-class tl new-args
                           (if (eq tl-class :VAR)
                               (cons tl new-lsts)
                             new-lsts))
                  ))))
           )))
      (t
       (error "Invalid pattern: ~S" pat))
      )))

#|
(collect-args '(a b _ (c 15 d . e) . f))
(collect-args '(&whole x a b c)) ;; invalid
(collect-args '('x))
(collect-args 'x)
(collect-args '_)
(collect-args :x) ;; invalid pattern
(collect-args 15) ;; invalid pattern

(collect-args '('%become arg))
|#

(defun duplicates-exist-p (lst)
  (and (consp lst)
       (consp (cdr lst))
       (or (member (car lst) (cdr lst))
           (duplicates-exist-p (cdr lst)))
       ))

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

#|
(transform-&rest '(&rest x))
 |#

(defun parse-match-clause (lbl fail msg clause)
  (let ((pat  (transform-&rest (car clause)))
        (body (cdr clause)))
    (multiple-value-bind (args lsts)
        (collect-args pat)
      (when (duplicates-exist-p args)
        (warn "duplicate binding names in match pattern: ~A" args))
      (when (and (consp args)
                 (some 'is-lambda-list-keyword? args))
        (warn "lambda list keywords are not valid pattern elements"))
      (labels
          ((lam (form)
             `(lambda* ,args
                (declare (ignorable ,@(mklist args)))
                ,@(when lsts
                    `((declare (list ,@lsts))))
                ,form))
           (xlate (tst body)
             `(block ,fail
                (match-clause ,msg ',pat ,tst
                              ,(lam 
                                `(return-from ,lbl
                                   (progn
                                     ,@body)))
                              ))
             ))
        (if (member (car body) '(when /))
            (xlate (lam (cadr body)) (cddr body))
          (xlate nil body))
        ))))

(defmacro match (msg &body clauses)
  (with-unique-names (lbl fail gmsg)
    `(block ,lbl
       (let ((,gmsg ,msg))
         (macrolet ((match-fail ()
                      `(return-from ,',fail)))
           ,@(mapcar (curry #'parse-match-clause lbl fail gmsg) clauses))))
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

(match '(a b . c) (msg diddly))

(match 'msg
  ((cust :ok) (print 'doit))
  (_
   (print 'didit)))

(:alt
 (:cons :var (:cons (:kw :ok) :null))
 :wild)

(:var (:alt
       (:kw :var (:alt
                  :when
                  t)
        (:var :var))))

(:cons :var (:cons (:alt
                    ((:kw :a) (:cons :var :null (:alt
                                                 :when
                                                 t)))
                    (:var (:cons :var :null)))))

(:alt
 (:var (:kw :a) :var :when)
 (:var (:kw :a) :var)
 (:var :var :var))

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

(defstruct end-node
  fn)

(defstruct (guard-node
            (:include end-node))
  tst)

(defstruct alt-node
  follows)

(defstruct (cons-node
            (:include alt-node)))

(defstruct (var-node
            (:include alt-node)))

(defstruct (wild-node
            (:include alt-node)))

(defstruct (atom-node
            (:include alt-node))
  atom)

(defstruct (symbol-node
            (:include alt-node))
  sym)

(defun add-parse-tree (tree pat tst fn)
  (let ((class   (classify-pat pat))
        (follows (alt-node-follows tree)))
    (case class
      ((:VAR :WILD)
       (if (find-if (lambda (node)
                      (or (var-node-p node)
                          (wild-node-p node)))
                    follows)
           (warn "Redundant global match elided")
         ;; else
         (let ((end (if tst
                        (make-guard-node
                         :tst tst
                         :fn  fn)
                      (make-end-node
                       :fn fn))))
         (setf (alt-node-follows tree)
               (append follows (list
                                (if (eq class :VAR)
                                    (make-var-node
                                     :follows end)
                                  (make-wild-node
                                   :follows end)))
                       ))
         )))
      (:CONS
       (let ((subtree (find-if #'cons-node-p follows)))
         (unless subtree
           (setf subtree (make-cons-node)
                 (alt-node-follows tree) (append follows (list subtree))
                 ))
         (nlet iter ((parent subtree)
                     (pat    pat))
           (let ((class   (classify-pat pat))
                 (follows (alt-node-follows parent)))
             (case class
               (:NULL
                (when (and follows
                           tst
                           (not (guard-node-p (car (last follows)))))
                  (warn "Guarded pattern follows unguarded"))
                (setf (alt-node-follows parent) (append follows (list
                                                                 (if tst
                                                                     (make-guard-node
                                                                      :tst tst
                                                                      :fn  fn)
                                                                   (make-end-node
                                                                    :fn fn)))))
                parent)
               
               (:VAR
                (let ((subtree (find-if #'var-node-p follows)))
                  (unless subtree
                    (setf subtree (make-var-node)
                          (alt-node-follows parent) (append follows (list subtree))))
                  subtree))
               (:WILD
                (let ((subtree (find-if #'wild-node-p follows)))
                  (unless subtree
                    (setf subtree (make-wild-node)
                          (alt-node-follows parent) (append follows (list subtree))))
                  subtree))
               (:KEYWORD
                (let ((subtree (find-if (lambda (node)
                                          (and (symbol-node-p node)
                                               (eq pat (symbol-node-sym node))))
                                        follows)))
                  (unless subtree
                    (setf subtree (make-symbol-node
                                   :sym pat)
                          (alt-node-follows parent) (append follows (list subtree))))
                  subtree))
               (:QUOTE
                (let* ((sym     (cadr pat))
                       (subtree (find-if (lambda (node)
                                           (and (symbol-node-p node)
                                                (eq sym (symbol-node-sym node))))
                                         follows)))
                  (unless subtree
                    (setf subtree (make-symbol-node
                                   :sym sym)
                          (alt-node-follows parent) (append follows (list subtree))))
                  subtree))
               (:ATOM
                (let ((subtree (find-if (lambda (node)
                                          (and (atom-node-p node)
                                               (equalp pat (atom-node-atom node))))
                                        follows)))
                  (unless subtree
                    (setf subtree (make-atom-node
                                   :atom pat)
                          (alt-node-follows parent) (append follows (list subtree))))
                  subtree))
               (:CONS
                (let ((hd  (car pat))
                      (tl  (cdr pat)))
                  (let ((new-parent (iter parent hd)))
                    (go-iter new-parent tl))
                  ))
               ))))
       ))))
  
(defun parse-tree (pats)
  (let* ((tree (make-alt-node)))
    (dolist (pat pats)
      (apply #'add-parse-tree tree pat))
    tree))

(defun xparse-match-clause (lbl clause)
  (let ((pat  (transform-&rest (car clause)))
        (body (cdr clause)))
    (multiple-value-bind (args lsts)
        (collect-args pat)
      (when (duplicates-exist-p args)
        (warn "duplicate binding names in match pattern: ~A" args))
      (when (some 'is-lambda-list-keyword? args)
        (warn "lambda list keywords are not valid pattern elements"))
      (labels
          ((lam (form)
             `(lambda* ,args
                (declare (ignorable ,@args))
                ,@(when lsts
                    `((declare (list ,@lsts))))
                ,form))
           (xlate (tst body)
             (list pat tst (lam `(return-from ,lbl
                                   (progn
                                     ,@body))))
             ))
        (if (member (car body) '(when /))
            (xlate (lam (cadr body)) (cddr body))
          (xlate nil body))
        ))))

(defmacro xmatch (msg &body clauses)
  (with-unique-names (lbl fail)
    (let ((tree (parse-tree
                 (mapcar (curry #'xparse-match-clause lbl) clauses))))
      `(block ,lbl
         (catch ,fail
           (macrolet ((match-fail ()
                        `(throw ,',fail)))
             (match-struct ,msg ,tree)))
         ))))

(defun match-struct (msg tree &optional args)
  ())

#|
(xmatch msg
  (_
   (diddly)))

(xmatch msg
  ((cust :add key album)
   (let ((tag (tag self)))
     (become (db-manager-beh (maps:add db key album) tag))
     (send-after 10 tag :save)
     (send cust :ok)))
   
  ((cust :remove key)
   (when (maps:find db key)
     (let ((tag (tag self)))
       (become (db-manager-beh (maps:remove db key) tag))
       (send-after 10 tag :save)))
   (send cust :ok))
  
  ((cust :lookup key)
   (send cust (maps:find db key)))
  
  ((cust :get-albums)
   (send cust (maps:fold db (lambda (k v acc)
                              (if (r128-album-p v)
                                  (cons (list k v) acc)
                                acc))
                         nil)))
  ((cust :contents)
   (send cust db))
  
  ((cust :save) / (eql cust tag)
   (send println "-- Saving album database --")
   (save-database db))
  
  ((cust :internal-reset)
   (let ((tag (tag self)))
     (become (db-manager-beh (maps:empty) tag))
     (send-after 10 tag :save)))

  (msg
   (diddly))
  )
|#
