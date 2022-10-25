
(in-package #:cps)

(define-symbol-macro =bind-cont %sk)
(define-symbol-macro =wait-cont %sk)

;; ------------------------------------------------------------------
;; Trampoline and Continuations... for simulated CPS

(define-condition no-trampoline (error)
  ()
  (:report "No trampoline installed"))

(aop:defdynfun trampoline (fn &rest args)
  (declare (ignore fn args))
  (error 'no-trampoline))

(defmacro with-trampoline (&body body)
  `(do-with-trampoline (lambda () ,@body)))

(aop:defdynfun do-with-trampoline (thunk)
  (block trampoline
    (let ((fn thunk)
          args)
      (aop:dflet ((do-with-trampoline (thunk)
                    (trampoline thunk))
                  (trampoline (new-fn &rest new-args)
                    (setq fn   new-fn
                          args new-args)
                    (throw 'trampoline nil)))
        (tagbody
         again
         (catch 'trampoline
           (return-from trampoline (apply fn args)))
         (go again))
        ))))

;; ----------------------------------------------------

(defun insert-sk (args)
  ;; Plant our callback %SK as the first argument in the lambda list.
  ;; But since we are inviting macro lambda list syntax with LAMBDA*,
  ;; DEFUN*, LABELS*, and FLET*, there might be a &WHOLE arg shown as
  ;; the first one. So we have to plant %SK after that when present.
  (if (eq '&whole (car args))
      `(&whole ,(cadr args) %sk ,@(cddr args))
    (cons '%sk args)))

(defmacro =lambda (args &rest body)
  ;; define an anonymous CPS function
  `(lambda* ,(insert-sk args)
     ,@body))

#+:LISPWORKS
(editor:setup-indent "=lambda"  1 2 8)

;; -----------------------------------------

(defun gen-def-macro (name =name)
  (let ((g!rest (gensym)))
    `(defmacro ,name (&rest ,g!rest)
       (list* ',=name '%sk ,g!rest))))

(defun get=sym (name)
  ;; be careful to get the correct package for the =name
  ;; this code assumes that NAME and =NAME are defined in the same package
  (intern (um:mkstr #\= name) (symbol-package name)))

(defmacro =defun (name args &rest body)
  ;; define a named CPS function
  (let ((=name (get=sym name)))
    `(progn
       ,(gen-def-macro name =name)
       (defun* ,=name ,(insert-sk args)
         ,@body))
    ))
    
#+:lispworks
(editor:setup-indent "=defun"  2 2 7)

;; ---------------------------------------------

(defun gen-method (=name body)
  (multiple-value-bind (combi args body)
      (if (symbolp (car body))
          (values (list (car body))
                  (cadr body)
                  (cddr body))
        ;; else
        (values ()
                (car body)
                (cdr body)))
    `(,=name ,@combi (%sk ,@args) ,@body)))
  
(defmacro =defgeneric (name args &rest options)
  (let ((=name  (get=sym name)))
    `(progn
       ,(gen-def-macro name =name)
       (defgeneric ,=name (%sk ,@args)
         ,@(mapcar (lambda (option)
                     (if (eq (car option) :method)
                         (gen-method :method (cdr option))
                       option))
                   options)))
    ))

(defmacro =defmethod (name &body body)
  ;; define a named CPS method
  (let ((=name  (get=sym name)))
    (if (fboundp =name)
        `(defmethod ,@(gen-method =name body))
      `(progn
         ,(gen-def-macro name =name)
         (defmethod ,@(gen-method =name body)))
      )))

#+:lispworks
(progn
  (editor:setup-indent "=defmethod" 2)
  (editor:setup-indent "=defgeneric" 2 2 4))

;; ----------------------------------------------------

(defun flet-setup (bindings)
  (let* ((names     (mapcar 'first bindings))
         (argss     (mapcar 'second bindings)) 
         (bodies    (mapcar 'cddr bindings))
         (=names    (mapcar (lambda (name)
                              (get=sym name))
                            names))
         (g!rest    (gensym))
         (macrolets (mapcar (lambda (name =name)
                              `(,name (&rest ,g!rest)
                                      (list* ',=name '%sk ,g!rest)))
                            names =names))
         (flets     (mapcar (lambda (=name args body)
                              `(,=name ,(insert-sk args)
                                       ,@body))
                            =names argss bodies)))
    (values macrolets flets)))

(defmacro =flet (bindings &rest body)
  (multiple-value-bind (macrolets flets)
      (flet-setup bindings)
    `(flet* ,flets
       (macrolet ,macrolets
         ,@body))))

(defmacro =labels (bindings &rest body)
  (multiple-value-bind (macrolets flets)
      (flet-setup bindings)
    `(macrolet ,macrolets
       (labels* ,flets
         ,@body))))

#+:LISPWORKS
(progn
  (editor:setup-indent "=flet" 1 nil nil 'flet)
  (editor:setup-indent "=labels" 1 nil nil 'flet))

;; -------------------------------------------------------

(defmacro =values (&rest retvals)
  ;; invoke a continuation. This should generally be in tail position
  `(funcall %sk ,@retvals))

(defun get=fn (fn)
  (if-let (hd (and (consp fn)
                   (car fn)))
      (case hd
        ((function quote)
         ;; be careful to get the correct package for the =name
         (let* ((sym (cadr fn))
                (pkg (symbol-package sym))
                (=fn (string (symb '= sym))))
           (setf =fn (or (find-symbol =fn pkg)
                         sym))
           `(,hd ,=fn)))
        (otherwise fn))
    ;; else
    fn))

(defmacro =funcall (fn &rest args)
  ;; invoke a CPS function
  (let ((=fn (get=fn fn)))
    `(funcall ,=fn %sk ,@args)))

(defmacro =apply (fn &rest args)
  (let ((=fn (get=fn fn)))
    ;; invoke a CPS function
    `(apply ,=fn %sk ,@args)))

(defmacro with-cps (&body body)
  ;; use around CPS code when the continuation %SK has already been
  ;; bound
  `(with-trampoline ,@body))

(defmacro with-cont (&body body)
  ;; for top level calling of CPS functions
  `(let ((%sk (=cont #'values)))
     (declare (ignorable %sk))
     (with-cps ,@body)))

;; --------------------------------------------------------------------------
;; NOTE: Keeping the continuation argument list simple (no &optional or &rest)
;; allows the LW Compiler to do TCO on the continuation forwarding calls.
;; DM/RAL 10/03/20
;;
;; Now moot. That was non-portable behavior, and we now use Trampolines.
;; DM/RAL 10/19/20

(defmacro =bind (args expr &body body)
  ;; It is expected that =BIND will always be used in tail position.
  `(let ((%sk (=cont (lambda* ,args ,@body))))
     ;; expr should return via =values
     ;; continuation will fire only one time
     ,expr))

(defmacro =future (args expr &body body)
  ;; =FUTURE can be cascaded and used in non-tail position, leaving a
  ;; trail of callback handlers
  `(let ((%sk (=fut (lambda* ,args ,@body))))
     ;; expr should return via =values
     ;; continuation can repeatedly fire
     ,expr))

#+:LISPWORKS
(progn
  (editor:setup-indent "=bind" 2 2 4)
  (editor:setup-indent "=future" 2 2 4))

;; ----------------------------------------------------------------

(defun do-wait (timeout errorp on-timeout fn cont)
  (let ((mbox  (mpcompat:make-mailbox)))
    (funcall fn (lambda (&rest args)
                  (mpcompat:mailbox-send mbox args)))
    (multiple-value-bind (ans ok)
        (mpcompat:mailbox-read mbox "In =WAIT" timeout)
      (if ok
          (apply cont ans)
        (if on-timeout
            (funcall on-timeout)
          (when errorp
            (error 'um:timeout))
          )))
    ))

(defmacro =wait ((args &key timeout errorp on-timeout) expr &body body)
  ;; A version of =BIND with blocking wait
  ;; This version allows ON-TIMEOUT to use =VALUES
  ;; We are blocking here, so no need for =CONT on the continuation.
  `(let ((%sk (lambda* ,args ,@body)))
     (do-wait ,timeout ,errorp
              ,(when on-timeout
                 `(lambda ()
                    ,on-timeout))
              (lambda (%sk)
                ,expr)
              %sk)))

#+:LISPWORKS
(editor:setup-indent "=wait" 2 2 4)

;; ------------------------------------------------------------------------
#|
(defmacro =nlet (name bindings &body body)
  ;; Allows to write tail-pure code, even for non-tail recursion using
  ;; =VALUES on return values, and splitting out the non-tail
  ;; recursions into cascaded =BIND clauses.
  (let ((=name  (get=sym name))
        (g!args (gensym)))
    `(macrolet ((,name (&rest ,g!args)
                  (list* ',=name '%sk ,g!args)))
       (progn ;; um:with-tail-pure-code
           (nlet ,=name ((%sk   #'values)
                         ,@bindings)
	     ,@body)))
    ))

(defmacro =nlet-tail (name bindings &body body)
  ;; Allows to write tail-pure code, even for non-tail recursion using
  ;; =VALUES on return values, and splitting out the non-tail
  ;; recursions into cascaded =BIND clauses.
  (let ((=name  (get=sym name))
        (g!args (gensym)))
    `(macrolet ((,name (&rest ,g!args)
                  (list* ',=name '%sk ,g!args)))
       (progn ;; um:with-tail-pure-code
           (nlet-tail ,=name ((%sk   #'values)
                              ,@bindings)
             ,@body)))
    ))
|#
#|
#+:LISPWORKS
(progn
  (editor:setup-indent "=nlet" 1 2 4)
  (editor:setup-indent "=nlet-tail" 1 2 4))
|#

(defmacro =tlet (name bindings &body body)
  ;; Allows to write tail-pure code, even for non-tail recursion using
  ;; =VALUES on return values, and splitting out the non-tail
  ;; recursions into cascaded =BIND clauses.
  (let ((=name  (get=sym name))
        (g!args (gensym)))
    `(macrolet ((,name (&rest ,g!args)
                  `(trampoline #',',=name %sk ,@,g!args)))
       (labels ((,=name ,(cons '%sk (mapcar 'car bindings)) ,@body))
         (let ((%sk #'values))
           (,name ,@(mapcar #'cadr bindings)))
         ))
    ))

;; ------------------------------------------------------------------------
#|
(defun tst (n)
  (progn
    (=nlet-tail iter ((n n))
      (if (< n 2)
          (=values 1)
        (=bind (t1)
            (iter (1- n))
          (=values (* n t1)))
        ))
    ;; (values)
    ))
(tst 5)

(let ((sav-k #'values)
      (sav-n 500))
  (block myblock
    (tagbody top
             (let ((k sav-k)
                   (n sav-n))
               (return-from myblock
                 (if (< n 2)
                     (funcall k 1)
                   (let ((k (lambda (t1)
                              (funcall k (* t1 n)))))
                     (setf sav-k k
                           sav-n (1- n))
                     (go top))
                   ))))))

(define-condition values-signal ()
  ((args  :reader values-signal-args :initarg :args)))

(defun retk (&rest args)
  (error (make-condition 'values-signal :args args)))

(defun fwdk (c)
  (apply 'funcall (values-signal-args c)))

(let ((sav-k #'values)
      (sav-n 5000))
  (block myblock
    (tagbody top
             (let ((k sav-k)
                   (n sav-n))
                   (return-from myblock
                     (handler-case
                         (if (< n 2)
                             (retk k 1)
                           (let ((k (lambda (t1)
                                      (retk k (* n t1)))))
                             (setf sav-k k
                                   sav-n (1- n))
                             (go top)))
                       (values-signal (c)
                         (block hndlr-block
                           (tagbody again
                                    (handler-case
                                        (return-from hndlr-block
                                          (fwdk c))
                                      (values-signal (cnew)
                                        (setf c cnew)
                                        (go again))))))
                       ))))))

(let ((sav-k #'values)
      (sav-n 15))
  (block myblock
    (tagbody top
             (let ((k sav-k)
                   (n sav-n))
               (handler-case
                   (return-from myblock
                     (if (< n 2)
                         (retk k 1)
                       (let ((k (lambda (t1)
                                  (let ((k (lambda (t2)
                                             (retk k (+ t1 t2)))))
                                    (setf sav-k k
                                          sav-n (- n 2))
                                    (go top)))))
                         (setf sav-k k
                               sav-n (1- n))
                         (go top))))
                 (values-signal (c)
                   (nlet-tail iter ((c c))
                     (handler-case
                         (return-from myblock (fwdk c))
                       (values-signal (cnew)
                         (iter cnew)))))
                 )))))

(=nlet-tail fact ((n 500))
  (if (< n 2)
      (=values 1)
    (=bind (t1)
        (fact (1- n))
      (=values (* n t1)))))

(=nlet fact ((n 500))
  (if (< n 2)
      (=values 1)
    (=bind (t1)
        (fact (1- n))
      (=values (* n t1)))))

(defun tst (n)
  (=nlet fact ((n n))
    (if (< n 2)
        (=values 1)
      (=bind (t1)
          (fact (1- n))
        (=values (* n t1))))))  
(tst 4000)

(=nlet-tail fib ((n  15))
  (if (< n 2)
      (=values 1)
    (=bind (t1)
        (fib (- n 1))
      (=bind (t2)
          (fib (- n 2))
        (=values (+ t1 t2))))))

(defun dummy (&rest args)
  args)

(defun tst (n)
    (block myblock
      (let ((sav-k #'values)
            (sav-n n))
        (tagbody
         top
         (let ((k sav-k)
               (n sav-n))
           (return-from myblock
             (if (< n 2)
                 (funcall k 1)
               (let ((k #'(lambda (t1)
                            (funcall k (* t1 n)))
                        ))
                 #|
                 (setf sav-k k
                       sav-n (1- n))
                 |#
                 (progn
                   (psetq sav-k k sav-n (1- n))
                   (go top)))
               )))))))

(defun tst (n)
  (progn ;; MACROLET ((FACT (&REST #1=#:G340116) (LIST* '=FACT '%SK #1#)))
    (PROGN
      (progn
        ;; MACROLET ((=FACT #2=(#:G340123 #:G340124)
        ;;            `(PROGN (PSETQ ,@(MAPCAN 'LIST '(#3=#:G340125 #4=#:G340126) (LIST . #2#))) #10=(GO #5=#:N340122))))
        (BLOCK #6=#:B340121
          (LET ((#3=#:G340125 #'VALUES) (#4=#:G340126 N))
            (TAGBODY
             #5=#:N340122
             (LET ((%SK #3#) (N #4#))
               (RETURN-FROM #6#
                 (PROGN
                   (IF (< N 2)
                       (FUNCALL %SK 1)
                     (LET ((%SK
                            #'(LAMBDA (T1) ;; (&OPTIONAL T1 &REST #7=#:IGNORED)
                                ;; (DECLARE (IGNORE #7#))
                                (FUNCALL %SK (* T1 N)))))
                       (PROGN
                         (PROGN
                           (LET* ()
                             (LET ((#8=#:|Store-Var-340132| %SK))
                               (LET* ()
                                 (LET ((#9=#:|Store-Var-340133| (1- N)))
                                   (PROGN (SETQ #3# #8#) (SETQ #4# #9#))))))
                           NIL)
                         (GO #5#))))))))))))))

(defun tst (n)
  (=nlet-tail fact ((n n))
    (if (< n 2)
        (=values 1)
      (=bind (t1)
          (fact (1- n))
        (=values (* t1 n))))))

(defun tst (n)
  (=nlet fact ((n n))
    (if (< n 2)
        (=values 1)
      (=bind (t1)
          (fact (1- n))
        (=values (* t1 n))))))

(tst 5000)
(ac:spawn (lambda ()
            (print (tst 5000))))

(defun tst (n)
  (nlet fact-aux ((k  #'values)
                  (n  n))
    (if (< n 2)
        (funcall k 1)
      (let ((k (lambda (t1)
                 (funcall k (* n t1)))))
        (fact-aux k (1- n)))
      )))

(defun tst (n)
  (nlet-tail fact-aux ((k  #'values)
                       (n  n))
    (if (< n 2)
        (funcall k 1)
      (let ((k (lambda (t1)
                 (funcall k (* n t1)))))
        (fact-aux k (1- n)))
      )))

(defun tst (n)
  (=tlet fact-aux ((n  n))
    (if (< n 2)
        (=values 1)
      (=bind (t1)
          (fact-aux (1- n))
        (=values (* n t1)))
      )))

(time
 (dotimes (ix 10)
   (tst 50000)))
 |#

