
(in-package :cps)

(define-symbol-macro =bind-cont %sk)
(define-symbol-macro =wait-cont %sk)

;; ----------------------------------------------------------
    
(define-condition value-return ()
  ((args  :reader value-return-args :initarg :args)
   ))

(defun =values (&rest args)
  (error (make-condition 'value-return
                         :args args)))

(defun expr-form (&rest forms)
  `(handler-case
       (progn
         ,@forms)
     (value-return (c)
       (apply %sk (value-return-args c)))
     ))

(defmacro with-cont (&body body)
  `(let ((%sk  #'values))
     ,(apply 'expr-form body)))

;; ----------------------------------------------------------
    
(defun insert-sk (args)
  (if (eq '&whole (car args))
      `(&whole ,(cadr args) %sk ,@(cddr args))
    (cons '%sk args)))

(defun gen=lambda (hd args &rest body)
  ;; define an anonymous CPS function
  `(,hd ,(insert-sk args)
        ,@body))

(defmacro =lambda (args &body body)
  (apply 'gen=lambda 'lambda args body))

(defmacro =lambda* (args &body body)
  ;; define an anonymous CPS function
  (apply 'gen=lambda 'lambda* args body))


#+:LISPWORKS
(progn
  (editor:setup-indent "=lambda"  1 2 8)
  (editor:setup-indent "=lambda*" 1 2 8))

;; -----------------------------------------

(defun gen=defun (hd name args &rest body)
  ;; define a named CPS function
  (let ((fn     (symb '= name))
        (g!rest (gensym)))
    `(progn
       (defmacro ,name (&rest ,g!rest)
         (list* ',fn '%sk ,g!rest))
       (,hd ,fn ,(insert-sk args)
            ,@body))
    ))
    
(defmacro =defun (name args &body body)
  (apply 'gen=defun 'defun name args body))

(defmacro =defun* (name args &body body)
  (apply 'gen=defun 'defun* name args body))


#+:lispworks
(progn
  (editor:setup-indent "=defun"  2)
  (editor:setup-indent "=defun*" 2))

;; ---------------------------------------------

(defmacro =defmethod (name args &body body)
  ;; define a named CPS method
  (let ((fn     (symb '= name))
        (g!rest (gensym)))
    (if (fboundp fn)
        `(defmethod ,fn (%sk ,@args)
           ,@body)
      `(progn
         (defmacro ,name (&rest ,g!rest)
           (list* ',fn '%sk ,g!rest))
         (defmethod ,fn (%sk ,@args)
           ,@body)))
    ))

#+:lispworks
(editor:setup-indent "=defmethod" 2)

;; ----------------------------------------------------

(defun gen=flet (hd bindings &rest body)
  (let* ((names  (mapcar 'first bindings))
         (argss  (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (symb '= name))
                         names))
         (g!rest (gensym)))
    `(,hd ,(mapcar (lambda (fn args body)
                     `(,fn ,(insert-sk args)
                           ,@body))
                   fns argss bodies)
          (macrolet ,(mapcar (lambda (name fn)
                               `(,name (&rest ,g!rest)
                                       (list* ',fn '%sk ,g!rest)))
                             names fns)
            ,@body))))

(defmacro =flet (bindings &body body)
  (apply 'gen=flet 'flet bindings body))

(defmacro =flet* (bindings &body body)
  (apply 'gen=flet 'flet* bindings body))


(defun gen=labels (hd bindings &rest body)
  (let* ((names  (mapcar 'first bindings))
         (argss  (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (symb '= name))
                         names))
         (g!rest (gensym)))
    `(macrolet ,(mapcar (lambda (name fn)
                          `(,name (&rest ,g!rest)
                                  (list* ',fn '%sk ,g!rest)))
                        names fns)
       (,hd ,(mapcar (lambda (fn args body)
                       `(,fn ,(insert-sk args)
                             ,@body))
                     fns argss bodies)
            ,@body))))

(defmacro =labels (bindings &body body)
  (apply 'gen=labels 'labels bindings body))

(defmacro =labels* (bindings &body body)
  (apply 'gen=labels 'labels* bindings body))


#+:LISPWORKS
(progn
  (editor:setup-indent "=flet" 1 nil nil 'flet)
  (editor:setup-indent "=flet*" 1 nil nil 'flet)
  (editor:setup-indent "=labels" 1 nil nil 'flet)
  (editor:setup-indent "=labels*" 1 nil nil 'flet))

;; -------------------------------------------------------
#|
(defmacro =values (&rest retvals)
  ;; invoke a continuation. This should generally be in tail position
  `(funcall %sk ,@retvals))
|#

(defmacro =funcall (fn &rest args)
  ;; invoke a CPS function
  `(funcall ,fn %sk ,@args))

(defmacro =apply (fn &rest args)
  ;; invoke a CPS function
  `(apply ,fn %sk ,@args))

#|
(defmacro with-cont (&body body)
  ;; for top level calling of CPS functions
  `(let ((%sk #'values))
     ,@body))
|#
;; --------------------------------------------------------------------------
#|
(defun =basic-cont (fn)
  ;; make fn into a one-shot execute
  (lambda (&rest args)
    (when fn
      (apply (shiftf fn nil) args))))
|#
(defun =basic-cont (fn)
  fn)

(defun =cont (fn)
  ;; will be augmeted with advice in Actors package
  (=basic-cont fn))

(defun cont-fn (args body)
  (if (member '&rest args)
      `#'(lambda ,args
           ,@body)
    ;; else - as in multiple-value-bind
    `#'(lambda (&optional ,@args &rest #1=#:ignored)
         (declare (ignore #1#))
         ,@body)))

(defmacro =bind ((&rest args) expr &body body)
  `(let ((%sk (=cont ,(cont-fn args body))))
     ;; expr should return via =values
     ,(expr-form expr)))

#+:LISPWORKS
(editor:setup-indent "=bind" 2 2 4)

;; ----------------------------------------------------------------

(defun do-wait (timeout errorp on-timeout fn cont)
  (let ((mbox  (mp:make-mailbox)))
    (funcall fn (=basic-cont
                 (lambda (&rest args)
                   (mp:mailbox-send mbox args))))
    (multiple-value-bind (ans ok)
        (mp:mailbox-read mbox "In =WAIT" timeout)
      (if ok
          (apply cont ans)
        (if on-timeout
            (funcall on-timeout)
          (when errorp
            (error (make-condition 'timeout:timeout)))
          )))
    ))

(defun prep-wait (do-fn args timeout errorp on-timeout expr body)
  `(,do-fn ,timeout ,errorp ,(if on-timeout
                                  `(lambda ()
                                     ,on-timeout))
            (lambda (%sk)
              ,expr)
            ,(cont-fn args body)))

(defmacro =wait ((&rest args) (&key (timeout 60) (errorp t) on-timeout) expr &body body)
  ;; a version of =bind with blocking wait
  (prep-wait 'do-wait args timeout errorp on-timeout expr body))

#+:LISPWORKS
(editor:setup-indent "=wait" 3 2 4)

;; ------------------------------------------------------------------------

(defmacro =nlet-tail (name bindings &body body)
  ;; Allows to write tail-pure code, even for non-tail recursion using
  ;; =VALUES on return values, and splitting out the non-tail
  ;; recursions into cascaded =BIND clauses.
  (let ((=name  (symb '= name))
        (g!args (gensym)))
    `(macrolet ((,name (&rest ,g!args)
                  (list* ',=name '%sk ,g!args)))
       (with-cont
         (nlet-tail ,=name ((%sk   %sk)
                            ,@bindings)
           (declare (ignorable %sk))
           ,@body)))
    ))

(defmacro =nlet (name bindings &body body)
  ;; Allows to write tail-pure code, even for non-tail recursion using
  ;; =VALUES on return values, and splitting out the non-tail
  ;; recursions into cascaded =BIND clauses.
  (let ((=name  (symb '= name))
        (g!args (gensym)))
    `(macrolet ((,name (&rest ,g!args)
                  (list* ',=name '%sk ,g!args)))
       (with-cont
         (nlet ,=name ((%sk   %sk)
                       ,@bindings)
           (declare (ignorable %sk))
           ,@body)))
    ))

#+:LISPWORKS
(progn
  (editor:setup-indent "=nlet" 1 2 4)
  (editor:setup-indent "=nlet-tail" 1 2 4))

;; ------------------------------------------------------------------------
#|
;; why does this even work? nlet, instead of =nlet?
(with-cont
  (nlet fact ((n 5))
    (if (< n 2)
        (=values 1)
      (=bind (t1)
          (fact (1- n))
        (format t "~&n = ~A" n)
        (=values (* n t1))
        (print :should-not-reach))
      )))

(with-cont
  (=nlet fact ((n 5))
    (if (< n 2)
        (=values 1)
      (=bind (t1)
          (fact (1- n))
        (format t "~&n = ~A" n)
        (=values (* n t1))
        (print :should-not-reach))
      )))

(nlet fact ((n 3))
  (if (< n 2)
      1
    (* n (fact (1- n)))))

(=bind (x)
    (=values 15)
  (print x))
  
|#
