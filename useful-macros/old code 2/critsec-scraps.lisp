(defun find-form (tree bag)
  ;; locate a form in the tree (a cons list) that
  ;; begins with one of the elements in the bag
  (um:nlet try-subtree ((subtree tree))
    (and (consp subtree)
         (or (member (car subtree) bag)
             (some #'try-subtree subtree))
         )))

(um:defmacro! defmonitor (name bindings &body body)
  (let* ((in-excl   "In Excl Monitor")
         (in-shared "In Shared Monitor")
         (vars      (binding-names bindings))
         (vals      (binding-vals bindings))                                                       
         (stbody    (if (find-form body '(static))
                        `((with-accessors ((,g!static  mon-parms-static)) ,name
                            (macrolet ((static (id data)
                                         `(critical-section
                                            (or (getf ,',g!static ,id)
                                                (setf (getf ,',g!static ,id) (load-time-value ,data))))))
                              ,@body)))
                      ;; else - no statics in body
                      body))
         (binds     (if bindings
                        `((symbol-macrolet ,(loop for pos from 0
                                                  for var in vars
                                                  collect
                                                  `(,var (svref (mon-parms-bindings ,name) ,pos)))
                            ,@stbody))
                      stbody)))
         
    (if (find-form body '(def-shared
                          lambda-shared
                          labels-shared
                          flet-shared
                          with-shared-access))
        `(locally
           (defvar ,name (make-mon-parms
                          :lock     (mp:make-lock :sharing t)
                          :bindings ,(when bindings `(vector ,@vals))))
           (with-accessors ((,g!lock  mon-parms-lock)) ,name
             (macrolet ((with-shared-access ((&optional timeout) &body body)
                          `(mp:with-sharing-lock (,',g!lock ,,in-shared ,timeout)
                             ,@body))
                        (with-exclusive-access ((&optional timeout) &body body)
                          `(mp:with-exclusive-lock (,',g!lock ,,in-excl ,timeout)
                             ,@body)))
               ,@binds)))
      ;; else - all are exclusive access
      `(locally
         (defvar ,name (make-mon-parms
                        :lock     (mp:make-lock)
                        :bindings ,(when bindings `(vector ,@vals))))
         (with-accessors ((,g!lock mon-parms-lock)) ,name
           (macrolet ((with-exclusive-access ((&optional timeout) &body body)
                        `(mp:with-lock (,',g!lock ,,in-excl ,timeout)
                           ,@body)))
             ,@binds)))
      )))
#|
(lw:defadvice (defun monitor-defun :around)
    (form env)
  (inspect env)
  (if (find 'in-monitor (slot-value env 'compiler::venv)
            :key (lambda (venv)
                   (slot-value venv 'compiler::name)))
      (destructuring-bind (def name args . body) form
        (lw:call-next-advice (wrap-def 'critical-section `(,def ,name ,args) body)
                             env))
    ;; else
    (lw:call-next-advice form env)))

(lw:remove-advice 'defun 'monitor-defun)
|#

;; -----------------------------------------------------
#|
(defun split-body (body)
  ;; split body into preamble and postamble,
  ;; collecting doc string and delcares into preamble
  (let ((pre nil))
    (um:nlet-tail iter ((lst body))
      (flet ((ret ()
               (values (nreverse pre) lst)))
        (if (endp lst)
            (ret)
          (destructuring-bind (hd . tl) lst
            (flet ((absorb ()
                     (push hd pre)
                     (iter tl)))
              (cond ((consp hd)
                     (cond ((eql 'declare (car hd))
                            (absorb))
                           (t
                            (ret))
                           ))
                    (t
                     (absorb))
                    ))))
        ))))

(defun wrap-def (wrapper hd body)
  ;; splice together a function form with its salient body wrapped
  (multiple-value-bind (pre post) (split-body body)
    `(,@hd ,@pre
           (,wrapper ()
                     ,@post))
    ))

(defun prep-bindings (wrapper bindings)
  ;; wrap the list of function bindings for flet and labels
  (mapcar (lambda (binding)
            (destructuring-bind (name args . body) binding
              (wrap-def wrapper `(,name ,args) body)))
          bindings))
|#
;; -----------------------------------------------------
;; These macros are valid only inside a DEFMONITOR region

#|
(defmacro def-shared (name args &body body)
  (wrap-def 'with-shared-access `(defun ,name ,args) body))

(defmacro lambda-shared (args &body body)
  (wrap-def 'with-shared-access `(lambda ,args) body))

(defmacro labels-shared (bindings &body body)
  `(labels ,(prep-bindings 'with-shared-access bindings)
     ,@body))

(defmacro flet-shared (bindings &body body)
  `(flet ,(prep-bindings 'with-shared-access bindings)
     ,@body))


(defmacro def-excl (name args &body body)
  (wrap-def 'with-exclusive-access `(defun ,name ,args) body))

(defmacro lambda-excl (args &body body)
  (wrap-def 'with-exclusive-access `(lambda ,args) body))

(defmacro labels-excl (bindings &body body)
  `(labels ,(prep-bindings 'with-exclusive-access bindings)
     ,@body))

(defmacro flet-excl (bindings &body body)
  `(flet ,(prep-bindings 'with-exclusive-access bindings)
     ,@body))
|#

#+:LISPWORKS
(progn
  (editor:setup-indent "def-excl" 2)
  (editor:setup-indent "def-shared" 2)
  (editor:setup-indent "lambda-excl" 1)
  (editor:setup-indent "lambda-shared" 1)
  (editor:setup-indent "labels-excl" 2)
  (editor:setup-indent "labels-shared" 2)
  (editor:setup-indent "flet-excl" 2)
  (editor:setup-indent "flet-shared" 2)
  )

(defun binding-names (bindings)
  (mapcar (lambda (binding)
            (if (consp binding)
                (first binding)
              binding))
          bindings))

(defun binding-vals (bindings)
  (mapcar (lambda (binding)
            (when (consp binding)
              (second binding)))
          bindings))

(um:defmacro! let-static (id bindings &body body)
  ;; only works properly when embedded in compiled code
  (let ((names (binding-names bindings))
        (vals  (binding-vals  bindings)))
    `(let ((,g!data  (load-time-value (vector ,@vals))))
       (symbol-macrolet ,(loop for name in names
                               for pos from 0
                               collect
                               `(,name (svref ,g!data ,pos)))
         ,@body))))
  
#+:LISPWORKS
(editor:setup-indent "let-static" 1)

;; ----------------------------------------------------------
#|
#+:LISPWORKS
(defun do-with-spinlock (cons fn)
  (unwind-protect
      (progn
        (do ()
            ((and (eq nil (car cons))
                  ;; Spin on fetch until we think we have a chance of
                  ;; succeeding with the CAS. This avoids excessive
                  ;; bus traffic.
                  (sys:compare-and-swap (car cons) nil mp:*current-process*))))
        (funcall fn))
    (sys:compare-and-swap (car cons) mp:*current-process* nil)))

#+:LISPWORKS
(um:defmacro! with-spinlock (cons &body body)
  `(flet ((,g!body ()
            ,@body))
     (declare (dynamic-extent #',g!body))
     (do-with-spinlock ,cons #',g!body)))

#+:LISPWORKS
(um:defmacro! defsponitor (clauses)
  `(let* ((,g!lock (list nil))
          (,g!lam  (lambda (&rest ,g!args)
                     (with-spinlock ,g!lock
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:LISPWORKS
(editor:setup-indent "DEFSPONITOR" 1 nil nil 'flet)
|#

;; ----------------------------------------------------------

#|
(defsponitor ()
  ((ensure-access ()
     (doit))
   (diddle-access (arg)
     (doit2 arg))))
==>
(LET* ((#:LOCK46671 (MP:MAKE-LOCK))
       (#:LAM46670  (LAMBDA (&REST #:ARGS46669)
                      (MP:WITH-LOCK (#:LOCK46671)
                        (DCASE #:ARGS46669
                          (ENSURE-ACCESS NIL
                                         (DOIT))
                          (DIDDLE-ACCESS (ARG)
                                         (DOIT2 ARG))
                          )))
                    ))
  (DEFUN ENSURE-ACCESS (&REST #:FARGS46668)
    (APPLY #:LAM46670 'ENSURE-ACCESS #:FARGS46668))
  (DEFUN DIDDLE-ACCESS (&REST #:FARGS46668)
    (APPLY #:LAM46670 'DIDDLE-ACCESS #:FARGS46668)))
|#

#|
#+:LISPWORKS
(um:defmacro! critical-section (&body body)
  ;; only works properly when embedded in compiled code
  `(let ((,g!lock  (load-time-value (mp:make-lock))))
     (mp:with-lock (,g!lock)
       ,@body)))

#+:CLOZURE
(um:defmacro! critical-section (&body body)
  `(let ((,g!lock  (load-time-value (ccl:make-lock))))
     (ccl:with-lock-grabbed (,g!lock)
        ,@body)))

#+:ALLEGRO
(um:defmacro! critical-section (&body body)
  `(excl:critical-section (:non-smp :without-interrupts)
                          ,@body))

#+(or sbcl)
(um:defmacro! critical-section (&body body)
  `(let ((,g!lock (load-time-value (sb-thread:make-mutex :name "Global critical section mutex lock"))))
     (sb-thread:with-recursive-lock (,g!lock)
        ,@body)))
|#

#|
(defun trim-lambda-args (args)
  (mapcan (lambda (arg)
            (cond
             ((member arg '(&optional &key &allow-other-keys &rest))
              nil)
             ((consp arg)
              (list (car arg)))
             (t
              (list arg))))
          args))

(defun parse-clause (clause)
  (destructuring-bind (def name args &rest body) clause
  (cond ((eql def 'defun)
         (values def name nil args body))
        ((eql def 'defmethod)
         (if (keywordp args)
             (values def name (list args) (car body) (cdr body))
           (values def name nil args body)))
        (t
         (error "unknown def-form: ~A" def))
        )))
               
(defun format-clauses-for-dcase (clauses)
    (mapcar (lambda (clause)
              (multiple-value-bind (def name kind args body)
                  (parse-clause clause)
                (declare (ignore def kind))
                `(,name ,(trim-lambda-args args)
                        ,@body)
                ))
            clauses))
      
(defun format-clauses-for-defun (g!lam clauses)
  (mapcar (lambda (clause)
            (multiple-value-bind (def fname kind fnargs body)
                (parse-clause clause)
              (declare (ignore body))
              (let ((args  (trim-lambda-args fnargs)))
                `(,def ,fname ,@kind ,fnargs
                       (funcall ,g!lam ',fname ,@args)))))
          clauses))

#+:LISPWORKS
(um:defmacro! defmonitor (&rest clauses)
  (let ((ro (find :read-only clauses
                  :key 'car)))
    (if ro
        (let ((rw  (remove ro clauses)))
          `(let* ((,g!lock (mp:make-lock :sharing t))
                  (,g!lam-ro (lambda (&rest ,g!args)
                               (mp:with-sharing-lock (,g!lock)
                                 (dcase ,g!args
                                   ,@(format-clauses-for-dcase (cdr ro))))
                               ))
                  (,g!lam  (lambda (&rest ,g!args)
                             (mp:with-exclusive-lock (,g!lock)
                               (dcase ,g!args
                                 ,@(format-clauses-for-dcase rw)))
                             )))
             ,@(format-clauses-for-defun g!lam rw)
             ,@(format-clauses-for-defun g!lam-ro (cdr ro))))
      ;; else
      `(let* ((,g!lock (mp:make-lock))
              (,g!lam  (lambda (&rest ,g!args)
                         (mp:with-lock (,g!lock)
                           (dcase ,g!args
                             ,@(format-clauses-for-dcase clauses)))
                         )))
         ,@(format-clauses-for-defun g!lam clauses)
         ))
    ))
;; #+:LISPWORKS
;; (editor:setup-indent "DEFMONITOR" 1 nil nil 'flet)

#+:ALLEGRO
(um:defmacro! defmonitor (&rest clauses)
  `(let ((,g!lam  (lambda (&rest ,g!args)
                    (excl:critical-section (:non-smp :without-interrupts)
                       (dcase ,g!args
                         ,@clauses)))
                  ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:CLOZURE
(um:defmacro! defmonitor (&rest clauses)
  `(let* ((,g!lock (ccl:make-lock))
          (,g!lam  (lambda (&rest ,g!args)
                     (ccl:with-lock-grabbed (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+(or sbcl)
(um:defmacro! defmonitor (&rest clauses)
  `(let* ((,g!lock (sb-thread:make-mutex))
          (,g!lam  (lambda (&rest ,g!args)
                     (sb-thread:with-recursive-lock (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))
|#

(defvar *X* nil)

(defun chk ()
  (print *X*))

(defun tst (*x*)
  ;; (declare (special *X*))
  (chk))
