#|
(defun collect-args (lst)
  ;; select out the arg symbols from compound arg lists
  (cond ((null lst)
         nil)
        ((consp lst)
         (cond ((eql (car lst) '&whole)
                (collect-args (cdr lst)))

               ((member (car lst) lambda-list-keywords)
                (mapcan (lambda (item)
                          ;; collect just the first symbol in the following compound arg lists
                          (cond ((member item lambda-list-keywords)
                                 nil)
                                ((symbolp item)
                                 (list item))
                                ((consp item)
                                 (list (car item)))
                                (t
                                 (error "Unexpected simple arg: ~A" item))
                                ))
                        (cdr lst)))
               (t
                ;; can just use map or dolist since we might have dotted list
                (nconc (collect-args (car lst))
                       (collect-args (cdr lst))))
               ))
        ((symbolp lst)
         (list lst))
        (t
         (error "Unexpected arglist element: ~A" lst))
        ))

#|
(collect-args '(a (b (c d . e) f) &optional (u x) &key (v z)))
 |#

(defun filter-decl-clause (clause args)
  ;; nil args selects out non-directed declarations, e.g., (SPEED 3), etc
  ;; return NIL or decl in list for mapcan
  (let* ((pos  (position-if 'symbolp clause :start (if (eql (car clause) 'type)
                                                    2
                                                  1)))
         (pre  (subseq clause 0 pos))
         (post (when pos
                 (subseq clause pos))))
    (cond (post
           (let ((new-post  (delete-if (complement (lambda (item)
                                                     (member item args)))
                                       post)))
             (when new-post
               (list (nconc pre new-post)))
             ))
          
          ((null args)
           (list pre))

          (t
           nil))))
  
(defun decls-for (decls args)
  ;; select out rebuilt decls pertaining to the args
  (mapcan (lambda (decl)
            (let ((clauses (mapcan (lambda (clause)
                                     (filter-decl-clause clause args))
                                   (cdr decl))))
              (when clauses
                `((declare ,@clauses)))
              ))
          decls))

#|
(let* ((decls '((declare (speed 3) (safety 1))
                (declare (fixnum a b)
                         (type consp c d)))))
  (decls-for decls nil)
  (decls-for decls '(a d)))
|#
#|
  (multiple-value-bind (docstr decls forms)
      (lambda-parsing:split-body body)
|#

(defun parse-xargs (args body)
  (multiple-value-bind (forms decls docstr)
      (alexandria:parse-body body)
    (multiple-value-bind (xargs pairs)
        (labels
            ((iter (args xargs pairs)
               (labels
                   ((ret ()
                      (values (nconc (nreverse xargs) args)
                              (nreverse pairs))))
                 (if (endp args)
                     (ret)
                   (destructuring-bind (hd . tl) args
                     (cond
                      ((consp hd)
                       (let ((sym (gensym)))
                         (iter tl (cons sym xargs) (acons sym hd pairs))))
                      
                      ((member hd lambda-list-keywords)
                       (ret))
                      
                      (t
                       (iter tl (cons hd xargs) pairs))
                      ))
                   ))))
          (iter args nil nil))
      ;; we need to string together all the list args for one single
      ;; destructuring to allow decls to work properly.
      (let* ((syms (mapcar 'car pairs))
             (lsts (mapcar 'cdr pairs))
             (nul-decls (decls-for decls nil))
             (top-decls (decls-for decls (collect-args xargs)))
             (bot-decls (mapcar (lambda (lst)
                                  (decls-for decls (collect-args lst)))
                                lsts)))
        
        (values xargs
                `(,@docstr
                  ,@nul-decls
                  (declare (list ,@syms))
                  ,@top-decls
                  ,@(gen-destr syms lsts bot-decls forms))
                )))))

(defun gen-destr (syms lsts decls body)
  (if (endp syms)
      body
    (let ((sym  (car syms))
          (lst  (car lsts))
          (decl (car decls)))
      `((destructuring-bind ,lst ,sym
         ,@decl
         ,@(gen-destr (cdr syms) (cdr lsts) (cdr decls) body))))))

(defun wrap-assembly (hd args body)
  (multiple-value-bind (xargs forms)
      (parse-xargs args body)
    `(,@hd ,xargs ,@forms)))
    
(defmacro lambda* (args &body body)
  (wrap-assembly `(lambda) args body))

(defmacro defun* (name args &body body)
  (wrap-assembly `(defun ,name) args body))

(defun wrap-bindings (hd bindings body)
  `(,hd ,(mapcar (lambda (binding)
                   ;; this is precisely why we invented LAMBDA* (not
                   ;; yet available here...)
                   (destructuring-bind (name args &rest body) binding
                     (wrap-assembly `(,name) args body)))
                 bindings)
        ,@body))
|#