;; lambda-parsing.lisp -- common shared code for parsing lambda functions
;;
;; DM/RAL  08/20
;; ----------------------------------------------------------------------

(in-package :lambda-parsing)

(defun binding-names (bindings)
  (mapcar (lambda (binding)
            (or (and (consp binding)
                     (car binding))
                binding))
          bindings))

(defun binding-vals (bindings)
  (mapcar (lambda (binding)
            (and (consp binding)
                 (cadr binding)))
          bindings))
              
(defun split-body (body)
  ;; Separate the docstr, decls, and body forms.
  ;; Returns: -> (docstr) (decl*) (form*)
  ;;   - a list of one docstr, or nil,for @,docstr inclusion,
  ;;   - a list of decls, and
  ;;   - the body forms list
  (labels
      ((iter (body decls docstr)
         (labels ((ret ()
                    (values docstr (nreverse decls) body)))
           (if (endp body)
               (ret)
             (destructuring-bind (hd . tl) body
               (cond
                ((consp hd)
                 (cond
                  ((eql 'declare (car hd))
                   (iter tl (cons hd decls) docstr))
                  
                  (t
                   (ret))
                  ))
                
                ((and (stringp hd)
                      (null docstr))
                 (iter tl decls (list hd)))
                    
                (t ;; things like #F
                   (iter tl (cons hd decls) docstr))
                ))
             ))))
    (iter body nil nil)))

