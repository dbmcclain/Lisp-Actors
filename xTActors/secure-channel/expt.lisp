;; Composite Y-Combinator for SETF-less mutually recursive LETREC bindings.
;; Works only for function bindings. But in that case, just use LABELS.
;;
;; DM/RAL 01/24
;; ------------------------------------------------------------------------

(defun y (&rest fns)
  (let* ((gz (lambda (gx)
               (lambda (&rest gfns)
                 (apply gx (mapcar (lambda (gfn)
                                     (lambda (&rest a)
                                       (apply (apply gfn gfns) a)))
                                   gfns)) )) )
         (gfns (mapcar (lambda (fn)
                         (funcall gz fn))
                       fns)))
    (mapcar (lambda (gfn)
              (apply (lambda (&rest ggfns)
                       (apply gfn ggfns))
                     gfns))
            gfns)
    ))
    
#|
(defun y3 (f g h)
  (let* ((gz  (lambda (gx)
                (lambda (gf gg gh)
                  (funcall gx
                           (lambda (&rest a)
                             (apply (funcall gf gf gg gh) a))
                           (lambda (&rest a)
                             (apply (funcall gg gf gg gh) a))
                           (lambda (&rest a)
                             (apply (funcall gh gf gg gh) a))))) )
         (fns (list
               (funcall gz f)
               (funcall gz g)
               (funcall gz h))))
    (values
     (apply (lambda (gf gg gh)
              (funcall gf gf gg gh))
            fns)
     (apply (lambda (gf gg gh)
              (funcall gg gf gg gh))
            fns)
     (apply (lambda (gf gg gh)
              (funcall gh gf gg gh))
            fns))))

(multiple-value-bind (f g h)
    (y (lambda (f g h)
          (lambda (n)
            (format t "~%F: ~A" n)
            (if (< n 5)
                (funcall g (1+ n)))))
        (lambda (f g h)
          (lambda (n)
            (format t "~%G: ~A" n)
            (funcall h (1+ n))))
        (lambda (f g h)
          (lambda (n)
            (format t "~%H: ~A" n)
            (funcall f (1+ n)))))
  (funcall f 1))
|#

(defmacro xletrec (bindings &body body)
  (let ((names (mapcar #'first  bindings))
        (fns   (mapcar #'second bindings)))
    `(apply (lambda ,names
              ,@body)
            (y ,@(mapcar (lambda (fn)
                           `(lambda ,names
                              ,fn))
                         fns)))
    ))

(xletrec ((f  (lambda (n)
                (format t "~%F: ~A" n)
                (if (< n 5)
                    (funcall g (1+ n)))))
          (g  (lambda (n)
                (format t "~%G: ~A" n)
                (funcall h (1+ n))))
          (h  (lambda (n)
                (format t "~%H: ~A" n)
                (funcall f (1+ n)))))
  (funcall f 1))


(labels ((f (n)
           (format t "~%F: ~A" n)
           (if (< n 5)
               (g (1+ n))))
         (g (n)
           (format t "~%G: ~A" n)
           (h (1+ n)))
         (h  (n)
           (format t "~%H: ~A" n)
           (f (1+ n))))
  (f 1))


(xletrec ((fact (lambda (n)
                  (if (> n 1)
                      (* n (funcall fact (1- n)))
                    1))))
  (funcall fact 7))

(labels ((fact (n)
           (if (> n 1)
               (* n (fact (1- n)))
             1)))
  (fact 7))

