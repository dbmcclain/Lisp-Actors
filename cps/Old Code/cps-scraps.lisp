#|
(defmacro with-cont (&body body)
  ;; for REPL toplevel call to function defined with =defun
  (let ((g!block (gensym)))
    `(block ,g!block
       (let ((%sk (lambda (&rest args)
                    (return-from ,g!block (values-list args)))))
         ,@body))))
|#

;; -------------------------------------------------------------------------
;; In anticipation of Actors, where callback functions must be executed in
;; the context of the owning Actor.

#|
(defclass continuation-function ()
  ()
  (:metaclass #+:LISPWORKS clos:funcallable-standard-class
              #+:ALLEGRO   mop:funcallable-standard-class
              #+:CLOZURE   ccl:funcallable-standard-class
              #+:SBCL      sb-mop:funcallable-standard-class
              ))

(defmethod initialize-instance :after ((obj continuation-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   #+:CLOZURE   ccl:set-funcallable-instance-function
   #+:SBCL      sb-mop:set-funcallable-instance-function
   obj behavior))

(defun =cont-action (fn)
  ;; we will augment in Actors
  fn)

(defgeneric =cont (fn)
  (:method ((fn continuation-function))
   fn)
  (:method ((fn function))
   (=cont-action fn))
  (:method ((fn symbol))
   (=cont-action fn)))
|#
#|
(defmacro =bind (args expr &body body)
  `(multiple-value-call
    ,(if (member '&rest args)
         `(lambda ,args
            ,@body)
       ;; else - as in multiple-value-bind
       `(lambda (&optional ,@args &rest #1=#:ignored)
          (declare (ignore #1#))
          ,@body))
     (with-cont
       ,expr)))
|#

#|
(=nlet iter ((a lst)
            (b nil))
  (if (endp a)
      (=values b)
    (=bind (left)
        (iter (caar a) nil)
      (=bind (right)
          (iter (cdar a) nil)
        (iter (cdr a)
              (list* left right b))
        ))))

Equiv to:

(funcall (lambda (iter)
           (funcall iter #'values iter lst nil))
         (lambda (%sk iter a b)
           (if (endp a)
               (funcall %sk b)
             (funcall iter (lambda (left)
                             (funcall iter (lambda (right)
                                             (funcall iter %sk iter (cdr a) (list* left right b)))
                                      iter (cdar a) nil))
                      iter (caar a) nil))))
 |#
