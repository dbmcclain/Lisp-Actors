
(defun nil-beh ()
  (um:dlambda
    (:null ()    t)
    (:type-of () #'nil-beh)
    (:iter (_ acc)
     (declare (ignore _))
     acc)))

(defparameter $nil (nil-beh))

(defun $cons (car $cdr)
  (um:dlambda
    (:null ()    nil)
    (:type-of () #'$cons)
    (:iter (fn acc)
     (funcall fn car $cdr acc))
    ))

(defun as-$cons (lst)
  (reduce #'$cons lst
          :from-end t
          :initial-value $nil))

(defun as-list ($cons)
  (um:letrec ((k-cont  (lambda (car $cdr acc)
                         (cons car (funcall $cdr :iter k-cont acc)))))
    (funcall $cons :iter k-cont nil)))

#|
(let* ((lst  '(a b c))
       ($lst (as-$cons lst)))
  (print lst)
  (print $lst)
  (print (as-list $lst))
  (print ($length $lst)))
 |#

(defun $car ($cons)
  (funcall $cons :iter (lambda (car $cdr acc)
                         (declare (ignore $cdr acc))
                         car)
           nil))

(defun $cdr ($cons)
  (funcall $cons :iter (lambda (car $cdr acc)
                         (declare (ignore car acc))
                         $cdr)
           nil))

(defun $length ($cons)
  (um:letrec ((k-cont (lambda (car $cdr acc)
                        (declare (ignore car))
                        (funcall $cdr :iter k-cont (1+ acc)))
                      ))
    (funcall $cons :iter k-cont 0)))

(defun $revappend ($cons $lst)
  (um:letrec ((k-cont (lambda (car $cdr acc)
                         (funcall $cdr :iter k-cont ($cons car acc)))
                      ))
    (funcall $cons :iter k-cont $lst)))

(defun $reverse ($cons)
  ($revappend $cons $nil))

(defun $append ($cons $lst)
  (um:letrec ((k-cont (lambda (car $cdr acc)
                        ($cons car (funcall $cdr :iter k-cont acc)))
                      ))
    (funcall $cons :iter k-cont $lst)))

(defun $do ($cons fn)
  (um:letrec ((k-cont (lambda (car $cdr acc)
                        (funcall fn car)
                        (funcall $cdr :iter k-cont acc))
                      ))
    (funcall $cons :iter k-cont $nil)))

(defun $map ($cons fn)
  (um:letrec ((k-cont (lambda (car $cdr $acc)
                        ($cons (funcall fn car)
                               (funcall $cdr :iter k-cont $acc)))
                      ))
    (funcall $cons :iter k-cont $nil)))

(defun $member ($cons x)
  (um:letrec ((k-cont (lambda (car $cdr acc)
                        (or (eql x car)
                            (funcall $cdr :iter k-cont acc)))
                      ))
  (funcall $cons :iter k-cont $nil)))
#|
(let* (($lst  (as-$cons '(1 2 3)))
       ($lstm ($map $lst #'1+)))
  (print (as-list $lst))
  (print ($length $lst))
  (print (as-list $lstm))
  ($do $lstm #'print)
  (print (as-list ($reverse $lstm)))
  ($member $lstm 3))
|#
