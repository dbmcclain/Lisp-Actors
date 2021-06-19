
(in-package :ac)

(make-remote-actor "echo@rincon.local"
                   :register :RECHO)

(make-remote-actor "eval@rincon.local"
                   :register :REVAL)

(make-remote-actor "eval@arroyo.local"
                   :register :REVAL)

(make-remote-actor "eval@10.0.0.142"
                   :register :REVAL)

(loop repeat 5 do
      ;; remote EVAL, print result on our local printer
      (send :reval (ac:usti println) `(machine-instance)))

(let ((a (make-actor
          (lambda ()
            (print :hello)))))
  (send a))
(send println :hello)
(get-actor-names println)
(find-actor println :reval)
(beta (actor)
    (find-actor beta :reval)
  (inspect actor))

((λ f (λ x (f f x))) (λ f (λ x (f f x))))
(let* ((g  (lambda (rec-f)
             (lambda (n)
               (if (zerop n) 1
                 (* n (funcall (funcall rec-f rec-f) (1- n)))
                 ))))
       (fact (funcall (lambda (f)
                        (funcall g f))
                      (lambda (f)
                        (funcall g f)))))
  (funcall fact 5))

(setf fact (funcall (lambda (f) (lambda (n) (if (zerop n) 1 (* n (funcall (funcall f f) (1- n))))))
                    (lambda (f) (lambda (n) (if (zerop n) 1 (* n (funcall (funcall f f) (1- n))))))
                    ))
(funcall fact 5)



(defmacro self-recursive (name args &body body)
  (lw:with-unique-names (f!rec g!hrec)
    `(symbol-macrolet ((,f!rec (funcall ,g!hrec ,g!hrec)))
       (macrolet ((,name ,args
                    `(funcall ,',f!rec ,,@args)))
         (let ((,g!hrec (lambda (,g!hrec)
                          (lambda ,args
                            ,@body))))
           ,f!rec))
       )))

(let ((fact (self-recursive fact (n) (if (zerop n) 1 (* n (fact (1- n)))))))
  (funcall fact 5))

(funcall (self-recursive fact (n) (if (zerop n) 1 (* n (fact (1- n))))) 5)

(funcall (self-recursive fact (n acc) (if (zerop n) (progn (break) acc) (fact (1- n) (* n acc)))) 5 1)

(defun tst (n)
  (let ((fact (self-recursive fact (n acc)
                (if (zerop n)
                    (progn
                      (break)
                      acc)
                  (fact (1- n) (* n acc))))
              ))
    (funcall fact n 1)))

(tst 5)

;; --------------------------------------------------------------------

(defmacro mut-recursive (clauses &body body)
  (let* ((fnames (mapcar (lambda (clause)
                           (gensym))
                         clauses))
         (gnames (mapcar (lambda (clause)
                           (gensym))
                         clauses))
         (names  (mapcar #'car clauses))
         (fargs  (mapcar #'cadr clauses))
         (bodies (mapcar #'cddr clauses)))
    `(symbol-macrolet ,(mapcar (lambda (fname gname)
                                 `(,fname (funcall ,gname ,@gnames)))
                               fnames gnames)
       (macrolet ,(mapcar (lambda (name fname args)
                            `(,name ,args
                                    `(funcall ,',fname ,,@args)))
                          names fnames fargs)
         (let ,(mapcar (lambda (gname args body)
                         `(,gname (lambda ,gnames
                                    (lambda ,args
                                      ,@body))))
                       gnames fargs bodies)
           ,@body)))
    ))

(mut-recursive ((fna (x)
                     (if (zerop x)
                         1
                       (fnb (+ x 1))))
                (fnb (y)
                     (if (zerop y)
                         2
                       (fna (- y 2)))))
  (fna 15))