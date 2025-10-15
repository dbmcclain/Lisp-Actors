
(in-package :ac)

(defun nil-beh ()
  (lambda (γ &rest msg)
    (list-match msg
      ((:null)
       (send γ t))
      ((:type-of)
       (send γ #'nil-beh))
      ((:iter _ acc)
       (send γ acc))
      )))

(defparameter $nil (make-actor (nil-beh)))

(defun cons-beh (car $cdr)
  (lambda (γ &rest msg)
    (list-match msg
      ((:null)
       (send γ nil))
      ((:type-of)
       (send γ #'cons-beh))
      ((:iter svc acc)
       (send svc γ car $cdr acc))
      )))

(defun $cons (hd $tl)
  (make-actor (cons-beh hd $tl)))

(defun as-$cons (cust lst)
  (send cust (reduce #'$cons lst
                     :from-end t
                     :initial-value $NIL)))

(defun as-list (cust $cons)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (beta (lst)
              (send $cdr beta :iter self acc)
            (send γ (cons car lst))))
        nil))
#|
(beta ($lst)
    (as-$cons beta '(a b c))
  (as-list println $lst))
|#
(defun $car (cust $cons)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (declare (ignore $cdr acc))
          (send γ car))
        $NIL))

(defun $cdr (cust $cons)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (declare (ignore car acc))
          (send γ $cdr))
        $NIL))

(defun $length (cust $cons)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (declare (ignore car))
          (send $cdr γ :iter self (1+ acc)))
        0))

(defun $revappend (cust $cons lst)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (send $cdr γ :iter self ($cons car acc)))
        lst))

(defun $reverse (cust $cons)
  ($revappend cust $cons $nil))

(defun $append (cust $cons lst)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (beta (ans)
              (send $cdr beta :iter self acc)
            (send γ ($cons car ans))))
        lst))

(defun $do (cust $cons svc)
  (send $cons cust :iter
        (actor (γ car $cdr acc)
          (send svc sink car)
          (send $cdr γ :iter self acc))
        $NIL))

(defun $sequentially (cust $cons svc)
  (um:letrec ((k-seq (actor (γ car $cdr acc)
                       (beta _
                           (send svc beta car)
                         (send $cdr γ :iter k-seq acc)))
                     ))
    (send $cons cust :iter k-seq $NIL)))

(defun $map (cust $cons xform)
  (um:letrec ((k-map (actor (γ car $cdr $acc)
                       (beta (new-car $ans)
                           (send (fork xform $cdr) beta
                                 `(,car)
                                 `(:iter ,k-map ,$acc))
                         (send γ ($cons new-car $ans))
                         ))
                     ))
  (send $cons cust :iter k-map $NIL)))

(defun $some (cust $cons pred)
  (send $cons cust :iter (actor (γ car $cdr acc)
                           (if (funcall pred car)
                               (send γ t)
                             (send $cdr γ :iter self acc)))
        nil))

(defun $notany (cust $cons pred)
  (not ($some cust $cons pred)))

(defun $not-every (cust $cons pred)
  ($some cust $cons (complement pred)))

(defun $every (cust $cons pred)
  (not ($not-every cust $cons pred)))

(defun $member (cust $cons item)
  ($some cust $cons (lambda (x)
                      (eql x item))))

(defun $nth (cust $cons n)
  (send $cons cust :iter (actor (γ car $cdr acc)
                           (if (zerop n)
                               (send γ car)
                             (progn
                               (decf n)
                               (send $cdr γ :iter self acc))))
        $NIL))

(defun $nth-cdr (cust $cons n)
  (send $cons cust :iter (actor (γ car $cdr acc)
                           (declare (ignore car))
                           (if (zerop n)
                               (send γ $cdr)
                             (progn
                               (decf n)
                               (send $cdr γ :iter self acc))))
        $NIL))

(defun $split (cust $cons pos)
  (let ((n  0))
    (send $cons cust :iter (actor (γ car $cdr acc)
                             (beta (ans)
                                 (cond ((>= n pos)
                                        (send γ (list $NIL $cdr)))
                                       (t
                                        (incf pos)
                                        (send $cdr beta :iter self acc)))
                               (send γ (list ($cons car (car ans)) (cadr ans)))
                               ))
          (list $NIL $NIL))
    ))

(defun $subseq (cust $cons start &optional end)
  (beta ($lst)
      ($nth-cdr $cons beta start)
    (if end
        (let ((pos 0))
          (beta ($lstx)
              (send $lst beta :iter
                    (actor (γ car $cdr $acc)
                      (beta ($ans)
                          (cond ((>= pos (- end start))
                                 (send γ $NIL))
                                (t
                                 (incf pos)
                                 (send $cdr beta :iter self $acc)))
                        (send γ ($cons car $ans))))
                    $NIL)
            (send cust $lstx)))
      ;; else
      (send cust $lst))
    ))

#|
(defun $remove-if (cust $cons pred &key from-end count key start end)
  (let ((key   (or key #'identity))
        (mid
        (start (or start 0))
        (pos   0)
        (stop  0))
    (if end
  (cond (count
         (if from-end
             (send $cons cust :iter (actor (γ car $cdr acc)
                                      (beta (ans)
                                          (send $cdr cust :iter self acc)
                                        (send γ (if (funcall pred (funcall key car))
                                                    ans
                                                  ($cons car ans)))))
                   $NIL))

(defun $remove (cust $cons item &key test from-end count key start end)
  ($remove-if cust $cons item (lambda (x)
                                (if test
                                    (funcall test x)
                                (funcall (or test
                                             #'eql)
                                         x item))
              :from-end from-end
              :count    count
              :key      key
              :start    start
              :end      end))
|#


  





(defun messenger-beh (msg)
  (lambda (dest)
    (send* dest msg)))

(defun messenger (&rest msg)
  (make-actor (messenger-beh msg)))

(defun $send-to-all ($cons &rest msg)
  ($do sink $cons (apply #'messenger msg)))

  
#|
(let ((lst ($cons 1 ($cons 2 ($cons 3 $NIL)))))
  (as-list println lst)
  (beta (ans)
      ($map beta lst (actor (γ x)
                       (send γ (1+ x))))
    (as-list println ans)))
 |#

#|
(defun as-list (cust cons)
  (send cons cust :foldr #'cons nil))
|#
#|
(defun as-list (cust cons)
  (send cons cust :sfoldr
        (actor (cust item acc)
          (send cust (cons item acc)))
        nil))
|#
#|
(defun $length (cust cons)
  (send cons cust :foldl
        (lambda (item acc)
          (declare (ignore item))
          (1+ acc))
        0))

(defun $reverse (cust cons)
  (send cons cust :foldl #'$cons $nil))

(defun $revappend (cust cons lst)
  (send cons cust :foldl #'$cons lst))

(defun $append (cust cons lst)
  (send cons cust :foldr #'$cons lst))
|#

#|
(defun $do (cons svc)
  (send cons sink :foldl
        (lambda (item acc)
          (declare (ignore acc))
          (send svc item))
        nil))

(defun $sequentially (cust cons svc)
  (send cons cust :sfoldl svc nil))
|#

;; Looks like we don't need anything more than FOLD and MAP

#|
(beta (lst)
    ($reverse beta ($cons 'a ($cons 'b ($cons 'c $nil))))
  (beta (ans)  ;; unpredictable print order unless we overtly sequence here
      (as-list beta lst)
    (send println ans)
    ($length println lst)))

(beta (lst)
    ($append beta ($cons 'a ($cons 'b ($cons 'c $nil)))
             ($cons 'd ($cons 'e ($cons 'f $nil))))
  (beta (ans)  ;; unpredictable print order unless we overtly sequence here
      (as-list beta lst)
    (send println ans)
    ($length println lst)))
|#
#|
(beta (lst)
    (send $nil beta :cons 'a)
  (beta (lst)
      (send lst beta :cons 'b)
    (beta (lst)
        (send lst beta :cons 'c)
      (beta (lst)
          (send lst beta :reverse)
        (as-list println lst)
        (send lst println :length)))))
|#

#|

  --{fb}--{a}--{fc}--{b}--{fg}--{c}--> g
 |#
#|
(a b c)
(label (a (label (b (label (c cust))))))

(let ((elts  '()))
  (reduce (lambda (dst acc)
            `(label ,dst ,acc))
          elts
          :from-end t
          :initial-value 'cust))

(defun pipe (&rest elts)
  (make-actor
   (lambda (cust &rest msg)
     (send* (reduce (lambda (dst acc)
                      (label dst acc))
                    elts
                    :from-end t
                    :initial-value cust)
            msg))))

(defun working-pipe-beh (cust elts)
  (lambda (&rest ans)
    (cond ((cdr elts)
           (send* (car elts) self ans)
           (become (working-pipe-beh cust (cdr elts))))
          (t
           (send* (car elts) cust ans))
          )))

(defun pipe (&rest elts)
  (cond ((cdr elts)
         (actor (cust &rest msg)
           (send* (car elts) self msg)
           (become (working-pipe-beh cust (cdr elts)))
           ))
        (elts  (car elts))
        (t     (fwd cust))
        ))
|#
#|
(let ((a  (actor (cust &rest msg)
            (send println "from a")
            (send* cust msg)))
      (b  (actor (cust &rest msg)
            (send println "from b")
            (send* cust msg)))
      (c  (actor (cust &rest msg)
            (send println "from c")
            (send* cust msg))))
  (send (pipe a b c) println "hello"))

  (send (pipe (collect (median (timing 1000 test)
                               3))
              (tee (plot)
                   (statistics)))
        println 1000)
        
|#
