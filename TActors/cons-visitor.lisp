
(in-package :ac)

;; use common function for inheritable behavior - delegation, instead of inheritance

(defparameter melech (make-actor
                      (make-sink-beh)))

(defun gatekeeper-beh (svc owner)
  (lambda (&rest msg)
    (list-match msg
      ((arg) when (eq arg owner)
       (become (make-sink-beh)))
      ( _
        (send* svc msg))
      )))

(defun security-tag ()
  (actor (cust obj)
    (send obj cust self)))

(defparameter secure-info-request    (security-tag))
(defparameter secure-info-gatekeeper (make-actor
                                      (gatekeeper-beh secure-info-request melech)))

;; ------------------------------------------------------------------

(defun nil-beh ()
  (lambda (γ &rest msg)
    (list-match msg
      ((tag) when (eq tag secure-info-request)
       (send γ 'nil-beh)))))

(defparameter $nil (make-actor (nil-beh)))

(defun cons-beh (car $cdr)
  (lambda (γ &rest msg)
    (list-match msg
      ((tag) when (eq tag secure-info-request)
       (send γ 'cons-beh car $cdr)))))

;; -------------------------------------------

(defun $cons (hd $tl)
  (make-actor (cons-beh hd $tl)))

(defun as-$cons (cust lst)
  (send cust (reduce #'$cons lst
                     :from-end t
                     :initial-value $NIL)))

#|
;; -----------------------------------------------------------------
;; --- Generic Function Dispatch on Actor Type and Function Name ---

(defparameter *gen-fns* nil)

(defun #1=lookup-fn (fn-name type)
  (let ((pair (assoc fn-name *gen-fns*)))
    (when pair
      (let ((sub-pair (assoc type (cdr pair))))
        (when sub-pair
          (return-from #1# (cdr sub-pair)))
        ))
    (error "Not found")))

(defun add-fn (fn-name type fn)
  (let ((pair (assoc fn-name *gen-fns*)))
    (if pair
        (setf (cdr pair) (acons type fn (cdr pair)))
      (setf *gen-fns* (acons fn-name (acons type fn nil) *gen-fns*))
      )))

(defun dispatch (fn-name cust obj &rest args)
  (beta (type &rest parms)
      (send secure-info-gatekeeper beta obj)
    (let ((fn (lookup-fn fn-name type)))
      (apply fn cust obj parms args))))

(defun as-list-nil (cust obj parms &rest args)
  (declare (ignore obj parms args))
  (send cust nil))

(defun as-list-cons (cust obj parms &rest args)
  (declare (ignore obj args))
  (beta (car)
      (if (actor-p (car parms))
          (as-list beta (car parms))
        (send beta (car parms)))
    (beta (cdr)
        (if (actor-p (cadr parms))
            (as-list beta (cadr parms))
          (send beta (cadr parms)))
      (send cust (cons car cdr))
      )))
(add-fn 'as-list 'nil-beh  #'as-list-nil)
(add-fn 'as-list 'cons-beh #'as-list-cons)

(defun as-list (cust $cons)
  (dispatch 'as-list cust $cons))
|#

(defmacro actor-typecase (obj &rest clauses)
  (lw:with-unique-names (parms gobj)
    `(let ((,gobj ,obj))
       (beta (&rest ,parms)
           (send secure-info-gatekeeper beta ,gobj)
         (um:dcase ,parms
           ,@clauses
           ,@(unless (find 't clauses :key #'car)
               `((t (&rest _)
                    (declare (ignore _))
                    (error "Unknown type: ~S" ,gobj))))
           )))
    ))

#+:LISPWORKS
(editor:indent-like "actor-typecase" 'typecase)

(defun as-list (cust $cons)
  (actor-typecase $cons
    (nil-beh ()
     (send cust nil))
    (cons-beh (car $cdr)
     (beta (car)
         (if (actor-p car)
             (as-list beta car)
           (send beta car))
       (beta (cdr)
           (if (actor-p $cdr)
               (as-list beta $cdr)
             (send beta $cdr))
         (send cust (cons car cdr))
         )))
    ))

#|
(defun as-list (cust $cons)
  (beta (type &rest parms)
      (send $cons beta)
    (ecase type
      (nil-beh  (send cust nil))
      (cons-beh (beta (car)
                    (if (actor-p (car parms))
                        (as-list beta (car parms))
                      (send beta (car parms)))
                  (beta (cdr)
                      (if (actor-p (cadr parms))
                          (as-list beta (cadr parms))
                        (send beta (cadr parms)))
                    (send cust (cons car cdr))
                    )))
      )))
|#

#|
(as-list println ($acons 'a 1 ($acons 'b 2 ($acons 'c 3 $nil))))
(beta ($lst)
    (as-$cons beta '(a b c))
  (as-list println $lst))
|#
(defun $car (cust $cons)
  (actor-typecase $cons
    (cons-beh (car _)
              (declare (ignore _))
              (send cust car))
    ))

(defun $cdr (cust $cons)
  (actor-typecase $cons
    (cons-beh (_ $cdr)
              (declare (ignore _))
              (send cust $cdr))))

(defun $null (cust $cons)
  (actor-typecase $cons
    (nil-beh  ()
              (send cust t))
    (cons-beh (&rest _)
              (declare (ignore _))
              (send cust nil))))

(defun $type (cust $cons)
  (beta (type &rest _)
      (send secure-info-gatekeeper beta $cons)
    (declare (ignore _))
    (send cust type)))

(defun $length (cust $cons)
  (actor-typecase $cons
    (nil-beh () (send cust 0))
    (cons-beh (_ $cdr)
              (declare (ignore _))
              (beta (ans)
                  ($length beta $cdr)
                (send cust (1+ ans))))
    ))

(defun $revappend (cust $cons lst)
  (actor-typecase $cons
    (nil-beh ()  (send cust lst))
    (cons-beh (car $cdr)
              ($revappend cust $cdr ($cons car lst)))
    ))

(defun $reverse (cust $cons)
  ($revappend cust $cons $nil))

#|
(defun $append (cust $cons lst)
  (beta (type &rest parms)
      (send $cons beta)
    (ecase type
      (nil-beh (send cust lst))
      (cons-beh
       ($append self (cadr parms) lst)
       (become (lambda (ans)
                 (send cust ($cons (car parms) ans)))))
      )))
|#

(defun $append (cust $cons lst)
  (actor-typecase $cons
    (nil-beh  () (send cust lst))
    (cons-beh (car $cdr)
              (beta (ans)
                  ($append beta $cdr lst)
                (send cust ($cons car ans))
                ))
    ))

(defstruct (%nil
            (:constructor %nil ())))

(defstruct (%cons
            (:constructor %cons (%car %cdr)))
  %car %cdr)

(defun as-%cons (cust lst)
  (funcall cust (reduce (lambda (elt ans)
                          (%cons elt ans))
                        lst
                        :from-end t
                        :initial-value (%nil))))

(defun %append (cust cons lst)
  (typecase cons
    (%nil (funcall cust lst))
    (%cons 
     (flet ((k-cont (ans)
              (funcall cust (%cons (%cons-%car cons) ans))))
       (%append #'k-cont (%cons-%cdr cons) lst)))
    ))

(defun $do (cust $cons svc)
  (actor-typecase $cons
    (nil-beh () (send cust))
    (cons-beh (car $cdr)
     (send svc sink car)
     ($do cust $cdr svc))
    ))

(defun $sequentially (cust $cons svc)
  (actor-typecase $cons
    (nil-beh () (send cust))
    (cons-beh (car $cdr)
              (beta _
                  (send svc beta car)
                ($sequentially cust $cdr svc)))
    ))

(defun $map (cust $cons xform)
  (actor-nlet $mapper ((cust  cust)
                       ($cons $cons))
    (actor-typecase $cons
      (nil-beh  () (send cust $NIL))
      (cons-beh (car $cdr)
                (beta (new-car $ans)
                    (send (fork xform $mapper) beta
                          `(,car)
                          `(,$cdr))
                  (send cust ($cons new-car $ans))
                  ))
      )))

(defun $some (cust $cons pred)
  (actor-typecase $cons
    (nil-beh  ()  (send cust nil))
    (cons-beh (car $cdr)
              (if (funcall pred car)
                  (send cust t)
                ($some cust $cdr pred)))
    ))

#|
(let ((lst ($cons 1 ($cons 2 ($cons 3 $nil)))))
  (beta (ans)
      ($map beta lst (actor (cust elt)
                       (send cust (1+ elt))))
    (as-list println ans)))

(let ((lst ($cons 1 ($cons 2 ($cons 3 $nil)))))
  (beta (ans)
      ($every beta lst #'numberp)
    (send println (if ans :yes :no))))

(let ((lst ($cons 1 ($cons 2 ($cons 3 $nil)))))
  (beta (ans)
      ($member beta lst 2)
    (send println (if ans :yes :no))))
|#

(defun $notany (cust $cons pred)
  (beta (ans)
      ($some beta $cons pred)
    (send cust (not ans))))

(defun $not-every (cust $cons pred)
  ($some cust $cons (complement pred)))

(defun $every (cust $cons pred)
  (beta (ans)
      ($not-every beta $cons pred)
    (send cust (not ans))))

(defun $member (cust $cons item &key (test #'eql) (key #'identity))
  ($some cust $cons (lambda (x)
                      (funcall test (funcall key x) item))
         ))

(defun $nth (cust $cons n)
  (check-type n (integer 0))
  (actor-typecase $cons
    (cons-beh (car $cdr)
              (if (zerop n)
                  (send cust car)
                ($nth cust $cdr (1- n))))
    ))

(defun $nth-cdr (cust $cons n)
  ($drop cust $cons n))

(defun $take (cust $cons n)
  (check-type n (integer 0))
  (actor-typecase $cons
    (nil-beh ()
             (if (zerop n)
                 (send cust $nil)
               (error "Invalid args")))
    (cons-beh (car $cdr)
              (if (zerop n)
                  (send cust $nil)
                (beta (ans)
                    ($take beta $cdr (1- n))
                  (send cust ($cons car ans))
                  )))
    ))

(defun $drop (cust $cons n)
  (check-type n (integer 0))
  (actor-typecase $cons
    (nil-beh ()
             (if (zerop n)
                 (send cust $nil)
               (error "Invalid args")))
    (cons-beh (_ $cdr)
              (declare (ignore _))
              (if (zerop n)
                  (send cust $cons)
                ($drop cust $cdr (1- n))))
    ))

(defun $split (cust $cons pos)
  (check-type pos (integer 0))
  (actor-typecase $cons
    (nil-beh ()
             (if (zerop pos)
                 (send cust $nil $nil)
               (error "Invalid arguments")))
    
    (cons-beh (car $cdr)
              (if (zerop pos)
                  (send cust $nil $cons)
                (beta (hd tl)
                    ($split beta $cdr (1- pos))
                  (send cust ($cons car hd) tl))))
    ))

#|
(beta (lst)
    (as-$cons beta '(1 2 3))
  (beta (hd tl)
      ($split beta lst 3)
    (beta (hlst)
        (as-list beta hd)
      (beta (tlst)
          (as-list beta tl)
        (send println (list hlst tlst)))
      )))
 |#
(defun $subseq (cust $cons start &optional end)
  ;; not strictly Lisp behavior... Lisp awlays returns copies of the sequence,
  ;; whereas, we are immutable and there seems no reason to force that behavior.
  (beta (tl)
      ($drop beta $cons start)
    (if end
        ($take cust tl (- end start))
      (send cust tl))))
#|
(beta (lst)
    (as-$cons beta '(1 2 3))
  (beta (seq)
      ($nth-cdr beta lst 3)
    (as-list println seq)))
|#

(defun $remove-if (cust $cons pred &key from-end count key start end)
  (beta (hd mid tl)
      (let ((cust beta))
        (if (or start end)
            (beta (hd tx)
                (if start
                    ($split beta $cons start)
                  (send beta $nil $cons))
              (beta (mid tl)
                  (if end
                      ($split beta tx (- end (or start 0)))
                    (send beta tx $nil))
                (send cust hd mid tl)))
          ;; else no start end
          (send cust $nil $cons $nil)))
    (let ((pred (if key
                    (lambda (x)
                      (funcall pred (funcall key x)))
                  pred)))
      (beta (ans)
          (if count
              (if from-end
                  (beta (_ ans)
                      (actor-nlet iter (($cons mid)
                                        (cust  beta)
                                        (count count))
                        (actor-typecase $cons
                          (nil-beh ()
                                   (send cust count $nil))
                          (cons-beh (car $cdr)
                                    (beta (new-count $new-cdr)
                                        (send iter beta $cdr count)
                                      (cond ((and (plusp new-count)
                                                  (funcall pred car))
                                             (send cust (1- new-count) $new-cdr))
                                            (t
                                             (send cust new-count ($cons car $new-cdr) ))
                                            )))))
                    (declare (ignore _))
                    (send beta ans))
                ;; else - not from-end
                (actor-nlet iter (($cons mid)
                                  (cust  beta)
                                  (count count))
                  (actor-typecase $cons
                    (nil-beh ()
                             (send cust $nil))
                    (cons-beh (car $cdr)
                              (cond ((and (plusp count)
                                          (funcall pred car))
                                     (send iter $cdr cust (1- count)))
                                    (t
                                     (beta (ans)
                                         (send iter $cdr beta count)
                                       (send cust ($cons car ans))))
                                    )))))
            ;; else - no count
            (actor-nlet iter (($cons mid)
                              (cust  beta))
              (actor-typecase $cons
                (nil-beh ()
                         (send cust $nil))
                (cons-beh (car $cdr)
                          (if (funcall pred car)
                              (send iter $cdr cust)
                            (beta (ans)
                                (send iter $cdr beta)
                              (send cust ($cons car ans)))
                            ))
                )))
        (beta (tl)
            ($append beta ans tl)
          ($append cust hd tl))
        ))))

(defun $remove (cust $cons item &key
                     test
                     key
                     from-end
                     count
                     start
                     end)
  ($remove-if cust $cons (um:rcurry (or test #'eql) item)
              :key      key
              :from-end from-end
              :count    count
              :start    start
              :end      end))

(defun $count-if (cust $cons pred &key key start end)
  (beta (mid)
      (beta (tl)
          (if start
              ($drop beta $cons start)
            (send beta $cons))
        (if end
            ($take beta tl (- end (or start 0)))
          (send beta tl)))
    (let ((pred (if key
                    (lambda (x)
                      (funcall pred (funcall key x)))
                  pred)))
      (actor-nlet iter (($cons mid)
                        (count 0))
        (actor-typecase $cons
          (nil-beh () (send cust count))
          (cons-beh (car $cdr)
                    (send iter $cdr
                          (if (funcall pred car)
                              (1+ count)
                            count)))
          ))
      )))

(defun $count (cust $cons item &key
                    test
                    key
                    start
                    end)
  ($count-if cust $cons (um:rcurry (or test #'eql) item)
              :key      key
              :start    start
              :end      end))

;; ---------------------------------------------------------
;; Association Lists - or 1Dicts (key val) pairs as CONS elements

(defun $acons (key val alst)
  ($cons ($cons key val) alst))

(defun $assoc (cust $alst item &key test key)
  (let* ((test (or test #'eql))
         (pred (if key
                   (lambda (x)
                     (funcall test (funcall key x) item))
                 (lambda (x)
                   (funcall test x item)))
               ))
    (actor-nlet iter (($cons $alst))
      (actor-typecase $cons
        (nil-beh ()
                 (send cust $NIL))
        (cons-beh (car $cdr)
                  (actor-typecase car
                    (cons-beh (acar _)
                              (declare (ignore _))
                              (if (funcall pred acar)
                                  (send cust car)
                                (send iter $cdr)))
                    ))
        ))
    ))

(defun $1lookup (cust $alst key)
  (beta (pair)
      ($assoc beta $alst key)
    (actor-typecase pair
      (nil-beh ()
               (send cust NIL))
      (cons-beh (_ $cdr)
                (declare (ignore _))
                (send cust $cdr))
      )))

(defun showme (title arg)
  (beta (repr)
      (as-list beta arg)
    (send println title repr)))

(defun $1add/replace (cust $alst key val)
  (beta (pair)
      ($assoc beta $alst key)
    (actor-typecase pair
      (nil-beh ()
               (send cust ($acons key val $alst)))
      (cons-beh _
                (declare (ignore _))
                (beta (new-alst)
                    ($remove beta $alst pair)
                  (send cust ($acons key val new-alst))
                  ))
      )))

#|
(defun $show-alist (cust $alst)
  (beta (lst)
      ($map beta $alst (actor (cust pair)
                         (beta (type &rest parms)
                             (send pair beta)
                           (declare (ignore type))
                           (send cust parms))))
    (as-list cust lst)))
|#
#|
(as-list println ($acons 'a 1 ($acons 'b 2 ($acons 'c 3 $nil))))

(beta (dict)
    ($1add/replace beta $nil 'a 1)
  (beta (dict)
      ($1add/replace beta dict 'b 2)
    (beta (dict)
        ($1add/replace beta dict 'a 3)
      (as-list println dict)
      ($1lookup println dict 'b))))
|#
;; ---------------------------------------------------------
;; 2Dicts - 1Dicts with 1Dict elements - 2 level keying

(defun $2lookup (cust $2dict key1 key2)
  (beta (sub)
      ($1lookup beta $2dict key1)
    (if sub
        ($1lookup cust sub key2)
      (send cust nil))))

(defun $2add/replace (cust $2dict key1 key2 val)
  (beta (sub)
      ($1lookup beta $2dict key1)
    (if sub
        (beta (new-sub)
            ($1add/replace beta sub key2 val)
          ($1add/replace cust $2dict key1 new-sub))
      (send cust ($acons key1
                         ($acons key2 val $nil)
                         $2dict))
      )))
#|
(defun $show-2dict (cust $2dict)
  (beta (lst)
      ($map beta $2dict
            (actor (cust pair)
              (beta (type &rest parms)
                  (send pair beta)
                (declare (ignore type))
                (beta (sublis)
                    ($show-alist beta (cadr parms))
                  (send cust (list* (car parms) sublis))
                  ))))
    (as-list println lst)))
|#
#|
(beta (dict)
    ($2add/replace beta $NIL 'a 1 'diddly)
  (beta (dict)
      ($2add/replace beta dict 'b 2 'doodad)
    (beta (dict)
         ($2add/replace beta dict 'a 3 'dolly)
      (as-list println dict)
      ($2lookup println dict 'b 2))))
|#

#|
(let ((dict ($acons 'a 1 ($acons 'b 2 ($acons 'c 3 $nil)))))
  (beta (pair)
      ($assoc beta dict 'd)
    (beta (type &rest parms)
        (send pair beta)
      (ecase type
        (nil-beh   (send println nil))
        (cons-beh  (send println (car parms) (cadr parms)))
        ))))
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
;; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;; -----------------------------------------------------------------------
#|
(defun tst-$append ($lst)
  (make-actor
   (lambda (cust niter)
     (declare (fixnum niter))
     (if (zerop niter)
         (send cust)
       (beta _
           ($append beta $lst ($cons 1 $nil))
         (send self cust (1- niter))))
     )))

(let ((lst (loop for ix from 0 below 10 collect ix)))
    (beta ($lst)
        (as-$cons beta lst)
      (beta ($lst)
          ($append beta $lst ($cons 1 $nil))
        (as-list println $lst))))

(let ((lst (loop for ix from 0 below 1000 collect ix)))
    (beta ($lst)
        (as-$cons beta lst)
      (send (timing (tst-$append $lst)) println 1)))

;;
;; $CONS is approx 160x slower than Lisp CONS
;;
(let ((lst (loop for ix from 0 below 1000000 collect ix)))
    (beta ($lst)
        (as-$cons beta lst)
      (send (med3 (timing (actor (cust)
                            ($append cust $lst ($cons 1 $nil)))))
                  (actor (ans)
                    (send println (* ans 1f-6))))))

(let ((lst (loop for ix from 0 below 1000000 collect ix)))
  (send (med3 (timing (actor (cust n)
                        (dotimes (ix n)
                          (append lst (cons 1 nil)))
                        (send cust))))
        (actor (ans)
          (send println (* ans 1f-6))) 200))

(defun make-$append-self-tst ($lst)
  #F
  (make-actor
   (lambda (cust niter)
     (declare (ignore niter))
     ($append cust $lst ($cons 1 $nil)))))

(let* ((niter 1)
       (npts  1000)
       (lst   (loop for ix from 0 below 1000 collect ix)))
  (beta ($lst)
      (as-$cons beta lst)
    (let ((dut   (simple-collector npts (* 1000 niter)
                                   (med3
                                    (timing
                                     (make-$append-self-tst $lst))))))
      (let ((act (actor (cust)
                   (beta (arr)
                       (send dut beta niter)
                     (send cust)
                     (send (histogram) arr)
                     (send (statistics) println arr)
                     ))))
        (send (timing act) println)))
    ))

(defun make-append-self-tst (lst)
  #F
  (make-actor
   (lambda (cust niter)
     (let (ans)
       (dotimes (ix niter)
         (setf ans (append lst (cons 1 nil))))
       (send sink ans)
       (send cust)))))

(let* ((niter 1000)
       (npts  1000)
       (lst   (loop for ix from 0 below 1000 collect ix))
       (dut   (simple-collector npts (* 1000 niter)
                                (med3
                                 (timing
                                  (make-append-self-tst lst))))))
  (let ((act (actor (cust)
               (beta (arr)
                   (send dut beta niter)
                 (send cust)
                 (send (histogram) arr)
                 (send (statistics) println arr)
                 ))))
    (send (timing act) println)))

(defun make-%append-self-tst (%lst)
  #F
  (make-actor
   (lambda (cust niter)
     (let (ans)
       (dotimes (ix niter)
         (setf ans (%append #'identity %lst (%cons 1 (%nil)))))
       (send sink ans)
       (send cust)))))

(let* ((niter 100)
       (npts  1000)
       (lst   (loop for ix from 0 below 1000 collect ix)))
  (flet ((k-cont (%lst)
           (let ((dut   (simple-collector npts (* 1000 niter)
                                          (med3
                                           (timing
                                            (make-%append-self-tst %lst)
                                            )))))
             (let ((act (actor (cust)
                          (beta (arr)
                              (send dut beta niter)
                            (send cust)
                            (send (histogram) arr)
                            (send (statistics) println arr)
                            ))))
               (send (timing act) println))
             )))
    (funcall #'as-%cons #'k-cont lst)))

(time
   (dotimes (ix 160)
     (progn
       (append lst (cons 1 nil))
       (values)))))
|#
;; -----------------------------------------------------------------------
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
